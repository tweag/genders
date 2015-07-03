-- |
-- Mostly pure interface to @libgenders@.
--
-- Most functions in this module are partial: they may throw 'GendersError'.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Genders
  ( DB
  , Attr(..)
  , attributeName
  , attributeValue
  , Query(..)
  , GendersError(..)
  , readDB
  , numNodes
  , numAttributes
  , maxAttributes
  , getSelfNode
  , nodes
  , nodesByAttribute
  , attributes
  , attributesByNode
  , query
  , lookup
  ) where

import Database.Genders.Internal

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Control.Applicative ((<$>))
import Control.Exception (Exception, throwIO)
import Control.Monad (join, when)
import Data.Function (on)
import Data.List (intercalate)
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import Foreign (alloca, allocaArray)
import Foreign.C (CInt, peekCString, withCString)
import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Storable (peek, peekElemOff)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding ((<$>),lookup)

-- Invariant: A DB always has its data pre-loaded into memory using
-- genders_load_data(). Therefore querying the DB does not incur side effects.
newtype DB = DB (ForeignPtr Handle)

-- | An attribute is either standalone or paired with a value. String literals
-- are overloaded when @-XOverloadedStrings@ is turned on, so that @"foo" ::
-- Attr@ is a legal expression (assumes ASCII encoding).
data Attr
  = Attr ByteString
  | ByteString :=: ByteString
  deriving (Eq, Generic, Typeable)

instance Show Attr where
  -- Can't assume any particular encoding, so assume each byte is a character.
  show (Attr a) = BS8.unpack a
  show (a :=: v) = intercalate "=" [BS8.unpack a, BS8.unpack v]

-- Enable implicit wrapping of strings with Attr.
instance IsString Attr where
  fromString x = Attr (BS8.pack x)

attributeName :: Attr -> ByteString
attributeName (Attr a) = a
attributeName (a :=: _) = a

attributeValue :: Attr -> Maybe ByteString
attributeValue (Attr _) = Nothing
attributeValue (_ :=: v) = Just v

-- | Embedded language of genders queries. See @genders_query(3)@ for the
-- meaning of each operator. Contrary to the syntax of queries found there,
-- operator precedence is the usual one for additive and multiplicative
-- operators in our language.
data Query = Atom Attr
           | Query :||: Query
           | Query :&&: Query
           | Query :--: Query
           | Neg Query
           deriving (Eq, Generic, Typeable)

infixl 7 :&&:
infixl 6 :||:
infixl 6 :--:

instance Show Query where
  show (Atom attr) = show attr
  show (q1 :||: q2) = "(" ++ intercalate "||" [show q1, show q2] ++ ")"
  show (q1 :&&: q2) = "(" ++ intercalate "&&" [show q1, show q2] ++ ")"
  show (q1 :--: q2) = "(" ++ intercalate "--" [show q1, show q2] ++ ")"
  show (Neg q) = "~" ++ show q

instance IsString Query where
  fromString x = Atom $ Attr $ BS8.pack x

data GendersError = GendersError
  { gendersErrorLocation :: String
  , gendersErrorMessage :: String
  } deriving (Show, Generic, Typeable)

instance Exception GendersError

withHandle :: ForeignPtr Handle -> (Handle -> IO a) -> IO a
withHandle fp f = withForeignPtr fp $ \ptr -> f (Handle ptr)

throwGendersErrno :: String -> Handle -> IO CInt -> IO Int
throwGendersErrno loc h f = do
    rc <- f
    when (rc == -1) $ do
      errcode <- genders_errnum h
      errstr <- genders_strerror errcode
      throwIO . GendersError loc =<< peekCString errstr
    return $ fromIntegral rc

-- | Load data from file.
readDB :: FilePath -> IO DB
readDB filepath = do
    h@(Handle ptr) <- genders_handle_create
    -- Register a finalizer to destroy the handle and get genders to reclaim
    -- memory automatically.
    fptr <- newForeignPtr genders_handle_destroy'funptr ptr
    when (ptr == nullPtr) $
        throwIO $ GendersError "new" $ "Cannot create new libgenders handle."
    _ <- withCString filepath $ \cstr ->
         throwGendersErrno "new" h (genders_load_data h cstr)
    return $ DB fptr

-- | The total number of nodes in the database.
numNodes :: DB -> Int
numNodes (DB fptr) = unsafePerformIO $ withHandle fptr $ \h ->
    fromIntegral <$> genders_getnumnodes h

-- | The total number of attributes in the database.
numAttributes :: DB -> Int
numAttributes (DB fptr) = unsafePerformIO $ withHandle fptr $ \h ->
    fromIntegral <$> genders_getnumattrs h

-- | The maximum number of attributes associated with any node.
maxAttributes :: DB -> Int
maxAttributes (DB fptr) = unsafePerformIO $ withHandle fptr $ \h ->
    fromIntegral <$> genders_getmaxattrs h

-- | The non fully qualified node name of the local host.
getSelfNode :: DB -> IO ByteString
getSelfNode (DB fptr) = withHandle fptr $ \h -> do
    maxlen <- throwGendersErrno "getSelfNode" h $ genders_getmaxnodelen h
    -- The local host might not be in the database, in which case we ought to
    -- have a minimum buffer size...
    let len = max maxlen 256
    allocaArray len $ \cstr -> do
      _ <- throwGendersErrno "getSelfNode" h $
           genders_getnodename h cstr (fromIntegral len)
      BS.packCString cstr

-- | All nodes in the database.
nodes :: DB -> Vector ByteString
nodes = nodes' "nodes" Nothing

-- | All nodes with the given attribute.
nodesByAttribute :: Attr -> DB -> Vector ByteString
nodesByAttribute a = nodes' "nodesByAttribute" (Just a)

nodes' :: String -> Maybe Attr -> DB -> Vector ByteString
nodes' loc mbattr (DB fptr) = unsafePerformIO $ withHandle fptr $ \h -> do
    alloca $ \ptr -> do
      n <- throwGendersErrno loc h $ genders_nodelist_create h ptr
      nodelist <- peek ptr
      num <- throwGendersErrno loc h $ case mbattr of
        Nothing -> genders_getnodes h nodelist (fromIntegral n) nullPtr nullPtr
        Just (Attr a) ->
          BS.useAsCString a $ \attr -> do
            genders_getnodes h nodelist (fromIntegral n) attr nullPtr
        Just (a :=: v) ->
          BS.useAsCString a $ \attr -> do
          BS.useAsCString v $ \val -> do
            genders_getnodes h nodelist (fromIntegral n) attr val
      result <- Vector.generateM num $ \i -> peekElemOff nodelist i >>= BS.packCString
      _ <- throwGendersErrno loc h $ genders_nodelist_destroy h nodelist
      return result

attributes :: DB -> Vector Attr
attributes = attributes' "attributes" Nothing

attributesByNode :: ByteString -> DB -> Vector Attr
attributesByNode node = attributes' "attributesByNode" (Just node)

attributes' :: String -> Maybe ByteString -> DB -> Vector Attr
attributes' loc mbnode (DB fptr) = unsafePerformIO $ withHandle fptr $ \h ->
    alloca $ \attrlistptr -> do
    alloca $ \vallistptr -> do
      nattr <- throwGendersErrno loc h $ genders_attrlist_create h attrlistptr
      attrlist <- peek attrlistptr
      nval <- throwGendersErrno loc h $ genders_vallist_create h vallistptr
      vallist <- peek vallistptr
      num <- throwGendersErrno loc h $ case mbnode of
        Nothing -> genders_getattr_all h attrlist (fromIntegral nattr)
        Just node ->
          BS.useAsCString node $ \nodestr -> do
            genders_getattr h attrlist vallist (fromIntegral nval) nodestr
      result <- Vector.generateM num $ \i -> do
        a <- peekElemOff attrlist i >>= BS.packCString
        peekElemOff vallist i >>= \val ->
          BS.packCString val >>= \v ->
          return $ if BS.null v then Attr a else a :=: v
      _ <- throwGendersErrno loc h $ genders_attrlist_destroy h attrlist
      _ <- throwGendersErrno loc h $ genders_attrlist_destroy h vallist
      return result

-- | Query the database. Example:
--
-- @
-- -- Determine the set of nodes that are not management or login nodes.
-- query (Neg ("mgmt" :||: "login"))
-- @
query :: Query -> DB -> Vector ByteString
query q db@(DB fptr) = unsafePerformIO $ withHandle fptr $ \h -> do
    alloca $ \ptr -> do
      let n = fromIntegral $ numNodes db
      _ <- throwGendersErrno "query" h $ genders_nodelist_create h ptr
      nodelist <- peek ptr
      withCString (show q) $ \cqstr -> do
        num <- throwGendersErrno "query" h $ genders_query h nodelist n cqstr
        result <- Vector.generateM num $ \i -> peekElemOff nodelist i >>= BS.packCString
        _ <- throwGendersErrno "query" h $ genders_nodelist_destroy h nodelist
        return result

-- | @lookup node attrname attrs@ returns the value of the first occurrence of
-- attribute name @attrname@ in @attrs@.
lookup :: ByteString                              -- ^ Attribute name
       -> Vector Attr                             -- ^ Sequence of attributes
       -> Maybe ByteString
lookup a =
    join . fmap attributeValue .
    Vector.find (on (==) attributeName (Attr a))
