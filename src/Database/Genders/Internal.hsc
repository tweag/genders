module Database.Genders.Internal where

import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <genders.h>

newtype Handle = Handle (Ptr Handle) deriving (Eq)

foreign import ccall unsafe genders_handle_create :: IO Handle
foreign import ccall unsafe genders_handle_destroy :: Handle -> IO ()

foreign import ccall unsafe genders_parse :: Handle -> CString -> Ptr CFile -> IO CInt

foreign import ccall unsafe genders_load_data :: Handle -> CString -> IO CInt
foreign import ccall unsafe genders_index_attrvals :: Handle -> CString -> IO CInt

foreign import ccall unsafe genders_errnum :: Handle -> IO CInt
foreign import ccall unsafe genders_strerror :: CInt -> IO CString
foreign import ccall unsafe genders_errormsg :: Handle -> IO CString
foreign import ccall unsafe genders_perror :: Handle -> CString -> IO ()

foreign import ccall unsafe genders_getnumnodes :: Handle -> IO CInt
foreign import ccall unsafe genders_getnumattrs :: Handle -> IO CInt
foreign import ccall unsafe genders_getmaxattrs :: Handle -> IO CInt
foreign import ccall unsafe genders_getmaxnodelen :: Handle -> IO CInt
foreign import ccall unsafe genders_getmaxattrlen :: Handle -> IO CInt
foreign import ccall unsafe genders_getmaxvallen :: Handle -> IO CInt

foreign import ccall unsafe genders_nodelist_create :: Handle -> Ptr (Ptr CString) -> IO CInt
foreign import ccall unsafe genders_nodelist_clear :: Handle -> Ptr CString -> IO CInt
foreign import ccall unsafe genders_nodelist_destroy :: Handle -> Ptr CString -> IO CInt

foreign import ccall unsafe genders_attrlist_create :: Handle -> Ptr (Ptr CString) -> IO CInt
foreign import ccall unsafe genders_attrlist_clear :: Handle -> Ptr CString -> IO CInt
foreign import ccall unsafe genders_attrlist_destroy :: Handle -> Ptr CString -> IO CInt

foreign import ccall unsafe genders_vallist_create :: Handle -> Ptr (Ptr CString) -> IO CInt
foreign import ccall unsafe genders_vallist_clear :: Handle -> Ptr CString -> IO CInt
foreign import ccall unsafe genders_vallist_destroy :: Handle -> Ptr CString -> IO CInt

foreign import ccall unsafe genders_getnodename :: Handle -> CString -> CInt -> IO CInt
foreign import ccall unsafe genders_getnodes :: Handle -> Ptr CString -> CInt -> CString -> CString -> IO CInt

foreign import ccall unsafe genders_getattr :: Handle -> Ptr CString -> Ptr CString -> CInt -> CString -> IO CInt
foreign import ccall unsafe genders_getattr_all :: Handle -> Ptr CString -> CInt -> IO CInt
foreign import ccall unsafe genders_testattr :: Handle -> CString -> CString -> CString -> CInt -> IO CInt
foreign import ccall unsafe genders_testattrval :: Handle -> CString -> CString -> CString -> IO CInt

foreign import ccall unsafe genders_isnode :: Handle -> CString -> IO CInt
foreign import ccall unsafe genders_isattr :: Handle -> CString -> IO CInt
foreign import ccall unsafe genders_isattrval :: Handle -> CString -> CString -> IO CInt

foreign import ccall unsafe genders_query :: Handle -> Ptr CString -> CInt -> CString -> IO CInt
foreign import ccall unsafe genders_testquery :: Handle -> CString -> CString -> IO CInt

foreign import ccall unsafe "&genders_handle_destroy" genders_handle_destroy'funptr
 :: FunPtr (Ptr Handle -> IO ())
