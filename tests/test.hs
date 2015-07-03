{-# LANGUAGE OverloadedStrings #-}
import Database.Genders as Genders

import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector as Vector
import Network.BSD (getHostName)
import Test.Hspec

import Data.List (intersect)

testFile :: FilePath
testFile = "examples/test.genders"

emptyFile :: FilePath
emptyFile = "examples/empty.genders"

-- No need to be exhaustive - we're not testing libgenders, merely that our
-- bindings to it work correctly.
main :: IO ()
main = hspec $ do
    describe "getSelfNode" $ do
      it "returns the same value as getHostName" $ do
        hostname <- getHostName
        db <- readDB emptyFile
        node <- getSelfNode db
        hostname `shouldContain` B8.unpack node

    describe "numNodes" $ do
      it "returns 0 when the database is empty" $ do
        db <- readDB emptyFile
        numNodes db `shouldBe` 0
      it "returns 10 on the test database" $ do
        db <- readDB testFile
        numNodes db `shouldBe` 10

    describe "nodes" $ do
      it "the number of nodes is numNodes" $ do
        db <- readDB testFile
        Vector.length (nodes db) `shouldBe` numNodes db

    describe "nodesByAttribute" $ do
      it "finds the same nodes as query" $ do
        db <- readDB testFile
        nodesByAttribute "z0_station" db `shouldBe` query "z0_station" db
      it "finds none of the same nodes as the opposite query" $ do
        db <- readDB testFile
        shouldSatisfy
          (Vector.toList (nodesByAttribute "z0_station" db) `intersect`
           Vector.toList (query (Neg "z0_station") db))
          null

    describe "attributes" $ do
      it "the number of attributes is numAttributes" $ do
        db <- readDB testFile
        Vector.length (attributes db) `shouldBe` numAttributes db
      it "None of the attributes have values" $ do
        db <- readDB testFile
        let hasNoValue = maybe True (const False) . attributeValue
        shouldSatisfy (attributes db) (Vector.all hasNoValue)

    describe "attributesByNode" $ do
      it "z0_all has no value" $ do
        db <- readDB testFile
        Genders.lookup "z0_all" (attributesByNode "pg155-n10" db) `shouldBe` Nothing

      it "finds the minimum receive queue length" $ do
        db <- readDB testFile
        Genders.lookup "z0_min_recv_q" (attributesByNode "pg155-n10" db) `shouldBe` Just "2"
