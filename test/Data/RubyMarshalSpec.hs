{-# LANGUAGE OverloadedStrings #-}

module Data.RubyMarshalSpec where


import Control.Applicative ((*>))
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict (evalStateT)
import Data.Attoparsec.ByteString (eitherResult)
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.ByteString (ByteString, readFile)
import Data.ByteString.Char8 (pack)
import Prelude hiding (readFile)
import Test.Hspec
import qualified Data.IntMap.Strict as Map


import Data.RubyMarshal


withFixture :: FilePath -> (ByteString -> IO ()) -> IO ()
withFixture path f = do
  bs <- readFile ("./test/fixtures/" ++ path)
  f bs
  return ()


parse :: Parser a -> ByteString -> Attoparsec.Result a
parse parser = Attoparsec.parse (evalStateT parser Map.empty)


testParse :: Parser a -> ByteString -> a
testParse p input =
  case eitherResult (parse p input) of
    Left msg -> error (msg ++ " with input: <" ++ show input ++ ">")
    Right x -> x


spec = do
  describe "long" $ do
    it "0" $
      testParse (version *> long) "\x04\x08i\NUL" `shouldBe` 0

    it "-1" $
      testParse (version *> long) "\x04\x08i\xFA" `shouldBe` -1

    it "3" $
      testParse (version *> long) "\x04\x08i\x08" `shouldBe` 3


  describe "ivar" $
    it "cat" $ withFixture "cat.dat" $ \fixture ->
      testParse (version *> ivar) fixture
        `shouldBe` IVar 1 (RString "cat") UTF8


  describe "string" $ do
    it "x" $
      testParse string "\"\x06x" `shouldBe` "x"

    it "xyz" $
      testParse string "\"\x08xyz" `shouldBe` "xyz"


  describe "symbol" $
    it ":foo" $
      testParse (version *> symbol) "\x04\b:\bfoo" `shouldBe` "foo"

  describe "hashEntry" $
    it "\"1234\" => 45" $
      testParse hashEntry "I\"\t1234\ACK:\ACKETi2" `shouldBe`
        (IVar 1 (RString "1234") UTF8, RFixnum 45)


  describe "hash" $ do
    it "1-entry hash, string keys" $ withFixture "hash.dat" $ \fixture ->
      testParse (version *> hash) fixture
        `shouldBe` RHash 1 [(IVar 1 (RString "xyz") UTF8, RFixnum 45)]

    it "2-entry hash, string keys" $ withFixture "hash2.dat" $ \fixture ->
      testParse (version *> hash) fixture
        `shouldBe` RHash 2
          [ (IVar 1 (RString "abc") UTF8, RFixnum 3)
          , (IVar 1 (RString "xyz") UTF8, RFixnum 4)
          ]

    it "1-entry hash with default" $ withFixture "hashDefault.dat" $ \fixture ->
      testParse (version *> hash) fixture
        `shouldBe` RHashDefault 1 (IVar 1 (RString "cat") UTF8)
          [(IVar 1 (RString "zzz") UTF8, RSymbol "foo")]


  describe "array" $ do
    it "[1]" $
      testParse array "[\x06i\x06" `shouldBe` RArray 1 [RFixnum 1]

    it "['cat', :dog]" $ withFixture "array.dat" $ \fixture ->
      testParse (version *> array) fixture
        `shouldBe` RArray 2
          [ IVar 1 (RString "cat") UTF8
          , RSymbol "dog"
          ]


  describe "marshal" $
    it "reads realistic Ruby data" $ withFixture "realistic.dat" $ \fixture ->
      testParse marshal fixture
        `shouldBe` RHash 1
          [ (IVar 1 (RString "results") UTF8, RHash 2
            [ (IVar 1 (RString "1234-5") UTF8, RFixnum 14)
            , (IVar 1 (RString "6789-0") UTF8, RFixnum 80)
            ])
          ]
