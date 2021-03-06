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


import Data.RubyMarshal hiding (parse)


withFixture :: FilePath -> (ByteString -> IO ()) -> IO ()
withFixture path f = do
  bs <- readFile ("./test/fixtures/" ++ path)
  f bs
  return ()


parse :: Parser a -> ByteString -> Attoparsec.Result a
parse parser input = Attoparsec.parse (evalStateT parser Map.empty) input


testParse :: Parser a -> ByteString -> a
testParse p input =
  case eitherResult (parse p input) of
    Left msg -> error (msg ++ " with input: <" ++ show input ++ ">")
    Right x -> x


rString :: ByteString -> RubyValue
rString s = IVar (RString s) [(RSymbol "E", RTrue)]


spec = do
  describe "long" $ do
    it "0" $
      testParse marshal "\x04\x08i\NUL" `shouldBe` RFixnum 0

    it "-1" $
      testParse marshal "\x04\x08i\xFA" `shouldBe` RFixnum (-1)

    it "3" $
      testParse marshal "\x04\x08i\x08" `shouldBe` RFixnum 3

    it "125" $ withFixture "01_number.dat" $ \fixture ->
      testParse marshal fixture `shouldBe` RFixnum 125

    it "-125" $ withFixture "ff_number.dat" $ \fixture ->
      testParse marshal fixture `shouldBe` RFixnum (-125)

    it "257" $ withFixture "02_number.dat" $ \fixture ->
      testParse marshal fixture `shouldBe` RFixnum 257

    it "-257" $ withFixture "fe_number.dat" $ \fixture ->
      testParse marshal fixture `shouldBe` RFixnum (-257)

    it "99,999" $ withFixture "03_number.dat" $ \fixture ->
      testParse marshal fixture `shouldBe` RFixnum 99999

    it "-99,999" $ withFixture "fd_number.dat" $ \fixture ->
      testParse marshal fixture `shouldBe` RFixnum (-99999)


  describe "float" $ do
    it "8.5" $ withFixture "8.5.dat" $ \fixture ->
      testParse marshal fixture `shouldBe` RFloat 8.5

    it "20.0" $ withFixture "20.0.dat" $ \fixture ->
      testParse marshal fixture `shouldBe` RFloat 20.0

  describe "ivar" $
    it "cat" $ withFixture "cat.dat" $ \fixture ->
      testParse marshal fixture
        `shouldBe` rString "cat"


  describe "nil" $
    it "is character '0'" $ 
      testParse marshal "\x04\b0"
        `shouldBe` RNil


  describe "string" $ do
    it "x" $
      testParse string "\"\x06x" `shouldBe` "x"

    it "xyz" $
      testParse string "\"\x08xyz" `shouldBe` "xyz"


  describe "symbol" $
    it ":foo" $
      testParse marshal "\x04\b:\bfoo" `shouldBe` RSymbol "foo"


  describe "hashEntry" $
    it "\"1234\" => 45" $
      testParse hashEntry "I\"\t1234\ACK:\ACKETi2" `shouldBe`
        (rString "1234", RFixnum 45)


  describe "hash" $ do
    it "1-entry hash, string keys" $ withFixture "hash.dat" $ \fixture ->
      testParse marshal fixture
        `shouldBe` RHash 1 [(rString "xyz", RFixnum 45)]

    it "2-entry hash, string keys" $ withFixture "hash2.dat" $ \fixture ->
      testParse marshal fixture
        `shouldBe` RHash 2
          [ (rString "abc", RFixnum 3)
          , (rString "xyz", RFixnum 4)
          ]

    it "1-entry hash with default" $ withFixture "hashDefault.dat" $ \fixture ->
      testParse marshal fixture
        `shouldBe` RHashDefault 1 (rString "cat")
          [(rString "zzz", RSymbol "foo")]


  describe "array" $ do
    it "[1]" $
      testParse array "[\x06i\x06" `shouldBe` RArray 1 [RFixnum 1]

    it "['cat', :dog]" $ withFixture "array.dat" $ \fixture ->
      testParse marshal fixture
        `shouldBe` RArray 2
          [ rString "cat"
          , RSymbol "dog"
          ]

    it "[:cat, :cat, :cat]" $ withFixture "tres_cats.dat" $ \fixture ->
      testParse marshal fixture
        `shouldBe` RArray 3
          [ RSymbol "cat"
          , RSymbol "cat"
          , RSymbol "cat"
          ]


  describe "time" $ do
    it "inner representation of: 2016-06-17 14:38:09 -0500" $
      withFixture "time_fragment.dat" $ \fixture ->
        testParse timeStamp fixture `shouldBe` "<TimeLit:3\SYN\GS\128\v\SOH\147\152>"

    it "parses a full time: <2016-06-17 14:38:09 -0500>" $ withFixture "time.dat" $ \fixture ->
      testParse marshal fixture
        `shouldBe` IVar (RSymbol "<TimeLit:3\SYN\GS\128\v\SOH\147\152>")
          [ (RSymbol "offset", RFixnum (-18000))
          , (RSymbol "zone", rString "CDT")
          ]

    it "xyyzy ActiveSupport::TimeWithZone <Thu, 07 Jul 2016 15:06:59 UTC +00:00>" $
      withFixture "time_with_zone.dat" $ \fixture ->
        testParse marshal fixture `shouldBe` RTime

    it "TimeWithZone: Array of [utc, time_zone.name, time] for <2016-05-13 14:27:21 UTC>" $
      withFixture "simple_time_with_zone.dat" $ \fixture ->
        testParse marshal fixture `shouldBe` RTime

    it "TimeWithZone: utc for <2016-05-13 14:27:21 UTC>" $
      withFixture "utc_time_with_zone.dat" $ \fixture ->
        testParse marshal fixture
          `shouldBe` IVar (RSymbol "<TimeLit:\174\DC1\GS\192\164\188Zm>")
            [(RSymbol "zone", rString "UTC")]

    it "a timestamp (2016-06-17 14:38:09 -0500)" $
      withFixture "trimmed_time.dat" $ \fixture ->
        testParse userDefined fixture
          `shouldBe` RSymbol "<TimeLit:3\SYN\GS\128\v\SOH\147\152>"


  describe "marshal" $ do
    it "reads realistic Ruby data" $ withFixture "realistic.dat" $ \fixture ->
      testParse marshal fixture
        `shouldBe` RHash 1
          [ (rString "results", RHash 2
            [ (rString "1234-5", RFixnum 14)
            , (rString "6789-0", RFixnum 80)
            ])
          ]

    it "reads rc44 data" $ withFixture "rc44.dat" $ \fixture ->
      testParse marshal fixture `shouldBe` RSymbol "barf"
