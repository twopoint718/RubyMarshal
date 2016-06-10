{-# LANGUAGE OverloadedStrings #-}

module Data.RubyMarshal where


import           Control.Applicative              ((<|>))
import           Control.Monad                    (guard)
import           Control.Monad.State.Strict       (StateT, get, modify')
import           Control.Monad.Trans              (lift)
import           Data.Attoparsec.ByteString       (parseTest, take, word8)
import           Data.Attoparsec.ByteString.Char8 (anyChar, char, char8)
import           Data.Attoparsec.Combinator       (count)
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            (pack)
import           Data.Char                        (ord)
import           Data.Int                         (Int8)
import           Data.String                      (IsString, fromString)
import           Data.Word                        (Word8)
import           Prelude hiding                   (lookup, take)

import qualified Data.Attoparsec.ByteString       as Attoparsec
import qualified Data.IntMap.Strict               as Map


type HashEntry = (Key, RubyValue)
type Key = RubyValue
type Long = Int
type Major = Int
type Minor = Int
type NumEntries = Long
type RubyString = ByteString
type RubySymbol = ByteString
type SymbolTable = Map.IntMap RubySymbol
type Parser = StateT SymbolTable Attoparsec.Parser


data Version = Version Major Minor deriving Show


data Encoding = UTF8 | ASCII deriving (Show, Eq)


data RubyValue
  = IVar Int RubyValue Encoding
  | RArray NumEntries [RubyValue]
  | RFixnum Long
  | RHash NumEntries [HashEntry]
  | RHashDefault NumEntries RubyValue [HashEntry]
  | RString RubyString
  | RSymbol RubySymbol
  deriving (Show, Eq)


marshal :: Parser RubyValue
marshal = version >> rubyValue


-- Only supports 4.8
version :: Parser Version
version = lift $ word8 4 *> word8 8 *> pure (Version 4 8)


rubyValue :: Parser RubyValue
rubyValue
  =   (RFixnum <$> long)
  <|> (RString <$> string)
  <|> (RSymbol <$> symbol)
  <|> array
  <|> hash
  <|> ivar


long :: Parser Long
long = do
  lift $ char 'i'
  size


string :: Parser RubyString
string = do
  lift $ char '"'
  n <- size
  lift $ take n


symbol :: Parser RubySymbol
symbol
  =   symbolLit
  <|> symbolRef


array :: Parser RubyValue
array = do
  lift $ char '['
  numEntries <- size
  entries <- count numEntries rubyValue
  return $ RArray numEntries entries


hash :: Parser RubyValue
hash
  =   basicHash
  <|> hashWithDefault


ivar :: Parser RubyValue
ivar = do
  lift $ char 'I'
  content <- rubyValue
  numIVars <- size
  enc <- encoding
  return $ IVar numIVars content enc


size :: Parser Long
size = do
  firstByte <- lift anyChar
  case firstByte of
    '\x00' -> return 0
    '\x01' -> error "one byte, following is 123 thru 255"
    '\xff' -> error "one byte, negative -1 thru -256"
    '\x02' -> error "two byte, positive little-endian integer"
    '\xfe' -> error "two bytes, negative little-endian integer"
    '\x03' -> error "three bytes, positive little-endian integer"
    '\xfd' -> error "three bytes, negative little-endian integer"
    '\x04' -> error "four bytes, positive little-endian integer"
    '\xfc' -> error "four bytes, negative little-endian integer"
    b -> return $ rubyShortInt b


symbolLit :: Parser RubySymbol
symbolLit = do
  lift $ char ':'
  n <- size
  sym <- lift $ take n
  modify' (addSymbol sym)
  return sym


symbolRef :: Parser RubySymbol
symbolRef = do
  lift $ char ';'
  refNum <- size
  table <- get
  case Map.lookup refNum table of
    Just ref -> return ref
    Nothing -> fail ("Could not resolve symbol reference: " ++ show refNum)


basicHash :: Parser RubyValue
basicHash = do
  lift $ char '{'
  numEntries <- size
  entries <- count numEntries hashEntry
  return $ RHash numEntries entries


hashWithDefault :: Parser RubyValue
hashWithDefault = do
  lift $ char '}'
  numEntries <- size
  entries <- count numEntries hashEntry
  defaultValue <- rubyValue
  return $ RHashDefault numEntries defaultValue entries


encoding :: Parser Encoding
encoding = do
  sym <- symbol
  guard (sym == "E")
  b <- bool
  return $ if b then UTF8 else ASCII


rubyShortInt :: Char -> Int
rubyShortInt c = fromIntegral offsetNum
  where
    i = c2i8 c
    offsetNum = if i >= 0 then i - 5 else i + 5


addSymbol :: RubySymbol -> SymbolTable -> SymbolTable
addSymbol sym table =
  Map.insert nextKey sym table
  where
    nextKey = case Map.size table of
      0 -> 0
      n -> n + 1


hashEntry :: Parser HashEntry
hashEntry = do
  k <- rubyValue
  v <- rubyValue
  return (k, v)


bool :: Parser Bool
bool
  = lift
  $   (char 'T' *> pure True)
  <|> (char 'F' *> pure False)


c2i8 :: Char -> Int8
c2i8 = fromIntegral . fromEnum
