{-# LANGUAGE OverloadedStrings #-}

module Data.RubyMarshal where


import           Control.Applicative              ((<|>))
import           Control.Monad                    (guard)
import           Control.Monad.State.Strict       (StateT(..), evalStateT, get, modify', put)
import           Control.Monad.Trans              (lift)
import           Data.Attoparsec.ByteString       (anyWord8, eitherResult, parseTest, take, word8)
import           Data.Attoparsec.ByteString.Char8 (anyChar, char, char8)
import           Data.Attoparsec.Combinator       (count)
import           Data.Bits                        (Bits, (.|.), complement, shiftL)
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            (pack, unpack)
import           Data.Char                        (ord)
import           Data.Int                         (Int8)
import           Data.Monoid                      ((<>))
import           Data.String                      (IsString, fromString)
import           Data.Word                        (Word8, Word16, Word32)
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
  = IVar Int RubyValue Encoding -- ^ Ruby instance variable
  | RArray NumEntries [RubyValue] -- ^ Ruby Array
  | RFixnum Long -- ^ Ruby long, variable length
  | RFloat Float -- ^ Ruby floating point number
  | RHash NumEntries [HashEntry] -- ^ A Ruby hash (key-value map)
  | RHashDefault NumEntries RubyValue [HashEntry] -- ^ Ruby hash with default
  | RNil -- ^ The Ruby nil value
  | RString RubyString -- ^ A Ruby String
  | RSymbol RubySymbol -- ^ A Ruby Symbol (see doc on 'symbol')
  | RTime -- ^ A Ruby timestamp (not implemented)
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
  <|> float
  <|> hash
  <|> ivar
  <|> nil
  <|> userDefined
  <|> userMarshal


long :: Parser Long
long = do
  lift $ char 'i'
  size


string :: Parser RubyString
string = do
  lift $ char '"'
  n <- size
  lift $ take n


{-|
  Ruby symbols are dumped as (essentially) strings, they are preceded
  by either a @:@ (colon) or a @;@ (semicolon).

  The colon means that the symbol is /directly/ included in the dump.
  A 'size' follows the colon and then the bytes which comprise the
  name of the symbol.

  > bytes :\0x8foo
  > ruby  :foo

  The other possibility is a symbol reference. This is a semicolon
  followed by a 'size' which indicates an index in the symbol lookup
  table. These sequentially number each symbol.

  > bytes \x04\x08[\a:\nhello;\x00
  > ruby  [:hello, :hello]

  In the above, the first @:hello@ is included literally. The second
  one is a reference to the zeroth index in the symbol table.
-}
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


float :: Parser RubyValue
float = do
  lift $ char 'f'
  numBytes <- size
  str <- lift $ take numBytes
  let num = read (unpack str)
  return $ RFloat num


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


nil :: Parser RubyValue
nil = lift $ char '0' >> pure RNil


userDefined :: Parser RubyValue
userDefined = do
  lift $ char 'u'
  className <- symbol <?> "userDefined className"
  let
    parser = case className of
      "Time" -> time <?> "time"
      otherwise -> fail $ "unknown user-defined type: (" ++ unpack className ++ ")"
  result <- parser
  return result


userMarshal :: Parser RubyValue
userMarshal = do
  lift $ char 'U'
  className <- symbol <?> "userMarshal className"
  let
    parser = case className of
      "ActiveSupport::TimeWithZone" -> timeWithZone <?> "TimeWithZone"
      otherwise -> fail $ "unknown user-defined type: (" ++ unpack className ++ ")"
  result <- parser
  return result
    

time :: Parser RubyValue
time = do
  encodedTime <- timeStamp <?> "timeStamp"
  offset <- symbol <?> ":offset"
  offsetValueSeconds <- long <?> "time zone offset"
  zone <- symbol <?> ":zone"
  zoneValue <- ivar <?> "time zone value (CDT)"
  return . RSymbol $ mconcat
    [ encodedTime, " "
    , rshow zoneValue, " "
    , pack . show $ offsetValueSeconds `div` 3600
    ]


{-|
  The ActiveSupport::TimeWithZone dump format looks like this:

  > def marshal_dump
  >   [utc, time_zone.name, time]
  > end

  utc is of type "Time," the same as the above definition. Next, will
  be a string naming the time zone (e.g.\"UTC\"). Lastly, there's another
  timestamp listing the time in the current time zone.
-}
timeWithZone :: Parser RubyValue
timeWithZone = do
  array <- rubyValue
  case array of
    RArray 3 (utc:zone:time:[]) -> return $ RTime --(utc, zone, time)
    _ -> fail "Invalid TimeWithZone"


-- TODO: parse 8-byte time value
timeStamp :: Parser ByteString
timeStamp = do
  mystery <- lift (take 1) -- I don't know what this byte means 0x0D
  encodedTime <- lift (take 8)
  mystery2 <- lift (take 1) -- I don't know what this byte means 0x07 (2?)
  return $ "<TimeLit:" <> encodedTime <> ">"
    

{-|
  Size is a variable-length encoded integer. It is used both as the
  underlying encodning of 'long' (a Ruby \"Fixnum\") and also as "number
  of bytes" within the marshal dump format itself.

  The first byte indicates how many bytes will follow in the encoding of the number:

    * @\x00@ the number zero (no bytes follow)
    * @\x01@ a positive one-byte number
    * @\xff@ a negative one-byte number
    * @\x02@ a positive two-byte number
    * @\xfe@ a negative two-byte number
    * @\x03@ a positive three-byte number
    * @\xfd@ a negative three-byte number
    * @\x04@ a positive four-byte number (not implemented)
    * @\xfc@ a negative four-byte number (not implemented)

  The last case is when the number is in the range -123 to 122. In
  this case, the number is represented as a signed single-byte. Positive
  numbers have an offset of -5 (subtract 5 from the marshaled value to
  arrive at the actual value) and negative values have an offset of +5.
  Example:

  > bytes \x06
  > ruby  1
-}
size :: Parser Long
size = do
  firstByte <- lift anyChar
  case firstByte of
    '\x00' -> return 0

    '\x01' ->
      oneBytePos <$> lift anyWord8

    '\xff' ->
      oneByteNeg <$> lift anyWord8

    '\x02' ->
      twoBytePos <$> lift anyWord8 <*> lift anyWord8

    '\xfe' ->
      twoByteNeg <$> lift anyWord8 <*> lift anyWord8 

    '\x03' ->
      threeBytePos <$> lift anyWord8 <*> lift anyWord8 <*> lift anyWord8
      
    '\xfd' -> 
      threeByteNeg <$> lift anyWord8 <*> lift anyWord8 <*> lift anyWord8

    '\x04' -> fail "four bytes, positive little-endian integer"
    '\xfc' -> fail "four bytes, negative little-endian integer"
    b -> return $ rubyShortInt b


oneByteNeg :: Word8 -> Long
oneByteNeg b = -1 * fromIntegral (complement b + 1)


oneBytePos :: Word8 -> Long
oneBytePos b = fromIntegral b


twoByteNeg :: Word8 -> Word8 -> Long
twoByteNeg lsb msb =
  -1 * fromIntegral (complement (msb' `shiftL` 8 .|. lsb') + 1)
  where
    msb', lsb' :: Word16
    msb' = fromIntegral msb
    lsb' = fromIntegral lsb


twoBytePos :: Word8 -> Word8 -> Long
twoBytePos lsb msb =
  fromIntegral msb `shiftL` 8 .|. fromIntegral lsb


threeBytePos :: Word8 -> Word8 -> Word8 -> Long
threeBytePos b1 b2 b3 = 
  (   fromIntegral b3 `shiftL` 16
  .|. fromIntegral b2 `shiftL` 8
  .|. fromIntegral b1
  )


threeByteNeg :: Word8 -> Word8 -> Word8 -> Long
threeByteNeg b1 b2 b3 = -1 *
  (   fromIntegral (complement b3) `shiftL` 16
  .|. fromIntegral (complement b2) `shiftL` 8 
  .|. fromIntegral (complement b1 + 1)
  )


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


-- Util


-- FIXME: I think there's a bug in this
(<?>) :: Parser a -> String -> Parser a
(<?>) parser failMessage = do
  currState <- get
  let parser' = evalStateT parser currState
  let namedParser = (Attoparsec.<?>) parser' failMessage
  -- IDEA:
  put currState
  lift namedParser


c2i8 :: Char -> Int8
c2i8 = fromIntegral . fromEnum


rshow :: RubyValue -> ByteString
rshow (IVar _ val _) = rshow val
rshow (RString s) = s
rshow _ = "<RubyValue>"


-- Run the parser


parse :: ByteString -> RubyValue
parse bs =
  case eitherResult (Attoparsec.parse (evalStateT marshal Map.empty) bs) of
    Left msg -> error (msg ++ " with input: <" ++ show bs ++ ">")
    Right x -> x
