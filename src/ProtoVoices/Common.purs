module ProtoVoices.Common where

import Prelude
import Data.Char as C
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Ratio (denominator, numerator)
import Data.Rational (Rational, (%))
import Data.String as S
import Text.Parsing.StringParser (Parser, runParser) as P
import Text.Parsing.StringParser.CodePoints (anyDigit, char) as P
import Text.Parsing.StringParser.Combinators (many1, option) as P

newtype MBS
  = MBS { m :: Int, b :: Int, s :: Rational }

derive newtype instance eqMBS :: Eq MBS

instance showMBS :: Show MBS where
  show (MBS { m, b, s }) =
    show m
      <> "."
      <> show b
      <> "."
      <> show (numerator s)
      <> "/"
      <> show (denominator s)

ascii0 :: Int
ascii0 = C.toCharCode '0'

digit :: Char -> Int
digit d = C.toCharCode d - ascii0

parseInt :: P.Parser Int
parseInt = do
  signChar <- P.option '+' $ P.char '-'
  let
    sign = if signChar == '-' then -1 else 1
  dgts <- P.many1 P.anyDigit
  pure $ sign * foldl (\acc d -> 10 * acc + digit d) 0 dgts

parseFrac :: P.Parser Rational
parseFrac = do
  num <- parseInt
  denom <-
    P.option 1
      $ do
          _ <- P.char '/'
          parseInt
  pure $ num % denom

parseMBS :: String -> Maybe MBS
parseMBS str
  | [ ms, bs, ss ] <- S.split (S.Pattern ".") str = do
    m <- hush $ P.runParser parseInt ms
    b <- hush $ P.runParser parseInt bs
    s <- hush $ P.runParser parseFrac ss
    pure $ MBS { m, b, s }
  | otherwise = Nothing

parseTime :: String -> Either String MBS
parseTime str = maybe (Left str) Right $ parseMBS str
