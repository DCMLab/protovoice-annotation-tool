module Common where

import Prelude
import Data.Char as C
import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Rational (Rational, (%))
import Text.Parsing.StringParser as P
import Text.Parsing.StringParser.CodePoints as P
import Text.Parsing.StringParser.Combinators as P

type MBS
  = { m :: Int, b :: Int, s :: Rational }

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
  | [ ms, bs, ss ] <- S.split (S.Pattern ",") str = do
    m <- hush $ P.runParser parseInt ms
    b <- hush $ P.runParser parseInt bs
    s <- hush $ P.runParser parseFrac ss
    pure { m, b, s }
  | otherwise = Nothing
