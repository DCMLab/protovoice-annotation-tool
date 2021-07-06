module SimplePitch where

import Prelude
import Common (parseInt)
import Data.Tuple (Tuple(..))
import Control.Alt ((<|>))
import Data.Char as C
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Monoid (power)
import Data.Ord (abs)
import Data.String as S
import Text.Parsing.StringParser as P
import Text.Parsing.StringParser.CodePoints as P
import Text.Parsing.StringParser.Combinators as P

newtype SimplePitch
  = SimplePitch { step :: Int, acc :: Int, oct :: Int }

derive instance simplePitchEq :: Eq SimplePitch

instance simplePitchOrd :: Ord SimplePitch where
  compare (SimplePitch p1) (SimplePitch p2) = compare t1 t2
    where
    t1 = Tuple p1.oct $ Tuple p1.step p1.acc

    t2 = Tuple p2.oct $ Tuple p2.step p2.acc

asciiA :: Int
asciiA = C.toCharCode 'A'

instance simplePitchShow :: Show SimplePitch where
  show (SimplePitch { step, acc, oct }) = n <> a <> o
    where
    n =
      S.singleton
        $ S.codePointFromChar
        $ fromMaybe 'X'
        $ C.fromCharCode
        $ ((step + 2) `mod` 7)
        + asciiA

    a = power (if acc > 0 then "♯" else "♭") (abs acc)

    o = show oct

parseAccs :: P.Parser Int
parseAccs = P.option 0 $ sharps <|> flats
  where
  munch = P.many1 <<< P.char

  sharps = length <$> (munch '♯' <|> munch '#')

  flats = negate <<< length <$> (munch '♭' <|> munch 'b')

parseSpelled :: P.Parser SimplePitch
parseSpelled = do
  name <- P.oneOf [ 'A', 'B', 'C', 'D', 'E', 'F', 'G' ]
  let
    step = (C.toCharCode name - asciiA - 2) `mod` 7
  acc <- parseAccs
  oct <- parseInt
  pure $ SimplePitch { step, acc, oct }

parseSimplePitch :: String -> Maybe SimplePitch
parseSimplePitch str = case P.runParser parseSpelled str of
  Left _ -> Nothing
  Right p -> Just p
