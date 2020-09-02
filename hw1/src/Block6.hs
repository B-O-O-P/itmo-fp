{-# LANGUAGE InstanceSigs #-}

module Block6
  ( Parser (..)
  , ok
  , eof
  , satisfy
  , element
  , stream
  , cbsParser
  , toNumber
  , intParser
  , listParser
  , listlistParser
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow (first)
import Data.Char (isDigit, digitToInt, isSpace)

-- Task 1

-- | Data type of parsers.
data Parser s a = Parser
  { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a    = Parser (\s -> Just (a, s))

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (Parser pf) (Parser parser) =
    Parser $ \s -> do
      (resultPf, tailPf) <- pf s
      (resultPa, tailPa) <- parser tailPf
      return (resultPf resultPa, tailPa)

instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (Parser parser) >>= pf =
    Parser $ \s -> do
      (resultPa, tailPa) <- parser s
      runParser (pf resultPa) tailPa

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (const Nothing)

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  p1 <|> p2 = Parser (\s -> runParser p1 s <|> runParser p2 s)

-- Task 2

-- | Parser which never falls and does not absorb input.
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | Checks that the parser has reached the end of the data stream
eof :: Parser s ()
eof = Parser $ \s ->
  case s of
    [] -> Just ((), s)
    _  -> Nothing

-- | Takes a predicate on a stream element, and returns an element that absorbs it from the stream,
-- if the predicate on the element is True, otherwise it falls.
satisfy :: (s -> Bool) -> Parser s s
satisfy p =
  Parser $ \s ->
    case s of
      [] -> Nothing
      (x:xs) ->
        if p x
          then Just (x, xs)
          else Nothing

-- | Parses one element.
element :: Eq s => s -> Parser s s
element e = satisfy (== e)

-- | Parses stream of elements.
stream :: Eq s => [s] -> Parser s [s]
stream = foldr ((<*>) . fmap (:) . element) ([] <$ ok)

-- Task 3

-- | Parser of correct bracket sequence.
cbsParser :: Parser Char ()
cbsParser = parseRec *> eof
  where
    parseRec :: Parser Char ()
    parseRec = (element '(' *> parseRec *> element ')' *> parseRec) <|> ok

-- | Returns predicate which accepts numbers.
toNumber :: Parser Char Int
toNumber = fmap digitToInt (satisfy isDigit)

-- | Skips any blank characters and returns rest parser.
skipBlank :: Parser Char ()
skipBlank = do
  parsed <- fmap Left (satisfy isSpace) <|> fmap Right ok
  case parsed of
    Left  _ -> skipBlank
    Right _ -> return ()

-- | An integer parser that can be preceded by a + or - sign.
intParser :: Parser Char Int
intParser = do
  skipBlank
  parsed <- parseSymbol <*> (foldl (\f a -> f * 10 + a) 0 <$> some toNumber)
  skipBlank
  return parsed
    where
      parseSymbol :: Num a => Parser Char (a -> a)
      parseSymbol = id <$ element '+' <|> negate <$ element '-' <|> id <$ ok

-- Task 4

-- | Parser of a list of numbers separated by a comma.
listParser :: Parser Char [Int]
listParser = (intParser <* element ',') >>= parseInt
  where
-- | Function which takes number of rest elements and parses current.
    parseInt :: Int -> Parser Char [Int]
    parseInt 1 = fmap return intParser
    parseInt i = fmap (:) (intParser <* element ',') <*> parseInt (i - 1)

-- | Parser of a list of lists of numbers separated by a comma.
listlistParser :: Parser Char [[Int]]
listlistParser = ((:) <$> listParser <*> many (element ',' *> listParser)) <|> const [] <$> ok