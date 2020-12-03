module Lib where

import Data.Char
import Control.Applicative

-- JSON AST
data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Integer  -- Note: no float support
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

-- Abstract Parser definition

-- Parsers take string input and return parsed data and the rest of the input
-- which is denoted by (String, a). We use Maybe in case parser cannot parse 
-- the input
newtype Parser a = Parser
                 { runParser :: String -> Maybe (String, a) 
                 }

-- If we want to error report, we might do something like this:
-- type Parser a = String -> Either (Int, Int, String) (String, a)
-- so that we have information about the error and the location in input

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do 
                                    (input', x) <- p input
                                    Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
                                               (input' , f) <- p1 input
                                               (input'', a) <- p2 input'
                                               Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input


charP :: Char -> Parser Char
charP x = Parser f
   where f (y:ys) 
           | y == x    = Just (ys, x)
           | otherwise = Nothing
         f [] = Nothing

stringP :: String -> Parser String
stringP input = sequenceA $ map charP input

-- runParser (charP 'h') "hello"
-- runParser (fmap ord $ charP 'h') "hello"

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"
-- jsonNull = JsonNull <$ stringP "null"

-- runParser jsonNull "null"

-- 2 different scenarios, we can use Alternative
jsonBool :: Parser JsonValue
jsonBool = fmap f $ stringP "true" <|> stringP "false"
    where f "true"  = JsonBool True
          f "false" = JsonBool False
          f _       = undefined

-- runParser jsonBool "true"
-- runParser jsonBool "whatever"

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (token, rest) = span f input 
                              in Just (rest, token)

-- runParser (spanP isDigit) "12345rest"

jsonNumber :: Parser JsonValue
jsonNumber = fmap f $ notNull $ spanP isDigit
    where f ds = JsonNumber $ read ds

-- runParser jsonNumber "12345rest"
-- runParser jsonNumber ""

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
                                  (input', xs) <- p input
                                  if null xs then Nothing
                                             else Just (input', xs)

-- Note: no escape support
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = fmap JsonString stringLiteral

-- runParser jsonString "\"12345rest\"""

jsonArray :: Parser JsonValue
jsonArray = fmap JsonArray $ charP '[' *> ws *> elements <* ws <* charP ']'
    where elements = sepBy sep jsonValue
          sep = ws *> charP ',' <* ws

ws :: Parser String
ws = spanP isSpace

-- runParser (many jsonNull) "nullnullnull"

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []   
-- <|> pure []   because of empty input!

-- runParser (sepBy (charP ',') (charP 'a')) "a,a,a,a"
-- runParser (sepBy (charP ',') (stringP "abc")) "abc,abc,abc,abc"
-- runParser (sepBy (charP ',') (charP 'a')) ""

-- runParser jsonArray "[1,\"hello\",true]"
-- runParser jsonArray "[1,\"hello\",true,[],[1,[]]]"

jsonObject :: Parser JsonValue
jsonObject = fmap JsonObject $ charP '{' *> ws *> pairs <* ws <* charP '}' 
    where pairs = sepBy (ws *> charP ',' <* ws) pair
          colon = ws *> charP ':' <* ws
          pair  = (\key _ value -> (key, value)) <$> stringLiteral <*> colon <*> jsonValue 

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject


parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile filename parser = do
    input <- readFile filename
    return $ snd <$> runParser parser input
    
parseJson :: FilePath -> IO (Maybe JsonValue)
parseJson filename = parseFile filename jsonValue