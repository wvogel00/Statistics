module ReadSource (run,startRead) where

import Control.Applicative hiding ( (<|>) , many )
import Text.Parsec
import Text.Parsec.String

length' = fromIntegral.length

run :: Parser [String] -> String -> IO [Double]
run p input = case (parse p "" input) of
    Left err -> print err >> return []
    Right xs -> return $ map length' xs

startRead :: Parser [String]
startRead = do
    x <- word
    xs <- (commentOrSpace *> startRead) <|> return []
    case x of
        Just a -> return $ a:xs
        Nothing -> return xs

word :: Parser (Maybe String)
word = (Just <$> (try types <|> try variables <|> try number) )
        <|>return  Nothing

number :: Parser String
number = try float <|> integer

integer :: Parser String
integer = many1 digit

float :: Parser String
float = (++) <$> integer <*> ((:) <$> char '.' <*> integer)

types :: Parser String
types = (++) <$> many1 upper <*> many alphaNum

variables :: Parser String
variables = f <$> many (char '_') <*> many1 lower
              <*> many alphaNum <*> many (char '\'') where
    f a b c d =  a++b++c++d

commentOrSpace :: Parser Char
commentOrSpace = (try space <|> try literals <|>
                  try skipLine <|> skipBlock <|>
                   anyChar) where --noneOf efficients) where
    skipLine = string "--" *> line *> return 'l'
    skipBlock = string "{-" *> contents *> return 'b'
    line = skipMany (noneOf "\n") *> newline
    contents = string "-}" <|> (anyChar *> contents)
    literals = stringL <|> charL
    stringL = char '\"' *> skipMany (noneOf "\"") *> char '\"'
    charL = char '\'' *> anyChar *> char '\''

efficients = ['a'..'z']++['A'..'Z']++concat (map show [0..9]) :: String
