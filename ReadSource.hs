module ReadSource where

import Control.Applicative hiding ( (<|>) , many )
import Text.Parsec
import Text.Parsec.String

run :: Parser [String] -> String -> IO [Int]
run p input = case (parse p "" input) of
    Left err -> print err >> return []
    Right xs -> return $ map length xs

start :: Parser [String]
start = do
    x <- word
    xs <- (commentOrSpace *> start) <|> return []
    case x of
        Just a -> return $ a:xs
        Nothing -> return xs

word :: Parser (Maybe String)
word = (Just <$> (types <|> variables <|> number) ) <|>return  Nothing

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
                  noneOf efficients) where
    skipLine = string "--" *> line *> return 'l'
    skipBlock = string "{-" *> contents *> return 'b'
    line = skipMany (noneOf "\n") *> newline
    contents = string "-}" <|> (anyChar *> contents)
    literals = stringL <|> charL
    stringL = char '\"' *> skipMany (noneOf "\"") *> char '\"'
    charL = char '\'' *> anyChar *> char '\''

efficients = ['a'..'z']++['A'..'Z']++concat (map show [0..9]) :: String

getLengthList str = return $ run start
