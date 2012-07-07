module ReadSource where

import Text.Parsec
import Text.Parsec.String

run :: Parser [String] -> String -> IO [Int]
run p input = case (parse p "" input) of
    Left err -> return []
    Right xs -> return $ map length xs

start :: Parser [String]
start = do
    (comment >> start) <|> return ["test"]

comment :: Parser ()
comment = (skipLine <|> skipLines ) >> return () where
    skipLine = string "--" >> skipMany1 digit >> string "\n"
    skipLines = string "{-" >> skipMany1 digit >> string "-}"

getLengthList str = return $ run start
