module ReadSource where

import Text.Parsec
import Text.Parsec.String

run :: Parser [String] -> String -> IO [Int]
run p input = case (parse p "" input) of
    Left err -> return []
    Right xs -> return $ map length xs

start :: Parser [String]
start = return ["test"]

getLengthList str = return $ run start
