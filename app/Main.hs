module Main where

import qualified Parsix
import Json

parseFile :: FilePath -> Parsix.Parser a -> IO (Maybe a)
parseFile filepath parser = do
    input <- readFile filepath
    return $ fmap snd $ Parsix.run parser input

main :: IO ()
main = undefined
