{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.String
import           Data.Monoid
import           Control.Monad
import           Filesystem.Path.CurrentOS
import           Posts.Parser
import           System.Directory
import           System.IO
import           System.Exit
import           System.Environment
import           System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "Only argument is filename"
    else do
      putStrLn $ "Reading and parsing threads from: " ++ head args
      withFile (head args) ReadMode $ \h -> do
        hSetNewlineMode h universalNewlineMode
        Right (user, threads) <- parseFile <$> T.hGetContents h
        let tld = fromText . _username $ user
        createDirectory $ encodeString tld
        forM_ threads $ \thread -> do
          let filename = T.replace "/" "|" $ _title thread
          print . _title $ thread
          let f = tld </> fromText filename
          withFile (encodeString f) WriteMode $ flip T.hPutStrLn (threadToText thread)
        result <- rawSystem "zip" ["-r", (encodeString tld) <> ".zip", encodeString tld]
        case result of
          ExitSuccess -> removeDirectoryRecursive $ encodeString tld
          ExitFailure _ -> putStrLn "Something went wrong with the zip! D:"
