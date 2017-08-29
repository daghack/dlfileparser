{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Posts.Parser where

import           Control.Applicative
import           Data.Monoid
import           Prelude hiding (takeWhile)
import           Data.Attoparsec.Text
import           Data.Attoparsec.Combinator (lookAhead)
import           HTMLEntities.Decoder
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

data User =
  User { _username :: T.Text
       , _chardescription :: T.Text
  } deriving(Show)

data Post =
  Post { _postauthor :: T.Text
       , _postdate :: T.Text
       , _post :: T.Text
  } deriving(Show)

data Thread =
  Thread { _title :: T.Text
         , _date :: T.Text
         , _posts :: [Post]
  } deriving (Show)

restOfLine :: Parser T.Text
restOfLine = TL.toStrict . TLB.toLazyText . htmlEncodedText <$> takeWhile (not . isEndOfLine) <* endOfLine

pUser :: Parser User
pUser = do
  string "Username: "
  username <- (restOfLine <* skipSpace)

  string "Char Description: "
  desc <- T.unlines <$> manyTill restOfLine (lookAhead $ string "Posts" >> endOfLine)
  return $ User username desc

pPostHeader :: Parser (T.Text, T.Text)
pPostHeader =
  (,) <$> (string "Posted By: " *> restOfLine)
      <*> (string "Posted On: " *> restOfLine)
      <*  skipSpace

pThreadHeader :: Parser (T.Text, T.Text)
pThreadHeader =
  (,) <$> (string "Topic Title: " *> restOfLine)
      <*> (string "Posted on: " *> restOfLine)
      <*  (string "Posts:" >> skipSpace)

pThread :: Parser Thread
pThread = do
  (title, date) <- pThreadHeader
  Thread title date <$> manyTill pPost (eitherP endOfInput $ lookAhead pThreadHeader)

pPost :: Parser Post
pPost = do
  (author, date) <- pPostHeader
  post <- T.unlines <$> manyTill restOfLine (eitherP endOfInput $ lookAhead $ pPostHeader <|> pThreadHeader)
  return $ Post author date post

pFile :: Parser (User, [Thread])
pFile = do
  restOfLine >> skipSpace
  user <- pUser
  string "Posts" >> skipSpace
  (user,) <$> many' pThread

parseFile = parseOnly pFile

postToText (Post user date content) =
  T.unlines $ [ "----------------------------------------------"
              , "User: " <> user
              , "Date Posted: " <> date
              , ""
              , content
              ]

threadToText (Thread topic date posts) =
  T.unlines $ [ "Topic: " <> topic
              , "Date Created: " <> date
              , ""
              , ""
              ] <> map postToText posts
