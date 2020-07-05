{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HTML.Scalpel
import Data.Text.IO (readFile)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (readFile, unwords)
import Control.Applicative (many)

data BusInfo = BusInfo
  { num   :: Text
  , route :: Text
  , from  :: Text
  , days  :: Text
  , times :: [Text]
  }
  deriving Show

(/.) :: Selector -> Selector -> Selector
s1 /. s2 = s1 // s2 `atDepth` 1

table :: IO (Maybe [BusInfo])
table = scrapeSource nums
  where
    scrapeSource :: Scraper Text a -> IO (Maybe a)
    -- scrapeSource = scrapeURL "http://galssbuss.lv/pages/info_detail.php?l=lv&id_section=3&id_route=2&id_station=4"
    scrapeSource scraper = do
      page <- readFile "page.html"
      return $ scrapeStringLike page scraper


    nums :: Scraper Text [BusInfo]
    nums = chroots  "tbody" $ inSerial $ do
      (num, route) <- seekNext $ chroot  "tr" $ inSerial $ do
        num   <- seekNext . text $ "td" // "p"
        route <- seekNext . text $ "td" // "p"
        return (num, route)
      (from, days) <- seekNext $ chroot  "tr" $ inSerial $ do
        from <- seekNext . text $ "td" // "p"
        seekNext $ matches "td"
        days <- seekNext . text $ "td" // "p"
        return (from, days)
      timess <- untilNext (matches $ "tr" // "td" // "p" ) $ many $ do
        times <- seekNext . texts $ "tr" /. "td"
        return times

      let times = filter (not . T.null) . concat $ timess
          days' = T.unwords . T.words $ days
      return $ BusInfo num route from days' times


main :: IO ()
main = table >>= print
