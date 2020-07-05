{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveGeneric #-}

module Main where

import Text.HTML.Scalpel
import Data.Text.IO (readFile)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (readFile, unwords)
import Control.Applicative (many)
import Text.Printf (printf)
import Control.Monad
import Control.Applicative ((<|>))
import Data.List (sortOn)
-- import Options.Generic

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

table :: Int -> Int -> IO (Maybe [BusInfo])
table routeId stationId = scrapeSource nums
  where
    scrapeSource :: Scraper Text a -> IO (Maybe a)
    scrapeSource = scrapeURL $ printf "http://galssbuss.lv/pages/info_detail.php?l=lv&id_section=3&id_route=%i&id_station=%i" routeId stationId
    -- scrapeSource scraper = do
    --   page <- readFile "page.html"
    --   return $ scrapeStringLike page scraper

    clean :: Text -> Text
    clean = T.unwords . T.words

    isTextual :: Scraper Text ()
    isTextual = do
      content <- text $ "tr" // "td"
      let n = length . T.words $ content
      guard (n > 1)

    nums :: Scraper Text [BusInfo]
    nums = chroots  "tbody" $ inSerial $ do
      (num, route) <- seekNext $ chroot  "tr" $ inSerial $ do
        num   <- seekNext . text $ "td"
        route <- seekNext . text $ "td"
        return (num, route)
      (from, days) <- seekNext $ chroot  "tr" $ inSerial $ do
        from <- seekNext . text $ "td"
        seekNext $ matches "td"
        days <- seekNext . text $ "td"
        return (from, days)
      timess <- untilNext ((matches $ "tr" // "td" // "p" ) <|> isTextual) $ many $ do
        times <- seekNext . texts $ "tr" /. "td"
        return times

      let num'   = clean num
          route' = clean route
          from'  = clean from
          days'  = clean days
          times  = map clean . filter (not . T.null) . concat $ timess
      return $ BusInfo num' route' from' days' times

type RouteNum = Text
type Time = Text

sortBusses :: [BusInfo] -> [(RouteNum, Time)]
sortBusses = sortOn snd . concat . map toTimes
  where toTimes b = map (\time -> (num b, time)) $ times b

fromRigaIds =
  [ (2, 4)
  , (3, 8)
  , (4, 13)
  ]
fromSilakrogsIds =
  [ (2, 5)
  , (3, 9)
  , (4, 14)
  ]

main :: IO ()
main = do
  buses <- mapM (uncurry table) $ fromRigaIds ++ fromSilakrogsIds
  print $ sortBusses . concat <$> buses
