{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HTML.Scalpel

data BusNum = BusNum String
  deriving Show

(/.) :: Selector -> Selector -> Selector
s1 /. s2 = s1 // s2 `atDepth` 1

table :: IO (Maybe [String])
table = scrapeURL "http://galssbuss.lv/pages/info_detail.php?l=lv&id_section=3&id_route=2&id_station=4" nums
  where
    nums :: Scraper String [String]
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

      return $ unwords [num, route, from, days]


main :: IO ()
main = table >>= print
