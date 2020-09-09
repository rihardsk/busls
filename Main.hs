{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE DeriveGeneric #-}

module Main where

import Text.HTML.Scalpel
import Data.Text.IO (readFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import Prelude hiding (readFile, unwords)
import Control.Applicative (many)
import Text.Printf (printf)
import Control.Monad
import Control.Applicative ((<|>))
import Data.List (sortOn, foldl')
-- import Options.Generic
import Data.List.Extra (groupSort)
import Data.Maybe (fromJust)
import Text.Regex.TDFA as R

-- import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Lazy as LBS
-- import Data.ByteString.Lazy.Builder (toLazyByteString, word8Hex)
import qualified Data.ByteString as BS


-- | Holds info on a single timetable
data BusInfo = BusInfo
  { num   :: Text -- ^ e.g., 6829
  , route :: Text -- ^ e.g., SILAKROGS - ROPAŽI
  , from  :: Text -- ^ e.g., no Silakroga
  , days  :: Text -- ^ e.g., Darba dienās
  , times :: [Text] -- ^ e.g., ["06.29", "09.39"]
  }
  deriving Show

(/.) :: Selector -> Selector -> Selector
s1 /. s2 = s1 // s2 `atDepth` 1

table :: Int -> Int -> IO (Maybe [BusInfo])
table routeId stationId = scrapeSource nums
  where
    scrapeSource :: Scraper Text a -> IO (Maybe a)
    scrapeSource = scrapeURLWithConfig scrapeConfig $ printf "http://galssbuss.lv/pages/info_detail.php?l=lv&id_section=3&id_route=%i&id_station=%i" routeId stationId
    -- scrapeSource scraper = do
    --   page <- readFile "page.html"
    --   return $ scrapeStringLike page scraper

    scrapeConfig = Config customUtf8Decoder Nothing
    customUtf8Decoder ::  TagSoup.StringLike str => Decoder str
    customUtf8Decoder = TagSoup.castString . Encoding.decodeUtf8 . filterBS wrong . LBS.toStrict . HTTP.responseBody

    wrong = "\xe4\xeb\xff" :: BS.ByteString
    filterBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
    filterBS substr bs = h <> if BS.null t then BS.empty else filterBS substr (BS.drop (BS.length substr) t)
      where (h, t) = BS.breakSubstring substr bs

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
          days'  = normalize $ clean days
          times  = filter (not . T.null) . map clean . concat $ timess
      return $ BusInfo num' route' from' days' times

normalize :: Text -> Text
normalize days
  | r `R.match` days = "Sestdienās, svētdienās un svētku dienās"
  -- -- | days =~ ("asdf" :: Text) = "Sestdienās, svētdienās un svētku dienās"
  where
        pat = "sestdien.*svētdien.*svētku" :: Text
        copt = defaultCompOpt{caseSensitive = False}
        r = makeRegexOpts copt defaultExecOpt pat
normalize days = days

type RouteNum = Text
type Route = Text
type Station = Text
type Days = Text
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

pprint :: [(RouteNum, Time)] -> Text
pprint = foldl' toLine ""
  where
    toLine :: Text -> (RouteNum, Time) -> Text
    toLine "" (r, t) = T.pack $ printf "%-8s\t%s" t r
    toLine s b = s <> "\n" <> toLine "" b

groupBuses :: [BusInfo] -> [(Days, [BusInfo])]
groupBuses = groupSort . map (\b -> (days b, b))

modifyDays :: [BusInfo] -> [BusInfo]
modifyDays = concatMap $ \b ->
  if (T.toLower . days) b == "katru dienu"
  then [b{days = "Darba dienās"}, b{days = "Sestdienās, svētdienās un svētku dienās"}]
  else [b]

filterOnlyRiga :: [BusInfo] -> [BusInfo]
filterOnlyRiga = filter $ T.isInfixOf "rīga" . T.toLower . route

main :: IO ()
main = do
  rbuses :: Maybe [BusInfo] <- mconcat <$> mapM (uncurry table) fromRigaIds
  sbuses :: Maybe [BusInfo] <- mconcat <$> mapM (uncurry table) fromSilakrogsIds
  putStrLn "### No Rīgas"
  fromJust $ printGroups <$> rbuses
  putStrLn "\n### No Silakroga"
  fromJust $ printGroups <$> sbuses
  where
    printBuses bs = putStrLn $ T.unpack . pprint . sortBusses $ bs
    printGroups = mapM_ (\(d, bs) -> (putStrLn . T.unpack) d >> printBuses bs) . groupBuses . modifyDays . filterOnlyRiga
