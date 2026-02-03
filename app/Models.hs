{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models
  ( GameId (..),
    GameCore (..),
    LocalPaths (..),
    GameWithLocal (..),
    CloudData (..),
    PlayTimeline (..),
    Link (..),
    RootState (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype GameId = GameId {unGameId :: Text}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

type Seconds = Int

type Minutes = Int

data PlayTimeline = PlayTimeline
  { start :: !Int, -- Unix timestamp (秒)
    end :: !Int,
    duration :: !Seconds
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Link = Link
  { name :: !Text,
    url :: !Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GameCore = GameCore
  { gameId :: !GameId,
    vndbId :: !(Maybe Text),
    bgmId :: !(Maybe Text),
    updateDate :: !Int, -- Unix timestamp
    title :: !Text,
    alias :: ![Text],
    cover :: !Text, -- URL 或本地路径
    description :: !Text,
    tags :: ![Text],
    playTimelines :: ![PlayTimeline],
    expectedPlayTime :: !Minutes,
    lastPlay :: !Int, -- 上次游玩的 timestamp
    createDate :: !Int,
    releaseDate :: !Int,
    rating :: !Double,
    developer :: !Text,
    images :: ![Text], -- 截图 URL/路径
    links :: ![Link]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data LocalPaths = LocalPaths
  { id :: !GameId,
    programFile :: !FilePath,
    savePath :: !(Maybe FilePath),
    guideFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GameWithLocal = GameWithLocal
  { game :: !GameCore,
    local :: !(Maybe LocalPaths)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data CloudData = CloudData
  { deleteIds :: ![GameId],
    datas :: ![GameCore]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RootState = RootState
  { gameCore :: ![GameCore],
    localPaths :: ![LocalPaths]
    -- sync :: !SyncState,
    -- settings :: !Settings
    -- ... 其他字段
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- deriving anyclass ()

-- data SyncState = SyncState
--   { time           :: !Int
--   , deleteIds      :: ![GameId]
--   , size           :: !Int
--   , visibility     :: !Text
--   , username       :: !Text
--   , bgmUsername    :: !Text
--   , vndbUsername   :: !Text
--   , avatar         :: !Text
--   -- ... 其他 sync 相关字段
--   } deriving stock (Show, Eq, Generic)
--     deriving anyclass (FromJSON, ToJSON)

-- -- Settings 可以继续细分
-- data Settings = Settings
--   { -- language :: Text
--     -- theme :: Text
--     -- autoSyncMinutes :: Int
--     -- ...
--   } deriving stock (Show, Eq, Generic)
--     deriving anyclass (FromJSON, ToJSON)
