{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified  Data.Map.Strict       as M
import            Data.Map.Merge.Strict (merge, SimpleWhenMissing, WhenMatched(..), mapMissing, dropMissing, zipWithMatched)
import            Data.Text             (Text)

main = return ()

data RefreshCache s e t = RefreshCache {
    source  :: s
  , extract :: s -> M.Map Text e
  , mapF    :: e -> t
  , invF    :: t -> e
  , mapped  :: M.Map Text t
  }

data WrappedRefreshCache s e t = WrappedRefreshCache {
    internalCache :: RefreshCache s e (e, t)
  }

mkRefreshCache :: s -> (s -> M.Map Text e) -> (e -> t) -> (t -> e) -> WrappedRefreshCache s e t
mkRefreshCache source extract mapF invF = WrappedRefreshCache { internalCache } where
  internalCache = RefreshCache { source, extract, mapF = newMapF, invF = newInvF, mapped = M.empty }
  newMapF = \raw -> (raw, mapF raw)
  newInvF = \(raw, mapped) -> raw

refresh :: Eq e => RefreshCache s e t -> RefreshCache s e t
refresh RefreshCache{..} = RefreshCache { source, extract, mapF, invF, mapped = newMap } where
  newMap = makeNewMapFromOld mapped (extract source) (\k x -> mapF x) (\k x -> invF x)

getValue :: Text -> RefreshCache s e t -> Maybe t
getValue key RefreshCache{mapped, ..} = M.lookup key mapped

-- | Applies a function to the new raw data (of type 'i') reusing values (of type 'o') from oldMappedData if the same
-- | key exists.
makeNewMapFromOld :: (Eq i, Ord k)
  => M.Map k o      -- ^ old map of computed data
  -> M.Map k i      -- ^ new map of raw data
  -> (k -> i -> o)  -- ^ function 'f' to apply to new raw data
  -> (k -> o -> i)  -- ^ inverse of 'f', used to check if old values have updated
  -> M.Map k o      -- ^ return map of new mapped values
makeNewMapFromOld oldMap newMap f inv = merge dropMissing (mapMissing f) unchangedKeys oldMap newMap where
  unchangedKeys = zipWithMatched $ \k x y -> if (inv k x == y) then x else f k y
