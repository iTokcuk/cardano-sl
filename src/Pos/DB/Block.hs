{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Interface to Blocks DB.

module Pos.DB.Block
       ( getBlock
       , getBlockHeader
       , getStoredBlock
       , getUndo
       , setBlockInMainChain
       , isBlockInMainChain

       , deleteBlock
       , putBlock
       , loadLastNBlocksWithUndo
       , loadBlocksWithUndoWhile
       , loadHeadersWhile

       , prepareBlockDB
       ) where

import           Control.Lens         ((^.))
import           Data.ByteArray       (convert)
import           Data.List.NonEmpty   (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty   as NE
import           Formatting           (sformat, (%))
import           Universum

import           Pos.Binary.Class     (Bi)
import           Pos.Binary.Modern.DB ()
import           Pos.Crypto           (shortHashF)
import           Pos.DB.Class         (MonadDB, getBlockDB)
import           Pos.DB.Error         (DBError (..))
import           Pos.DB.Functions     (rocksDelete, rocksGetBi, rocksPutBi)
import           Pos.DB.Types         (StoredBlock (..))
import           Pos.Ssc.Class.Types  (Ssc)
import           Pos.Types            (Block, BlockHeader, GenesisBlock, HeaderHash, Undo,
                                       genesisHash, headerHash, prevBlockL)
import qualified Pos.Types            as T


-- | Get StoredBlock by hash from Block DB.
getStoredBlock
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe (StoredBlock ssc))
getStoredBlock = getBi . blockKey

-- | Get block with given hash from Block DB.
getBlock
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe (Block ssc))
getBlock = fmap (fmap sbBlock) . getStoredBlock

-- | Returns header of block that was requested from Block DB.
getBlockHeader
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe (BlockHeader ssc))
getBlockHeader h = fmap T.getBlockHeader <$> getBlock h

-- | Sets block's inMainChain flag to supplied value. Does nothing if
-- block wasn't found.
setBlockInMainChain
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> Bool -> m ()
setBlockInMainChain h inMainChain =
    whenJustM (getBlock h) $ \blk ->
        putBi (blockKey h) $ StoredBlock blk inMainChain

-- | Get block with given hash from Block DB.
isBlockInMainChain
    :: (Ssc ssc, MonadDB ssc m)
    => HeaderHash ssc -> m Bool
isBlockInMainChain = fmap (maybe False sbInMain) . getStoredBlock

-- | Get undo data for block with given hash from Block DB.
getUndo
    :: (MonadDB ssc m)
    => HeaderHash ssc -> m (Maybe Undo)
getUndo = getBi . undoKey

-- | Put given block, its metadata and Undo data into Block DB.
putBlock
    :: (Ssc ssc, MonadDB ssc m)
    => Undo -> Bool -> Block ssc -> m ()
putBlock undo inMainChain blk = do
    let h = headerHash blk
    putBi
        (blockKey h)
        StoredBlock
        { sbBlock = blk
        , sbInMain = inMainChain
        }
    putBi (undoKey h) undo

deleteBlock :: (MonadDB ssc m) => HeaderHash ssc -> m ()
deleteBlock = delete . blockKey

-- | Load last @count@ blocks and corresponding undos.
-- Start from block with header hash equals @hash@.
-- The head of returned list is the youngest block.
loadLastNBlocksWithUndo :: (Ssc ssc, MonadDB ssc m)
                        => HeaderHash ssc -> Word -> m (NonEmpty (Block ssc, Undo))
loadLastNBlocksWithUndo _    0 = panic "Number of blocks must be nonzero"
loadLastNBlocksWithUndo hash count = NE.reverse <$> doIt hash count
  where
    doIt h 1 = (:| []) <$> getBlockWithUndo h
    doIt h n = do
        bu@(b, _) <- getBlockWithUndo h
        (bu<|) <$> doIt (b ^. prevBlockL) (n - 1)

getBlockWithUndo :: (Ssc ssc, MonadDB ssc m)
                 => HeaderHash ssc -> m (Block ssc, Undo)
getBlockWithUndo hash =
    maybe (throwM $ DBMalformed $ sformat errFmt hash) pure =<<
    (liftA2 (,) <$> getBlock hash <*> getUndo hash)
  where
    errFmt =
        ("getBlockWithUndo: no block or undo with such HeaderHash: " %shortHashF)

-- | Load blocks starting from block with header hash equals @hash@ and while @predicate@ is true.
-- The head of returned list is the youngest block.
loadBlocksWithUndoWhile :: (Ssc ssc, MonadDB ssc m)
                        => HeaderHash ssc -> (Block ssc -> Int -> Bool) -> m [(Block ssc, Undo)]
loadBlocksWithUndoWhile hash predicate = reverse <$> doIt 0 hash
  where
    doIt depth h = do
        bu@(b, _) <- getBlockWithUndo h
        let prev = b ^. prevBlockL
        if predicate b depth && (prev /= genesisHash)
            then (bu:) <$> doIt (succ depth) prev
            else pure []

-- | Takes a starting header hash and queries blockchain while some
-- condition is true or parent wasn't found. Returns headers newest
-- first.
loadHeadersWhile
    :: forall ssc m.
       (MonadDB ssc m, Ssc ssc)
    => HeaderHash ssc
    -> (BlockHeader ssc -> Int -> Bool)
    -> m [BlockHeader ssc]
loadHeadersWhile startHHash cond = reverse <$> loadHeadersWhileDo startHHash 0
  where
    errFmt =
        ("getBlockWithUndo: no block or undo with such HeaderHash: " %shortHashF)
    loadHeadersWhileDo :: HeaderHash ssc -> Int -> m [BlockHeader ssc]
    loadHeadersWhileDo curH depth = do
        curHeaderM <- getBlockHeader curH
        case curHeaderM of
            Nothing -> throwM $ DBMalformed $ sformat errFmt curH
            Just curHeader
                | cond curHeader depth && (curHeader ^. prevBlockL) /= genesisHash ->
                    (curHeader :) <$>
                    loadHeadersWhileDo (curHeader ^. prevBlockL) (succ depth)
                | otherwise -> pure []

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: forall ssc m.
       (Ssc ssc, MonadDB ssc m)
    => GenesisBlock ssc -> m ()
prepareBlockDB = putBlock [] True . Left

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

getBi
    :: (MonadDB ssc m, Bi v)
    => ByteString -> m (Maybe v)
getBi k = rocksGetBi k =<< getBlockDB

putBi
    :: (MonadDB ssc m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getBlockDB

delete :: (MonadDB ssc m) => ByteString -> m ()
delete k = rocksDelete k =<< getBlockDB

blockKey :: HeaderHash ssc -> ByteString
blockKey h = "b" <> convert h

undoKey :: HeaderHash ssc -> ByteString
undoKey h = "u" <> convert h
