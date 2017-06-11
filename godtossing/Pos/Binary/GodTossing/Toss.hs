-- | Binary instances for Toss types.

module Pos.Binary.GodTossing.Toss
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi (..), Size (..), getWord8, label,
                                                putField, putWord8)
import           Pos.Ssc.GodTossing.Toss.Types (GtTag (..), TossModifier (..))

instance Bi GtTag where
    size = ConstSize 1
    put msgtag = case msgtag of
        CommitmentMsg     -> putWord8 0
        OpeningMsg        -> putWord8 1
        SharesMsg         -> putWord8 2
        VssCertificateMsg -> putWord8 3
    get = label "GtTag" $ do
        getWord8 >>= \case
            0 -> pure CommitmentMsg
            1 -> pure OpeningMsg
            2 -> pure SharesMsg
            3 -> pure VssCertificateMsg
            tag -> fail ("get@MsgTag: invalid tag: " ++ show tag)

instance Bi TossModifier where
    sizeNPut = putField _tmCommitments
            <> putField _tmOpenings
            <> putField _tmShares
            <> putField _tmCertificates
    get = label "TossModifier" $ do
        _tmCommitments <- get
        _tmOpenings <- get
        _tmShares <- get
        _tmCertificates <- get
        return $ TossModifier {..}
