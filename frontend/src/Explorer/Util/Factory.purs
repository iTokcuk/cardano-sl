module Explorer.Util.Factory where

import Prelude
import Data.Array (foldl)
import Data.Time.NominalDiffTime (mkTime)
import Data.Tuple (Tuple(..))
import Data.Lens ((^.))
import Pos.Core.Lenses.Types (_Coin, getCoin)
import Pos.Core.Types (Coin(..))
import Pos.Explorer.Web.ClientTypes (CAddress(..), CAddressSummary(..), CHash(..), CTxEntry(..), CTxId(..))


mkCHash :: String -> CHash
mkCHash = CHash

mkCTxId :: String -> CTxId
mkCTxId =
    CTxId <<< mkCHash

mkCoin :: Int -> Coin
mkCoin coin =
  Coin {getCoin: coin}

mkCAddress :: String -> CAddress
mkCAddress = CAddress

-- | Helper to summarize coins by a list of Tx inputs or outputs
sumCoinOfInputsOutputs :: Array (Tuple CAddress Coin) -> Coin
sumCoinOfInputsOutputs list =
    mkCoin $ foldl (\acc (Tuple _ coin) -> (+) acc $ coin ^. (_Coin <<< getCoin)) 0 list

-- All the following helper function `mkEmpty**` are for debugging only
-- We do need these to mock live data
-- It can be removed if all endpoints are ready


mkEmptyCTxEntry :: CTxEntry
mkEmptyCTxEntry = CTxEntry
    { cteId: mkCTxId "--"
    , cteTimeIssued: mkTime 0.0
    , cteAmount: mkCoin 0
    }

mkEmptyCAddressSummary :: CAddressSummary
mkEmptyCAddressSummary = CAddressSummary
    { caAddress: mkCAddress "--"
    , caTxNum: 0
    , caBalance: mkCoin 0
    , caTxList: []
    }
