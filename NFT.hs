{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.NFT where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool --txOutRef references tracsaction output (utxo) ->token name of NFT ->redeemer -> Context-> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           && --check whether utxo not consumed
                          traceIfFalse "wrong amount minted" checkMintedAmount --only mint the token once and exactly once
  where
    info :: TxInfo --helper function used by other functions
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info --it checks any inputs that has consumed the given oref utxo

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of --
        [(_, tn', amt)] -> tn' == tn && amt == 1 --check the amount or nft coin is 1
        _               -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy --compiling the policy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

data NFTParams = NFTParams
    { npToken   :: !TokenName
    , npAddress :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    utxos <- utxosAt $ npAddress np --returns the utxos on the address
    case Map.keys utxos of --mapping utxos
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken np --token name
            let val     = Value.singleton (curSymbol oref tn) tn 1 --val
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos --providing utxos for building transaction but right now providing only one 
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref --making oref utxo to spend pubkey output
            ledgerTx <- submitTxConstraintsWith @Void lookups tx --submitted transaction
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx --waiting
            Contract.logInfo @String $ printf "forged %s" (show val) --logging result

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint --calling endpoint
--running emulator
test :: IO ()
test = runEmulatorTraceIO $ do 
    let tn = "ABC"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
