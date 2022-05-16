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

module Week05.Free where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
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
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

--on-chain part
{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool --validation script as in minting only redeemer and context we have so redeemer is the () and context is ScriptContext and it returns a Bool
mkPolicy () _ = True -- this will allow all sort of mint or burn for currency symbol given by policy
--compiling the policy into plutus script
policy :: Scripts.MintingPolicy --result is minting policy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])--
--we can get currency symbol
curSymbol :: CurrencySymbol 
curSymbol = scriptCurrencySymbol policy --can get currency symbol using function script currency symbol applied to policy

--off chain part
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()--end point mint takes MintParams if amount is positive then forged else burn 
mint mp = do
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)--val we want to forged or burn 
        lookups = Constraints.mintingPolicy policy --minting policy
        tx      = Constraints.mustMintValue val --must mint the value we computed before
    ledgerTx <- submitTxConstraintsWith @Void lookups tx --transfer minted value to the wallet if positive else find tokens to burn in user's wallet
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx --wait for confirmation
    Contract.logInfo @String $ printf "forged %s" (show val)--and then log a message

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint --calling endpoint here

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

test :: IO ()--running emulator
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1
