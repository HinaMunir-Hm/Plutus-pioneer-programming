{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

import Week04.Vesting --imported vesting file

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace --run the trace emulator and returns the result on console

myTrace :: EmulatorTrace ()--endpoint to start the contract it-self
myTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints --actiavteContractWallet takes two arguments 1st wallet and 2nd contract result will be in contract handle h1
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParams --call endpoint function , @"give" is a type of endpoint, GiveParams->beneficiary, deadline,  pubkey hash
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s
