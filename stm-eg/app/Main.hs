module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           GHC.Conc
import           System.IO              ()

type Account = TVar Int

limitedWithdraw :: Account
                -> Int
                -> STM ()
limitedWithdraw acc amount = do
    bal <- readTVar acc
    check (amount <= 0 || amount <= bal)
    writeTVar acc (bal - amount)

showAcc :: Show a => String
                  -> TVar a
                  -> IO()
showAcc name acc = do
    bal <- atomically (readTVar acc)
    putStrLn (name ++ ": $")
    putStrLn (show bal ++ "\n")

limitedWithdraw2  :: Account
                  -> Account
                  -> Int
                  -> STM ()
-- (limitedWithdraw2 acc1 acc2 amt) withdraws amt from acc1,
-- if acc1 has enough money, otherwise from acc2.
-- If neither has enough, it retries.
limitedWithdraw2 acc1 acc2 amt
  = orElse (limitedWithdraw acc1 amt) (limitedWithdraw acc2 amt)

limitedWithdraw3  :: Account
                  -> Int
                  -> STM ()
limitedWithdraw3 acc1 amt = orElse (limitedWithdraw acc1 amt) failure
  where
    failure :: STM ()
    failure =  unsafeIOToSTM $ putStrLn "Not enough founds"

delayDeposit :: (Num a, Show a) => String
                                -> TVar a
                                -> a
                                -> IO ()
delayDeposit name acc amount = do
    threadDelay 3000000
    putStrLn ("Depositing $" ++ show amount ++ " into " ++ name ++ "\n")
    atomically ( do bal <- readTVar acc
                    writeTVar acc (bal + amount) )

main1 :: IO ()
main1 = do
    acc1 <- atomically (newTVar 100)
    acc2 <- atomically (newTVar 100)
    showAcc "Left pocket" acc1
    showAcc "Right pocket" acc2
    _ <- forkIO (delayDeposit "Right pocket" acc2 1)
    putStrLn "Withdrawing $101 from either pocket...\n"
    atomically (limitedWithdraw2 acc1 acc2 101)
    putStrLn  "Successful withdrawal!\n"
    showAcc "Left pocket" acc1
    showAcc "Right pocket" acc2

main :: IO ()
main = do
    acc <- atomically (newTVar 100)
    showAcc "Account: " acc
    _ <- forkIO (delayDeposit "Account"  acc 1)
    putStrLn "Withdrawing $101 from either pocket...\n"
    atomically (limitedWithdraw3 acc 101)
    showAcc "Account " acc
--
