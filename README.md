# Cardano Cloud: Persistent Runtime Foundation for Off-Chain DSLs

A serverless Haskell monad that turns smart-contract specs into verifiable, restart-proof flows.

## Status
- Core runtime: Working (emulator/local Ogmios)
- Testnet integration: In progress (Milestone 1, Dec 2025)
- Full MVP: Q1 2026 via Catalyst F15 / IOR CFP

## Example: 25-line Auction
```haskell
defiAuction = do
  lockTx <- liftCTL $ lock 10_000_000 "initial"
  bids <- collect 100 3600000000 $ minput "/bid" minPayload
  winner <- selectWinner bids
  liftCTL $ pay winner (amount winner * 1_000_000)
