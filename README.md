# Cardano Cloud: Persistent Runtime Foundation for Off-Chain DSLs

A serverless Haskell monad that turns smart-contract specs into verifiable, restart-proof flows.

This is an stub where examples and specific cardano-cloud primitives will be included. That is
- integration with ogmios, cardano-api, cardano-node.
- Specific cardano-cloud patterns and primitives.

## Status
- Core runtime: Working. It essentially the Transient stack with additions for persistence
    - like persistent collect https://x.com/AGoCorona/status/1993762316358209733?s=20
    - The transient stack: https://github.dev/transient-haskell/transient-stack
- Testnet integration: In progress (Milestone 1, Jan 2026)
- Full MVP: Q1 2026 via Catalyst F15 / IOR CFP

## Catalyst proposal

https://app.projectcatalyst.io/proposal/019a8454-c3c7-768a-89c6-d6a7a9c6b371

## Example: Auction
```haskell
defiAuction = do
  lockTx <- liftCTL $ lock 10_000_000 "initial"
  bids <- collect 100 3600000000 $ minput "/bid" minPayload
  winner <- selectWinner bids
  liftCTL $ pay winner (amount winner * 1_000_000)
