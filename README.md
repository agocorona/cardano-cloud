# Cardano Cloud: Persistent Runtime Foundation for Off-Chain DSLs

A serverless Haskell monad that turns smart-contract specs into reusable library calls that run verifiable, restart-proof flows.

This is an stub where examples and specific cardano-cloud primitives will be included. That is
- integration with ogmios, cardano-api, cardano-node.
- Specific cardano-cloud patterns and primitives.

## Status
- Core runtime: Working. It essentially the Transient stack with additions for persistence
    - like persistent collect https://x.com/AGoCorona/status/1993762316358209733?s=20
    - The transient stack: https://github.dev/transient-haskell/transient-stack
- Most of the work carried out now is devoted to develop and tune the persistence primitives
    - To preserve non serializable state across shutdowns and restarts. Done
    - To manage state when many requests are summarized in a single response such is the case in collect and algebraic operations with various terms
    - Specially when the state includes non serializable things like backtracking handlers
          - imagine millions of of requests like this
```haskell
 ...
 allInvestments <- collect anyNumber forOneMonth $ do
        investment <- minput "/invest" InvestmentPayload
        return investment `onBack` \FailedFunding -> do
            wallet <- getInvestorWallet investment
            fees   <- estimateFees
            refund wallet (investmentAmount investment - fees)
 ...
```
Should get HTTP requests from investors for a month even if the program should restart.

for each request, and there may be thounsands of them, the failed funding handler should:

- respond to the FailedFunding backtracking event even if the programs was restarted in the middle
- get fees that was set at the end of the contract 
- refund the investor even if the server has been shut down in the middle

Testnet integration: In progress (Milestone 1, Jan 2026)

Full MVP: Q1 2026 via Catalyst F15 / IOR CFP

## Catalyst proposal

https://app.projectcatalyst.io/proposal/019a8454-c3c7-768a-89c6-d6a7a9c6b371

## Example: Auction
```haskell
defiAuction = do
  lockTx <- liftCTL $ lock 10_000_000 "initial"
  bids <- collect 100 3600000000 $ minput "/bid" minPayload
  winner <- selectWinner bids
  liftCTL $ pay winner (amount winner * 1_000_000)
