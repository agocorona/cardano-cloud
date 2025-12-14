**Cardano Cloud: Persistent Runtime Foundation for Off-Chain Domain-Specific Languages in eUTxO Smart Contracts**

**Abstract**

Cardano’s eUTxO model and Plutus offer strong on-chain guarantees, but the majority of contract logic remains off-chain, where it is verbose, lacks static verification, loses state on restarts, and requires manual handling of UTxO races. This limits the development of reliable, long-running applications in domains such as legal agreements, supply chains, and generational assets.

Cardano Cloud introduces a Haskell monad that expresses any monadic and algebraic combination of computations — threading, parallelism, multi-user web endpoints, streaming, durability, backtracking, and distributed execution — in simple `do` blocks using ordinary Haskell.

These blocks form a general-purpose DSL in which requirement specifications map one-to-one to code, without analysis, reordering, or adaptation. The runtime is the execution environment itself, transparently providing node orchestration, distributed architecture, persistence, recovery, dynamic endpoint generation, and all necessary infrastructure — fully programmable and compilable in standard Haskell without external tools.

State machines are replaced by monadic sequences; endpoint tables by dynamic endpoints; user session states by stack variables; checkpoints by stack serialization; databases by optional IPFS; node configurations become program logic; callbacks integrate seamlessly; streams, exceptions, and retries preserve the high-level description.

The result is a single, statically-verified Haskell expression that encapsulates multi-user, multi-node processes and can be packaged as a reusable library function.

Cardano Cloud provides the first persistent, serverless, distributed off-chain runtime for Cardano, serving as the foundational layer for third-party DSLs. As proof-of-concept, the runtime will execute representative Marlowe contracts, demonstrating extension of existing Cardano DSLs with persistence and distribution not available today.

(248 words)

**Research Team and Institutional Profile**

Lead investigator: Alberto Gómez (@agocorona), independent research engineer with more than fifteen years of experience in Haskell. Maintainer of the Transient library (600+ GitHub stars, nine years in production). Delivered DAppFlow in Catalyst Fund 3 (7 000 ADA, 100 % audited close-out report). Active in the Cardano community as Plutus Pioneer mentor and speaker.

The project is led by an independent researcher with a strong open-source focus (all deliverables under MIT licence). No formal institutional affiliation is held at present.

Negotiations are currently underway with several Haskell and Cardano teams (Tweag, Well-Typed, Gimbalabs, MLabs) to form a consortium. These discussions, initiated in early December 2025, have not yet been consolidated due to time constraints ahead of the submission deadline, but remain active. The lead investigator remains open to incorporating partners for formal verification expertise and broader adoption support.

Staffing for the 2026 MVP is 1.0 FTE (lead investigator), with provision for additional contributors if agreements are reached post-submission.

**Scope and Objectives**

Scope  
The project will develop Cardano Cloud as a complete, self-contained off-chain runtime for Cardano, implemented entirely in standard Haskell without external orchestration tools or configuration files. The runtime itself provides persistence, distributed execution, dynamic endpoint generation, node orchestration and recovery mechanisms. It is not a translator for external EDSLs but the execution environment in which any domain-specific language can be expressed and run.

The MVP is limited to the core primitives, Cardano integration, two testnet prototypes (auction and distributed DAO vote), and a proof-of-concept execution of representative Marlowe contracts. Full domain-specific DSL development is deferred to future phases.

Objectives  
1. Deliver a persistent, serverless, distributed off-chain runtime that serves as the foundation for third-party domain-specific languages in legal agreements, supply chains and generational assets.  
2. Enable one-to-one mapping of requirement specifications to executable Haskell code, with full static verification.  
3. Formalise the continuation-based semantics that underlie persistence and distribution in eUTxO off-chain execution.  
4. Demonstrate the runtime’s ability to execute representative Marlowe contracts (escrow and zero-coupon bond) as proof-of-concept.  
5. Publish a research paper on continuation semantics for off-chain DSL execution in the extended UTXO model.

Key Innovation  
The runtime replaces traditional state machines with monadic sequences, static endpoint tables with dynamic generation, session state with stack variables, ad-hoc data checkpoints with execution stack serializations, external databases with optional IPFS, and separate node configuration with program logic itself. Callbacks, streams, exceptions and retries integrate without breaking the high-level description.

## Illustrative Examples

### Distributed DAO Vote (multi-year, fault-tolerant, leaderless)

```haskell
distributedVotation duration options = do
    aggregated <- collectp 0 duration $ do
        workerNode <- local getMailbox
        runAt workerNode $ do
            localResults <- collectp 0 duration $ voteapi "/vote" options
            checkpoint                                   -- store-and-forward on failure
            return localResults

    peerNodes <- getPeerNodes
    sync peerNodes                                       -- replicate state across peers, if necessary

    checkpoint                                           -- persist thread on return failure
                                                         -- resend result when calling node notifies readiness

    return $ flatten aggregated
```

### Crowdfunding with automatic refund if goal not reached

```haskell
data FailedFunding= FailedFunding

crowdFunding desiredAmount duration = do
    investments <- collectp 0 duration $ do
        investment <- minput "/invest" InvestmentPayload
        return investment `onBack` \FailedFunding -> do
            wallet <- getInvestorWallet investment
            fees   <- estimateFees
            refund wallet (investmentAmount investment - fees)

    let total = sum (map investmentAmount investments)

    guard (total > desiredAmount) <|> back FailedFunding

    payTo projectOwner total
```

### A set of auctions where the client opt for one of the products. Receive a stream of bids (with chunked encoding) and can stream back its own bids at any moment for that product

```haskell
auctions prods= foldr (<|>) empty $ map auctionStream prods
-- send a set of links and their current status

auctionStream prod= do
    stateBid <- getStateBid prod
    minput "/enterAuction" stateBid   -- the user opt for that product

    (minput "/enterBid" message >>= updateState)  <|> streamStates <|> streamAnyOtherThings
    
  where
    currentState = getStateBid prod >>= moutput
    streamStates = currentState <|> 
        if endBid then empty else streamStates
```


### Dynamic Collateral Lending with Reactive Volatility Adjustment

```haskell
dynamicCollateralLending = do
    -- Initialise reactive state
    setRState $ CurrentCollateral 0

    -- Borrower requests loan amount
    loanAmount <- minput "/getLoan" "enter loan amount"

    -- Persistence point: ensures the contract resumes correctly after restarts
    checkpoint

    -- Reactive calculation: requiredCollateral updates automatically
    -- whenever volatilityFactor changes (stream from oracle)
    requiredCollateral <- local $ loanAmount * baseRatio * (1 + volatilityFactor)

    -- Read current collateral from reactive state
    CurrentCollateral currentCollateral <- getRState

    -- Compute shortfall (positive if more collateral needed)
    let shortFall = requiredCollateral - currentCollateral

    if shortFall > 0 then do
        -- Collect additional collateral deposits (unlimited number, with a time limit)
        coll <- collect 0 time $ 
            minput "/enterCollateral" Collateral{explanation, amount = shortFall}
            
        -- Check if total additional collateral meets the shortfall
        if sum (map amount coll) < shortFall
            then
                -- Insufficient collateral provided
                if currentCollateral == 0
                    then moutput "Loan request rejected: insufficient collateral"
                    else liquidation currentCollateral   -- trigger liquidation of existing collateral
            else do
                -- Sufficient collateral received: return excess to borrower (negative shortfall)
                w <- getWallet
                payTo w (-shortFall)                     -- refund excess payment
                setRState $ CurrentCollateral requiredCollateral
    else
        -- No shortfall (excess collateral already present)
        return ()

  where
    -- Continuous stream of volatility values from oracle
    volatilityFactor = waitEvents $ poll oracle for volatility calculations
```

### Explanation

This contract implements a **dynamic collateral lending protocol** with the following key properties:

- **Reactive volatility adjustment**: `volatilityFactor` is a continuous stream from an oracle. The expression `requiredCollateral <- local $ loanAmount * baseRatio * (1 + volatilityFactor)` automatically recomputes whenever volatility changes, without explicit loops.
- **Automatic shortfall detection**: `shortFall` is recalculated reactively as volatility or collateral changes.
- **Collateral collection**: Users can deposit additional collateral via repeated `/enterCollateral` inputs.
- **Liquidation on failure**: If the total deposited collateral is insufficient, the contract triggers liquidation of existing collateral (unless no collateral was provided, in which case the loan is simply rejected).
- **Excess refund**: If more collateral is provided than required, the excess is automatically refunded.
- **Persistence**: `checkpoint` ensures the contract state survives node restarts or network interruptions.

The design relies on the runtime’s reactive and persistent primitives to provide continuous, automatic adjustment without manual polling or external orchestration.

These examples illustrate how complex, long-running, multi-user contracts reduce to concise, verifiable Haskell code while the runtime handles persistence, distribution, streaming, recovery and automatic undo on failure.

**Work Plan and Approach**

Methodology  
The runtime is built on Haskell monads extended with continuation-based concurrency. The high-level primitives — `lockAt`, `payTo`, `getBalance`, `waitUntilBalance`, `collect`, and `currentSlot` — encapsulate all interaction with the Cardano chain, providing a clean interface for contract developers. These primitives handle UTxO selection, transaction balancing, fee calculation, signing, and submission internally, while automatically applying backtracking on failure.

Additional primitives such as `checkpoint` (persistence across restarts) and `sync` (state replication and distributed continuation) enable long-running and multi-node execution. Streaming support is native: `minput` and `moutput` combined with alternative composition produce continuous, bidirectional HTTP streams using chunked encoding, allowing real-time UI updates, push oracles, and monitoring without additional infrastructure.

The runtime is the execution environment itself: node orchestration, dynamic endpoint generation, and inter-node communication are expressed directly in the contract code. No external configuration or orchestration tools are required.

Approach  

Month 1  
Complete the high-level primitives (`lockAt`, `payTo`, `getBalance`, `waitUntilBalance`, `collect`, `currentSlot`) and core persistence/distribution primitives (`checkpoint`, `sync`). Achieve Hackage release v0.3 with >90 % test coverage. Internal testing in emulator and local node environment.

Month 2  
Deploy two testnet prototypes on Preview/Preprod:  
- A DeFi auction using `collect` and `waitUntilBalance`, with streaming progress updates.  
- A distributed DAO vote demonstrating multi-node aggregation via `sync` and persistence across restarts via `checkpoint`.  
Each prototype will generate at least 10 valid on-chain transactions and demonstrate backtracking on simulated failures. Public demonstration and logs available.

Month 3  
Develop proof-of-concept execution of representative Marlowe contracts (escrow and zero-coupon bond) using the runtime primitives. Draft research paper on continuation-based semantics for persistence and distribution in eUTxO off-chain execution, submitted to suitable venue. Prepare workshop materials and final open-source release (Hackage v0.5).

Future extensions (post-MVP, subject to further funding):  
- Global voluntary compute swarm via IPNS resolution.  
- Enhanced formal verification of continuation semantics.
- Stronger types to codify invariants at the type level, using Transient.TypeLevel

Risks and Mitigation  
Technical integration delays: fallback to minimal Cardano transaction wrappers.  
Single-researcher scope: all code open-source from day one with clear handover plan. Community testing will be encouraged throughout.

**Deliverables and Milestones**

Milestone 1 (end of Q1 2026)  
- Hackage release v0.3 of the core runtime with high-level primitives (`lockAt`, `payTo`, `getBalance`, `waitUntilBalance`, `collect`, `currentSlot`) and persistence/distribution primitives (`checkpoint`, `sync`).  
- Test suite achieving >90 % coverage.  
- Internal demo video showing persistence across restarts and basic distributed execution in emulator/local node environment.

Milestone 2 (end of Q2 2026)  
- Deployment of two testnet prototypes on Preview/Preprod:  
  - DeFi auction with timed bid collection, automatic payout, and streaming progress updates.  
  - Distributed DAO vote demonstrating multi-node aggregation, persistence across restarts, and streaming vote tally.  
- Each prototype generating at least 10 valid on-chain transactions and demonstrating backtracking on simulated failures.  
- Public demonstration and logs available.

Milestone 3 (end of Q3 2026)  
- Proof-of-concept execution of representative Marlowe contracts (escrow and zero-coupon bond) using Cardano Cloud primitives.  
- Draft research paper on continuation-based semantics for persistence and distribution in eUTxO off-chain execution, submitted to suitable venue.  
- Final Hackage release v0.5.  
- Generic JavaScript client library for all Cardano Cloud applications, supporting bidirectional streaming (`minput`/`moutput`), state subscription, and transaction submission from browser environments.  
- Workshop materials and documentation for community adoption.

Final Deliverables  
- Fully open-source runtime (MIT licence) with comprehensive documentation.  
- Two verifiable testnet prototypes.  
- Marlowe proof-of-concept demonstrating extension of existing Cardano DSLs.  
- Generic JavaScript client for seamless frontend integration.  
- Research paper on the formal semantics underlying the runtime.

**Staffing, Budget, and Justification**

Staffing  
The project will be led by the principal investigator (1.0 FTE), who will carry out all core development, testing, and documentation. Negotiations are underway with several Haskell and Cardano teams to establish formal collaboration. These discussions, initiated in early December 2025, have not yet been consolidated due to time constraints ahead of the submission deadline, but remain active. Should agreements be reached, additional expertise in formal verification and community adoption will be incorporated, providing the team diversity valued by the call. In particular, a collaborating Haskell firm will contribute to the research paper on continuation-based semantics.

Budget (2026, total $120,000 USD)  
- Personnel: $100,000 (83 %) — lead investigator time (12 months at average $8,333/month)  
- Knowledge transfer and formal assessment: $20,000 (17 %) — contract with a major Haskell firm for knowledge transfer, formal verification support, independent assessment of the continuation-based approach, and co-authorship of the research paper  

Justification  
The requested amount reflects a 12-month timeline required for robust implementation, thorough testnet validation, Marlowe proof-of-concept, and a research paper of publishable quality. The budget remains modest relative to the theme allocation while providing sufficient resources for high-impact foundational infrastructure that enables multiple future DSLs. The dedicated allocation for knowledge transfer and formal assessment ensures independent review, long-term sustainability, and strengthened academic output. All funds are directed to delivery; overhead is minimal. Payment schedule: 40 % upfront, 30 % at Milestone 2, 30 % at completion.

**References**

- Gómez, A. (2021). DAppFlow: Continuous Workflows for Cardano.  https://github.com/agocorona/DAppFlow, 

- Gómez, A. (2021) - Catalyst Fund 3 close-out report. https://drive.google.com/file/d/1LaVgVOMgZA9yTw8wafrAkX0-kDnKcKWe/view

- Transient Haskell library. GitHub repository: https://github.com/transient-haskell/transient-stack.

- Marlowe Team. Marlowe: A domain-specific language for financial contracts on Cardano. Input Output Global documentation and publications.

- Plutus Team (2023). Plutus Application Backend deprecation announcement. Input Output Global.

- Chakravarty, M. et al. Extended UTXO model and Plutus Core. Input Output Global research papers.

- Jones, S. P. et al. Continuation-based concurrency and monadic programming in Haskell. Various publications.

- Hoskinson, C. et al. Hydra and Leios: Cardano scaling roadmap. Input Output Global updates.

