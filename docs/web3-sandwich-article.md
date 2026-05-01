# The Web3 Sandwich — and why fixing the infrastructure isn't enough

*This article is a reaction to [this video](https://www.youtube.com/watch?v=-ih0-eMaha8) in which Charles Hoskinson reads and comments on Moxie Marlinspike's essay [My first impressions of Web3](https://moxie.org/2022/01/07/web3-first-impressions.html) (2022). Moxie — founder of Signal and one of the most respected voices in cryptography and security — argued that Web3 was quietly re-centralizing around a handful of infrastructure providers, betraying its own founding promise. Charles used that critique to frame Cardano's strategy around Midnight and Blockfrost. This article takes that conversation one step further.*

---

Moxie nailed it. Your "decentralized" DApp is a React website. It doesn't talk to the blockchain — it talks to Infura. Infura talks to the blockchain.

But the rabbit hole goes deeper than Moxie described.

The centralization doesn't just happen at the top, with the APIs serving browsers. It also happens at the bottom, with the nodes themselves — and for exactly the same reason Moxie gives: **nobody wants to run their own servers.**

---

## The sandwich

```
        [ Users / browsers ]
               ↓
    [ INFURA ]   [ ALCHEMY ]       ← dark layer: centralized access
               ↓
    ════════ BLOCKCHAIN ════════   ← the bright middle
               ↑
  [ AWS ]            [ AZURE ]     ← dark layer: centralized infrastructure
```

The blockchain is decentralized at the protocol level. But the nodes running it live on Amazon. And the apps reading it go through Infura. You're building a sovereign network on rented ground, with a centralizing lens on top.

---

## Why nobody runs their own servers — in one line each

- **Availability**: a server must be on 24/7, and when it dies at 3am, your contract dies with it
- **Maintenance**: security patches, kernel updates, firewall configs — a full-time job on top of your actual work
- **Backups**: one hardware failure without a tested recovery strategy means total data loss
- **Networking**: fixed IPs, NAT, port forwarding, DNS — a discipline unto itself
- **Economics**: AWS is cheaper than doing it right yourself

This isn't laziness. It's rational.

---

## The infrastructure fix

The solutions exist:

- **Hash-based addressing** instead of IP: point to *what* is running, not *where*. When a node goes down, the network finds another node with the same service that can recover the same execution state and continue from that state — no static IP needed, no load balancer
- **State persistence on IPFS**: the server checkpoints its execution state to a distributed store. When it recovers — or when another node picks it up — it resumes from where it left off
- **Self-contained binaries**: a single executable with no external dependencies. No environment to configure, no infrastructure to manage. You contribute compute; the function knows how to keep itself alive. There is no separate configuration; it is part of the execution state.



- **Execution state consensus**: once services and states are identified by hashes, randomly selected nodes can independently verify any individual transition — same binary, same previous state, same input, same result. This makes permissionless execution trustworthy without central coordination and without re-executing the full history.

This is the *liquid server*: not a machine in a rack, but an execution state floating in a distributed network.

---

## The missing layer: the application itself

The top layer of the sandwich — browsers and mobile apps — has the same problem from the other direction. Those apps are not participants in any distributed network either. They are downloaded from centralized servers, identified by URLs and store listings controlled by companies, not by content hashes. The same censorship and surveillance risk applies: the platform that delivers the app can remove it, and the API provider it connects to can filter or watch everything it does.

The fix is the same as for the server layer: content-addressing. An application identified by its hash can be loaded from any node that has it — no domain, no App Store approval, no single point of removal.

Once both the application and the execution state are identified by hashes, a client request is no longer bound to a specific server. Any node in the distributed network that holds the application binary and the relevant state hash can handle it. The request routes to wherever capacity is available — no central dispatcher, no single point of failure, no single point of surveillance. The bottleneck disappears by design.

And here is where the model unifies. A user request arriving at a node is structurally identical to a server resuming after a crash. In both cases, a continuation needs a node willing to execute it from a known state. In the crash case the state was persisted to IPFS. In the web request case the state is the user's session — but the execution demand is the same: *here is a binary hash, here is a state hash, here is an input, produce a response and continue*.

The difference is only the final step: after a crash, the continuation resumes silently. After a web request, the continuation sends a response back to the user. Same mechanism. Same runtime. Same distributed network of nodes that can serve it.

---

## But the programming model has to change too

Moxie saw this coming too. In the same essay he writes that software already demands an absurd amount of human effort — that even simple apps require a group of people in front of a computer eight hours a day, forever. And that distributed systems, rather than helping, tend to make things *more* complicated and more difficult, not less. His conclusion: as long as software requires so much specialized effort, it will serve the interests of the people building it, not broader goals.

That's not just a critique of Web3. It's a critique of what distribution did to programming itself.

Here's what the infrastructure conversation misses.

Even if you solve addressing, persistence and deployment — even if every node is sovereign and self-healing — your smart contract can still be a mess of callbacks, scattered state and rollback logic that nobody can reason about.

There was a time when a program read like what it did. A shell script, a console app — the logic was there, line by line, plain to see. Then distribution happened. Suddenly your clean intention — *submit a transaction, wait for confirmation, refund if it fails* — had to be shredded across message queues, event listeners, retry workers, state machines and database tables, just to survive a network boundary. A dozen frameworks tried to paper over this: Actor systems, reactive streams, saga orchestrators, workflow engines — each with its own mental model, its own failure semantics, its own lock-in. None of them composable with each other. All of them replacing the legibility of your original idea with the accidental complexity of their own abstractions.

The clean code you had in your mind — the direct expression of what you intended — was destroyed not by the problem, but by the infrastructure required to run it across machines.

The *liquid server* needs a *liquid contract*.

A contract where execution state is a first-class value, not something buried in a database schema. Where compensation logic is co-located with the action it compensates, not scattered across error handlers. Where blockchain events — new blocks, mempool transactions — are just values your contract reads, not callbacks you register and wire.

And crucially: where the boundary between server, node and browser is not a configuration decision made at deploy time, but a logic decision made by the program itself at runtime.

In the transient model, an application is written as a single monolithic program. Distribution is not an architecture imposed from outside — it is expressed inside the program, as part of its logic. A computation can decide, based on what it knows about its environment and the capabilities of each participant, whether a step runs on the server, on a remote node, or directly in the browser. The same program that coordinates a multi-node smart contract can serve a web interface, handle browser wallet interactions, and run lightweight logic client-side — not because a DevOps configuration says so, but because the program says so.

This means the top layer of the sandwich — the one Moxie criticized, where browsers can never be real peers — is not a hard constraint. It is a consequence of the programming models currently in use. A program that controls its own distribution can push as much or as little as makes sense to each participant, including the browser.

This is what cardano-cloud does.

```
-- ChainSync and mempool: not APIs. Just values.
block  ←  next block from chain
tx     ←  next tx from mempool

-- Compensation: written once, guaranteed always
on failure → refund user

submit tx
wait for confirmation in block
done
```

The execution state is serialized at every step. If the server crashes, another node picks up the log and continues. The `on failure → refund user` line runs even across restarts — not because the developer managed it, but because the runtime guarantees it.

The infrastructure makes the server liquid.
The programming model makes the contract liquid.

You need both.

---

## What if the protocol were just a program?

Moxie makes another observation that cuts deep: *"A protocol moves much more slowly than a platform."* The reason is structural. A protocol is a specification implemented independently by multiple parties. Changing it requires coordinating all of them simultaneously — different teams, different codebases, different incentives. Email has existed for decades without native encryption not because encryption is hard, but because changing a protocol means getting everyone to agree at once. A centralized platform can ship a fix tomorrow.

This is the same root problem as the complexity explosion in distributed programming. In both cases, distribution is treated as something external to the logic — a specification that must be interpreted by many independent implementations, or an architecture imposed from outside the code. The result is always the same: coordination overhead, divergence, rigidity, and complexity that crushes the original intent.

But what if the protocol were not a specification distributed among independent implementations — what if it were a single program?

A program written as a monolithic specification of the full logic. A program that distributes itself not through an external infrastructure layer, but through how it is started on each node. Each node that runs the program declares, through its own startup context and capabilities, which branches of the logic it executes. There is no orchestrator. There is no configuration file mapping roles to machines. The distribution is in the code itself:

```
main = keep $ initNode $
    inputNodes <|> serverLogic <|> browserLogic
browserLogic= do
    request <- onBrowser $ widtgets...
    resp <- atServer $ do...
    ...
serverLogic= onServer $ do
    r <- onAllServerNodes $ notify..
    onAllClientNodes....
```

A node that starts with browser capabilities executes the browser branches. A node with server capacity executes the server branches. The same program, started differently, produces the distributed system — without any external coordination layer.

In this model a protocol is not a document. It is a binary identified by a hash. Updating it means publishing a new hash. Nodes adopt the new version by fetching the new binary — atomically, without coordination between independent implementors, without versioning committees. It moves faster than a centralized platform with managed infrastructure, because there is no infrastructure to manage.

And because the program is written as a single expression of the full logic — not shredded across frameworks, message queues and configuration files — it can be understood, reasoned about and verified at the level of its specification. The complexity that Moxie predicted would make distributed systems harder, not easier, disappears when distribution is a property of how the program starts, not of how it is architectured.

---

## What Moxie proposed — and what this adds

Moxie said: accept that nobody runs servers. Use cryptography to distribute trust, not infrastructure.

The full stack requires every layer to be addressed:

| Layer | Solution |
|---|---|
| Chain | **Midnight** — zero-knowledge proofs, multiparty computation, trusted execution |
| Infrastructure | **Blockfrost** — a decentralized Alchemy for the whole ecosystem |
| Programming runtime | **Transient** — durable execution state, composable distributed computation, typed backtracking |
| Cardano application | **cardano-cloud** — Transient applied to Cardano smart contracts |

Midnight and Blockfrost are well-funded projects with teams behind them. Transient is different: nine years of independent research, demonstrating that the programming model layer is not a theoretical problem but a solved one — in running code, available today.

cardano-cloud is built on that foundation. It is the evidence that the last piece of the puzzle already exists.

---

## The Web3 rabbit hole runs deep. So does the fix.

The problem Moxie diagnosed in 2022 isn't just about Infura. It's about incentives. Developers centralize because the decentralized path is harder.

If the decentralized path reads like a whiteboard sketch — if chain events are just values, if rollback is one line, if the whole contract fits on a napkin — the incentive to shortcut disappears.

That's the bet.

---

## A note on where this stands

The infrastructure requirements described here — hash-based addressing, serialized execution state, migration between nodes, guaranteed compensation across restarts — are strict. Taken together, they are not the roadmap of any current project, framework or protocol that we are aware of, except one.

[Transient](https://github.com/agocorona/transient) has spent nine years demonstrating that these properties are not theoretical. Durable execution state, composable distributed computation, typed backtracking that survives restarts — these work today, in running code. Not at production scale, not with all the pieces in place, but proven possible and within reach.

cardano-cloud is the application of that foundation to Cardano smart contracts. It is a research project, not a finished product. But it is evidence that the programming model piece of this puzzle has a concrete answer — one that already exists, already runs, and already composes.

The rest — IPFS anchoring, execution state consensus, permissionless verification — are the natural next steps for a runtime that was designed, from the beginning, with exactly these properties in mind.

---

*cardano-cloud: https://github.com/agocorona/cardano-cloud*
*Transient: https://github.com/agocorona/transient*
