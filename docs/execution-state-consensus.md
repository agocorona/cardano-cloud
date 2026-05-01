# Execution State Consensus: Permissionless Computation as a Merkle DAG

## The Problem

A blockchain achieves consensus over a linear sequence of state transitions. But a real computation is not linear — it branches. User inputs, timeouts, external events, parallel threads: each one can take the execution down a different path.

A consensus model for permissionless computation must reflect this reality.

## The Model

### Identity

A service is identified by two hashes:

```
service_id = hash(binary) + hash(previous_state)
```

- `hash(binary)` — identifies *what* is running: the exact executable, pinned in IPFS
- `hash(previous_state)` — identifies *where* it is in its execution: the serialized log produced by the transient runtime

### The Execution DAG

Computation is not a chain. It is a tree — or more precisely, a Directed Acyclic Graph (DAG):

```
         [S0] ──input_A──→ [S1] ──input_B──→ [S3]
          |                  |
          └──input_C──→ [S2] └──timeout──→ [S4]
```

Each node `[Sn]` is `hash(serialized_execution_state)` — the log that the transient runtime already produces via `logged`/`endpoint`.

Each edge is a transition:
```
(hash_binary, hash_S_prev, input) → hash_S_next
```

The full DAG lives in IPFS: immutable, content-addressed, without central authority.

### Consensus Per Transition, Not Per Tree

Consensus is **local to each edge**, not global to the tree.

> Given the same binary, the same previous state, and the same input — does the execution produce the same next state?

A randomly selected verifier node only needs to:
1. Fetch `hash_binary` and `hash_S_prev` from IPFS
2. Apply `input`
3. Check that the resulting `hash_S_next` matches the executor's claim

No knowledge of the full tree is required. No global coordination. Each transition is independently verifiable.

### Configurable Consensus

The number of verifiers per transition is a parameter of the service:

| Level | Description | Use case |
|---|---|---|
| `n=1` | No consensus, single executor | Development, low-risk services |
| `n=2` | One executor + one random verifier | Minimum for permissionless trust |
| `n=k of m` | Configurable quorum | High-value contracts |

Disagreement between verifiers triggers a challenge protocol — the disputed transition is re-executed by additional nodes until quorum is reached or the transition is rejected.

## Connection to the Transient Runtime

This model is not a new abstraction layered on top of transient. It is a formalization of what transient already produces.

The `logged`/`endpoint` mechanism in transient serializes the execution state at every step — inputs received, outputs produced, new state. That serialized log **is** the node content of the DAG.

What this consensus model adds:
- A content-addressed store (IPFS) for each node
- A verification protocol for each edge
- A quorum rule that makes individual transitions trustworthy without trusting any single executor

## Connection to Cardano

The accepted `hash_S_next` of a verified transition can be anchored on-chain — in transaction metadata or a script — giving off-chain computation an on-chain proof without executing everything on-chain.

This positions the execution DAG as a natural complement to Cardano's on-chain layer:

| Layer | Role |
|---|---|
| Cardano L1 | Settlement, anchoring of verified transition hashes |
| Execution DAG (IPFS) | Full history of all branches, immutable and auditable |
| Transient runtime | Produces the state serialization that populates each DAG node |
| Consensus protocol | Verifies individual transitions permissionlessly |

## Open Questions

- **Divergence handling**: if two executors start from the same state but receive different inputs (race condition), both branches are valid — how are competing branches disambiguated or merged?
- **Input availability**: verifiers need the same input the executor received — how is input canonicalized and stored so it is reproducible?
- **Challenge protocol**: what exactly happens when verifiers disagree — who pays the cost of re-execution?
- **Incentives**: what motivates nodes to act as verifiers for services they did not initiate?
