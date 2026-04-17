Multisign, multi assets, multi payment code programmed and compiled. Really hard to overcome the version incompatibilities of cardano-api, even with AI. I hope that this is a pain worth for the improvement of the cardano infrastructure. However I guess that the loooong error messages of the haskell compiler and the type system works well into giving good context for the AI.
Multisign tested. 
created a universal web client interface for cardanoC/transient applications for testing and as a mookup from which the frontside programmer could get examples and code from the interactions.

⬆️ Done upto 26-02-2026

- 26-02-2026  compiling Sync.hs, a cache for utxtos for fast retrieval of wallet assets, using TCache. rollbacks and UTxO updates works well. multithreaded. 
- 02-03       testing sync
- 10-03       sync works,as well as the mempol stream. blocks and individual transactiions of the mempool channeled  trough mailboxes to make it available for the programmer as a simple library call
 -13-04  Now interested in the Mempool as a native L2 layer. Study of Leios. See other docs. Fixes some bugs. Many non planned functionalities done. integrating multisgned transacitions, transaction tracking. Doing the testing of store-and-forward (durability).
