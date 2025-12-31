-- Store and forward primitives as well a backtracing preserving primitives that allows for
-- transactions in durable and distributed computations with failure recovery.
-- Handlers of backtraking transactions are honored even in presence of failures and shutdowns
-- WORK IN PROGRESS
module CardanoC.SF (runSF, job, collectp) where
import Transient.Move.Job
import Transient.Console
import Transient.Move.Utils
runSF mx= keep $ initNode $ do
    runJobs
    mx

-- runAt

-- teleport

-- sync
