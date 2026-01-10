-- Store and forward primitives as well a backtracing preserving primitives that allows for
-- transactions in durable and distributed computations with failure recovery.
-- Handlers of backtraking transactions are honored even in presence of failures and shutdowns
-- WORK IN PROGRESS
module CardanoC.SF (runC, job, collect) where
import Transient.Move
import Transient.Move.Job
import Transient.Console
import Transient.Move.Utils

runC mx= keep $ initNode $ do
    runJobs
    mx

collect :: Loggable a => Int -> Int -> Cloud a -> Cloud [a]
collect =Transient.Move.Job.collectc

-- runAt

-- teleport

-- sync
