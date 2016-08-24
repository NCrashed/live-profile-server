{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Profile.Live.Server.Events.Model
Description : DB representation of an eventlog event
Copyright   : (c) Anton Gushcha, 2016
License     : MIT
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Events.Model where

import Data.Word 
import Database.Persist
import Database.Persist.TH 

import GHC.RTS.Events 

type EventTypeNum = Word16
type EventTypeDesc = String
type EventTypeSize = Word16
type BlockSize = Word32
type Capset   = Word32
type PID = Word32
type StringId = Word32
type ParConjDynId = Word64
type ParConjStaticId = StringId
type SparkId = Word32
type FutureId = Word64
type PerfEventTypeNum = Word32
type MessageTagImpl = Word 
type KernelThreadIdImpl = Word64
type CapsetTypeImpl = Word 

share [mkPersist sqlSettings
     , mkDeleteCascade sqlSettings
     , mkMigrate "migrateAll"] [persistLowerCase|

EventTypeImpl
  num EventTypeNum
  desc EventTypeDesc
  size EventTypeSize Maybe 
  deriving Show Eq 

EventImpl
  time Timestamp
  spec EventInfoImplId
  cap Int Maybe 
  deriving Show 

ThreadStopStatusImpl 
  num Word 
  threadId ThreadId Maybe 

EventInfoImpl
  -- EventBlock
  endTime Timestamp Maybe
  cap Int Maybe -- Also TaskCreate, TaskMigrate,
    -- CapCreate, CapDelete, CapDisable, 
    -- CapEnable, CapsetAssignCap, CapsetRemoveCap
  blockSize BlockSize Maybe -- Also GCStatsGHC
  -- UnknownEvent
  ref EventTypeNum Maybe 
  -- Startup 
  ncaps Int Maybe 
  -- Shutdown is not presented
  -- CreateThread, RunThread, StopThread,
  --  ThreadRunnable, MigrateThread, WakeupThread,
  --  ThreadLabel, AssignThreadToProcess
  thread ThreadId Maybe 
  -- StopThread
  status ThreadStopStatusImpl Maybe
  -- MigrateThread, TaskMigrate
  newCap Int Maybe 
  -- WakeupThread
  otherCap Int Maybe 
  -- ThreadLabel
  threadlabel String Maybe 
  -- CreateSparkThread
  sparkThread ThreadId Maybe 
  -- SparkCounters
  sparksCreated Word64 Maybe 
  sparksDud Word64 Maybe 
  sparksOverflowed Word64 Maybe 
  sparksConverted Word64 Maybe 
  sparksFizzled Word64 Maybe 
  sparksGCd Word64 Maybe
  sparksRemaining Word64 Maybe 
  -- SparkCreate
  -- SparkDud
  -- SparkOverflow
  -- SparkRun
  -- SparkSteal
  victimCap Int Maybe
  -- SparkFizzle
  -- SparkGC
  -- TaskCreate, TaskMigrate, TaskDelete
  taskId TaskId Maybe
  -- TaskCreate, PerfCounter, PerfTracepoint
  tid KernelThreadIdImpl Maybe 
  -- RequestSeqGC
  -- RequestParGC
  -- StartGC
  -- GCWork
  -- GCIdle
  -- GCDone
  -- EndGC
  -- GlobalSyncGC
  -- GCStatsGHC, HeapAllocated, HeapSize, HeapLive,
  --  HeapInfoGHC
  heapCapset Capset Maybe  
  -- GCStatsGHC
  gen Int Maybe 
  copied Word64 Maybe 
  slop Word64 Maybe 
  frag Word64 Maybe 
  parNThreads Int Maybe 
  parMaxCopied Word64 Maybe 
  parTotCopied Word64 Maybe 
  -- HeapAllocated
  gens Int Maybe 
  maxHeapSize Word64 Maybe 
  allocAreaSize Word64 Maybe 
  mblockSize Word64 Maybe
  -- CapCreate
  -- CapDelete
  -- CapDisable
  -- CapEnable
  -- CapsetCreate, CapsetDelete, CapsetAssignCap, CapsetRemoveCap, RtsIdentifier, ProgramArgs, ProgramEnv, OsProcessPid, OsProcessParentPid, WallClockTime
  capset Capset Maybe 
  -- CapsetCreate
  capsetType CapsetTypeImpl Maybe 
  -- CapsetDelete
  -- CapsetRemoveCap
  -- RtsIdentifier
  rtsident String Maybe 
  -- ProgramArgs
  args [String] Maybe 
  -- ProgramEnv
  env [String] Maybe 
  -- OsProcessPid
  pid PID Maybe 
  -- OsProcessParentPid
  ppid PID Maybe 
  -- WallClockTime
  sec Word64 Maybe 
  nsec Word64 Maybe 
  -- Message, UserMessage
  msg String Maybe 
  -- UserMarker
  markername String Maybe 
  -- Version
  version String Maybe 
  -- ProgramInvocation
  commandline String Maybe 
  -- CreateMachine, KillMachine
  machine MachineId Maybe
  -- CreateMachine 
  realtime Timestamp Maybe 
  -- CreateProcess, KillProcess, AssignThreadToProcess
  process ProcessId Maybe 
  -- EdenStartReceive
  -- EdenEndReceive
  -- SendMessage, ReceiveMessage, SendReceiveLocalMessage
  mesTag MessageTagImpl Maybe 
  senderProcess ProcessId Maybe 
  senderThread ThreadId Maybe 
  receiverMachine MachineId Maybe
  receiverProcess ProcessId Maybe 
  receiverInport PortId Maybe
  -- ReceiveMessage 
  senderMachine MachineId Maybe 
  messageSize MessageSize Maybe
  -- SendReceiveLocalMessage
  -- InternString
  str String Maybe 
  sId StringId Maybe 
  -- MerStartParConjunction, MerEndParConjunction, MerEndParConjunct, MerCreateSpark
  dynId ParConjDynId Maybe 
  -- MerStartParConjunction
  staticId ParConjStaticId Maybe 
  -- MerCreateSpark
  sparkId SparkId Maybe 
  -- MerFutureCreate, MerFutureWaitNosuspend, MerFutureWaitSuspended, MerFutureSignal
  futureId FutureId Maybe 
  -- MerFutureCreate
  nameId StringId Maybe 
  -- MerFutureWaitNosuspend
  -- MerFutureWaitSuspended
  -- MerFutureSignal
  -- MerLookingForGlobalThread
  -- MerWorkStealing
  -- MerLookingForLocalSpark
  -- MerReleaseThread
  threadId ThreadId Maybe
  -- MerCapSleeping
  -- MerCallingMain
  -- PerfName, PerfCounter, PerfTracepoint
  perfNum PerfEventTypeNum Maybe 
  -- PerfName
  name String Maybe 
  -- PerfCounter
  period Word64 Maybe
|]

-- | Helper to decode from DB representation
fromThreadStopStatusImpl :: ThreadStopStatusImpl -> Maybe ThreadStopStatus
fromThreadStopStatusImpl ThreadStopStatusImpl{..} = case threadStopStatusImplNum of 
  0 -> Just NoStatus
  1 -> Just HeapOverflow
  2 -> Just StackOverflow
  3 -> Just ThreadYielding
  4 -> Just ThreadBlocked
  5 -> Just ThreadFinished
  6 -> Just ForeignCall
  7 -> Just BlockedOnMVar
  8 -> Just BlockedOnMVarRead
  9 -> Just BlockedOnBlackHole
  10 -> Just BlockedOnRead
  11 -> Just BlockedOnWrite
  12 -> Just BlockedOnDelay
  13 -> Just BlockedOnSTM
  14 -> Just BlockedOnDoProc
  15 -> Just BlockedOnCCall
  16 -> Just BlockedOnCCall_NoUnblockExc
  17 -> Just BlockedOnMsgThrowTo
  18 -> Just ThreadMigrating
  19 -> Just BlockedOnMsgGlobalise
  20 -> BlockedOnBlackHoleOwnedBy <$> threadStopStatusImplThreadId
  _ -> Nothing

-- | Helper to convert into DB representation
toThreadStopStatusImpl :: ThreadStopStatus -> ThreadStopStatusImpl 
toThreadStopStatusImpl st = case st of 
  NoStatus -> ThreadStopStatusImpl 0 Nothing
  HeapOverflow -> ThreadStopStatusImpl 1 Nothing
  StackOverflow -> ThreadStopStatusImpl 2 Nothing
  ThreadYielding -> ThreadStopStatusImpl 3 Nothing
  ThreadBlocked -> ThreadStopStatusImpl 4 Nothing
  ThreadFinished -> ThreadStopStatusImpl 5 Nothing
  ForeignCall -> ThreadStopStatusImpl 6 Nothing
  BlockedOnMVar -> ThreadStopStatusImpl 7 Nothing
  BlockedOnMVarRead -> ThreadStopStatusImpl 8 Nothing
  BlockedOnBlackHole -> ThreadStopStatusImpl 9 Nothing
  BlockedOnRead -> ThreadStopStatusImpl 10 Nothing
  BlockedOnWrite -> ThreadStopStatusImpl 11 Nothing
  BlockedOnDelay -> ThreadStopStatusImpl 12 Nothing
  BlockedOnSTM -> ThreadStopStatusImpl 13 Nothing
  BlockedOnDoProc -> ThreadStopStatusImpl 14 Nothing
  BlockedOnCCall -> ThreadStopStatusImpl 15 Nothing
  BlockedOnCCall_NoUnblockExc -> ThreadStopStatusImpl 16 Nothing
  BlockedOnMsgThrowTo -> ThreadStopStatusImpl 17 Nothing
  ThreadMigrating -> ThreadStopStatusImpl 18 Nothing
  BlockedOnMsgGlobalise -> ThreadStopStatusImpl 19 Nothing
  BlockedOnBlackHoleOwnedBy t -> ThreadStopStatusImpl 20 (Just t)

-- | Helper to convert from DB representation
fromMessageTagImpl :: MessageTagImpl -> Maybe MessageTag 
fromMessageTagImpl t = case t of 
  0 -> Just Ready
  1 -> Just NewPE
  2 -> Just PETIDS
  3 -> Just Finish
  4 -> Just FailPE
  5 -> Just RFork
  6 -> Just Connect
  7 -> Just DataMes
  8 -> Just Head
  9 -> Just Constr
  10 -> Just Part
  11 -> Just Terminate
  12 -> Just Packet
  _ -> Nothing 

-- | Helper to convert to DB representation
toMessageTag :: MessageTag -> MessageTagImpl 
toMessageTag t = case t of 
  Ready -> 0 
  NewPE -> 1 
  PETIDS -> 2 
  Finish -> 3 
  FailPE -> 4 
  RFork -> 5 
  Connect -> 6 
  DataMes -> 7 
  Head -> 8 
  Constr -> 9 
  Part -> 10 
  Terminate -> 11 
  Packet -> 12 

-- | Helper to convert from DB representation
fromCapsetTypeImpl :: CapsetTypeImpl -> Maybe CapsetType 
fromCapsetTypeImpl i = case i of 
  0 -> Just CapsetCustom
  1 -> Just CapsetOsProcess
  2 -> Just CapsetClockDomain
  3 -> Just CapsetUnknown
  _ -> Nothing

-- | Helper to convert to DB representation
toCapsetTypeImpl :: CapsetType -> CapsetTypeImpl
toCapsetTypeImpl c = case c of 
  CapsetCustom -> 0
  CapsetOsProcess -> 1
  CapsetClockDomain -> 2 
  CapsetUnknown -> 3