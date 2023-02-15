module Ext.Plutarch.Extra.Time where

import Plutarch.Api.V1.Time
import Plutarch.Num ((#*), (#+))
import Plutarch.Prelude

daysToMilliseconds :: Term s (PInteger :--> PInteger)
daysToMilliseconds = phoistAcyclic $
  plam $ \days -> days #* 24 #* 60 #* 60 #* 1000

addTimes :: Term s (PAsData PPOSIXTime :--> PAsData PPOSIXTime :--> PAsData PPOSIXTime)
addTimes = phoistAcyclic $
  plam $ \time1 time2 -> pdata $ pfromData time1 #+ pfromData time2

toPosixTime :: Term s (PInteger :--> PPOSIXTime)
toPosixTime = phoistAcyclic $ plam $ \milliSeconds -> pcon $ PPOSIXTime milliSeconds
