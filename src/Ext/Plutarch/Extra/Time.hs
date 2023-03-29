module Ext.Plutarch.Extra.Time where

import Plutarch.Api.V1.Time
import Plutarch.Num ((#*), (#+))
import Plutarch.Prelude

minutesToMilliseconds :: Term s (PInteger :--> PInteger)
minutesToMilliseconds = phoistAcyclic $
  plam $ \minutes -> minutes #* 60 #* 1000

addTimes :: Term s (PAsData PPOSIXTime :--> PAsData PPOSIXTime :--> PAsData PPOSIXTime)
addTimes = phoistAcyclic $
  plam $ \time1 time2 -> pdata $ pfromData time1 #+ pfromData time2

toPosixTime :: Term s (PInteger :--> PPOSIXTime)
toPosixTime = phoistAcyclic $ plam $ \milliSeconds -> pcon $ PPOSIXTime milliSeconds

minutesToPosixDuration :: Term s (PInteger :--> PAsData PPOSIXTime :--> PAsData PPOSIXTime)
minutesToPosixDuration = phoistAcyclic $
  plam $ \durationMinutes startedAt ->
    let durationMs = minutesToMilliseconds # durationMinutes
        durationPosix = toPosixTime # durationMs
     in addTimes # startedAt # pdata durationPosix
