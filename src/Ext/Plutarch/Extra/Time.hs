module Ext.Plutarch.Extra.Time where

import Plutarch.Api.V1.Time
import qualified Plutarch.Monadic as P
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

type OrdinalDay = PInteger
type Epoch = PInteger -- one epoch lasts `epochSize` days
type DayOfEpoch = PInteger

epochSize :: Term s PInteger
epochSize = 100

toOrdinalDay :: Term s (PAsData PPOSIXTime :--> OrdinalDay)
toOrdinalDay = phoistAcyclic $
  plam $ \time ->
    pmatch (pfromData time) $ \(PPOSIXTime ms) -> pdiv # ms # (1000 #* 60 #* 60 #* 24)

toEpoch :: Term s (OrdinalDay :--> Epoch)
toEpoch = phoistAcyclic $
  plam $ \ordinalDay -> pdiv # ordinalDay # epochSize

toDayOfEpoch :: Term s (OrdinalDay :--> DayOfEpoch)
toDayOfEpoch = phoistAcyclic $
  plam $ \ordinalDay -> pmod # ordinalDay # epochSize

posixToEpoch :: Term s (PAsData PPOSIXTime :--> Epoch)
posixToEpoch = phoistAcyclic $
  plam $ \time -> toEpoch #$ toOrdinalDay # time

posixToDayOfEpoch :: Term s (PAsData PPOSIXTime :--> DayOfEpoch)
posixToDayOfEpoch = phoistAcyclic $
  plam $ \time -> toDayOfEpoch #$ toOrdinalDay # time
