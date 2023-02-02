## Protocol.Validator
111: Wrong pkh
112: Manager pkh shouldn't be changed
113: tokenOriginRef shoudn't be changed
114: config datums should be changed
115: protocol value shouldn't be changed
116: protocol thread token isn't in value

## Shared.Checks
201: 1 or more outputs found in context, expected 0
202: should burn 1 thread token
203: should be minAda in pkh output
204: Wrong NFT mint amount
205: NFT missed in transaction inputs
206: NFT missed in transaction outputs
207: Invalid tx time range

## Shared.ScriptContextV2
301: no datum in script output
302: expected inline datum, but found DatumHash
303: not a spending tx (getCtxInfoForSpending)
304: not a spending tx (getOrefInfoForSpending)
305: no own inputs
306: no outputs found in context, expected 1
307: many outputs found in context, expected 1

## Fundraising.Validator
401: Fundraising datum change not expected while Donating
402: Unexpected output value while Donating
403: Amount to donate must be greater than minAda amount
404: Funds have been fully raised already
405: Verification token missed in fundraising script input (donating)
406: Verification token missed in fundraising script output (donating)
407: Thread token missed in fundraising script input (donating)
408: Thread token missed in fundraising script output (donating)