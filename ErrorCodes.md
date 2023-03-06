## Protocol.Validator
111: Wrong pkh
112: Manager pkh shouldn't be changed
113: tokenOriginRef shouldn't be changed
114: config datums should be changed
115: protocol value shouldn't be changed
116: protocol thread token isn't in value
117: protocol datum shouldn't be changed
118: wrong fee on start fundrising
119: fundrising goal is out of amount range
120: VerToken is not in fundrising output
121: ThreadToken is not in fundrising output
122: should be minAda in fundrising output
123: should burn 1 thread token on close protocol
124: should mint verToken on start fundrising
125: should mint threadToken on start fundrising
126: fundraising duration is out of configured in protocol range

## Shared.Checks
201: 1 or more outputs found in context, expected 0
203: should be minAda in pkh output
204: Wrong NFT mint amount
205: NFT missed in transaction inputs
206: NFT missed in transaction outputs

## Shared.ScriptContextV2
301: no datum in script output
302: expected inline datum, but found DatumHash
303: not a spending tx (getCtxInfoForSpending)
304: not a spending tx (getOrefInfoForSpending)
305: no own inputs
306: no outputs found in context, expected 1
307: many outputs found in context, expected 1
308: Can't get time from infinite lower bound
309: Can't get time from infinite upper bound

## Fundraising.Validator
401: Fundraising datum change not expected while Donating
402: Unexpected output value while Donating
403: Amount to donate must be greater than minAda amount
404: Funds have been fully raised already
405: Verification token missed in fundraising script input (donating)
406: Verification token missed in fundraising script output (donating)
407: Thread token missed in fundraising script input (donating)
408: Thread token missed in fundraising script output (donating)
409: Verification token missed in fundraising script input (receivingFunds)
410: Thread token missed in fundraising script input (receivingFunds)
411: ReceiveFunds transaction not signed by transaction creator
412: Impossible to receive funds as fundraising is not completed
413: should burn verToken on close fundraising
414: should burn threadToken on close fundraising
415: can't donate after fundrising deadline
