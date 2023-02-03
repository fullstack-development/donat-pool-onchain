## Protocol.Validator
111: Wrong pkh
112: Manager pkh shouldn't be changed
113: tokenOriginRef shouldn't be changed
114: config datums should be changed
115: protocol value shouldn't be changed
116: protocol thread token isn't in value
117: protocol datum shouldn't be changed on start fundrise


## Shared.Checks
201: 1 or more outputs found in context, expected 0
202: should burn 1 thread token
202.1: should mint 1 thread token
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
