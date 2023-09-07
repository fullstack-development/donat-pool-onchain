## Protocol.Validator
111: Wrong pkh
112: Manager pkh shouldn't be changed
113: tokenOriginRef shouldn't be changed
114: config datums should be changed
115: protocol value shouldn't be changed
116: protocol thread token isn't in value
117: protocol datum shouldn't be changed
118: wrong fee on start fundraising
119: fundraising goal is out of amount range
120: VerToken is not in fundraising output
121: ThreadToken is not in fundraising output
122: should be minAda in fundraising output
123: should burn 1 thread token on close protocol
124: should mint verToken on start fundraising
125: should mint threadToken on start fundraising
126: fundraising duration is out of the range configured in protocol
127: Unexpected managerPkh in output Fundraising Datum while starting fundraising
128: MinPoolAmount must be greater then minimal txOut amount
129: MinPoolAmount must be less than MaxPoolAmount
130: Minimal duration must be greater than 1 minute
131: Minimal duration must be less than maximal duration
132: Protocol fee value must be between 0 and 100 
133: Fundraising description length must be shorter or equal to 35
134: Create Fundraising transaction not signed by creator

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
310: No reference inputs with current threadToken found
311: More than one reference inputs with current threadToken found
312: Impossible to extract PubKeyHash from script credentials
313: No minting performed in transaction

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
415: can't donate after fundraising deadline
416: insufficient fee amount to pay to FeePool
417: unexpected fee amount paid to FeePool
418: minAda must be paid to FeePoolInfo
419: fee amount must be less or equal to 2*minAda while receiving funds without fee


## Ext.Plutarch.Extra.Bool
501: Can't parse integer to make Bool

## MintingPolicy.Proposal
601: Unexpected Vote token name length
602: Vote token name first symbol must be 'v'

## MintingPolicy.Governance
701: Unexpected minting governance tokens transaction signer

## Governance.Validator
801: Governance ThreadToken missed in the CreateProposal transaction
802: Protocol proposal doesn't contain any changes
804: Proposal fee less than minAda
805: Unexpected create proposal initiator
806: Unexpected Ada value in output while proposal creation
807: Unexpected non-Ada value in output while proposal creation
808: Unexpected proposal in the output datum
809: Unexpected quorum in the output datum
810: Unexpected for and against values in tne output datum
812: Governance datum shouldn't be changed during the Create Proposal transaction
813: Governance value shouldn't be changed during the Create Proposal transaction
814: New proposal shouldn't be applied
815: Proposal duration is out of the range configured in governance
816: should mint threadToken on create proposal
817: should mint verToken on create proposal

## Governance.Proposal.Validator
901: No thread token in vault input
902: No verification token in vault input
903: Vote transaction is not signed by voter 
904: Proposal should not be changed during the transaction
905: Proposal PolicyRef should not be changed during the Vote transaction
906: Proposal quorum should not be changed during the Vote transaction
907: Proposal initiator should not be changed during the Vote transaction
908: Unexpected votes number added in VoteFor case
909: Unexpected votes number added in VoteAgainst case
910: Unexpected out value during the Vote transaction
911: Unexpected payment to the Voter during the Vote transaction
912: Can't vote for applied proposal
913: Can't vote after proposal deadline
914: Proposal deadline should not be changed during the Vote transaction
915: Proposal applied field value should not be changed during the Vote transaction
916: Should mint Vote receipt during the vote transaction

## FeePool.Validator & FeePool.FeePoolInfo.Validator
1001: Input value must be the same as output value while adding a record to FeePoolInfo
1002: FeePoolInfo verification token missed in transaction input while adding new record to datum
1003: FeePool thread token missed in transaction input
1004: FeePool thread token missed in transaction output
1005: FeePool datum mustn't change while adding funds with current epoch
1006: Unexpected FeePool output value while receiving funds with current epoch
1007: Incorrect timestamp provided while receiving funds with current epoch
1008: Calculated epoch must be the same as current epoch while receiving funds with current epoch
1009: Unexpected epoch in FeePoolInfo output datum while receiving funds with current epoch
1010: Unexpected fee map in FeePoolInfo output datum while receiving funds with current epoch
1011: Unexpected epoch in FeePool output datum while receiving funds with new epoch
1012: Calculated epoch must be equal to output epoch while receiving funds with new epoch
1013: Calculated epoch must be different from input epoch while receiving funds with new epoch
1014: Incorrect timestamp provided while receiving funds with new epoch
1015: Payment to FeePool must be greater than minAda amount
1016: Unexpected FeePool output value while receiving funds with new epoch
1017: Unexpected epoch in FeePoolInfo output datum while receiving funds with new epoch
1018: Unexpected fee map in FeePoolInfo output datum while receiving funds with new epoch
1019: Unexpected FeePoolInfo output value while receiving funds with new epoch
1020: FeePoolInfo verification token not minted while receiving funds with new epoch
