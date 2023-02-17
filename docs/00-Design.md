tag: "docs-00-desing"
date: 2022/07/07
author: ["cstml", "Borja"]
---

# Design Document

**Table of Contents**

- [Design Document](#design-document)
  - [Scope](#scope)
  - [Demo Scope](#demo-scope)
  - [Demo Assumptions](#demo-assumptions)

## Scope

The scope of Thrift Finance is to:

1. Allow the pooling of funds for a common transaction

2. Allow the claiming of the rewards/benefit of the common transaction by one of
   the initial contributors

## Demo Scope

The scope of the initial demo is to:

1. Create a smart contract that allows pooling funds for (FIXME: to be defined
   specific transaction)
   
2. Execute the transaction

3. Allow one of the initial poolers to claim the rewards

4. (*additional*) Allow all the poolers to claim their fair-share of the initial
   investment

## Demo Protocol Assumptions

1. The demo will be using a third-party smart contract to deposit the pooled
   actions - this third-party is: a mock protocol.

2. All the initial poolers will be able to claim their initial deposit - or part
   of the initial contribution.

3. The demo will be tested (initially) on an EmulatorTrace - as a POC.

4. Fees are paid by all protocol participants.

5. The protocol imposes a hard-limit on the number of total contributors a pool
   can have - dependent on the limits of Cardano. TBC what this limit is.
   
## MVP Demo

The MVP demo is a simple example of the protocol in action. It consists of two
Scripts, and one Minting Policy. The story of the MVP is as follows:

> User A (aka. Ana) starts a Funding Pool that aims to deposit 1,000,000
> Lovelace into the trivial protocol, wanting to receive 1,000,000 Trivial
> Tokens back. Ana initialises the Pool and sets the contribution to 250,000
> Lovelace. Ana also makes a deposit of 250,000 Lovelave. User B (aka. Ben)
> deposits (locks in the EUTxO initialised by Ana) 250,000 Lovelace and User C
> (aka. Charlie) deposits 500,000 Lovelace in two separate transactions. As the
> goal of 1,000,000 Lovelace has been achieved, Ben now triggers the the
> protocol and creates a transaction that:

>   1. mints 1,000,000 Trivial Tokens, and deposits them in an EUTxO at the
>      Funding Pool's address. The EUTxO contains a datum specifying the
>      proportions of each user's contribution

>   2. deposits the 1,000,000 Lovelace at the correct Address (specified by the
>      Trivial Protocol) - we call this address the Protocol's Address

>   3. mints a ThriftFinance Token with Token Name ActiveDeposit and attaches it
>      to the EUTxO

> After some time passes, Charlie considers that the time has come to collect
> rewards and burns the 1,000,000 Trivial Tokens, claiming back the 1,000,000
> locked Ada from the protocol. Charlie now creates a transaction that:

>   1. Burns the trivial Tokens,

>   2. unlocks the 1,000,000 Lovelace from the Protocol's Address, and sends
>      each of them back to each contributor - in a proportion equal to that of
>      their initial contribution.

>   3. Mints Reward Tokens in proportion of 1:5 for each burnt Trivial Token -
>      in this case 200,000 - and sends them to the Pool winner.

>   4. burns the ThriftFinance Token with Token Name "ActiveDeposit".

> Now, each user has received back their proportion of the Return - based on the
> intial deposit - given our example Ana receives 250,000 Lovelace, Ben 250,000
> Lovelace, Charlie 500,000. Only one of the three users receives the 200,000
> Return Reward Tokens. The user in this round is randomly chosen to be Charlie,
> so Charlie also receives the 200,000 Reward Tokens as well.

The above user Story makes clear the flow of actions, but raises a couple of
technical questions, which we will aim to clarify:

1. _How is the Random Winner chosen?_ Any fair random algorithm implementation
   will suffice - to keep it simple we put forward the following algorithm. The
   hash of each included EUTxO from the initial deposit together with the hash
   of the Return EUTxO are collapsed into a number. This number modulus the
   number of pool participants gives the index of the winner - from the list of
   participants - this protocol can be later improved to offer a stronger
   randomness guaranteed - and extended to allow multiple round claiming.

2. _What information must be stored in the initial Pool deposits?_ Each pool
   deposit contains a datum which keeps track of the wallet address that can
   claim the Return.
