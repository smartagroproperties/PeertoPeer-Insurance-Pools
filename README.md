# 🛡️ Peer-to-Peer Insurance Pools 🛡️

A decentralized community-funded insurance platform with profit/loss sharing built on Stacks blockchain.

## 🌟 Overview

This smart contract enables communities to create self-managed insurance pools where members can:

- 💰 Contribute funds to a shared pool
- 🤝 Join existing insurance pools
- 📝 File claims against the pool
- 🗳️ Vote on the validity of claims
- 💸 Receive payouts for approved claims
- 📈 Share in the profits of well-managed pools

## 🚀 Features

- **Pool Creation**: Anyone can create an insurance pool with custom coverage amounts
- **Membership Management**: Join or leave pools at any time
- **Democratic Claim Processing**: Community voting determines claim validity
- **Profit Sharing**: Pool creators can distribute profits to members
- **Governance Parameters**: Adjustable contribution minimums and voting periods

## 📋 Contract Functions

### Pool Management

- `create-pool`: Create a new insurance pool
- `join-pool`: Join an existing pool with a contribution
- `leave-pool`: Exit a pool and receive your contribution back

### Claims

- `file-claim`: Submit a claim against a pool
- `vote-on-claim`: Vote yes/no on a pending claim
- `process-claim`: Finalize a claim after the voting period

### Administration

- `distribute-profits`: Share pool profits with members
- `set-min-contribution`: Update minimum contribution amount
- `set-claim-threshold`: Set threshold for automatic claim approval
- `set-voting-period`: Change the duration of voting periods
- `set-profit-sharing-percentage`: Adjust profit distribution rate

## 🔧 Usage Examples

### Creating a Pool

```clarity
(contract-call? .peertopeer create-pool "Health Emergency Fund" "Community pool for unexpected medical expenses" u10000000)
```

### Joining a Pool

```clarity
(contract-call? .peertopeer join-pool u0 u5000000)
```
