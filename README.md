# EscrowBased Online Marketplace


A trustless buying and selling platform built with Clarity smart contracts on Stacks blockchain.

## Overview

This marketplace allows users to:
- Create listings for items they want to sell
- Purchase items with funds held in escrow
- Confirm delivery to release funds to the seller
- Open disputes if there are issues with a transaction
- Get refunds directly from sellers

The contract includes a fee mechanism where the marketplace takes a small percentage of each successful transaction.

## Contract Functions

### Listing Management

- `create-listing`: Create a new item listing with title, description, and price
- `update-listing`: Update an existing listing (seller only)
- `cancel-listing`: Cancel an active listing (seller only)

### Transaction Flow

- `purchase-listing`: Buy an item, placing STX in escrow
- `confirm-delivery`: Confirm receipt of item, releasing funds to seller
- `refund-buyer`: Return funds to buyer (seller only)

### Dispute Resolution

- `open-dispute`: Open a dispute for a transaction in escrow
- `resolve-dispute`: Resolve a dispute (contract owner only)

### Administrative

- `set-fee-percentage`: Update the marketplace fee (contract owner only)

### Read-Only Functions

- `get-listing`: Get details about a specific listing
- `get-dispute`: Get details about a dispute
- `get-fee-percentage`: Get the current marketplace fee percentage
- `calculate-fee`: Calculate the fee for a given amount

## Usage Example

```clarity
;; Create a new listing
(contract-call? .marketplace create-listing "Vintage Guitar" "1969 Fender Stratocaster in excellent condition" u1000000000)

;; Purchase a listing
(contract-call? .marketplace purchase-listing u1)

;; Confirm delivery (as buyer)
(contract-call? .marketplace confirm-delivery u1)

;; Open a dispute (if there's an issue)
(contract-call? .marketplace open-dispute u1 "Item not as described")
```

## Deployment

Deploy this contract using Clarinet:

```bash
clarinet contract publish
```

## Security Considerations

- All funds are held in escrow until delivery is confirmed
- Only the buyer can confirm delivery
- Only the seller can issue refunds
- Disputes can be opened by either party
- Only the contract owner can resolve disputes