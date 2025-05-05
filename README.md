# Decentralized Seed Bank Registry

A blockchain-based solution for registering, verifying, and trading rare and native seeds. This smart contract enables seed owners to securely document their seed inventory, get verification from trusted authorities, and trade seeds with other users.

## Features

- **Seed Registration**: Register seeds with detailed information including species, origin, and harvest date
- **Verification System**: Trusted verifiers can authenticate the legitimacy of registered seeds
- **Marketplace**: List seeds for sale and facilitate peer-to-peer trading
- **Ownership Tracking**: Maintain transparent records of seed ownership and provenance

## Contract Functions

### For Seed Owners

- `register-seed`: Register a new seed with detailed information
- `update-seed-details`: Update the information for a registered seed
- `update-seed-quantity`: Change the quantity of a registered seed
- `set-seed-for-sale`: List a seed for sale with a specified price
- `confirm-trade`: Confirm a seed trade as a seller

### For Seed Buyers

- `purchase-seed`: Initiate a purchase of seeds listed for sale

### For Verifiers

- `verify-seed`: Verify the authenticity of a registered seed

### For Contract Owner

- `add-verifier`: Add a new trusted verifier to the system
- `remove-verifier`: Remove a verifier from the system

### Read-Only Functions

- `get-seed-details`: Get detailed information about a specific seed
- `get-seeds-by-owner`: Get all seeds owned by a specific address
- `get-trade-details`: Get information about a specific trade
- `is-verifier`: Check if an address is a trusted verifier

## Usage Example

1. Register a new seed:
```clarity
(contract-call? .seed register-seed "Heirloom Tomato" "Solanum lycopersicum" "Italy" u100 u20230615 u5000000 true)
```

2. Verify a seed (as a verifier):
```clarity
(contract-call? .seed verify-seed u1)
```

3. Purchase a seed:
```clarity
(contract-call? .seed purchase-seed u1 u50)
```

4. Confirm a trade (as the seller):
```clarity
(contract-call? .seed confirm-trade u1)
```

## Note

This contract is a Minimum Viable Product and may require additional features and security enhancements for production use.