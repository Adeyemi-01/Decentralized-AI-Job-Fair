# Decentralized AI Job Fair

A blockchain-hosted virtual job fair platform for AI professionals with smart contract escrow for interview payments and bounties.

## Features

- Job posting with bounty escrow
- Application submission and tracking
- Interview scheduling with payment escrow
- Automated bounty release upon completion
- Transparent matchmaking process

## Smart Contract Functions

### Public Functions

- `post-job` - Create job posting with bounty in escrow
- `submit-application` - Apply for posted positions
- `schedule-interview` - Company schedules interview with escrow
- `release-bounty` - Release payment to successful candidate

### Read-Only Functions

- `get-job` - Retrieve job posting details
- `get-application` - Check application status
- `get-escrow` - View escrow details
- `get-job-nonce` - Get current job counter

## Usage

Companies post jobs with bounties, candidates apply, and payments are securely managed through smart contract escrow.