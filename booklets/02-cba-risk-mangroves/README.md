# 02 — Mangrove Restoration CBA & Risk (20,000 ha)

## What this booklet covers
- Base CBA (NPV, IRR, B/C) for a restoration project
- One-at-a-time sensitivity (±20%) with spider + tornado charts
- Monte Carlo risk analysis (NPV distribution, histogram, empirical CDF)
- One-page qualitative risk register template

## Notes on valuation
Annual benefits combine:
- Ecosystem services (storm protection, fisheries, recreation)
- Carbon sequestration valued at a social cost of carbon (SCC)
All benefits are scaled by a restoration success factor (RSF).

## How to run
```r
source("mangrove_cba_risk.R")
