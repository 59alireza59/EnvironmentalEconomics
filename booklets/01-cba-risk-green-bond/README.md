# 01 — Green Bond CBA & Risk (100-MW Onshore Wind)

## What this booklet covers
- Base **economic cost–benefit analysis** (NPV, IRR, B/C)
- **One-at-a-time sensitivity** (±20%) + spider & tornado charts
- **Monte Carlo** risk analysis (NPV distribution, histogram, empirical CDF)
- A compact **qualitative risk register** skeleton

## Inputs (real 2024 USD)
Capacity 100 MW; CF 0.35; r = 6%; CAPEX at t=0 and t=1; O&M; value of electricity; CO₂ benefit
via EF and SCC (see the PDF in this folder).

## How to run
```r
source("green_bond_wind_cba_risk.R")
