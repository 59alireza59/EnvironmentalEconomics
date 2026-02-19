# EnvironmentalEconomics — Academic Booklet Repository

This repository is an academic booklet collection for core methods in Environmental Economics:
**Cost–Benefit Analysis under Risk**, **Valuation of Ecosystem Services**, **Revealed Preference Methods**
(Hedonic Pricing, Travel Cost), and **Biodiversity Valuation / Conservation Portfolio Choice**
(Noah’s Ark, Weitzman diversity).

Each booklet folder contains:
- A PDF “booklet” (problem + worked solution / lecture note)
- A **fully runnable** code file (R)
- A short, professional README describing the method, assumptions, and outputs

## Booklets (start here)

### Cost–Benefit Analysis & Risk Assessment
- **01 — Green Bond: 100-MW Onshore Wind CBA + Sensitivity + Monte Carlo**
  - Folder: `booklets/01-cba-risk-green-bond/`
- **02 — Mangrove Restoration (20,000 ha) CBA + Sensitivity + Monte Carlo**
  - Folder: `booklets/02-cba-risk-mangroves/`

### Revealed Preference Methods
- **03 — Hedonic Price Method (semi-log interpretation, MWTP, capitalization, CI)**
  - Folder: `booklets/03-revealed-preference-hedonic/`
- **04 — Travel Cost Method (zonal demand, CS, Poisson sign-check, sensitivity)**
  - Folder: `booklets/04-revealed-preference-travel-cost/`

### Biodiversity Concepts & Valuation
- **05 — Phylogenetic Diversity (PD) + Noah’s Ark Allocation**
  - Folder: `booklets/05-biodiversity-pd-noahs-ark/`
- **06 — Weitzman Diversity (MST from distance matrix) + Noah’s Ark**
  - Folder: `booklets/06-biodiversity-weitzman-mst/`

## How to run the code
All scripts are self-contained.

1. Install R (≥ 4.2 recommended).
2. Install required packages (most scripts use tidyverse + ggplot2):
   ```r
   install.packages(c("dplyr","tidyr","purrr","ggplot2","scales","MASS"))
