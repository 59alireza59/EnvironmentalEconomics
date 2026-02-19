# EnvironmentalEconomics — Academic Booklet (Notes + Full Code)

This repository is an **academic booklet** for Environmental Economics with:
- **Cost–Benefit Analysis (CBA) + Risk Assessment**
- **Valuation of Ecosystem Services**
- **Revealed Preference Methods** (Hedonic Pricing, Travel Cost Method)
- **Biodiversity Concepts & Valuation** (Noah’s Ark allocations; PD and Weitzman diversity)

Each module contains:
- a short **lecture-style note** (`README.md`)
- a **full, runnable R script** (`analysis.R`)
- the source **PDF booklet** in `/booklets`

## Modules
### 1) CBA + Risk
- `01-cba-risk/green-bond-wind/` — 100 MW wind: CBA, sensitivity (spider/tornado), Monte Carlo, qualitative risk register
- `01-cba-risk/mangrove-restoration/` — 20,000 ha mangroves: ecosystem services + carbon, same risk toolkit

### 2) Revealed Preference Methods
- `02-revealed-preference/hedonic-price-method/` — semi-log hedonic interpretation, MWTP, CIs, and (optional) estimation template
- `02-revealed-preference/travel-cost-method/` — zonal TCM: generalized travel cost, demand, consumer surplus, Poisson/NB check

### 3) Biodiversity Valuation (Noah’s Ark)
- `03-biodiversity-valuation/phylogenetic-diversity-noahs-ark/` — expected PD with branch representation probabilities + exact portfolio search
- `03-biodiversity-valuation/weitzman-mst-noahs-ark/` — expected MST diversity + exact portfolio search under budget

## How to run
Open any module folder in RStudio and run:
- `analysis.R`

Packages used are listed at the top of each script.

## Citation
If you use this repo in academic work, cite it via `CITATION.cff`.

## Disclaimer
Educational material for learning environmental-economic methods. Inputs are teaching-case values.
