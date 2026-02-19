
```r
# Hedonic Price Method — valuation from reported coefficients (semi-log model)
# Computes MWTP, capitalization, and 95% CIs (delta method)

# ---- Setup: coefficients, SEs, and policy inputs --------------------------
beta_PM <- -0.0120   # per μg/m3
se_beta_PM <- 0.0030

beta_noise <- -0.0040 # per dB (optional)
se_beta_no <- 0.0012

beta_nearpark <- 0.0550 # dummy (near park = 1)
se_beta_np <- 0.0150

Pbar <- 400000   # average house price (USD)
N <- 10000       # affected homes
delta_PM <- -5   # PM2.5 drops by 5 μg/m3 (negative = reduction)

# ---- MWTP for PM2.5 at Pbar -----------------------------------------------
MWTP_PM <- beta_PM * Pbar
SE_MWTP_PM <- Pbar * se_beta_PM
CI95_MWTP_PM <- MWTP_PM + c(-1, 1) * 1.96 * SE_MWTP_PM

MWTP_PM
CI95_MWTP_PM

# ---- Capitalization per house for the policy change -----------------------
# ΔP per house ≈ beta_PM * ΔPM * Pbar
dP_house <- beta_PM * delta_PM * Pbar
dP_house

# 95% CI for ΔP per house (multiply MWTP CI by |ΔPM|)
CI95_dP_house <- -delta_PM * CI95_MWTP_PM
CI95_dP_house

# ---- Aggregate capitalization across all homes ----------------------------
total_benefit <- N * dP_house
CI95_total <- N * CI95_dP_house

total_benefit
CI95_total

# ---- Near-park premium (dummy in semi-log) --------------------------------
pct_premium_nearpark <- (exp(beta_nearpark) - 1) * 100
dollar_premium_nearpark <- (exp(beta_nearpark) - 1) * Pbar

pct_premium_nearpark
dollar_premium_nearpark

# ---- (Optional) MWTP for noise at Pbar ------------------------------------
MWTP_noise <- beta_noise * Pbar
SE_MWTP_noise <- Pbar * se_beta_no
CI95_MWTP_noise <- MWTP_noise + c(-1, 1) * 1.96 * SE_MWTP_noise

MWTP_noise
CI95_MWTP_noise
