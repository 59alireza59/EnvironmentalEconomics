
```r
# =========================
# BIODIVERSITY CBA & RISK — 20,000-ha MANGROVE RESTORATION
# Base CBA + Sensitivity (Spider & Tornado) + Monte Carlo (Hist & CDF) + Risk register skeleton
# =========================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

set.seed(42)

# ---- Base inputs (real 2024 USD) -------------------------------------------
area_ha <- 20000
rsf_base <- 0.80

years_op <- 25               # operations begin in year 2 (t = 2..26)
r <- 0.06

capex_y0 <- 120e6
capex_y1 <- 60e6

om_per_ha_yr <- 80
value_es <- 600
ef_tCO2_ha <- 6
scc_usd_tCO2 <- 190

# ---- Timeline and PV factor ------------------------------------------------
t <- 0:(years_op + 1)        # 0..26
op_years <- 2:(years_op + 1) # 2..26
disc <- 1 / (1 + r)^t
F_ops <- sum(disc[op_years]) # operating PV factor for a constant annual flow

# ---- Base annual flows -----------------------------------------------------
b_ha <- rsf_base * (value_es + ef_tCO2_ha * scc_usd_tCO2)  # USD/ha-yr
B_yr <- area_ha * b_ha                                    # USD/yr
OM_yr <- area_ha * om_per_ha_yr                            # USD/yr

# Cash-flow vectors (for IRR + B/C decomposition)
capex <- numeric(length(t))
capex[1] <- capex_y0
capex[2] <- capex_y1

benefit <- numeric(length(t))
cost_om <- numeric(length(t))
benefit[op_years] <- B_yr
cost_om[op_years] <- OM_yr

net_benefit <- benefit - cost_om - capex

# ---- Base metrics ----------------------------------------------------------
npv <- sum(net_benefit * disc)
irr <- uniroot(function(i) sum(net_benefit / (1 + i)^t), c(0, 1))$root

pvB <- sum(benefit * disc)
pvC <- sum((capex + cost_om) * disc)
bc <- pvB / pvC

message(sprintf(
  "BASE: NPV = $%s | IRR = %s | B/C = %.3f",
  comma(npv, accuracy = 1),
  percent(irr, accuracy = 0.1),
  bc
))

# ---- Sensitivity (±20%, one-at-a-time) -------------------------------------
recompute_npv <- function(B_year, OM_year, cap0, cap1) {
  # NPV = (B-OM)*F_ops - (I0 + I1/(1+r))
  pv_ops <- (B_year - OM_year) * F_ops
  pv_capex <- cap0 + cap1 / (1 + r)
  pv_ops - pv_capex
}

band <- c(0.8, 1.0, 1.2)

drivers <- list(
  RSF = function(x) {
    b_ha_x <- (rsf_base * x) * (value_es + ef_tCO2_ha * scc_usd_tCO2)
    B_x <- area_ha * b_ha_x
    recompute_npv(B_x, OM_yr, capex_y0, capex_y1)
  },
  CAPEX = function(x) {
    recompute_npv(B_yr, OM_yr, capex_y0 * x, capex_y1 * x)
  },
  OandM = function(x) {
    OM_x <- area_ha * (om_per_ha_yr * x)
    recompute_npv(B_yr, OM_x, capex_y0, capex_y1)
  },
  VES = function(x) {
    b_ha_x <- rsf_base * ((value_es * x) + ef_tCO2_ha * scc_usd_tCO2)
    B_x <- area_ha * b_ha_x
    recompute_npv(B_x, OM_yr, capex_y0, capex_y1)
  },
  EFc = function(x) {
    b_ha_x <- rsf_base * (value_es + (ef_tCO2_ha * x) * scc_usd_tCO2)
    B_x <- area_ha * b_ha_x
    recompute_npv(B_x, OM_yr, capex_y0, capex_y1)
  },
  SCC = function(x) {
    b_ha_x <- rsf_base * (value_es + ef_tCO2_ha * (scc_usd_tCO2 * x))
    B_x <- area_ha * b_ha_x
    recompute_npv(B_x, OM_yr, capex_y0, capex_y1)
  }
)

sens_tbl <- imap_dfr(drivers, function(f, nm) {
  tibble(Driver = nm, Mult = band, NPV = map_dbl(band, f))
})

swing_tbl <- sens_tbl %>%
  group_by(Driver) %>%
  summarise(
    NPV_0.8 = NPV[Mult == 0.8],
    NPV_1.0 = NPV[Mult == 1.0],
    NPV_1.2 = NPV[Mult == 1.2],
    Swing   = NPV_1.2 - NPV_0.8,
    Slope_mUSD_per_1pct = (Swing / 0.40) / 1e6,
    .groups = "drop"
  ) %>%
  arrange(desc(abs(Slope_mUSD_per_1pct)))

print(sens_tbl)
print(swing_tbl)

# Spider chart
spider <- ggplot(sens_tbl, aes(x = Mult, y = NPV, color = Driver, group = Driver)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = npv, linetype = "dashed") +
  labs(title = "Spider Chart — NPV vs Input Multiplier", x = "Input level", y = "NPV (USD)") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  theme_minimal()
print(spider)

# Tornado chart
tornado_tbl <- sens_tbl %>%
  group_by(Driver) %>%
  summarise(Low = min(NPV), High = max(NPV), .groups = "drop") %>%
  left_join(swing_tbl %>% select(Driver, Slope_mUSD_per_1pct), by = "Driver") %>%
  arrange(desc(abs(Slope_mUSD_per_1pct)))

tornado_plot <- ggplot(tornado_tbl, aes(y = reorder(Driver, abs(Slope_mUSD_per_1pct)))) +
  geom_segment(aes(x = Low, xend = High, yend = reorder(Driver, abs(Slope_mUSD_per_1pct))),
               linewidth = 8, color = "grey75") +
  geom_vline(xintercept = npv, linetype = "dashed") +
  labs(title = "Tornado Chart — NPV Swing (80% → 120%)", x = "NPV (USD)", y = NULL) +
  scale_x_continuous(labels = label_number(big.mark = ",")) +
  theme_minimal()
print(tornado_plot)

# Optional: show PV_B, PV_C, B/C for two drivers (RSF and CAPEX) across band
bc_check <- bind_rows(
  tibble(Driver = "RSF", Mult = band) %>%
    mutate(
      rsf = rsf_base * Mult,
      b_ha = rsf * (value_es + ef_tCO2_ha * scc_usd_tCO2),
      B = area_ha * b_ha,
      OM = OM_yr,
      PV_B = B * F_ops,
      PV_C = (capex_y0 + capex_y1/(1+r)) + OM * F_ops,
      BC = PV_B / PV_C
    ),
  tibble(Driver = "CAPEX", Mult = band) %>%
    mutate(
      B = B_yr,
      OM = OM_yr,
      PV_B = B * F_ops,
      PV_C = (capex_y0*Mult + (capex_y1*Mult)/(1+r)) + OM * F_ops,
      BC = PV_B / PV_C
    )
)

print(bc_check %>% select(Driver, Mult, PV_B, PV_C, BC))

# ---- Monte Carlo (N=5000, seed=42, independent) ----------------------------
rtri <- function(n, a, b, c) {
  u <- runif(n)
  F <- (b - a) / (c - a)
  ifelse(u < F,
         a + sqrt(u * (c - a) * (b - a)),
         c - sqrt((1 - u) * (c - a) * (c - b)))
}

N <- 5000

# RSF ~ Truncated Normal(0.80, 0.08; [0.40,1.00])
rsf_draw <- pmin(pmax(rnorm(N, mean = rsf_base, sd = 0.08), 0.40), 1.00)

capex_mult <- rlnorm(N, meanlog = 0, sdlog = 0.15)
om_mult <- rlnorm(N, meanlog = 0, sdlog = 0.10)

ves_mult <- rtri(N, 0.8, 1.0, 1.2)
ef_mult  <- rtri(N, 0.8, 1.0, 1.2)
scc_mult <- rtri(N, 0.8, 1.0, 1.2)

npv_mc <- map_dbl(1:N, function(i) {
  b_ha_i <- rsf_draw[i] * (
    (value_es * ves_mult[i]) +
      (ef_tCO2_ha * ef_mult[i]) * (scc_usd_tCO2 * scc_mult[i])
  )

  B_i <- area_ha * b_ha_i
  OM_i <- area_ha * om_per_ha_yr * om_mult[i]

  pv_capex_i <- (capex_y0 + capex_y1/(1+r)) * capex_mult[i]

  (B_i - OM_i) * F_ops - pv_capex_i
})

npv_mean <- mean(npv_mc)
npv_sd <- sd(npv_mc)
q <- quantile(npv_mc, probs = c(0.05, 0.50, 0.95))
p_loss <- mean(npv_mc <= 0)

message(sprintf(
  "MC NPV: mean=$%s, sd=$%s, p5=$%s, p50=$%s, p95=$%s, Pr(NPV<=0)=%.3f",
  comma(npv_mean, accuracy = 1), comma(npv_sd, accuracy = 1),
  comma(q[1], accuracy = 1), comma(q[2], accuracy = 1), comma(q[3], accuracy = 1),
  p_loss
))

mc_df <- tibble(npv = npv_mc)

p_hist <- ggplot(mc_df, aes(x = npv)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = npv_mean) +
  geom_vline(xintercept = q[2]) +
  geom_vline(xintercept = q[1]) +
  geom_vline(xintercept = q[3]) +
  labs(title = "Monte Carlo NPV — Histogram", x = "NPV (USD)", y = "Count") +
  scale_x_continuous(labels = label_number(big.mark = ",")) +
  theme_minimal()
print(p_hist)

p_cdf <- ggplot(mc_df, aes(x = npv)) +
  stat_ecdf(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = npv_mean) +
  geom_vline(xintercept = q[2]) +
  geom_vline(xintercept = q[1]) +
  geom_vline(xintercept = q[3]) +
  geom_hline(yintercept = p_loss, linetype = "dashed") +
  labs(title = "Monte Carlo NPV — Empirical CDF", x = "NPV (USD)", y = "F̂(NPV)") +
  scale_x_continuous(labels = label_number(big.mark = ",")) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  theme_minimal()
print(p_cdf)

# ---- Qualitative risk register (starter skeleton) ---------------------------
qual_register <- tibble::tribble(
  ~Event, ~Probability, ~Severity, ~RiskLevel, ~Owner, ~Mitigation, ~Residual,
  "CAPEX overrun", "B (10–33%)", "III", "Moderate", "Project sponsor",
  "Independent cost review; fixed-price packages where feasible", "Low–Moderate",
  "Hydrological works delay", "B–C", "IV", "High", "Engineering lead",
  "Critical-path planning; early permits; contractor incentives", "Moderate",
  "Seedling mortality spike", "C", "III", "Moderate", "Ecology team",
  "Nursery quality control; adaptive replanting; monitoring", "Moderate",
  "Cyclone / storm damage", "B", "V", "High", "Sponsor",
  "Staged planting; resilient design; insurance", "Moderate",
  "Invasive species rebound", "C", "III", "Moderate", "Ecology team",
  "Rapid response protocol; routine surveys", "Low–Moderate",
  "Land tenure / access conflict", "B–C", "IV", "High", "Community liaison",
  "FPIC; benefit-sharing; clear agreements", "Moderate",
  "Policy shift on SCC / carbon accounting", "B", "III", "Moderate", "Policy lead",
  "Scenario planning; document co-benefits beyond carbon", "Moderate"
)

print(qual_register)
