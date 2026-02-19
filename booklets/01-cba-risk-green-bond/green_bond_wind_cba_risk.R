
```r
# =========================
# GREEN BOND TEACHING CASE — 100 MW ONSHORE WIND
# Economic CBA + Sensitivity (Spider & Tornado) + Monte Carlo (Hist & CDF) + Risk register skeleton
# =========================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

set.seed(42)

# ---- Base inputs (real 2024 USD) -------------------------------------------
cap_mw <- 100
cf_base <- 0.35
years_op <- 20               # operations begin in year 2 (t = 2..21)
r <- 0.06

capex_y0 <- 90e6
capex_y1 <- 60e6

om_per_kw_yr <- 43
value_of_elec <- 60
ef_tCO2_MWh <- 0.40
scc_usd_tCO2 <- 190

# ---- Derived annual quantities ---------------------------------------------
cap_kw <- cap_mw * 1e3
energy_yr <- cap_mw * 8760 * cf_base         # MWh/yr
om_yr <- cap_kw * om_per_kw_yr               # USD/yr

# Timeline
t <- 0:(years_op + 1)                        # 0..21
op_years <- 2:(years_op + 1)                 # 2..21

# Cash-flow vectors
capex <- numeric(length(t))
capex[1] <- capex_y0
capex[2] <- capex_y1

benefit <- numeric(length(t))
cost_om <- numeric(length(t))

benefit[op_years] <- energy_yr * value_of_elec +
  energy_yr * ef_tCO2_MWh * scc_usd_tCO2
cost_om[op_years] <- om_yr

# Discounting and base metrics
disc <- 1 / (1 + r)^t
net_benefit <- benefit - cost_om - capex

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
recompute_npv <- function(b_vec = benefit, om_vec = cost_om, cap_vec = capex) {
  sum((b_vec - om_vec - cap_vec) * disc)
}

drivers <- list(
  CF = function(x) {
    En <- cap_mw * 8760 * (cf_base * x)
    b <- benefit
    b[op_years] <- En * value_of_elec + En * ef_tCO2_MWh * scc_usd_tCO2
    recompute_npv(b_vec = b)
  },
  CAPEX = function(x) {
    cvec <- capex
    cvec[1] <- capex_y0 * x
    cvec[2] <- capex_y1 * x
    recompute_npv(cap_vec = cvec)
  },
  OandM = function(x) {
    om <- cost_om
    om[op_years] <- om_yr * x
    recompute_npv(om_vec = om)
  },
  VoE = function(x) {
    b <- benefit
    elec_part <- energy_yr * (value_of_elec * x)
    co2_part  <- energy_yr * ef_tCO2_MWh * scc_usd_tCO2
    b[op_years] <- elec_part + co2_part
    recompute_npv(b_vec = b)
  },
  EF = function(x) {
    b <- benefit
    elec_part <- energy_yr * value_of_elec
    co2_part  <- energy_yr * (ef_tCO2_MWh * x) * scc_usd_tCO2
    b[op_years] <- elec_part + co2_part
    recompute_npv(b_vec = b)
  },
  SCC = function(x) {
    b <- benefit
    elec_part <- energy_yr * value_of_elec
    co2_part  <- energy_yr * ef_tCO2_MWh * (scc_usd_tCO2 * x)
    b[op_years] <- elec_part + co2_part
    recompute_npv(b_vec = b)
  }
)

band <- c(0.8, 1.0, 1.2)

sens_tbl <- imap_dfr(drivers, function(f, nm) {
  tibble(
    Driver = nm,
    Mult   = band,
    NPV    = map_dbl(band, f)
  )
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
  labs(
    title = "Spider Chart — NPV vs Input Multiplier",
    x = "Input level",
    y = "NPV (USD)"
  ) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  theme_minimal()
print(spider)

# Tornado chart
tornado_tbl <- sens_tbl %>%
  group_by(Driver) %>%
  summarise(
    Low  = min(NPV),
    High = max(NPV),
    .groups = "drop"
  ) %>%
  left_join(swing_tbl %>% select(Driver, Slope_mUSD_per_1pct), by = "Driver") %>%
  arrange(desc(abs(Slope_mUSD_per_1pct)))

tornado_plot <- ggplot(tornado_tbl, aes(y = reorder(Driver, abs(Slope_mUSD_per_1pct)))) +
  geom_segment(aes(x = Low, xend = High, yend = reorder(Driver, abs(Slope_mUSD_per_1pct))),
               linewidth = 8, color = "grey75") +
  geom_vline(xintercept = npv, linetype = "dashed") +
  labs(
    title = "Tornado Chart — NPV Swing (80% → 120%)",
    x = "NPV (USD)",
    y = NULL
  ) +
  scale_x_continuous(labels = label_number(big.mark = ",")) +
  theme_minimal()
print(tornado_plot)

# ---- B/C consistency check (component PVs) ---------------------------------
bc_sens <- imap_dfr(drivers, function(f, nm) {
  map_dfr(band, function(x) {
    En <- cap_mw * 8760 * (cf_base * ifelse(nm == "CF", x, 1))
    VoE <- value_of_elec * ifelse(nm == "VoE", x, 1)
    EFm <- ef_tCO2_MWh * ifelse(nm == "EF", x, 1)
    SCCm <- scc_usd_tCO2 * ifelse(nm == "SCC", x, 1)

    b_vec <- benefit
    b_vec[op_years] <- En * VoE + En * EFm * SCCm

    om_vec <- cost_om
    if (nm == "OandM") om_vec[op_years] <- om_yr * x

    cap_vec <- capex
    if (nm == "CAPEX") {
      cap_vec[1] <- capex_y0 * x
      cap_vec[2] <- capex_y1 * x
    }

    PVB <- sum(b_vec * disc)
    PVC <- sum((cap_vec + om_vec) * disc)

    tibble(Driver = nm, Mult = x, PVB = PVB, PVC = PVC, BC = PVB / PVC)
  })
})

print(bc_sens)

# ---- Monte Carlo (N=5000, seed=42, independent) ----------------------------
rtri <- function(n, a, b, c) {
  u <- runif(n)
  F <- (b - a) / (c - a)
  ifelse(u < F,
         a + sqrt(u * (c - a) * (b - a)),
         c - sqrt((1 - u) * (c - a) * (c - b)))
}

N <- 5000

cf_draw <- pmin(pmax(rnorm(N, mean = cf_base, sd = 0.03), 0.10), 0.60)
capex_mult <- rlnorm(N, meanlog = 0, sdlog = 0.15)
om_mult <- rlnorm(N, meanlog = 0, sdlog = 0.10)
voe_mult <- rtri(N, 0.8, 1.0, 1.2)
ef_mult  <- rtri(N, 0.8, 1.0, 1.2)
scc_mult <- rtri(N, 0.8, 1.0, 1.2)

npv_mc <- map_dbl(1:N, function(i) {
  En <- cap_mw * 8760 * cf_draw[i]

  Bop <- En * (value_of_elec * voe_mult[i]) +
    En * (ef_tCO2_MWh * ef_mult[i]) * (scc_usd_tCO2 * scc_mult[i])

  b_vec <- benefit
  b_vec[op_years] <- Bop

  om_vec <- cost_om
  om_vec[op_years] <- om_yr * om_mult[i]

  cap_vec <- capex
  cap_vec[1] <- capex_y0 * capex_mult[i]
  cap_vec[2] <- capex_y1 * capex_mult[i]

  sum((b_vec - om_vec - cap_vec) * disc)
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
  "CAPEX overrun (+15%)", "B (10–33%)", "III", "Moderate", "Promoter",
  "EPC fixed-price; independent cost review", "Low–Moderate",
  "COD delay (6–12 months)", "B–C", "IV", "Moderate–High", "EPC",
  "LDs; critical path control; early permits", "Moderate",
  "CF shortfall (-3pp)", "C", "III", "Moderate", "Owner/O&M",
  "P50/P90 analysis; availability guarantees", "Low–Moderate",
  "Electricity value drop (-20%)", "C", "III", "Moderate", "Commercial",
  "PPA/CfD; indexation", "Low–Moderate",
  "EF/SCC down (-20%)", "B–C", "III", "Moderate", "Policy",
  "Stakeholder engagement; co-benefits in appraisal", "Moderate",
  "O&M overrun (+10%)", "C", "II", "Moderate", "O&M",
  "Long-term O&M contract with caps; CBM; spares strategy", "Low",
  "Force majeure", "A (<10%)", "V", "Moderate", "Promoter",
  "CAR/DSU insurance; emergency plans", "Low"
)

print(qual_register)
