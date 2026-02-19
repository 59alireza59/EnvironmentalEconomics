
```r
# --- Libraries --------------------------------------------------------------
library(dplyr)
library(ggplot2) # optional
library(MASS)    # glm.nb if needed
set.seed(42)

# --- Inputs: zones, populations, distances, wages ---------------------------
zones <- 1:6
S_km <- c(25, 75, 125, 175, 225, 275) # one-way distance (km)
Pop <- c(80000, 120000, 160000, 200000, 180000, 150000)
w_hr <- c(10, 12, 14, 16, 18, 20) # hourly wage (USD/h)

# --- Travel-cost parameters -------------------------------------------------
ckm <- 0.25 # money cost per km ($/km)
v_kmh <- 60 # average speed (km/h)
phi <- 1/3  # value-of-time fraction of wage

# --- Compute generalized travel cost TC = money + time ----------------------
D_km <- 2 * S_km # roundtrip distance
moneyCost <- ckm * D_km
timeCost <- (D_km / v_kmh) * (phi * w_hr)
TC <- moneyCost + timeCost

# --- Linear per-capita demand (given) ---------------------------------------
VR <- 200 - 0.8 * TC  # visits per 1,000 residents
V <- round(VR * (Pop / 1000))

df <- tibble(
  zone = zones, Pop, S_km, w_hr,
  D_km, moneyCost, timeCost, TC, VR, V
)

print(df)

# --- Poisson count model with offset log(Pop/1000) --------------------------
pois_fit <- glm(V ~ TC + offset(log(Pop/1000)),
                data = df, family = poisson(link = "log"))
summary(pois_fit)

dispersion <- pois_fit$deviance / pois_fit$df.residual
dispersion

if (dispersion > 1.5) {
  nb_fit <- MASS::glm.nb(V ~ TC + offset(log(Pop/1000)), data = df)
  print(summary(nb_fit))
}

# --- Welfare (consumer surplus) calculations --------------------------------
TC_choke <- 200 / 0.8
CS_per1000 <- 0.5 * (TC_choke - TC) * VR
CS_zone_USD <- CS_per1000 * (Pop / 1000)
CS_total <- sum(CS_zone_USD)

results <- df |>
  mutate(
    TC_choke = TC_choke,
    CS_per1000 = CS_per1000,
    CS_zone_USD = CS_zone_USD
  ) |>
  select(zone, TC, VR, V, TC_choke, CS_per1000, CS_zone_USD)

print(results)
print(CS_total)

# --- (Optional) Sensitivity: phi = 1/2 --------------------------------------
phi2 <- 1/2
timeCost2 <- (D_km / v_kmh) * (phi2 * w_hr)
TC2 <- moneyCost + timeCost2
VR2 <- 200 - 0.8 * TC2
CS_per1000_2 <- 0.5 * (TC_choke - TC2) * VR2
CS_zone_USD_2 <- CS_per1000_2 * (Pop / 1000)
CS_total_2 <- sum(CS_zone_USD_2)

message(sprintf("CS (phi=1/3) = $%.0f; CS (phi=1/2) = $%.0f", CS_total, CS_total_2))
