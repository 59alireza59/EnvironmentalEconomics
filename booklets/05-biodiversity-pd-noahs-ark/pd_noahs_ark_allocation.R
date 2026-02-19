
```r
## ---- 1. Encode the phylogeny (branches, lengths, descendant sets) ---------
species <- c("A","B","C","D","E")

L <- c(e1=5.0, e2=4.0, e3=2.0, e4=2.2, e5=1.8, e6=1.5, e7=0.9, e8=0.7)

Dset <- list(
  e1 = c("A","B"),
  e2 = c("C","D","E"),
  e3 = c("A"),
  e4 = c("B"),
  e5 = c("C"),
  e6 = c("D","E"),
  e7 = c("D"),
  e8 = c("E")
)

## ---- 2. Baseline survival p(0), investment gains, and costs ---------------
p0 <- c(A=0.70, B=0.55, C=0.60, D=0.50, E=0.40)
deltp <- c(A=0.20, B=0.25, C=0.20, D=0.35, E=0.40)
cost <- c(A=4, B=5, C=6, D=7, E=6)
BUDG <- 12

## ---- 3. Function to compute expected PD given a p-vector -------------------
expPD <- function(p_vec) {
  contrib <- sapply(names(L), function(e) {
    one_minus_all_fail <- 1 - prod(1 - p_vec[Dset[[e]]])
    L[e] * one_minus_all_fail
  })
  sum(contrib)
}

EPD0 <- expPD(p0)
EPD0

## ---- 4. Single-species increments and cost-effectiveness -------------------
single_tbl <- lapply(species, function(sp) {
  p1 <- p0
  p1[sp] <- min(1, p0[sp] + deltp[sp])
  EPD1 <- expPD(p1)
  dEPD <- EPD1 - EPD0
  tibble::tibble(
    Species = sp,
    Cost = cost[sp],
    EPD0 = EPD0,
    EPD1 = EPD1,
    dEPD = dEPD,
    CE = dEPD / cost[sp]
  )
}) |> dplyr::bind_rows() |>
  dplyr::arrange(dplyr::desc(CE))

print(single_tbl)

## ---- 5. Exact portfolio search under budget --------------------------------
n <- length(species)
best <- list(mask = rep(FALSE, n), cost = 0, EPD = EPD0, dEPD = 0)

for (mask in 0:(2^n - 1)) {
  pick <- as.logical(intToBits(mask))[1:n]
  sel <- species[pick]
  cst <- sum(cost[sel])

  if (cst <= BUDG) {
    p1 <- p0
    if (length(sel) > 0) {
      p1[sel] <- pmin(1, p0[sel] + deltp[sel])
    }
    EPD1 <- expPD(p1)
    if (EPD1 > best$EPD) {
      best <- list(mask = pick, set = sel, cost = cst, EPD = EPD1, dEPD = EPD1 - EPD0)
    }
  }
}

print(best)

## ---- 6. Optional monetization ---------------------------------------------
alpha <- 8  # $ million per PD-unit/year
annual_benefit_mUSD <- best$dEPD * alpha
annual_benefit_mUSD

## ---- 7. Show a few notable portfolios explicitly (for teaching) -----------
check_sets <- list(
  A_B = c(A=TRUE,B=TRUE,C=FALSE,D=FALSE,E=FALSE),
  A_D = c(A=TRUE,B=FALSE,C=FALSE,D=TRUE,E=FALSE),
  B_E = c(A=FALSE,B=TRUE,C=FALSE,D=FALSE,E=TRUE),
  C_E = c(A=FALSE,B=FALSE,C=TRUE,D=FALSE,E=TRUE),
  B_D = c(A=FALSE,B=TRUE,C=FALSE,D=TRUE,E=FALSE) # often the winner in the booklet
)

tab <- lapply(check_sets, function(sel){
  p1 <- p0
  p1[sel] <- pmin(1, p0[sel] + deltp[sel])
  EPD1 <- expPD(p1)
  dPD <- EPD1 - EPD0
  cost_sel <- sum(cost[sel])
  c(cost = cost_sel, EPD = EPD1, dPD = dPD)
})

do.call(rbind, tab)
