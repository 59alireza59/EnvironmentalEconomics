
```r
## -------------------- 0) Inputs ---------------------------------------------
sp <- c("A","B","C","D","E","F")

Dmat <- matrix(c(
  0,   2.9, 4.4, 1.8, 3.7, 2.6,
  2.9, 0,   2.2, 3.1, 2.9, 3.8,
  4.4, 2.2, 0,   4.5, 1.9, 3.3,
  1.8, 3.1, 4.5, 0,   3.6, 2.1,
  3.7, 2.9, 1.9, 3.6, 0,   4.0,
  2.6, 3.8, 3.3, 2.1, 4.0, 0
), nrow = 6, byrow = TRUE)

rownames(Dmat) <- colnames(Dmat) <- sp

p0 <- c(A=0.65, B=0.55, C=0.50, D=0.60, E=0.45, F=0.40)
dp <- c(A=0.20, B=0.25, C=0.30, D=0.20, E=0.35, F=0.40)
cost <- c(A=4, B=5, C=6, D=3, E=6, F=5)
BUDG <- 11

## -------------------- 1) MST length for a given subset ----------------------
mst_length <- function(Dsub) {
  n <- nrow(Dsub)
  if (n <= 1) return(0)
  inTree <- rep(FALSE, n)
  key <- rep(Inf, n)
  key[1] <- 0
  total <- 0
  for (k in 1:n) {
    u <- which(!inTree)[which.min(key[!inTree])]
    inTree[u] <- TRUE
    total <- total + key[u]
    for (v in which(!inTree)) {
      if (Dsub[u, v] < key[v]) key[v] <- Dsub[u, v]
    }
  }
  total
}

mst_len_subset <- function(subset_names) {
  if (length(subset_names) <= 1) return(0)
  idx <- match(subset_names, sp)
  Dsub <- Dmat[idx, idx, drop = FALSE]
  mst_length(Dsub)
}

## -------------------- 2) Exact expected diversity given p-vector ------------
expD_exact <- function(p) {
  n <- length(p)
  ED <- 0
  for (mask in 0:(2^n - 1)) {
    pick <- as.logical(intToBits(mask))[1:n]
    Rset <- sp[pick]
    D_R <- mst_len_subset(Rset)

    pr <- 1
    for (i in 1:n) pr <- pr * ifelse(pick[i], p[i], (1 - p[i]))

    ED <- ED + D_R * pr
  }
  ED
}

ED0 <- expD_exact(p0)
cat(sprintf("Baseline E[D] = %.6f\n", ED0))

## -------------------- 3) Single-species funding increments ------------------
single_tbl <- lapply(sp, function(k) {
  p1 <- p0
  p1[k] <- min(1, p0[k] + dp[k])
  ED1 <- expD_exact(p1)
  dED <- ED1 - ED0
  data.frame(
    Species = k,
    Cost = cost[k],
    ED0 = ED0,
    ED1 = ED1,
    dED = dED,
    CE = dED / cost[k]
  )
}) |> do.call(rbind, _)

single_tbl <- single_tbl[order(-single_tbl$CE), ]
print(single_tbl)

## -------------------- 4) Optimal portfolio search under budget --------------
n <- length(sp)
best <- list(set = character(0), cost = 0, ED = ED0, dED = 0)

for (mask in 0:(2^n - 1)) {
  pick <- as.logical(intToBits(mask))[1:n]
  sel <- sp[pick]
  cst <- sum(cost[sel])

  if (cst <= BUDG) {
    p1 <- p0
    if (length(sel) > 0) {
      p1[sel] <- pmin(1, p0[sel] + dp[sel])
    }
    ED1 <- expD_exact(p1)
    if (ED1 > best$ED) {
      best <- list(set = sel, cost = cst, ED = ED1, dED = ED1 - ED0)
    }
  }
}

cat("\nBest portfolio under budget:\n")
print(best)

## -------------------- 5) Monetization (optional) ----------------------------
alpha_mUSD <- 10 # $ million per diversity unit
annual_benefit_mUSD <- best$dED * alpha_mUSD
cat(sprintf("\nAnnual monetized gain vs baseline: $%.2f million\n", annual_benefit_mUSD))
