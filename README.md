# Monetary Policy Shocks and Macro-Financial Dynamics: A Time-Series Analysis


## Overview

This project constructs a monthly euro area monetary policy shock (MPS) series using high-frequency OIS rate movements around ECB announcements (2003–2023), and estimates its dynamic effect on Luxembourg CPI inflation via local projections.

## Methodology

1. **Shock extraction** — PCA on intraday OIS rate changes (SW–2Y maturities) in the ECB press release window; first principal component = raw monetary policy shock.
2. **Predictability correction** — the monthly shock is regressed on lagged macro-financial variables (STOXX, VSTOXX, industrial production, HICP, S&P 500); residuals = cleaned exogenous MPS.
3. **Stationarity testing** — ADF/KPSS tests on all series, with log-difference/growth-rate transformations where needed.
4. **Local projections** — horizon-specific regressions (h = 0–16) of Luxembourg CPI on the cleaned MPS, with post-double-selection LASSO for control selection and HAC (Newey-West) inference.

## Key Findings

- A single dominant factor (PC1) explains ~75% of OIS co-movement, consistent with Gürkaynak et al. (2005) and Brand et al. (2010).
- The monthly MPS is only weakly predictable (adj. R² ≈ 0.12); large, non-linear equity/volatility moves (squared STOXX/VSTOXX terms) matter more than linear ones.
- Luxembourg inflation shows **no significant response** to the MPS at horizons 0–6 or 8–16; only horizon **h = 7** is significant (≈ −0.11, 95% CI excludes zero) — a modest, temporary disinflation.
- Overall: euro area monetary policy surprises have limited detectable pass-through to inflation in a small, open economy like Luxembourg.

## References

Altavilla et al. (2019), Bauer & Swanson (2022, 2023), Belloni et al. (2014), Gürkaynak et al. (2005), Jordà (2005). Full bibliography in the report.
