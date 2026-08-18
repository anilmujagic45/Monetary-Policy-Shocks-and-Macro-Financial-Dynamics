## =============================================================================

## Time Series Project

# Author: Anil Mujagic
# Student-ID: 65067


## =============================================================================

## Overview:

## =============================================================================

## 1) Packages (33 - 57)

## 2) Project Part 1 (58 - 117)

## 3) Project Part 2 (118 - 604)

## 4) Project Part 3 (605 - 995)

## =============================================================================
##
##
## Set your project folder here. This folder should contain:
##   - all data files
##   - the file "ic.glmnet.R"
##
.
## =============================================================================

data_dir <- "C:\\Users\\anilm\\Desktop\\Times Series\\Project\\Data"   # <-- ADAPT THIS


## =============================================================================
## 1) Packages
## =============================================================================

library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(tidyr)
library(rio)
library(readr)
library(car)
library(lmtest)
library(quantmod)
library(sandwich)
library(ggplot2)
library(urca)
library(glmnet)


# install.packages()

## Load custom information-criterion function for glmnet
source(file.path(data_dir, "ic.glmnet.R"))

## =============================================================================
## 2) Project Part 1: PCA on OIS data
## =============================================================================

## Data import and preparation
## -----------------------------------------------------------------------------

my_data_raw <- read_excel(
  file.path(data_dir, "Dataset_EA-MPD.xlsx"),
  sheet = 2
)

my_data_raw <- my_data_raw %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"))

my_data_OIS <- my_data_raw %>%
  select(date, OIS_SW, OIS_1M, OIS_3M, OIS_6M, OIS_1Y, OIS_2Y) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= ymd("2003-06-01") & date <= ymd("2023-10-31"))

my_data <- na.omit(my_data_OIS)
data_OIS <- my_data %>% select(-date)

summary(my_data)

## Correlation matrix
cor_matrix <- cor(data_OIS)
print(round(cor_matrix, 3))

## PCA
## -----------------------------------------------------------------------------

pca_res <- prcomp(data_OIS, scale. = TRUE)
summary(pca_res)

cat("\nPC1 loadings:\n")
print(pca_res$rotation[, 1])
cat("\n")

## Construct monetary policy shock from PC1
pca_scores <- pca_res$x
mp_shock_raw <- pca_scores[, 1]

my_data$MP_Shock <- scale(mp_shock_raw, center = TRUE, scale = TRUE)
head(my_data)
summary(my_data$MP_Shock)

## Plot monthly monetary policy shock (high-frequency PC1 aggregate)
ggplot(my_data, aes(x = date, y = MP_Shock)) +
  geom_line(color = "#003f5c") +
  labs(
    x = "Date",
    y = "MPS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

## =============================================================================
## 3) Project Part 2: Macro-financial predictors & MPS regression
## =============================================================================

## Data import / cleaning
## -----------------------------------------------------------------------------

## 1) STOXX data
STOXX_data <- import(
  file.path(data_dir, "ECB Data Portal_20251014132031.csv"),
  show_col_types = FALSE
) %>%
  rename(price = `EURO STOXX 50 Equity Index - Historical close, average of observations through period (FM.M.U2.EUR.DS.EI.DJES50I.HSTA)`) %>%
  mutate(date = as.Date(DATE)) %>%
  select(date, price)

## 2) VSTOXX data
VSTOXX_data <- import(file.path(data_dir, "VSTOXX.txt")) %>%
  mutate(
    date = as.Date(Date, format = "%d.%m.%Y")
  ) %>%
  select(date, Indexvalue) %>%
  arrange(date) %>%
  mutate(month_date = floor_date(date, "month")) %>%
  group_by(month_date) %>%
  summarise(
    vstoxx = last(Indexvalue[order(date)]),
    .groups = "drop"
  )

## 3) HICP (EA)
HICP_data <- read_excel(file.path(data_dir, "HICP.xlsx")) %>%
  select(-`time period`) %>%
  mutate(date = as.Date(date))

## 4) Industrial Production (EA)
ind_prod_data <- read_excel(
  file.path(data_dir, "Industrial Production.xlsx"),
  range = "A11:D330"
)

ind_prod_data <- ind_prod_data[-1, ] %>%
  rename(
    date        = `GEO (Labels)`,
    ind_prod_EA = `Euro area – 20 countries (from 2023)`
  ) %>%
  mutate(
    date = ymd(paste0(date, "-01")),
    date = ceiling_date(date, "month") - days(1)
  ) %>%
  select(date, ind_prod_EA)

## 5) S&P 500 (requires internet connection via Yahoo Finance)
getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = Sys.Date())

sp500 <- data.frame(date = index(GSPC), price_sp500 = as.numeric(GSPC$GSPC.Adjusted))


## -----------------------------------------------------------------------------
## Time-series plots
## -----------------------------------------------------------------------------

## STOXX
plot(STOXX_data$date, STOXX_data$price,
     type = "l",
     col  = "#003f5c",
     lwd  = 2,
     main = "Time Series Plot: STOXX Price",
     xlab = "Time",
     ylab = "STOXX Price",
     las  = 1)
grid()

## VSTOXX
plot(VSTOXX_data$month_date, VSTOXX_data$vstoxx,
     type = "l",
     col  = "#003f5c",
     lwd  = 2,
     main = "Time Series Plot: VSTOXX",
     xlab = "Time",
     ylab = "VSTOXX Index",
     las  = 1)
grid()

## HICP
plot(HICP_data$date, HICP_data$hicp,
     type = "l",
     col  = "#003f5c",
     lwd  = 2,
     main = "Time Series Plot: HICP",
     xlab = "Time",
     ylab = "HICP",
     las  = 1)
grid()

## Industrial Production (EA)
plot(ind_prod_data$date, ind_prod_data$ind_prod_EA,
     type = "l",
     col  = "#003f5c",
     lwd  = 2,
     main = "Time Series Plot: Industrial Production (EA)",
     xlab = "Time",
     ylab = "Industrial Production",
     las  = 1)
grid()

## S&P 500
plot(sp500$date, sp500$price_sp500,
     type = "l",
     col  = "#003f5c",
     lwd  = 2,
     main = "Time Series Plot: S&P 500",
     xlab = "Time",
     ylab = "S&P 500 Price",
     las  = 1)
grid()

## -----------------------------------------------------------------------------
## ACF and PACF analysis (levels)
## -----------------------------------------------------------------------------

acf(STOXX_data$price,
    lag.max = 40,
    main    = "ACF: STOXX Price",
    col     = "#003f5c",
    lwd     = 2)

pacf(STOXX_data$price,
     lag.max = 40,
     main    = "PACF: STOXX Price",
     col     = "#003f5c",
     lwd     = 2)

acf(VSTOXX_data$vstoxx,
    lag.max = 40,
    main    = "ACF: VSTOXX",
    col     = "#003f5c",
    lwd     = 2)

pacf(VSTOXX_data$vstoxx,
     lag.max = 40,
     main    = "PACF: VSTOXX",
     col     = "#003f5c",
     lwd     = 2)

acf(HICP_data$hicp,
    lag.max = 40,
    main    = "ACF: HICP",
    col     = "#003f5c",
    lwd     = 2)

pacf(HICP_data$hicp,
     lag.max = 40,
     main    = "PACF: HICP",
     col     = "#003f5c",
     lwd     = 2)

acf(ind_prod_data$ind_prod_EA,
    lag.max = 40,
    main    = "ACF: Industrial Production (EA)",
    col     = "#003f5c",
    lwd     = 2)

pacf(ind_prod_data$ind_prod_EA,
     lag.max = 40,
     main    = "PACF: Industrial Production (EA)",
     col     = "#003f5c",
     lwd     = 2)

acf(sp500$price_sp500,
    lag.max = 40,
    main    = "ACF: S&P 500",
    col     = "#003f5c",
    lwd     = 2)

pacf(sp500$price_sp500,
     lag.max = 40,
     main    = "PACF: S&P 500",
     col     = "#003f5c",
     lwd     = 2)

## -----------------------------------------------------------------------------
## Unit root testing (ADF & KPSS)
## -----------------------------------------------------------------------------

run_tests <- function(series, name, lags = 4) {
  cat("\n===============================================\n")
  cat("Testing:", name, "\n")
  cat("===============================================\n\n")
  
  x <- as.numeric(series)
  
  cat("ADF Test (drift):\n")
  adf <- ur.df(x, type = "drift", lags = lags)
  print(summary(adf))
  
  cat("\nKPSS Test:\n")
  kpss <- ur.kpss(x, type = "mu")
  print(summary(kpss))
  
  cat("\n")
}

run_tests(STOXX_data$price,           "STOXX Price",             lags = 4)
run_tests(VSTOXX_data$vstoxx,         "VSTOXX",                  lags = 4)
run_tests(HICP_data$hicp,             "HICP",                    lags = 4)
run_tests(ind_prod_data$ind_prod_EA,  "Industrial Production (EA)", lags = 4)
run_tests(sp500$price_sp500,          "S&P 500",                 lags = 4)

## -----------------------------------------------------------------------------
## Stationary transformations and unit root tests
## -----------------------------------------------------------------------------

## STOXX: 3-month log change
STOXX_monthly <- STOXX_data %>%
  arrange(date) %>%
  mutate(
    stoxx_3m = log(lag(price, 1)) - log(lag(price, 3)),
    year     = year(date),
    month    = month(date)
  )

run_tests(na.omit(STOXX_monthly$stoxx_3m),
          name = "STOXX 3M log change",
          lags = 4)

## VSTOXX: 3-month log change
VSTOXX_monthly <- VSTOXX_data %>%
  arrange(month_date) %>%
  mutate(
    vstoxx_3m = log(lag(vstoxx, 1)) - log(lag(vstoxx, 3)),
    year      = year(month_date),
    month     = month(month_date)
  )

run_tests(na.omit(VSTOXX_monthly$vstoxx_3m),
          name = "VSTOXX 3M log change",
          lags = 4)

## HICP: 15-month change
HICP_monthly <- HICP_data %>%
  arrange(date) %>%
  mutate(
    hicp_change_15m = lag(hicp, 1) - lag(hicp, 15),
    hicp_lag1       = lag(hicp_change_15m, 1),
    hicp_lag2       = lag(hicp_change_15m, 2),
    year            = year(date),
    month           = month(date)
  )

run_tests(na.omit(HICP_monthly$hicp_change_15m),
          name = "HICP 15-month change",
          lags = 4)

## Industrial production: growth rate and lags
ind_prod_monthly <- ind_prod_data %>%
  arrange(date) %>%
  mutate(
    indprod_gr  = 100 * (log(ind_prod_EA) - log(lag(ind_prod_EA, 1))),
    indprod_lag1 = lag(indprod_gr, 1),
    indprod_lag2 = lag(indprod_gr, 2),
    year         = year(date),
    month        = month(date)
  )

run_tests(na.omit(ind_prod_monthly$indprod_gr),
          name = "Industrial Production growth",
          lags = 4)

## S&P 500: 3-month log change
sp500_monthly <- sp500 %>%
  mutate(date = as.Date(cut(date, "month"))) %>%
  group_by(date) %>%
  summarise(price_sp500 = last(price_sp500), .groups = "drop") %>%
  arrange(date) %>%
  mutate(
    sp500_3m = log(lag(price_sp500, 1)) - log(lag(price_sp500, 3)),
    year     = year(date),
    month    = month(date)
  )

run_tests(na.omit(sp500_monthly$sp500_3m),
          name = "S&P 500 3M log change",
          lags = 4)

## -----------------------------------------------------------------------------
## Construct monthly MPS series
## -----------------------------------------------------------------------------

monthly_shocks <- my_data %>%
  mutate(
    month     = floor_date(date, "month"),
    MP_Shock_1 = as.numeric(MP_Shock)
  ) %>%
  group_by(month) %>%
  summarise(
    MP_Shock         = sum(MP_Shock_1, na.rm = TRUE),
    first_event_date = min(date),
    n_events         = n(),
    .groups          = "drop"
  ) %>%
  mutate(
    year  = year(month),
    month = month(month)
  ) %>%
  group_by(year) %>%
  tidyr::complete(month = 1:12) %>%
  mutate(
    MP_Shock = ifelse(is.na(MP_Shock), 0, MP_Shock),
    n_events = ifelse(is.na(n_events), 0, n_events)
  ) %>%
  ungroup() %>%
  mutate(month_date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  arrange(month_date) %>%
  filter(month_date >= as.Date("1999-12-01"))

head(monthly_shocks)

## -----------------------------------------------------------------------------
## Merge monthly MPS with transformed macro-financial variables
## -----------------------------------------------------------------------------

Joined_data <- monthly_shocks %>%
  left_join(
    STOXX_monthly %>%
      select(year, month, stoxx_3m),
    by = c("year", "month")
  ) %>%
  left_join(
    VSTOXX_monthly %>%
      select(year, month, vstoxx_3m),
    by = c("year", "month")
  ) %>%
  left_join(
    HICP_monthly %>%
      select(year, month, hicp_change_15m, hicp_lag1, hicp_lag2),
    by = c("year", "month")
  ) %>%
  left_join(
    ind_prod_monthly %>%
      select(year, month, indprod_gr, indprod_lag1, indprod_lag2),
    by = c("year", "month")
  ) %>%
  left_join(
    sp500_monthly %>%
      select(year, month, sp500_3m),
    by = c("year", "month")
  ) %>%
  drop_na()

head(Joined_data)

## Correlation matrix for regression variables
cor_matrix <- Joined_data %>%
  select(MP_Shock, stoxx_3m, vstoxx_3m, hicp_change_15m, indprod_gr, sp500_3m) %>%
  cor(use = "pairwise.complete.obs")

cor_matrix

## -----------------------------------------------------------------------------
## MPS regression models
## -----------------------------------------------------------------------------

## Model 1: Baseline specification
model_1 <- lm(MP_Shock ~ stoxx_3m + vstoxx_3m + sp500_3m +
                hicp_change_15m + indprod_gr,
              data = Joined_data)
summary(model_1)

## Model 2: Add one-period lags of HICP and industrial production
model_2 <- lm(MP_Shock ~
                stoxx_3m + vstoxx_3m + sp500_3m +
                hicp_change_15m + indprod_gr +
                hicp_lag1 + indprod_lag1,
              data = Joined_data)
summary(model_2)

## Model 3: Quadratic terms + lags
model_3 <- lm(MP_Shock ~
                stoxx_3m + I(stoxx_3m^2) +
                vstoxx_3m + I(vstoxx_3m^2) +
                sp500_3m +
                hicp_change_15m + I(hicp_change_15m^2) +
                indprod_gr +
                hicp_lag1 + indprod_lag1,
              data = Joined_data)
summary(model_3)

## Model 4: Polynomial terms and interaction (preferred model)
model_4 <- lm(MP_Shock ~
                poly(stoxx_3m, 2, raw = TRUE) +
                poly(vstoxx_3m, 2, raw = TRUE) +
                stoxx_3m:vstoxx_3m +
                sp500_3m + hicp_change_15m + indprod_gr,
              data = Joined_data)
summary(model_4)

## -----------------------------------------------------------------------------
## Model comparison: AIC and BIC
## -----------------------------------------------------------------------------

aic_values <- AIC(model_1, model_2, model_3, model_4)
bic_values <- BIC(model_1, model_2, model_3, model_4)

model_comparison <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  AIC   = aic_values$AIC,
  BIC   = bic_values$BIC,
  N     = rep(nobs(model_1), 4)
)

print(model_comparison)

## -----------------------------------------------------------------------------
## Regression diagnostics helper
## -----------------------------------------------------------------------------

run_diagnostics <- function(mod, name = "model") {
  cat("\n==============================\n")
  cat("Diagnostics for", name, "\n")
  cat("==============================\n")
  
  ## b) Multicollinearity (VIF / GVIF)
  cat("\n[b) Multicollinearity - VIF]\n")
  vif_out <- tryCatch(car::vif(mod), error = function(e) e)
  if (inherits(vif_out, "error")) {
    cat("VIF error:", vif_out$message, "\n")
  } else {
    print(vif_out)
    if (any(grepl("GVIF", names(attributes(vif_out))))) {
      cat("\nNote: GVIF reported for multi-df terms. You can convert via GVIF^(1/(2*Df)).\n")
    }
  }
  
  ## c) Autocorrelation (Durbin-Watson)
  cat("\n[c) Autocorrelation - Durbin-Watson]\n")
  dw <- tryCatch(lmtest::dwtest(mod), error = function(e) e)
  if (inherits(dw, "error")) {
    cat("DW error:", dw$message, "\n")
  } else {
    print(dw)
  }
  
  ## d) Heteroskedasticity (Breusch-Pagan)
  cat("\n[d) Heteroskedasticity - Breusch-Pagan]\n")
  bp <- tryCatch(lmtest::bptest(mod), error = function(e) e)
  if (inherits(bp, "error")) {
    cat("BP error:", bp$message, "\n")
  } else {
    print(bp)
    if (bp$p.value < 0.05) {
      cat("-> Heteroskedasticity detected (p < 0.05). HAC or robust SEs are recommended.\n")
    }
  }
  
  ## e) Normality (Shapiro-Wilk, plus QQ-plot if desired)
  cat("\n[e) Normality - Shapiro-Wilk]\n")
  res <- residuals(mod)
  sh  <- tryCatch(shapiro.test(res), error = function(e) e)
  if (inherits(sh, "error")) {
    cat("Shapiro error:", sh$message, "\n")
  } else {
    print(sh)
  }
  
  invisible(list(vif = vif_out, dw = dw, bp = bp, shapiro = sh))
}

run_diagnostics(model_4, "Model 4")

## -----------------------------------------------------------------------------
## HAC-robust standard errors for Model 4 and cleaned MPS
## -----------------------------------------------------------------------------

cov.hac <- NeweyWest(model_4, lag = 3, prewhite = FALSE)
coeftest(model_4, vcov. = cov.hac)

## Cleaned MPS = residuals from Model 4
mps_data <- Joined_data %>%
  mutate(MP_Shock_hat = residuals(model_4)) %>%
  select(month_date, MP_Shock_hat)

head(mps_data)



## =============================================================================
## 4) Project Part 3: Local projections for Luxembourg inflation
## =============================================================================

## Data import / cleaning
## -----------------------------------------------------------------------------

## 1) Luxembourg CPI (STATEC / PRIX_CONSO)
cip_data <- import(file.path(data_dir, "LU1,DSD_PRIX_CONSO@DF_E5100,1.0+all.csv"))

cip_monthly <- cip_data %>%
  filter(FREQ == "M") %>%
  mutate(
    date     = as.Date(paste0(TIME_PERIOD, "-01")),
    LU_cpi   = as.numeric(OBS_VALUE)
  ) %>%
  arrange(date) %>%
  mutate(
    LU_cpi_log = 100 * (log(LU_cpi) - log(lag(LU_cpi)))
  ) %>%
  drop_na(LU_cpi) %>%
  mutate(month_date = floor_date(date, "month")) %>%
  select(month_date, LU_cpi, LU_cpi_log)

## 2) German 1-year bond yield (DE1Y) from EA-MPD
DE_Bond <- my_data_raw %>%
  select(date, DE1Y) %>%
  drop_na(DE1Y) %>%
  mutate(
    date       = as.Date(date),
    month_date = floor_date(date, "month")
  ) %>%
  group_by(month_date) %>%
  summarise(DE1Y = mean(DE1Y, na.rm = TRUE), .groups = "drop")


## -----------------------------------------------------------------------------
## Final merged dataset for local projections
## -----------------------------------------------------------------------------

final_data <- mps_data %>%
  inner_join(DE_Bond,     by = "month_date") %>%
  inner_join(cip_monthly, by = "month_date") %>%
  select(month_date, MP_Shock_hat, DE1Y, LU_cpi, LU_cpi_log) %>%
  drop_na(MP_Shock_hat, DE1Y, LU_cpi)

summary(final_data)


## -----------------------------------------------------------------------------
## Time-series plots (Part 3 variables)
## -----------------------------------------------------------------------------

plot(final_data$month_date, final_data$MP_Shock_hat,
     type = "l",
     col  = "#003f5c",
     lwd  = 2,
     main = "Time Series Plot: MP_Shock (cleaned)",
     xlab = "Time",
     ylab = "MP_Shock_hat",
     las  = 1)
grid()

plot(final_data$month_date, final_data$DE1Y,
     type = "l",
     col  = "#003f5c",
     lwd  = 2,
     main = "DE1Y",
     xlab = "Time",
     ylab = "DE1Y",
     las  = 1)
grid()

plot(final_data$month_date, final_data$LU_cpi,
     type = "l",
     col  = "#003f5c",
     lwd  = 2,
     main = "Luxembourg CPI level",
     xlab = "Time",
     ylab = "LU CPI",
     las  = 1)
grid()

plot(final_data$month_date, final_data$LU_cpi_log,
     type = "l",
     col  = "#003f5c",
     lwd  = 2,
     main = "Luxembourg CPI (log difference)",
     xlab = "Time",
     ylab = "LU CPI log diff",
     las  = 1)
grid()


## -----------------------------------------------------------------------------
## ACF and PACF (Part 3 variables)
## -----------------------------------------------------------------------------

acf(final_data$MP_Shock_hat,
    lag.max = 40,
    main    = "ACF: MP_Shock_hat",
    col     = "purple",
    lwd     = 2)

pacf(final_data$MP_Shock_hat,
     lag.max = 40,
     main    = "PACF: MP_Shock_hat",
     col     = "purple",
     lwd     = 2)

acf(final_data$DE1Y,
    lag.max = 40,
    main    = "ACF: DE1Y",
    col     = "purple",
    lwd     = 2)

pacf(final_data$DE1Y,
     lag.max = 40,
     main    = "PACF: DE1Y",
     col     = "purple",
     lwd     = 2)

acf(final_data$LU_cpi,
    lag.max = 40,
    main    = "ACF: Luxembourg CPI level",
    col     = "purple",
    lwd     = 2)

pacf(final_data$LU_cpi,
     lag.max = 40,
     main    = "PACF: Luxembourg CPI level",
     col     = "purple",
     lwd     = 2)

acf(final_data$LU_cpi_log,
    lag.max = 40,
    main    = "ACF: Luxembourg CPI (log difference)",
    col     = "purple",
    lwd     = 2)

pacf(final_data$LU_cpi_log,
     lag.max = 40,
     main    = "PACF: Luxembourg CPI (log difference)",
     col     = "purple",
     lwd     = 2)



## -----------------------------------------------------------------------------
## Unit root tests (Part 3 variables)
## -----------------------------------------------------------------------------

run_tests(final_data$MP_Shock_hat, "MP_Shock_hat")
run_tests(final_data$DE1Y,         "DE1Y")
run_tests(final_data$LU_cpi,       "LU_cpi")
run_tests(final_data$LU_cpi_log,   "LU_cpi_log")


## =============================================================================
## Post-double-selection LASSO and local projections
## =============================================================================

## -----------------------------------------------------------------------------
## Build lagged variables
## -----------------------------------------------------------------------------

z  <- final_data$MP_Shock_hat
y1 <- final_data$DE1Y
y2 <- final_data$LU_cpi_log

z_lag_0 <- z[4:length(z)]
z_lag_1 <- z[3:(length(z) - 1)]
z_lag_2 <- z[2:(length(z) - 2)]
z_lag_3 <- z[1:(length(z) - 3)]

y1_lag_0 <- y1[4:length(y1)]
y1_lag_1 <- y1[3:(length(y1) - 1)]
y1_lag_2 <- y1[2:(length(y1) - 2)]
y1_lag_3 <- y1[1:(length(y1) - 3)]

y2_lag_0 <- y2[4:length(y2)]
y2_lag_1 <- y2[3:(length(y2) - 1)]
y2_lag_2 <- y2[2:(length(y2) - 2)]
y2_lag_3 <- y2[1:(length(y2) - 3)]

## Controls x_{t-1}: all lags of z, y1, y2 (from t-1 to t-3)
lags <- cbind(
  z_lag_1, z_lag_2, z_lag_3,
  y1_lag_1, y1_lag_2, y1_lag_3,
  y2_lag_1, y2_lag_2, y2_lag_3
)

## -----------------------------------------------------------------------------
## LASSO model selection (post-double selection)
## -----------------------------------------------------------------------------

## 1) LASSO: z_t on x_{t-1}  -> controls relevant for MP_Shock
lasso_z <- ic.glmnet(lags, z_lag_0, crit = "bic")
nz_z    <- which(as.vector(coef(lasso_z))[-1] != 0)
selected_lags_z <- lags[, nz_z, drop = FALSE]

## 2) LASSO: y2_t on x_{t-1} -> controls relevant for inflation
lasso_y2 <- ic.glmnet(lags, y2_lag_0, crit = "bic")
nz_y2    <- which(as.vector(coef(lasso_y2))[-1] != 0)
selected_lags_y2 <- lags[, nz_y2, drop = FALSE]

## 3) Union of selected controls (post-double selection)
all_indices <- unique(c(nz_z, nz_y2))
union_lags  <- lags[, all_indices, drop = FALSE]

## -----------------------------------------------------------------------------
## Post-LASSO OLS for h = 0
## -----------------------------------------------------------------------------

model <- lm(y2_lag_0 ~ z_lag_0 + union_lags)

direct_forecast_0 <- coef(model)[2]

## HAC covariance matrix with automatic bandwidth (Newey-West)
hac_vcov   <- NeweyWest(model, lag = NULL, prewhite = FALSE, adjust = TRUE)
hac_results <- coeftest(model, vcov. = hac_vcov)

## Extract coefficient and 95% CI for z_lag_0
z_index <- which(rownames(hac_results) == "z_lag_0")
z_coef  <- hac_results[z_index, 1]
z_se    <- hac_results[z_index, 2]

lower <- z_coef - qnorm(0.975) * z_se
upper <- z_coef + qnorm(0.975) * z_se
conf_int <- cbind(lower, upper)

cat("Coefficient for z_lag_0:", z_coef, "\n")
cat("95% Confidence Interval:", conf_int[1], "to", conf_int[2], "\n")

## -----------------------------------------------------------------------------
## Residual diagnostics for baseline local projection (h = 0)
## -----------------------------------------------------------------------------

residuals_lp <- residuals(model)

acf(residuals_lp,
    lag.max = 40,
    main    = "ACF: residuals (h = 0)",
    col     = "purple",
    lwd     = 2)

pacf(residuals_lp,
     lag.max = 40,
     main    = "PACF: residuals (h = 0)",
     col     = "purple",
     lwd     = 2)

lb_test <- Box.test(residuals_lp, lag = 10, type = "Ljung-Box")
print(lb_test)

bg_test <- bgtest(model, order = 2)
print(bg_test)

BP_test <- bptest(model, ~ fitted.values(model) + I(fitted.values(model)^2))
print(BP_test)

## -----------------------------------------------------------------------------
## Local projections with LASSO for horizons h = 0,...,16
## -----------------------------------------------------------------------------

lp_lasso_per_h <- function(z, y1, y2, Hmax = 16) {
  T <- length(z)
  stopifnot(length(y1) == T, length(y2) == T)
  
  results <- vector("list", Hmax + 1)
  
  for (h in 0:Hmax) {
    
    ## 1) Construct lags for each horizon h
    z_lag_0 <- z[4:(T - h)]
    z_lag_1 <- z[3:(T - h - 1)]
    z_lag_2 <- z[2:(T - h - 2)]
    z_lag_3 <- z[1:(T - h - 3)]
    
    y1_lag_1 <- y1[3:(T - h - 1)]
    y1_lag_2 <- y1[2:(T - h - 2)]
    y1_lag_3 <- y1[1:(T - h - 3)]
    
    y2_lag_0 <- y2[(4 + h):T]
    y2_lag_1 <- y2[3:(T - h - 1)]
    y2_lag_2 <- y2[2:(T - h - 2)]
    y2_lag_3 <- y2[1:(T - h - 3)]
    
    n_eff <- length(z_lag_0)
    if (any(c(
      length(z_lag_1), length(z_lag_2), length(z_lag_3),
      length(y1_lag_1), length(y1_lag_2), length(y1_lag_3),
      length(y2_lag_0), length(y2_lag_1), length(y2_lag_2), length(y2_lag_3)
    ) != n_eff)) {
      stop(paste("Length mismatch at h =", h))
    }
    
    ## 2) Controls (x_{t-1}) for this horizon
    lags_h <- cbind(
      z_lag_1, z_lag_2, z_lag_3,
      y1_lag_1, y1_lag_2, y1_lag_3,
      y2_lag_1, y2_lag_2, y2_lag_3
    )
    
    ## 3) LASSO with BIC for z_t and y2_{t+h}
    lasso_z_h  <- ic.glmnet(lags_h, z_lag_0,  crit = "bic")
    lasso_y2_h <- ic.glmnet(lags_h, y2_lag_0, crit = "bic")
    
    idx_z  <- which(as.vector(coef(lasso_z_h ))[-1] != 0)
    idx_y2 <- which(as.vector(coef(lasso_y2_h))[-1] != 0)
    
    all_idx   <- sort(unique(c(idx_z, idx_y2)))
    union_lags <- lags_h[, all_idx, drop = FALSE]
    
    ## 4) Post-LASSO OLS
    df_h <- data.frame(
      y = y2_lag_0,
      z = z_lag_0,
      union_lags
    )
    
    model_h <- lm(y ~ z + ., data = df_h)
    
    ## 5) Newey-West HAC standard errors and CI
    hac_vcov_h <- NeweyWest(model_h, lag = NULL, prewhite = FALSE, adjust = TRUE)
    hac_res_h  <- coeftest(model_h, vcov. = hac_vcov_h)
    
    beta_z <- hac_res_h["z", 1]
    se_z   <- hac_res_h["z", 2]
    
    lower <- beta_z - qnorm(0.975) * se_z
    upper <- beta_z + qnorm(0.975) * se_z
    
    cat("\n--------- h =", h, "---------\n")
    cat("Coefficient for z_t:", beta_z, "\n")
    cat("95% confidence interval:", lower, "to", upper, "\n")
    
    results[[h + 1]] <- list(
      h          = h,
      coef       = beta_z,
      se         = se_z,
      ci         = c(lower, upper),
      model      = model_h,
      hac_output = hac_res_h,
      selected   = all_idx
    )
  }
  
  return(results)
}

## Run local projections up to horizon 16
res <- lp_lasso_per_h(z, y1, y2, Hmax = 16)

## -----------------------------------------------------------------------------
## Build Impulse Response Function
## -----------------------------------------------------------------------------

valid_res <- Filter(function(x) !is.null(x), res)

irf_list <- lapply(valid_res, function(x) {
  if (is.null(x$coef) || is.null(x$ci)) return(NULL)
  data.frame(
    h     = x$h,
    coef  = x$coef,
    lower = x$ci[1],
    upper = x$ci[2]
  )
})

irf_list <- Filter(function(x) !is.null(x), irf_list)
irf_df   <- do.call(rbind, irf_list)

irf_df

## -----------------------------------------------------------------------------
## Plot local projection impulse response with 95% HAC confidence band
## -----------------------------------------------------------------------------

ggplot(irf_df, aes(x = h, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  geom_point() +
  labs(
    x     = "Horizon h",#
    y     = "Impulse response",
    title = "Local Projection Impulse Response",
    subtitle = "Point estimates with 95% Newey-West confidence intervals"
  ) +
  theme_minimal()



