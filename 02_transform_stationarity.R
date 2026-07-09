# =====================================================================
# 02_transform_stationarity.R
# Log transform -> X-13 seasonal adjustment -> ADF/KPSS/Zivot-Andrews
# stationarity testing -> first-differenced stationary panel.
# Inputs : data/processed/analysis_panel_levels.rds
# Outputs: data/processed/{analysis_panel_logSA,stationary_panel}.rds,
#          data/processed/stationarity_results.csv
# =====================================================================
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(seasonal) # X-13ARIMA-SEATS 
checkX13()        # check if seasonal works
library(tseries)  # adf.test, kpss.test
library(urca)     # Zivot-Andrews break test
library(seasonalview)

panel <- readRDS("data/processed/analysis_panel_levels.rds")
dates <- panel$date
mat <- as.matrix(panel[,-1]) # removal of the date column in the panel data
print(mat)
ind <- colnames(mat)         # the 43 NAICS codes

# --- Log transformation -> Variance stabilisation (heteroskedasticity correction) ---
logmat <- log(mat) # applies ln to every cell in the matrix

# --- X-13 Seasonal Adjustment Loop ---
sy <- year(dates[1])  # sy for start year
sm <- month(dates[1]) # sm for start month


sa_cols <- lapply(seq_along(ind), function(j) {               # iterates over every j from 1 to 43 applying the same function to each
  x <- ts(logmat[, j], start = c(sy, sm), frequency = 12)     # pulls out j out of the log matrix and tells R its a monthly ts starting from (sy,sm)
  as.numeric(tryCatch(final(seas(x)), error = function(e) x)) # strips out the seasonal component and what remains is the trend-cycle component
})
logSA <- do.call(cbind, sa_cols) 
colnames(logSA) <- ind

# --- Fallback Diagnostic ---
failed <- ind[vapply(seq_along(ind), function(j) {
  x <- ts(logmat[, j], start = c(sy, sm), frequency = 12)
  inherits(tryCatch(seas(x), error = function(e) e), "error")
}, logical(1))]
cat("X-13 fell back to raw log for:",
    if (length(failed)) paste(failed, collapse = ", ") else "none", "\n")

# IMPORTANT: check.names = FALSE keeps numeric column names as "327",
# not "X327". (This was the earlier bug that broke the break test.)
analysis_panel_logSA <- data.frame(date = dates, logSA, check.names = FALSE)
saveRDS(analysis_panel_logSA, "data/processed/analysis_panel_logSA.rds")

#    STATIONARITY TESTS on the log-SA levels.
#    ADF  : H0 = unit root (non-stationary).  small p  -> stationary
#    KPSS : H0 = stationary.                  small p  -> non-stationary
safe_adf  <- function(x) tryCatch(adf.test(x)$p.value,  error = function(e) NA)
safe_kpss <- function(x) tryCatch(kpss.test(x)$p.value, error = function(e) NA)

results <- lapply(ind, function(j) {
  lev <- logSA[, j]; dif <- diff(lev) # first difference
  data.frame(naics = j,
             adf_level = safe_adf(lev),  kpss_level = safe_kpss(lev),
             adf_diff  = safe_adf(dif),  kpss_diff  = safe_kpss(dif))
}) %>% bind_rows() %>%
  # require BOTH tests to agree before declaring a verdict
  mutate(verdict = case_when(
    adf_level <= 0.05 & kpss_level >  0.05 ~ "I(0) stationary in levels",
    adf_diff  <= 0.05 & kpss_diff  >  0.05 ~ "I(1) difference it",
    TRUE                                   ~ "ambiguous -> break test"
  ))

# --- Zivot Andrews break test for the "ambiguous" series ---
# the series flagged ambiguous by ADF/KPSS
ambiguous <- results %>% filter(verdict == "ambiguous -> break test") %>% pull(naics)

# dates for the DIFFERENCED series (one shorter than the level: drop first date)
dseq <- analysis_panel_logSA$date[-1]

za <- lapply(ambiguous, function(n) {
  x <- diff(analysis_panel_logSA[[n]])          # test the first difference
  tryCatch({
    t     <- ur.za(x, model = "both", lag = 4)  # allow 1 break in intercept & trend
    stat  <- as.numeric(t@teststat)
    crit5 <- as.numeric(t@cval[2])              # cval = c(1%, 5%, 10%); take 5%
    data.frame(naics      = n,
               za_stat    = round(stat, 2),
               za_crit5   = crit5,
               za_reject  = stat < crit5,        # TRUE => stationary under a break
               break_date = dseq[t@bpoint])      # estimated break date
  }, error = function(e)
    data.frame(naics = n, za_stat = NA, za_crit5 = NA,
               za_reject = NA, break_date = as.Date(NA)))
}) %>% bind_rows()

print(za)

# --- Results consolidation
results <- results %>%
  select(-any_of(c("za_stat","za_crit5","za_reject","break_date"))) %>%
  left_join(za, by = "naics")

# view safely (tibble printing avoids the na.print error)
as_tibble(results) %>% print(n = Inf)
write.csv(results, "data/processed/stationarity_results.csv", row.names = FALSE)

cat("\nVerdict counts:\n");            print(table(results$verdict))
cat("\nZivot-Andrews rejections (TRUE = stationary under a break):\n")
print(table(results$za_reject, useNA = "ifany"))

# --- Building stationary panel ---
ind <- setdiff(names(analysis_panel_logSA), "date")
stationary_panel <- analysis_panel_logSA %>%
  arrange(date) %>%
  mutate(across(all_of(ind), ~ c(NA, diff(.)))) %>%
  slice(-1)
cat("stationary_panel:", nrow(stationary_panel), "x",
    ncol(stationary_panel)-1, "| NAs:", sum(is.na(stationary_panel)), "\n")
saveRDS(stationary_panel, "data/processed/stationary_panel.rds")
