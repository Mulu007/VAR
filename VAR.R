library(readr)
library(tidyverse)
library(xtable)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)

# --- Project folder structure ---
# data/raw/       BLS source files (pc.series, pc.industry, pc.product, pc.data.*)
# data/processed/ built panels and results (.rds, .csv)
# tables/         LaTeX table output (.tex)
# figures/        figure output (.pdf)
for (d in c("data/raw", "data/processed", "tables", "figures"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# --- Downloading the BLS pc.series file ---
download.file(
  "https://download.bls.gov/pub/time.series/pc/pc.series",
  destfile = "data/raw/pc.series",
  mode = "wb",
  headers = c("User-Agent" = "uvb20@example.com")
)

# --- Reading in the reference tables ---
series   <- read_tsv("data/raw/pc.series",   trim_ws = TRUE, show_col_types = FALSE) %>%
  mutate(across(where(is.character), str_trim))
industry <- read_tsv("data/raw/pc.industry", trim_ws = TRUE, show_col_types = FALSE) %>%
  mutate(across(where(is.character), str_trim))
product  <- read_tsv("data/raw/pc.product",  trim_ws = TRUE, show_col_types = FALSE) %>%
  mutate(across(where(is.character), str_trim))

# --- Isolating 3-digit net-output totals (industry code == product code) ---
three_digit <- series %>%
  filter(str_detect(industry_code, "^[0-9]{3}---$"),
         industry_code == product_code) %>%
  left_join(industry, by = "industry_code") %>%
  transmute(series_id, industry_code, industry_name,
            base_date, begin_year, end_year) %>%
  arrange(industry_code)

# --- panel assignment (by data availability) + flags ---
# excluded from baseline models (margins / finance-info / weak oil linkage); kept in panel just flagged only
baseline_excluded <- c("423","424","441","444","445","455","456","458",
                       "516","517","523","524","721")

roster <- three_digit %>%
  mutate(
    naics  = str_sub(industry_code, 1, 3),
    panel  = case_when(begin_year <= 1993 ~ "A",       # continuous from 1993
                       begin_year <= 2002 ~ "B",       # 1996-1999 starters
                       begin_year <= 2021 ~ "C",       # post-2003 rollout
                       TRUE               ~ "drop"),   # 2022 starters dropped
    base   = paste0(str_sub(base_date, 1, 4), ":", str_sub(base_date, 5, 6)),
    excl   = naics %in% baseline_excluded,
    rail   = naics == "482",
    industry_tex = str_replace_all(industry_name, "&", "\\\\&"),
    start  = as.character(begin_year),
    start  = ifelse(series_id == "PCU493---493---", 
                    "1993$^{a}$", as.character(begin_year))
  ) %>%
  filter(panel != "drop") %>%
  arrange(panel, naics)

# --- Coverage Table for the appendix (LaTeX) ---
emit_three_panel <- function(df, file, caption, label, panel_titles, notes) {
  con <- file(file, "w")
  on.exit(close(con))
  cat("% requires \\usepackage{booktabs, longtable}\n", file = con)
  cat("{\\footnotesize\n", file = con)
  cat("\\begin{longtable}{llp{5cm}cc}\n", file = con)
  cat(sprintf("\\caption{%s}\\label{%s}\\\\\n", caption, label), file = con)
  hdr <- "\\toprule\nNAICS & Base Month & Industry & Start & End \\\\\n\\midrule\n"
  cat(hdr, file = con); cat("\\endfirsthead\n", file = con)
  cat(hdr, file = con); cat("\\endhead\n", file = con)
  cat("\\midrule\\multicolumn{5}{r}{\\textit{continued on next page}}\\\\\n\\endfoot\n",
      file = con)
  cat("\\bottomrule\n\\endlastfoot\n", file = con)
  for (p in names(panel_titles)) {
    sub <- df %>% filter(panel == p)
    if (nrow(sub) == 0) next
    cat(sprintf("\\multicolumn{5}{l}{\\textbf{%s}}\\\\\n\\midrule\n",
                panel_titles[[p]]), file = con)
    for (i in seq_len(nrow(sub))) {
      name <- sub$industry_tex[i]
      if (isTRUE(sub$excl[i])) name <- paste0(name, "$^{b}$")
      if (isTRUE(sub$rail[i])) name <- paste0(name, "$^{c}$")
      cat(sprintf("%s & %s & %s & %s & %s \\\\\n",
                  sub$naics[i], sub$base[i], name, sub$start[i], sub$end_year[i]),
          file = con)
    }
    cat("\\addlinespace\n", file = con)
  }
  cat("\\end{longtable}\n", file = con)
  cat("\\par\\vspace{-0.4em}\\textit{Notes:} ", notes, "\n}\n", file = con)
}

panel_titles <- list(
  A = "Panel A: Long-run historical timeline (continuous data, 1993:07--2026)",
  B = "Panel B: Mid-range timeline (staggered entry, data begin 1996--1999)",
  C = "Panel C: Modern timeline (post-2003 NAICS-basis overhaul)"
)

notes <- paste(
  "All series are BLS PPI net-output industry indexes.",
  "Base Month is the index reference period; Start is the first month of available data.",
  "Panels reflect data availability, not separate datasets.",
  "The estimation sample is the common window across all subsectors, 2004:01--2026,",
  "the first full year after the December 2003 NAICS-basis overhaul.",
  "$^{a}$Warehousing and storage (493) is available monthly from 1993:07.",
  "$^{b}$Trade-margin, finance/information and weak-oil-linkage service subsectors",
  "measure margins or services rather than physical net output.",
  "$^{c}$Rail transportation (482) is available from 1996."
)

emit_three_panel(roster, "tables/ppi_coverage.tex",
  "PPI Net-Output Industry Series by Data Availability",
  "tab:coverage", panel_titles, notes
)

# --- Download data files for roster series
base_url <- "https://download.bls.gov/pub/time.series/pc/"
files <- c("pc.data.1.OilAndGas","pc.data.4.Food","pc.data.13.PetroleumCoalProducts",
           "pc.data.14.Chemicals","pc.data.15.PlasticsRubberProducts","pc.data.16.NonmetallicMineral",
           "pc.data.17.PrimaryMetal","pc.data.18.FabricatedMetalProduct","pc.data.23.Furniture",
           "pc.data.36.AirTransportation","pc.data.43.PostalService","pc.data.45.WarehousingStorage",
           "pc.data.50.Hospitals","pc.data.2.Mining","pc.data.3.MiningSupport","pc.data.46.Utilities",
           "pc.data.5.BeverageTobacco","pc.data.6.Textile","pc.data.7.TextileProduct","pc.data.8.Apparel",
           "pc.data.10.Wood","pc.data.11.Paper","pc.data.12.Printing","pc.data.19.Machinery",
           "pc.data.20.ComputerProduct","pc.data.21.ElectricalMachinery","pc.data.22.TransportationEquipment",
           "pc.data.24.Miscellaneous","pc.data.76.WholesaleTrade","pc.data.25.MotorVehicleDealers",
           "pc.data.28.BuildingGardenStores","pc.data.29.FoodBeverageStores","pc.data.34.GeneralStores",
           "pc.data.30.HealthStores","pc.data.32.ClothingStores","pc.data.37.RailTransportation",
           "pc.data.38.WaterTransportation","pc.data.39.TruckTransportation","pc.data.42.TransportationSupport",
           "pc.data.44.CouriersAndMessengers","pc.data.54.Broadcasting","pc.data.55.Telecommunications",
           "pc.data.57.Finance","pc.data.58.InsuranceCarriers","pc.data.71.Accommodation")

for (f in files) {
  dest <- file.path("data/raw", f)
  if (!file.exists(dest)) {
    download.file(paste0(base_url, f), destfile = dest, mode = "wb",
                  headers = c("User-Agent" = "uvb20@txst.edu"))
  }
}

# --- Reading values and building the long table ---
# Excluding two overlapping files
# pc.data.0.Current & pc.data.01.aggregates
data_files <- list.files("data/raw", pattern = "^pc\\.data\\.[0-9]+\\.",
                         full.names = TRUE)
data_files <- data_files[!basename(data_files) %in%
                           c("pc.data.0.Current", "pc.data.01.aggregates")]

raw <- bind_rows(lapply(data_files, function(f)               # read each, stack rows
  read_tsv(f, trim_ws = TRUE, show_col_types = FALSE,
           col_types = cols(.default = col_character())) %>%
    mutate(series_id = str_trim(series_id))                   # defensive trim
))

# Keeping my 46 roster series, dropping annual averages and building proper monthly date
long <- raw %>%
  filter(series_id %in% roster$series_id, period != "M13") %>%
  mutate(
    month = as.integer(str_sub(period, 2, 3)),               # M07 -> 7
    date  = make_date(as.integer(year), month, 1),           # -> 2007-07-01
    value = suppressWarnings(as.numeric(value))              # index to numeric
  ) %>%
  filter(!is.na(value)) %>%                                   # drop any non-numeric
  distinct(series_id, date, .keep_all = TRUE) %>%             # safety dedupe
  left_join(select(roster, series_id, naics, panel, excl),    # attach labels
            by = "series_id") %>%
  arrange(series_id, date)

# --- coverage / integrity checks (read these) ------------------------
dup_check <- long %>% count(series_id, date) %>% filter(n > 1) %>% nrow()
cat("Duplicate series-month rows (want 0):", dup_check, "\n")

missing <- roster %>% filter(!series_id %in% long$series_id) %>%
  select(naics, industry_name, panel)
cat("--- MISSING (should be empty) ---\n"); print(missing, n = Inf)

# Building single analysis panel (2004:01 - 2026, all 46)
# Each column becomes one industry by NAICS each row one month
to_wide <- function(df) df %>%
  select(date, naics, value) %>%
  pivot_wider(names_from = naics, values_from = value) %>%
  arrange(date)

#  Drop three series that cannot support a balanced 2004:01 panel:
#  423 wholesalers durable    -> data start 2004:06
#  423 wholesalers nondurable -> data start 2005:06
#  493 warehousing            -> 2004:01 - 2006:11 SUSPENDED (data gap)
drop_naics <- c("423", "424", "493")

analysis_panel <- long %>%
  filter(date >= as.Date("2004-01-01"),
         !naics %in% drop_naics) %>%
  to_wide()

# --- final checks: balance (all NA counts must be 0), dimensions ---
cat("\nanalysis_panel:", nrow(analysis_panel), "x", ncol(analysis_panel)-1, "\n")
cat("NA counts (want all 0):\n"); print(colSums(is.na(analysis_panel)))

# --- Saving Files ---
saveRDS(analysis_panel, "data/processed/analysis_panel_levels.rds")  # MAIN sample
saveRDS(panelA,         "data/processed/panelA_core_levels.rds")     # robustness (1993)
saveRDS(long,           "data/processed/ppi_long.rds")               # tidy long form
saveRDS(roster,         "data/processed/roster.rds")                 # series metadata

message("Done. analysis_panel: ", nrow(analysis_panel), " x ",
        ncol(analysis_panel)-1, " industries, 2004:01 onward.")

# ----------------------------------------------------------------------------------------------------------------
# DATA TRANSFORMATION SECTION
# Raw PPI levels -> log transformation -> X-13 seasonal adjustment -> unit root & stationarity testing -> first-difference
# ----------------------------------------------------------------------------------------------------------------
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

# =====================================================================
# 03_tables_figures.R
# Generate LaTeX tables + figures for the data section / progress doc.
#
# Inputs : results (stationarity_results.csv), analysis_panel_levels.rds,
#          analysis_panel_logSA.rds, stationary_panel.rds, roster.rds
# Outputs: tab_stationarity.tex, and figures/*.pdf
# =====================================================================
library(dplyr); library(tidyr); library(ggplot2); library(lubridate); library(stringr)

dir.create("figures", showWarnings = FALSE)

results   <- read.csv("data/processed/stationarity_results.csv")
levels_p  <- readRDS("data/processed/analysis_panel_levels.rds")
logSA_p   <- readRDS("data/processed/analysis_panel_logSA.rds")
stat_p    <- readRDS("data/processed/stationary_panel.rds")
roster    <- readRDS("data/processed/roster.rds")

# make sure numeric column names are clean (not X327) everywhere
fix_names <- function(df){ names(df) <- sub("^X","",names(df)); df }
logSA_p <- fix_names(logSA_p); stat_p <- fix_names(stat_p); levels_p <- fix_names(levels_p)

# ----------------------------------------------------------
# VISUALISATION
# ----------------------------------------------------------
stat_p <- readRDS("data/processed/stationary_panel.rds")
names(stat_p) <- sub("^X", "", names(stat_p))

stat_long <- stat_p %>%
  pivot_longer(-date, names_to = "naics", values_to = "dlog")

p_all <- ggplot(stat_long, aes(date, dlog)) +
  # Shock Period Shading
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-06-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "steelblue") +
  annotate("rect", xmin = as.Date("2026-02-01"), xmax = max(stat_long$date),
           ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "firebrick") +
  geom_line(linewidth = 0.25) +
  facet_wrap(~ naics, scales = "free_y", ncol = 6) +
  labs(title = "First differences (monthly inflation), all 43 industries",
       subtitle = "Blue: COVID (2020:03-2022:06)   Red: 2026 Iran war (2026:02-)",
       x = NULL, y = expression(Delta~log~price)) +
  theme_minimal(base_size = 8)

p_all

# -----------------
# GENERATION OF WORKING DRAFT (data section)
# ----------------------
dir.create("figures", showWarnings = FALSE)

results <- read.csv("data/processed/stationarity_results.csv")
stat_p  <- readRDS("data/processed/stationary_panel.rds")
roster  <- readRDS("data/processed/roster.rds")
names(stat_p) <- sub("^X","",names(stat_p))

# =====================================================================
# TABLE: Zivot-Andrews results for the 15 ambiguous series
# =====================================================================
za_tab <- results %>%
  filter(!is.na(za_stat)) %>%
  transmute(NAICS = naics, adf = round(adf_diff,3), kpss = round(kpss_diff,3),
            za = za_stat, brk = substr(break_date,1,7),
            rej = ifelse(za_reject,"Yes","No")) %>%
  arrange(brk)

con <- file("tables/tab_stationarity.tex","w")
cat("% requires \\usepackage{booktabs}\n{\\footnotesize\n", file=con)
cat("\\begin{tabular}{lccccc}\n\\toprule\n", file=con)
cat("NAICS & ADF (diff) & KPSS (diff) & ZA stat & Break & Reject $H_0$ \\\\\n\\midrule\n", file=con)
for(i in seq_len(nrow(za_tab)))
  cat(sprintf("%s & %.3f & %.3f & %.2f & %s & %s \\\\\n",
              za_tab$NAICS[i],za_tab$adf[i],za_tab$kpss[i],za_tab$za[i],za_tab$brk[i],za_tab$rej[i]),file=con)
cat("\\bottomrule\n\\end{tabular}\n}\n", file=con)
close(con)
cat("wrote tables/tab_stationarity.tex\n")

# =====================================================================
# FIGURES: all 43 industries, 5 per figure, in the stacked-strip style
# =====================================================================
# short readable names for facet titles: "324  Petroleum and coal products mfg"
name_map <- roster %>% distinct(naics, industry_name) %>%
  mutate(label = paste0(naics, "  ", industry_name))

stat_long <- stat_p %>%
  pivot_longer(-date, names_to="naics", values_to="dlog") %>%
  left_join(name_map, by = "naics")

all_naics <- sort(unique(stat_long$naics))          # 43 codes
groups    <- split(all_naics, ceiling(seq_along(all_naics)/5))   # 9 groups of <=5

for (g in seq_along(groups)) {
  sub <- stat_long %>%
    filter(naics %in% groups[[g]]) %>%
    mutate(label = factor(label, levels = unique(label[order(naics)])))
  
  p <- ggplot(sub, aes(date, dlog)) +
    geom_line(linewidth = 0.3, colour = "grey15") +
    facet_wrap(~ label, ncol = 1, scales = "free_y") +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(title = sprintf("First differences (monthly inflation) — industries %d–%d of 43",
                         (g-1)*5 + 1, (g-1)*5 + length(groups[[g]])),
         x = NULL, y = expression(Delta~log~price)) +
    theme_minimal(base_size = 10) +
    theme(strip.text = element_text(size = 8.5),
          plot.title = element_text(size = 11))
  
  fn <- sprintf("figures/fig_series_%d.pdf", g)
  ggsave(fn, p, width = 7, height = 1.55 * length(groups[[g]]) + 0.7,
         device = cairo_pdf)
  cat("wrote", fn, "\n")
}

# =====================================================================
# FIGURE: Zivot-Andrews break-year histogram
# =====================================================================
byr <- results