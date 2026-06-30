library(readr)
library(tidyverse)
library(xtable)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)

# Downloading the BLS pc.series file
download.file(
  "https://download.bls.gov/pub/time.series/pc/pc.series",
  destfile = "pc.series",
  mode = "wb",
  headers = c("User-Agent" = "your_email@example.com")
)

# --- Reading in the reference tables ---
series   <- read_tsv("pc.series",   trim_ws = TRUE, show_col_types = FALSE) %>%
  mutate(across(where(is.character), str_trim))
industry <- read_tsv("pc.industry", trim_ws = TRUE, show_col_types = FALSE) %>%
  mutate(across(where(is.character), str_trim))
product  <- read_tsv("pc.product",  trim_ws = TRUE, show_col_types = FALSE) %>%
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

emit_three_panel(roster, "ppi_coverage.tex",
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
  if (!file.exists(f)) {
    download.file(paste0(base_url, f), destfile = f, mode = "wb",
                  headers = c("User-Agent" = "uvb20@txst.edu"))
  }
}

# =======================================================
# Reading values and building the long table
# Excluding two overlapping files
# pc.data.0.Current & pc.data.01.aggregates
# =======================================================
data_files <- list.files(pattern = "^pc\\.data\\.[0-9]+\\.")
data_files <- data_files[!data_files %in%
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

# Drop three series that cannot support a balanced 2004:01 panel:
#  423 wholesalers durable    -> data start 2004:06
#  423 wholesalers nondurable -> data start 2005:06
#  493 warehousing            -> 2004:01 - 2006:11 SUSPENDED (data gap)
drop_naics <- c("423", "424", "493")

analysis_panel <- long %>%
  filter(date >= as.Date("2004-01-01"),
         !naics %in% drop_naics) %>%
  to_wide()

# --- final checks: balance (all NA counts must be 0), dimensions ------
cat("\nanalysis_panel:", nrow(analysis_panel), "x", ncol(analysis_panel)-1, "\n")
cat("NA counts (want all 0):\n"); print(colSums(is.na(analysis_panel)))

# =====================================================================
# Saving Files
# =====================================================================
saveRDS(analysis_panel, "analysis_panel_levels.rds")  # MAIN sample
saveRDS(panelA,         "panelA_core_levels.rds")     # robustness (1993)
saveRDS(long,           "ppi_long.rds")               # tidy long form
saveRDS(roster,         "roster.rds")                 # series metadata

message("Done. analysis_panel: ", nrow(analysis_panel), " x ",
        ncol(analysis_panel)-1, " industries, 2004:01 onward.")


