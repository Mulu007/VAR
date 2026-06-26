library(readr)
library(tidyverse)
library(xtable)
library(lubridate)

# Downloading the BLS pc.series file
download.file(
  "https://download.bls.gov/pub/time.series/pc/pc.series",
  destfile = "pc.series",
  mode = "wb",
  headers = c("User-Agent" = "your_email@example.com")
)

# Reading in the reference tables
series   <- read_tsv("pc.series",   trim_ws = TRUE, show_col_types = FALSE)
industry <- read_tsv("pc.industry", trim_ws = TRUE, show_col_types = FALSE)
product  <- read_tsv("pc.product",  trim_ws = TRUE, show_col_types = FALSE)

# Isolating 3-digit industry level series
three_digit <- series %>%
  filter(str_detect(industry_code, "^[0-9]{3}---$"),
         industry_code == product_code) %>%
  left_join(industry, by = "industry_code") %>%
  transmute(series_id, industry_code, industry_name,
            base_date, begin_year, end_year) %>%
  arrange(industry_code)

# Data availability checks by year
# three_digit %>% count(begin_year) %>% arrange(begin_year)
# three_digit %>% filter(begin_year >= 2003) %>% print(n = Inf)   # the late starters, named :34 industries from 2003
# three_digit %>% filter(begin_year <= 1993) %>% print(n = Inf)   # what's clean from 1993 :13 industries start from and before 1993

# Checking time series data from pc.data
# Start from 1998 - 2026 months = 369 roughly 30 years
# dat <- read_tsv("pc.data.0.Current", trim_ws = TRUE, show_col_types = FALSE)
# names(dat)                                  # series_id, year, period, value, footnote_codes
# dat %>% filter(series_id == "PCU325---325---") %>%
#  summarise(min_year = min(year), max_year = max(year), n = n())

# Per-subsector files therefore carry the complete history that current was truncating
# Start from 1984 - 2026 = 539 months
mfg_files <- c("pc.data.4.Food", "pc.data.13.PetroleumCoalProducts",
               "pc.data.14.Chemicals", "pc.data.15.PlasticsRubberProducts",
               "pc.data.16.NonmetallicMineral", "pc.data.17.PrimaryMetal",
               "pc.data.18.FabricatedMetalProduct", "pc.data.1.OilAndGas")

dat <- lapply(mfg_files, read_tsv, trim_ws = TRUE, show_col_types = FALSE) %>%
  bind_rows()

# coverage check: chemicals should now reach 1984
dat %>% filter(series_id == "PCU325---325---") %>%
  summarise(min_year = min(year), max_year = max(year), n = n())

# 491/493 verification -> base year/ start year conflict
# 491 (Postal Service) - monthly data every month from 1972
# 493 (Warehousing) - data starts 1993:07
postal <- read_tsv("pc.data.43.PostalService", trim_ws = TRUE, show_col_types = FALSE)
ware   <- read_tsv("pc.data.45.WarehousingStorage", trim_ws = TRUE, show_col_types = FALSE)

bind_rows(postal, ware) %>%
  filter(series_id %in% c("PCU491---491---", "PCU493---493---"),
         period != "M13") %>%
  mutate(value = as.numeric(value)) %>%
  filter(year <= 1996) %>%
  group_by(series_id, year) %>%
  summarise(months = n(), .groups = "drop") %>%
  arrange(series_id, year) %>%
  print(n = Inf)


# Panel assignment (by begin_year) + flags
# excluded from baseline models (margins / finance-info / weak oil linkage)
baseline_excluded <- c("423","424","441","444","445","455","456","458",
                       "516","517","523","524","721")

roster <- three_digit %>%
  mutate(
    naics  = str_sub(industry_code, 1, 3),
    panel  = case_when(begin_year <= 1993 ~ "A",
                       begin_year <= 2002 ~ "B",
                       begin_year <= 2021 ~ "C",
                       TRUE               ~ "drop"),   # 2022 starters dropped
    base   = paste0(str_sub(base_date, 1, 4), ":", str_sub(base_date, 5, 6)),
    excl   = naics %in% baseline_excluded,
    rail   = naics == "482",
    industry_tex = str_replace_all(industry_name, "&", "\\\\&"),
    start  = as.character(begin_year),
    start  = ifelse(series_id == "PCU493---493---", "1993$^{a}$", start)
  ) %>%
  filter(panel != "drop") %>%
  arrange(panel, naics)

# three-panel table writer
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
  A = "Panel A: Long-Run Historical Timeline (Continuous data from 1993:07--2026)",
  B = "Panel B: Mid-Range Historical Timeline (Staggered Entry,data from 1996--1999)",
  C = "Panel C: Comprehensive Modern Timeline (Post-2003 Overhaul Rollout)"
)

notes <- paste(
  "All series are BLS PPI net-output industry indexes",
  "Base Month is the index reference period; Start is the first month of available data",
  "Panels reflect data availability rather separate datasets:",
  "Panel C, estimated over 2004:01-2006 (the first full year after the December 2003 NAICS-basis overhaul)",
  "Panel A is estimated over 1993:07--2026, set by the start date of $^{a}$Warehousing and Storage (493)",
  "$^{b}$Trade-margin, finance/information and weak-oil-linkage service subsectors",
  "measure margins or services rather than physical net output; they are excluded from baseline, used only in robustness checks.",
  "$^{c}$Rail transportation (482) is available from 1996 and enters rail-augmented core robustness check from that year."
)

emit_three_panel(
  roster, "ppi_coverage.tex",
  "PPI Net-Output Industry Series by Analytical Sample",
  "tab:coverage", panel_titles, notes
)

# Download of the rest of the other files
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
