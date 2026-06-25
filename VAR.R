library(readr)
library(tidyverse)
library(xtable)

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


# ---------------------------------------------------------------------
#. . ROSTER CREATION
#    core  : begin_year <= 1993  -> in both samples
#    broad : 2003-2005 starts    -> broad-network only
#    drop  : 2022 starts         -> too short, excluded
#    margin/finance subsectors flagged (kept, but marked as robustness-only)
# ---------------------------------------------------------------------
margin_finance <- c("423","424","441","444","455","456","458",  # trade margins
                    "516","517","523","524")                     # finance / info svcs

roster <- three_digit %>%
  mutate(
    naics  = str_sub(industry_code, 1, 3),
    sample = case_when(begin_year <= 1993 ~ "core",
                       begin_year >= 2022 ~ "drop",
                       TRUE               ~ "broad"),
    base   = paste0(str_sub(base_date, 1, 4), ":", str_sub(base_date, 5, 6)),
    start  = ifelse(series_id == "PCU493---493---",
                    "1993:07$^{a}$", as.character(begin_year)),
    excl   = naics %in% margin_finance,
    industry_tex = str_replace_all(industry_name, "&", "\\\\&")  # escape & for LaTeX
  ) %>%
  filter(sample != "drop") %>%
  arrange(naics)

# table writer (booktabs + longtable, with a notes block)
emit_table <- function(df, file, caption, label, notes) {
  con <- file(file, "w")
  on.exit(close(con))
  cat("% requires \\usepackage{booktabs, longtable}\n", file = con)
  cat("{\\footnotesize\n", file = con)
  cat("\\begin{longtable}{llp{5cm}cc}\n", file = con)
  cat(sprintf("\\caption{%s}\\label{%s}\\\\\n", caption, label), file = con)
  hdr <- "\\toprule\nNAICS & Base & Industry & Start & End \\\\\n\\midrule\n"
  cat(hdr, file = con)
  cat("\\endfirsthead\n", file = con)
  cat(hdr, file = con)
  cat("\\endhead\n", file = con)
  cat("\\midrule\\multicolumn{5}{r}{\\textit{continued on next page}}\\\\\n\\endfoot\n",
      file = con)
  cat("\\bottomrule\n\\endlastfoot\n", file = con)
  for (i in seq_len(nrow(df))) {
    name <- df$industry_tex[i]
    if (isTRUE(df$excl[i])) name <- paste0(name, "$^{b}$")
    cat(sprintf("%s & %s & %s & %s & %s \\\\\n",
                df$naics[i], df$base[i], name, df$start[i], df$end_year[i]),
        file = con)
  }
  cat("\\end{longtable}\n", file = con)
  cat("\\par\\vspace{-0.4em}\\textit{Notes:} ", notes, "\n}\n", file = con)
}

core_notes <- paste(
  "All series are PPI net-output industry indexes, not seasonally adjusted.",
  "Series identifiers follow the pattern \\texttt{PCU} + three-digit NAICS code,",
  "hyphen-padded to six characters and repeated",
  "(e.g.\\ chemical manufacturing is \\texttt{PCU325{-}{-}{-}325{-}{-}{-}}).",
  "Base is the index reference month. $^{a}$Warehousing and storage (493) is",
  "available monthly from 1993:07; the manufacturing-core panel begins 1993:07",
  "so that every series is present from the first observation."
)

broad_notes <- paste(
  "See Table~\\ref{tab:core} for series construction and conventions.",
  "These subsectors enter the broad-network sample, which begins in 2004",
  "(first full year after the December 2003 NAICS-basis rollout).",
  "$^{b}$Trade-margin and finance/information subsectors measure a margin or",
  "service-price concept rather than net output and are excluded from the",
  "baseline network; they enter only in robustness specifications."
)

emit_table(filter(roster, sample == "core"),  "ppi_core.tex",
           "PPI industry series, manufacturing-core sample (1993:07--2026)",
           "tab:core", core_notes)

emit_table(filter(roster, sample == "broad"), "ppi_broad.tex",
           "Additional PPI industry series, broad-network sample (2004--2026)",
           "tab:broad", broad_notes)
