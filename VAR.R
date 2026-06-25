library(readr)
library(dplyr)
library(stringi)
library(fpp2)
library(tidyverse)

# Downloading the BLS pc.series file
download.file(
  "https://download.bls.gov/pub/time.series/pc/pc.series",
  destfile = "pc.series",
  mode = "wb",
  headers = c("User-Agent" = "your_email@example.com")
)

# Reading in the reference tables
series   <- read_tsv("pc.series",   trim_ws = TRUE, show_col_types = FALSE)
industry <- read_tsv("pc.industry", trim_ws = TRUE, show_col_types = FALSE)
product  <- read_tsv("pc.product",  trim_ws = TRUE, show_col_types = FALSE)

view(industry)

# Isolating 3-digit industry level series
three_digit <- series %>%
  filter(str_detect(industry_code, "^[0-9]{3}---$"),
         industry_code == product_code) %>%
  left_join(industry, by = "industry_code") %>%
  transmute(series_id, industry_code, industry_name,
            base_date, begin_year, end_year) %>%
  arrange(industry_code)

view(three_digit)

# Data availability by year
three_digit %>% count(begin_year) %>% arrange(begin_year)
three_digit %>% filter(begin_year >= 2003) %>% print(n = Inf)   # the late starters, named :34 industries from 2003
three_digit %>% filter(begin_year <= 1993) %>% print(n = Inf)   # what's clean from 1993 :13 industries start from and before 1993

# Checking time series data from pc.data
# Start from 1998 - 2026 months = 369 roughly 30 years
dat <- read_tsv("pc.data.0.Current", trim_ws = TRUE, show_col_types = FALSE)
names(dat)                                  # series_id, year, period, value, footnote_codes
dat %>% filter(series_id == "PCU325---325---") %>%
  summarise(min_year = min(year), max_year = max(year), n = n())

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
