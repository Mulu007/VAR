# =====================================================================
# 05_bea_ppi_audit.R   (v4: complete rewrite, single self-contained run)
#
# Matching audit between BEA detail industries and BLS industry PPIs.
#
# Inputs : data/raw/CxI_DR_Detail.xlsx   (sheet "NAICS Codes")
#          data/raw/pc.series
#
# Outputs: data/bea_ppi_audit.csv        one row per BEA industry (403)
#                                        -> coverage summary: meeting + paper
#          data/bea_ppi_matches_all.csv  one row per BEA-PPI link
#                                        -> roster consumed by the build script
#
# Legend decoded from the BEA sheet (rows 1022-1027):
#   n.a. = BEA industry has no NAICS counterpart (gov't, specials)
#   *    = NAICS industry split across multiple BEA industries (construction)
# =====================================================================
library(readxl); library(dplyr); library(stringr)
library(tidyr);  library(readr);  library(purrr)

dir_raw  <- "data/raw"
dir_data <- "data"

# =====================================================================
# 1. BEA side: detail code -> related NAICS codes (+ flags)
# =====================================================================
nc <- read_excel(file.path(dir_raw, "CxI_DR_Detail.xlsx"),
                 sheet = "NAICS Codes", col_names = FALSE,
                 .name_repair = "minimal")

bea <- tibble(code  = as.character(nc[[4]]),
              title = as.character(nc[[5]]),
              naics = as.character(nc[[7]])) %>%
  filter(str_detect(code, "^[0-9A-Z]{6}$")) %>%
  mutate(
    na_flag  = is.na(naics) | str_to_lower(str_trim(naics)) %in% c("n.a.","na",""),
    ast_flag = str_detect(coalesce(naics, ""), "\\*")
  )

# expand "11113-6, 11119" style strings into individual codes
expand_naics <- function(s) {
  if (is.na(s)) return(character(0))
  s <- str_remove_all(s, "\\*")
  parts <- str_trim(unlist(str_split(s, ",")))
  out <- c()
  for (p in parts) {
    if (p == "" || str_to_lower(p) == "n.a.") next
    if (str_detect(p, "-")) {
      lr    <- str_split_fixed(p, "-", 2)
      left  <- str_trim(lr[1]); right <- str_trim(lr[2])
      lo    <- as.integer(str_sub(left, nchar(left) - nchar(right) + 1))
      hi    <- as.integer(right)
      stem  <- str_sub(left, 1, nchar(left) - nchar(right))
      out   <- c(out, paste0(stem, lo:hi))
    } else out <- c(out, p)
  }
  unique(out)
}

bea_long <- bea %>%
  filter(!na_flag) %>%
  rowwise() %>%
  mutate(naics_codes = list(expand_naics(naics))) %>%
  ungroup() %>%
  unnest(naics_codes)

# =====================================================================
# 2. NAICS 2017 -> 2022 vintage recode (BEFORE matching: BEA codes are
#    2017-basis, pc.series is 2022-basis)
# =====================================================================
recode_2022 <- c("452"="455", "446"="456", "448"="458",   # retail renames
                 "447"="457", "454"="459",                # 454 approximate
                 "442"="449", "443"="449",                # furniture/electronics
                 "451"="459", "453"="459",                # sporting/misc retail
                 "515"="516",                             # broadcasting
                 "511"="513")                             # publishing

bea_long <- bea_long %>%
  mutate(prefix3     = str_sub(naics_codes, 1, 3),
         naics_codes = ifelse(prefix3 %in% names(recode_2022),
                              paste0(recode_2022[prefix3],
                                     str_sub(naics_codes, 4)),
                              naics_codes)) %>%
  select(-prefix3)

cat("tripwire A - recoded codes present (want > 0):",
    sum(str_sub(bea_long$naics_codes, 1, 3) %in%
          c("449","455","456","457","458","459","513","516")), "\n")

# =====================================================================
# 3. BLS side: all industry net-output PPIs at every digit level
#    industry codes are 6 chars, dash-padded: "324---","3241--","32411-","324110"
# =====================================================================
series <- read_tsv(file.path(dir_raw, "pc.series"), trim_ws = TRUE,
                   show_col_types = FALSE) %>%
  mutate(across(where(is.character), str_trim))

ppi <- series %>%
  filter(industry_code == product_code) %>%          # net-output totals
  transmute(series_id, industry_code, begin_year, end_year,
            naics_ppi = str_remove_all(industry_code, "-"),
            digits    = nchar(naics_ppi))

# =====================================================================
# 4. Match every BEA-NAICS code to the finest PPI available (6->5->4->3)
# =====================================================================
find_ppi <- function(code) {
  for (d in rev(3:min(6, nchar(code)))) {
    hit <- ppi %>% filter(naics_ppi == str_sub(code, 1, d))
    if (nrow(hit)) return(hit %>% slice_min(begin_year, n = 1) %>%
                            mutate(match_digits = d))
  }
  tibble(series_id = NA_character_, industry_code = NA_character_,
         begin_year = NA, end_year = NA, naics_ppi = NA_character_,
         digits = NA, match_digits = NA)
}

matches <- bea_long %>%
  rowwise() %>%
  mutate(hit = list(find_ppi(naics_codes))) %>%
  ungroup() %>%
  unnest(hit)

# ---- 4a. FULL LINK TABLE: every BEA-PPI connection (build-script roster)
audit_all <- matches %>%
  filter(!is.na(series_id)) %>%
  transmute(bea_code = code, bea_title = title, ast_flag,
            matched_naics = naics_ppi, match_digits,
            ppi_series = series_id,
            begin_year = as.integer(begin_year)) %>%
  distinct(bea_code, ppi_series, .keep_all = TRUE)

# ---- 4b. ONE-PER-INDUSTRY SUMMARY: best match per BEA industry
audit_best <- matches %>%
  group_by(code, title, ast_flag) %>%
  slice_max(match_digits, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(bea_code = code, bea_title = title, ast_flag,
            matched_naics = naics_ppi, match_digits,
            ppi_series = series_id,
            begin_year = as.integer(begin_year))

audit_full <- bind_rows(
  audit_best,
  bea %>% filter(na_flag) %>%
    transmute(bea_code = code, bea_title = title, ast_flag,
              matched_naics = NA, match_digits = NA,
              ppi_series = NA, begin_year = NA)
) %>%
  mutate(status = case_when(
    !is.na(ppi_series) ~ "matched",
    str_detect(bea_code, "^(S00|GSL|GFE|GFG|4200ID|531HSO)") ~
      "n.a. (no NAICS counterpart)",
    TRUE ~ "NAICS exists, no PPI published"
  )) %>%
  left_join(audit_all %>% count(bea_code, name = "n_ppi"), by = "bea_code") %>%
  mutate(n_ppi = coalesce(n_ppi, 0L)) %>%
  arrange(bea_code)

write.csv(audit_full, file.path(dir_data, "bea_ppi_audit.csv"),
          row.names = FALSE)
write.csv(audit_all,  file.path(dir_data, "bea_ppi_matches_all.csv"),
          row.names = FALSE)

# =====================================================================
# 5. Summary
# =====================================================================
cat("\nBEA detail industries:", nrow(audit_full), "\n")
print(table(audit_full$status))
cat("\nMatch granularity (best match per industry):\n")
print(table(audit_full$match_digits, useNA = "ifany"))
cat("\nUsable from 2004 or earlier (best match):",
    sum(audit_full$begin_year <= 2004, na.rm = TRUE), "\n")
cat("\nFull link table:", nrow(audit_all), "BEA-PPI links |",
    sum(audit_full$n_ppi > 1), "BEA industries with multiple PPIs\n")

cat("\nRecode spot-check (renumbered sectors):\n")
print(audit_full %>%
        filter(bea_code %in% c("446000","448000","452000","511110",
                               "511200","515100","515200")) %>%
        select(bea_code, matched_naics, match_digits, begin_year))

# =====================================================================
# 6. Orphan check: PPIs (4+ digits, data by 2004) claimed by NO BEA code.
#    Bidirectional prefix comparison against EVERY code any BEA industry
#    relates to. Residue should be: 2022 six-digit consolidations,
#    discontinued series, BLS special aggregates (OMFG/OMIN etc).
# =====================================================================
claimed <- unique(bea_long$naics_codes)
cat("\ntripwire B - claimed code set size:", length(claimed), "\n")

orphans <- ppi %>%
  filter(digits >= 4, begin_year <= 2004,
         !map_lgl(naics_ppi, function(p)
           any(str_starts(p, claimed) | str_starts(claimed, p)))) %>%
  arrange(naics_ppi)

cat("Orphan PPIs:", nrow(orphans), "\n")
print(orphans %>% select(naics_ppi, series_id, begin_year), n = 30)

message("\nWritten: data/bea_ppi_audit.csv (summary, one row per BEA industry)")
message("Written: data/bea_ppi_matches_all.csv (roster, one row per BEA-PPI link)")
