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