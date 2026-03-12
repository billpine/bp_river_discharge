# ============================================================
# USGS discharge deviation heatmaps (dataRetrieval)
# Station: 02323500
# Data window: 1950-01-01 to 2026-02-28
# Long-term monthly mean ("climatology"): 1950-2025 only
#
# Heatmaps:
#   (A) % deviation bins with WHITE within ±10%
#   (B) Δ cfs bins where thresholds are ±% of LT monthly mean
#
# NA = black
# Y-axis ticks every year
# Plots:
#   1950-2026
#   2000-2026
#   2010-2026
#
# Notes:
# - 2026 includes only Jan-Feb because the month index is built
#   directly from start_dt to end_dt by month.
# - Black should only appear where monthly data are unavailable.
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dataRetrieval)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(ggplot2)
})

# ----------------------------
# USER INPUTS
# ----------------------------
station  <- "02323500"
start_dt <- "1950-01-01"
end_dt   <- "2026-02-28"

# ----------------------------
# READ DATA
# ----------------------------
stinfo  <- readNWISsite(station)
site_nm <- stinfo$station_nm[1]

dis <- readNWISdv(
  siteNumbers = station,
  parameterCd = "00060",
  statCd      = "00003",
  startDate   = start_dt,
  endDate     = end_dt
) %>%
  renameNWISColumns()

dis2 <- dis %>%
  transmute(
    Date     = as.Date(Date),
    flow_cfs = Flow,
    year     = year(Date),
    month    = month(Date)
  )

# ----------------------------
# MONTHLY MEANS BY YEAR-MONTH
# ----------------------------
mo <- dis2 %>%
  group_by(year, month) %>%
  summarise(avg = mean(flow_cfs, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    avg = ifelse(is.nan(avg), NA_real_, avg)
  )

# Long-term monthly mean (calendar-month climatology) using 1950-2025 only
mo_porc <- mo %>%
  filter(year >= 1950, year <= 2025) %>%
  group_by(month) %>%
  summarise(avg_porc = mean(avg, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    avg_porc = ifelse(is.nan(avg_porc), NA_real_, avg_porc)
  )

# ----------------------------
# CREATE FULL MONTH INDEX
# Includes only actual months from start_dt to end_dt
# so 2026 will include Jan-Feb only
# ----------------------------
month_seq <- seq.Date(
  from = as.Date(format(as.Date(start_dt), "%Y-%m-01")),
  to   = as.Date(format(as.Date(end_dt),   "%Y-%m-01")),
  by   = "month"
)

mo_index <- tibble(
  year  = year(month_seq),
  month = month(month_seq)
)

# Join observed monthly means + LT monthly mean
mo_full <- mo_index %>%
  left_join(mo,      by = c("year", "month")) %>%
  left_join(mo_porc, by = "month") %>%
  arrange(year, month) %>%
  mutate(
    month_lab    = factor(month, levels = 1:12, labels = month.abb),
    diff         = avg - avg_porc,
    diff_percent = 100 * (avg - avg_porc) / avg_porc
  )

# ----------------------------
# BIN DEFINITIONS + COLORS
# ----------------------------
breaks_per <- c(-Inf, -50, -25, -10, 10, 25, 50, Inf)
labels_bin <- c(
  "<-50%",
  "-50 to -25%",
  "-25 to -10%",
  "-10 to 10%",
  "10 to 25%",
  "25 to 50%",
  ">50%"
)

bin_cols <- c(
  "<-50%"       = "#67001f",
  "-50 to -25%" = "#b2182b",
  "-25 to -10%" = "#ef8a62",
  "-10 to 10%"  = "#f7f7f7",
  "10 to 25%"   = "#67a9cf",
  "25 to 50%"   = "#2166ac",
  ">50%"        = "#053061"
)

# ----------------------------
# ASSIGN BINS
# ----------------------------

# (A) Percent deviation bin
mo_full <- mo_full %>%
  mutate(
    per_bin = cut(
      diff_percent,
      breaks = breaks_per,
      labels = labels_bin,
      include.lowest = TRUE,
      right = TRUE
    ),
    per_bin = factor(per_bin, levels = labels_bin)
  )

# (B) Delta-cfs bin using the same percent thresholds
#     applied to the month-specific LT monthly mean
mo_full <- mo_full %>%
  mutate(
    cfs_bin = case_when(
      is.na(diff) | is.na(avg_porc) ~ NA_character_,
      diff <= -0.50 * avg_porc ~ "<-50%",
      diff <= -0.25 * avg_porc ~ "-50 to -25%",
      diff <= -0.10 * avg_porc ~ "-25 to -10%",
      diff <=  0.10 * avg_porc ~ "-10 to 10%",
      diff <=  0.25 * avg_porc ~ "10 to 25%",
      diff <=  0.50 * avg_porc ~ "25 to 50%",
      diff >   0.50 * avg_porc ~ ">50%",
      TRUE ~ NA_character_
    ),
    cfs_bin = factor(cfs_bin, levels = labels_bin)
  )

# ----------------------------
# OPTIONAL DIAGNOSTIC CHECKS
# ----------------------------
print(setdiff(levels(mo_full$per_bin), names(bin_cols)))
print(setdiff(levels(mo_full$cfs_bin), names(bin_cols)))

print(table(mo_full$per_bin, useNA = "ifany"))
print(table(mo_full$cfs_bin, useNA = "ifany"))

# ----------------------------
# BASE PLOTS
# ----------------------------
ybreaks_all <- seq(min(mo_full$year), max(mo_full$year), by = 1)

p_per_base <- ggplot(mo_full, aes(x = month_lab, y = year, fill = per_bin)) +
  geom_tile() +
  scale_fill_manual(
    values = bin_cols,
    drop = FALSE,
    na.value = "black",
    name = "% deviation\n(binned)"
  ) +
  scale_y_continuous(breaks = ybreaks_all) +
  labs(
    x = "Month",
    y = "Year",
    title = paste0(
      "Monthly discharge (% deviation) from long-term monthly mean\n",
      site_nm, " (", station, ")"
    ),
    subtitle = paste0(
      "Data window: 1950-01 to 2026-02 | LT monthly mean based on 1950-2025 only\n",
      "WHITE = within ±10% of LT monthly mean | Red below | Blue above | NA = black"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 6)
  )

p_cfs_base <- ggplot(mo_full, aes(x = month_lab, y = year, fill = cfs_bin)) +
  geom_tile() +
  scale_fill_manual(
    values = bin_cols,
    drop = FALSE,
    na.value = "black",
    name = expression(Delta~"cfs bins"~"\n(based on % of LT mean)")
  ) +
  scale_y_continuous(breaks = ybreaks_all) +
  labs(
    x = "Month",
    y = "Year",
    title = paste0(
      "Monthly discharge anomaly (Δ cfs) from long-term monthly mean\n",
      site_nm, " (", station, ")"
    ),
    subtitle = paste0(
      "Data window: 1950-01 to 2026-02 | LT monthly mean based on 1950-2025 only\n",
      "Bins are defined by ±10%, ±25%, and ±50% of the month-specific LT mean | ",
      "WHITE = |Δ cfs| ≤ 10% of LT mean | NA = black"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 6)
  )

# ----------------------------
# PLOT WINDOWS
# ----------------------------

# 1) 1950-2026
p_per_1950_2026 <- p_per_base +
  coord_cartesian(ylim = c(1950, 2026)) +
  labs(subtitle = paste0(
    "1950-2026 (2026 includes Jan-Feb only) | LT monthly mean based on 1950-2025 only\n",
    "WHITE = within ±10% of LT monthly mean | NA = black"
  ))

p_cfs_1950_2026 <- p_cfs_base +
  coord_cartesian(ylim = c(1950, 2026)) +
  labs(subtitle = paste0(
    "1950-2026 (2026 includes Jan-Feb only) | LT monthly mean based on 1950-2025 only\n",
    "WHITE = |Δ cfs| ≤ 10% of LT monthly mean (month-specific) | NA = black"
  ))

# 2) 2000-2026
p_per_2000_2026 <- p_per_base +
  coord_cartesian(ylim = c(2000, 2026)) +
  labs(subtitle = paste0(
    "2000-2026 (2026 includes Jan-Feb only) | LT monthly mean based on 1950-2025 only\n",
    "WHITE = within ±10% of LT monthly mean | NA = black"
  ))

p_cfs_2000_2026 <- p_cfs_base +
  coord_cartesian(ylim = c(2000, 2026)) +
  labs(subtitle = paste0(
    "2000-2026 (2026 includes Jan-Feb only) | LT monthly mean based on 1950-2025 only\n",
    "WHITE = |Δ cfs| ≤ 10% of LT monthly mean (month-specific) | NA = black"
  ))

# 3) 2010-2026
p_per_2010_2026 <- p_per_base +
  coord_cartesian(ylim = c(2010, 2026)) +
  labs(subtitle = paste0(
    "2010-2026 (2026 includes Jan-Feb only) | LT monthly mean based on 1950-2025 only\n",
    "WHITE = within ±10% of LT monthly mean | NA = black"
  ))

p_cfs_2010_2026 <- p_cfs_base +
  coord_cartesian(ylim = c(2010, 2026)) +
  labs(subtitle = paste0(
    "2010-2026 (2026 includes Jan-Feb only) | LT monthly mean based on 1950-2025 only\n",
    "WHITE = |Δ cfs| ≤ 10% of LT monthly mean (month-specific) | NA = black"
  ))

# Print plots
print(p_per_1950_2026)
print(p_cfs_1950_2026)

print(p_per_2000_2026)
print(p_cfs_2000_2026)

print(p_per_2010_2026)
print(p_cfs_2010_2026)

# ============================================================
# SAVE OUTPUTS (JPG) to ./discharge_graphing
# ============================================================

out_dir <- file.path(getwd(), "discharge_graphing")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

fn_per_1950_2026 <- file.path(out_dir, paste0("discharge_percent_dev_", station, "_1950_2026.jpg"))
fn_cfs_1950_2026 <- file.path(out_dir, paste0("discharge_delta_cfs_",   station, "_1950_2026.jpg"))

fn_per_2000_2026 <- file.path(out_dir, paste0("discharge_percent_dev_", station, "_2000_2026.jpg"))
fn_cfs_2000_2026 <- file.path(out_dir, paste0("discharge_delta_cfs_",   station, "_2000_2026.jpg"))

fn_per_2010_2026 <- file.path(out_dir, paste0("discharge_percent_dev_", station, "_2010_2026.jpg"))
fn_cfs_2010_2026 <- file.path(out_dir, paste0("discharge_delta_cfs_",   station, "_2010_2026.jpg"))

ggsave(fn_per_1950_2026, plot = p_per_1950_2026, width = 10, height = 12, dpi = 300)
ggsave(fn_cfs_1950_2026, plot = p_cfs_1950_2026, width = 10, height = 12, dpi = 300)

ggsave(fn_per_2000_2026, plot = p_per_2000_2026, width = 10, height = 7, dpi = 300)
ggsave(fn_cfs_2000_2026, plot = p_cfs_2000_2026, width = 10, height = 7, dpi = 300)

ggsave(fn_per_2010_2026, plot = p_per_2010_2026, width = 10, height = 6, dpi = 300)
ggsave(fn_cfs_2010_2026, plot = p_cfs_2010_2026, width = 10, height = 6, dpi = 300)

message("Saved JPGs to: ", out_dir)