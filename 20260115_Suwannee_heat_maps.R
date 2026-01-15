# ============================================================
# USGS discharge deviation heatmaps (dataRetrieval)
# Station: 02323500
# Window: 1950-01-01 to 2025-12-31
# Heatmaps:
#   (A) % deviation bins with WHITE within ±10%
#   (B) ?? cfs bins where thresholds are ±% of LT monthly mean (month-specific)
# NA = black
# Y-axis ticks every year
# Plots: 1950-2025, 2000-2025, 2010-2025 (for both A and B)
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
end_dt   <- "2025-12-31"

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
    Date = as.Date(Date),
    flow_cfs = Flow,
    year  = year(Date),
    month = month(Date)
  )

# ----------------------------
# MONTHLY MEANS BY YEAR-MONTH
# ----------------------------
mo <- dis2 %>%
  group_by(year, month) %>%
  summarise(avg = mean(flow_cfs, na.rm = TRUE), .groups = "drop")

# Long-term monthly mean (calendar-month climatology) over the full window
mo_porc <- mo %>%
  group_by(month) %>%
  summarise(avg_porc = mean(avg, na.rm = TRUE), .groups = "drop")

# Create full year x month grid, then join avg + avg_porc
years_all <- seq(year(as.Date(start_dt)), year(as.Date(end_dt)))

mo_full <- tidyr::expand_grid(year = years_all, month = 1:12) %>%
  left_join(mo,      by = c("year", "month")) %>%
  left_join(mo_porc, by = "month") %>%
  arrange(year, month) %>%
  mutate(
    month_lab = factor(month, levels = 1:12, labels = month.abb),
    diff = avg - avg_porc,
    diff_percent = 100 * (avg - avg_porc) / avg_porc
  )

# ----------------------------
# BIN DEFINITIONS + COLORS
# ----------------------------
breaks_per <- c(-Inf, -50, -25, -10, 10, 25, 50, Inf)
labels_bin <- c("??? -50%", "-50 to -25%", "-25 to -10%", "-10 to 10%",
                "10 to 25%", "25 to 50%", "??? 50%")

bin_cols <- c(
  "??? -50%"      = "#67001f",
  "-50 to -25%" = "#b2182b",
  "-25 to -10%" = "#ef8a62",
  "-10 to 10%"  = "#f7f7f7",  # WHITE band (±10%)
  "10 to 25%"   = "#67a9cf",
  "25 to 50%"   = "#2166ac",
  "??? 50%"       = "#053061"
)

# % deviation bin (forces WHITE within ±10%)
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

# ?? cfs bin using the SAME percent thresholds, but applied to avg_porc
# WHITE band is: |diff| <= 0.10 * avg_porc (month-specific)
mo_full <- mo_full %>%
  mutate(
    cfs_bin = case_when(
      is.na(diff) | is.na(avg_porc) ~ NA_character_,
      diff <= -0.50 * avg_porc ~ "??? -50%",
      diff <= -0.25 * avg_porc ~ "-50 to -25%",
      diff <= -0.10 * avg_porc ~ "-25 to -10%",
      diff <=  0.10 * avg_porc ~ "-10 to 10%",   # WHITE band
      diff <=  0.25 * avg_porc ~ "10 to 25%",
      diff <=  0.50 * avg_porc ~ "25 to 50%",
      diff >   0.50 * avg_porc ~ "??? 50%",
      TRUE ~ NA_character_
    ),
    cfs_bin = factor(cfs_bin, levels = labels_bin)
  )

# ----------------------------
# BASE PLOTS (year ticks every year)
# ----------------------------
ybreaks_all <- years_all

p_per_base <- ggplot(mo_full, aes(x = month_lab, y = year, fill = per_bin)) +
  geom_tile() +
  scale_fill_manual(values = bin_cols, drop = FALSE, na.value = "black",
                    name = "% dev\n(binned)") +
  scale_y_continuous(breaks = ybreaks_all) +
  labs(
    x = "Month", y = "Year",
    title = paste0("Monthly discharge (% deviation) from long-term monthly mean (1950-2025)\n",
                   site_nm, " (", station, ")"),
    subtitle = "WHITE = within ±10% of LT monthly mean | Red below | Blue above | NA = black"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 6)
  )

p_cfs_base <- ggplot(mo_full, aes(x = month_lab, y = year, fill = cfs_bin)) +
  geom_tile() +
  scale_fill_manual(values = bin_cols, drop = FALSE, na.value = "black",
                    name = "?? cfs\n(binned by\n% of LT)") +
  scale_y_continuous(breaks = ybreaks_all) +
  labs(
    x = "Month", y = "Year",
    title = paste0("Monthly discharge (?? cfs) from long-term monthly mean (1950-2025)\n",
                   site_nm, " (", station, ")"),
    subtitle = "Bins are % of LT monthly mean (month-specific); WHITE = |??| ??? 10% of LT mean | NA = black"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 6)
  )

# ----------------------------
# 3 REQUESTED WINDOWS (for BOTH plots)
# ----------------------------
# 1) 1950-2025
print(p_per_base + coord_cartesian(ylim = c(1950, 2025)))
print(p_cfs_base + coord_cartesian(ylim = c(1950, 2025)))

# 2) 2000-2025
print(p_per_base + coord_cartesian(ylim = c(2000, 2025)) +
        labs(subtitle = "2000-2025 | WHITE = within ±10% of LT monthly mean | NA = black"))
print(p_cfs_base + coord_cartesian(ylim = c(2000, 2025)) +
        labs(subtitle = "2000-2025 | WHITE = |??| ??? 10% of LT monthly mean (month-specific) | NA = black"))

# 3) 2010-2025
print(p_per_base + coord_cartesian(ylim = c(2010, 2025)) +
        labs(subtitle = "2010-2025 | WHITE = within ±10% of LT monthly mean | NA = black"))
print(p_cfs_base + coord_cartesian(ylim = c(2010, 2025)) +
        labs(subtitle = "2010-2025 | WHITE = |??| ??? 10% of LT monthly mean (month-specific) | NA = black"))
###

# ============================================================
# SAVE OUTPUTS (JPG) to ./discharge_graphing
# ============================================================

out_dir <- file.path(getwd(), "discharge_graphing")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Build the 6 plot objects (so we can save them)
p_per_1950_2025  <- p_per_base  + coord_cartesian(ylim = c(1950, 2025)) +
  labs(subtitle = "1950-2025 | WHITE = within ±10% of LT monthly mean | NA = black")

p_cfs_1950_2025  <- p_cfs_base  + coord_cartesian(ylim = c(1950, 2025)) +
  labs(subtitle = "1950-2025 | WHITE = |??| ??? 10% of LT monthly mean (month-specific) | NA = black")

p_per_2000_2025  <- p_per_base  + coord_cartesian(ylim = c(2000, 2025)) +
  labs(subtitle = "2000-2025 | WHITE = within ±10% of LT monthly mean | NA = black")

p_cfs_2000_2025  <- p_cfs_base  + coord_cartesian(ylim = c(2000, 2025)) +
  labs(subtitle = "2000-2025 | WHITE = |??| ??? 10% of LT monthly mean (month-specific) | NA = black")

p_per_2010_2025  <- p_per_base  + coord_cartesian(ylim = c(2010, 2025)) +
  labs(subtitle = "2010-2025 | WHITE = within ±10% of LT monthly mean | NA = black")

p_cfs_2010_2025  <- p_cfs_base  + coord_cartesian(ylim = c(2010, 2025)) +
  labs(subtitle = "2010-2025 | WHITE = |??| ??? 10% of LT monthly mean (month-specific) | NA = black")

# Filenames (safe + descriptive)
fn_per_1950_2025 <- file.path(out_dir, paste0("discharge_percent_dev_", station, "_1950_2025.jpg"))
fn_cfs_1950_2025 <- file.path(out_dir, paste0("discharge_delta_cfs_",   station, "_1950_2025.jpg"))

fn_per_2000_2025 <- file.path(out_dir, paste0("discharge_percent_dev_", station, "_2000_2025.jpg"))
fn_cfs_2000_2025 <- file.path(out_dir, paste0("discharge_delta_cfs_",   station, "_2000_2025.jpg"))

fn_per_2010_2025 <- file.path(out_dir, paste0("discharge_percent_dev_", station, "_2010_2025.jpg"))
fn_cfs_2010_2025 <- file.path(out_dir, paste0("discharge_delta_cfs_",   station, "_2010_2025.jpg"))

# Save
ggsave(fn_per_1950_2025, plot = p_per_1950_2025, width = 10, height = 12, dpi = 300)
ggsave(fn_cfs_1950_2025, plot = p_cfs_1950_2025, width = 10, height = 12, dpi = 300)

ggsave(fn_per_2000_2025, plot = p_per_2000_2025, width = 10, height = 7, dpi = 300)
ggsave(fn_cfs_2000_2025, plot = p_cfs_2000_2025, width = 10, height = 7, dpi = 300)

ggsave(fn_per_2010_2025, plot = p_per_2010_2025, width = 10, height = 6, dpi = 300)
ggsave(fn_cfs_2010_2025, plot = p_cfs_2010_2025, width = 10, height = 6, dpi = 300)

message("Saved JPGs to: ", out_dir)
