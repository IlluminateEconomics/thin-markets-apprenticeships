# ==============================================================================
# 06_quadrant_sensitivity.R
# Threshold sensitivity and the median-based comparison
# ==============================================================================
#
# READS:
#   thin_market_is8   (04) classified standards with starts and provider counts
#
# PRODUCES:
#   sens_starts          thin-market count as the starts threshold varies
#   sens_providers       thin-market count as the provider threshold varies
#   sens_grid            both thresholds varied together
#   median_quadrant      the median-based four-way split, for comparison
#   median_thresholds    the median values themselves
#
# DEPENDS ON: 00_config.R, 01_functions.R, 04_source_of_truth.R
#
# The thin-market definition rests on two chosen numbers: fewer than 50 starts
# and three or fewer providers. This script tests whether the headline finding
# depends on those particular values. The question is not whether the count
# changes - it obviously does - but whether it changes smoothly, and whether
# the same standards remain identified as most at risk.
# ==============================================================================

library(tidyverse)

source("00_config.R")
source("01_functions.R")


# --- SECTION 1: STARTS THRESHOLD ----------------------------------------------
# Provider threshold held at the primary value; starts threshold swept.

base <- thin_market_is8 %>%
  filter(!is.na(.data$providers)) %>%
  select(standard_ref, standard_name, starts, providers)

sens_starts <- tibble(starts_threshold = sens_starts_values) %>%
  mutate(
    standards = map_int(
      .data$starts_threshold,
      ~ sum(base$starts < .x & base$providers <= tier_thresholds$thin$providers)
    ),
    total_starts = map_dbl(
      .data$starts_threshold,
      ~ sum(base$starts[base$starts < .x & base$providers <= tier_thresholds$thin$providers])
    )
  ) %>%
  mutate(
    label         = paste0("<", .data$starts_threshold, " starts"),
    pct_standards = round(100 * .data$standards / nrow(base), 1),
    pct_starts    = round(100 * .data$total_starts / sum(base$starts), 2)
  )

print(sens_starts)


# --- SECTION 2: PROVIDER THRESHOLD --------------------------------------------
# Starts threshold held at the primary value; provider threshold swept.

sens_providers <- tibble(provider_threshold = sens_providers_values) %>%
  mutate(
    standards = map_int(
      .data$provider_threshold,
      ~ sum(base$starts < tier_thresholds$thin$starts & base$providers <= .x)
    )
  ) %>%
  mutate(
    label         = paste0("<=", .data$provider_threshold, " providers"),
    pct_standards = round(100 * .data$standards / nrow(base), 1)
  )

print(sens_providers)


# --- SECTION 3: BOTH TOGETHER -------------------------------------------------
# The full grid, so the interaction between the two thresholds is visible
# rather than inferred from two one-dimensional sweeps.

sens_grid <- expand_grid(
  starts_threshold   = sens_starts_values,
  provider_threshold = sens_providers_values
) %>%
  mutate(
    standards = map2_int(
      .data$starts_threshold, .data$provider_threshold,
      ~ sum(base$starts < .x & base$providers <= .y)
    ),
    pct_standards = round(100 * .data$standards / nrow(base), 1)
  )


# --- SECTION 4: STABILITY OF THE IDENTIFIED GROUP -----------------------------
# The count is sensitive to the threshold by construction. The more useful
# test is whether the standards identified at a tighter threshold remain
# identified at a looser one - that is, whether the thresholds pick out a
# stable group or reshuffle it.

standards_at <- function(starts_cut, provider_cut = tier_thresholds$thin$providers) {
  base$standard_ref[base$starts < starts_cut & base$providers <= provider_cut]
}

primary_set <- standards_at(tier_thresholds$thin$starts)

overlap_with_primary <- tibble(starts_threshold = sens_starts_values) %>%
  mutate(
    set          = map(.data$starts_threshold, standards_at),
    n_standards  = map_int(.data$set, length),
    in_primary   = map_int(.data$set, ~ sum(.x %in% primary_set)),
    pct_in_primary = round(100 * .data$in_primary / .data$n_standards, 1)
  ) %>%
  select(-set)

print(overlap_with_primary)


# --- SECTION 5: MEDIAN-BASED COMPARISON ---------------------------------------
# The alternative approach: split at the median on each axis. Reported for
# comparison only.
#
# A relative threshold moves as the market changes, so a standard can shift
# category without anything about it changing. Fixed thresholds are used for
# the classification precisely to avoid that.

median_thresholds <- tibble(
  median_starts    = median(base$starts, na.rm = TRUE),
  median_providers = median(base$providers, na.rm = TRUE)
)

print(median_thresholds)

median_quadrant <- base %>%
  mutate(
    starts_group   = if_else(.data$starts < median_thresholds$median_starts,
                             "Low starts", "High starts"),
    provider_group = if_else(.data$providers < median_thresholds$median_providers,
                             "Few providers", "Many providers"),
    quadrant       = paste(.data$starts_group, provider_group, sep = " + ")
  ) %>%
  count(.data$quadrant, name = "standards") %>%
  mutate(pct_standards = round(100 * .data$standards / sum(.data$standards), 1))

print(median_quadrant)


# --- SECTION 6: SAVE ----------------------------------------------------------

# Report Table 4 is the starts-threshold sensitivity table. Other outputs in
# this section are analytical audit tables rather than numbered report tables.
table_04_starts_threshold_sensitivity <- sens_starts

save_table(table_04_starts_threshold_sensitivity,
           "table_04_starts_threshold_sensitivity.csv")
save_table(sens_providers,       "sensitivity_provider_threshold.csv")
save_table(sens_grid,            "sensitivity_grid.csv")
save_table(overlap_with_primary, "sensitivity_group_stability.csv")
save_table(median_quadrant,      "median_quadrant.csv")

message("\n06_quadrant_sensitivity.R complete.")
