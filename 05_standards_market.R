# ==============================================================================
# 05_standards_market.R
# Size distribution, concentration and the long tail over time
# ==============================================================================
#
# READS:
#   ees_subject_standards   (02) multi-year standards series
#   ees_provider_starts     (02) provider-level starts
#   historical_standard_starts_exact (03) exact standard-year starts
#   thin_market_is8         (04) classified standards, reference year
#
# PRODUCES:
#   standards_distribution      standards by starts band, reference year
#   standards_concentration     cumulative share of starts by rank
#   standards_delivered_trend   exact standards with at least one start by year
#   low_start_trend             exact low-start counts and shares by year
#   historical_concentration_trend
#                               exact top-100 share through time
#   low_start_by_subject        low-or-zero cells by subject, explicitly not a
#                               count of active low-start standards
#   provider_concentration      cumulative share of starts by provider rank
#
# DEPENDS ON: 00_config.R, 01_functions.R, 02_load_data.R,
#             03_prep_geography.R, 04_source_of_truth.R
#
# The reference-year figures use the dashboard-derived classification from 04.
# Historical activity, low-start and concentration trends use the unsuppressed
# underlying starts archives aggregated in 03. The public table-tool extract is
# retained only for the explicitly labelled subject-level source diagnostic.
# ==============================================================================

library(tidyverse)

source("00_config.R")
source("01_functions.R")


# --- SECTION 1: SIZE DISTRIBUTION, REFERENCE YEAR -----------------------------
# How many standards fall in each size band. The shape of this distribution is
# the starting point for the thin-market argument: a long tail of very small
# standards alongside a small number of very large ones.

standards_distribution <- thin_market_is8 %>%
  mutate(starts_band = band_starts(.data$starts)) %>%
  count(.data$starts_band, name = "standards") %>%
  mutate(
    pct_standards = round(100 * .data$standards / sum(.data$standards), 1),
    cumulative_pct = cumsum(.data$pct_standards)
  )

print(standards_distribution)


# --- SECTION 2: CONCENTRATION OF STARTS ---------------------------------------
# What share of all starts the largest standards account for. Reported at
# fixed ranks so the figure is directly quotable.

standards_ranked <- thin_market_is8 %>%
  filter(.data$starts > 0) %>%
  arrange(desc(.data$starts)) %>%
  mutate(
    rank             = row_number(),
    cumulative_starts = cumsum(.data$starts),
    cumulative_share  = round(100 * .data$cumulative_starts / sum(.data$starts), 1)
  )

standards_concentration <- standards_ranked %>%
  filter(.data$rank %in% c(10, 25, 50, 100, 200)) %>%
  select(top_n_standards = rank, cumulative_starts, cumulative_share)

print(standards_concentration)


# --- SECTION 3: EXACT DELIVERED STANDARDS, BY YEAR ----------------------------
# A delivered standard has at least one recorded start in the academic year.
# The underlying files are unsuppressed, so this is an exact activity count,
# not a publication-threshold proxy and not the stock of approved standards.

standards_series <- ees_subject_standards %>%
  keep_total_breakdown() %>%
  filter(
    .data$geographic_level == "National",
    .data$apps_level_detailed == "Total",
    .data$ssa_tier_1 != "Total",
    .data$std_fwk_name_stcode != "Total",
    !is.na(.data$standard_ref)
  )

standards_delivered_trend <- historical_standard_starts_exact %>%
  group_by(.data$time_period, .data$academic_year) %>%
  summarise(
    standards_with_starts = n_distinct(.data$standard_ref),
    total_standard_starts = sum(.data$starts),
    .groups = "drop"
  ) %>%
  arrange(.data$time_period)

print(standards_delivered_trend)


# --- SECTION 4: THE LONG TAIL OVER TIME ---------------------------------------
# Exact annual count and share of delivered standards below 50 starts.

low_start_trend <- historical_standard_starts_exact %>%
  group_by(.data$time_period, .data$academic_year) %>%
  summarise(
    standards_with_starts = n_distinct(.data$standard_ref),
    standards_below_50 = sum(.data$starts < low_start_cutoff),
    total_standard_starts = sum(.data$starts),
    .groups = "drop"
  ) %>%
  mutate(
    share_below_50 = round(
      100 * .data$standards_below_50 / .data$standards_with_starts,
      1
    )
  ) %>%
  arrange(.data$time_period)

message("\n  Exact delivered-standard and low-start estimates, by year:")
low_start_trend %>%
  select(
    academic_year,
    standards_with_starts,
    standards_below_50,
    share_below_50
  ) %>%
  print(n = Inf)

print(low_start_trend)


# --- SECTION 5: CONCENTRATION OVER TIME ---------------------------------------
# Exact share of standard-based starts accounted for by the 100 largest
# delivered standards in each year.

historical_ranked <- historical_standard_starts_exact %>%
  group_by(.data$time_period, .data$academic_year) %>%
  arrange(desc(.data$starts), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

historical_concentration_trend <- historical_ranked %>%
  group_by(.data$time_period, .data$academic_year) %>%
  summarise(
    standards_with_starts = n(),
    top_n_standards = min(100L, n()),
    top_n_starts = sum(.data$starts[.data$rank <= 100L]),
    total_standard_starts = sum(.data$starts),
    .groups = "drop"
  ) %>%
  mutate(
    top_n_share = round(
      100 * .data$top_n_starts / .data$total_standard_starts,
      1
    )
  ) %>%
  arrange(.data$time_period)

print(historical_concentration_trend)


# --- SECTION 6: LOW-OR-ZERO CELLS BY SUBJECT ----------------------------------
# This is a source diagnostic only. The published `low` token combines zero and
# 1-4 starts, so these counts cannot be interpreted as active low-start
# standards. The reference-year dashboard analysis is the valid source for a
# genuine low-start subject composition.

low_start_by_subject <- standards_series %>%
  group_by(.data$time_period, .data$academic_year, .data$ssa_tier_1, .data$standard_ref) %>%
  summarise(
    starts = sum(.data$starts, na.rm = TRUE),
    is_low = any(.data$starts_low),
    .groups = "drop"
  ) %>%
  filter(.data$is_low | (.data$starts > 0 & .data$starts < low_start_cutoff)) %>%
  count(.data$time_period, .data$academic_year, .data$ssa_tier_1,
        name = "low_or_zero_standards") %>%
  group_by(.data$time_period) %>%
  mutate(
    pct_of_low_or_zero = round(
      100 * .data$low_or_zero_standards / sum(.data$low_or_zero_standards),
      1
    ),
    interpretation = paste(
      "Not an active low-start count:",
      "the source's low token combines zero with 1-4 starts."
    )
  ) %>%
  ungroup() %>%
  arrange(.data$time_period, desc(.data$low_or_zero_standards))


# --- SECTION 7: PROVIDER CONCENTRATION ----------------------------------------
# The equivalent concentration measure on the supply side: what share of starts
# the largest providers account for.

provider_ranked <- ees_provider_starts %>%
  filter(
    .data$time_period == ref_year,
    .data$apps_level == "Total",
    .data$ssa_tier_1 == "Total",
    !is.na(.data$starts),
    .data$starts > 0
  ) %>%
  distinct(.data$provider_ukprn, .keep_all = TRUE) %>%
  arrange(desc(.data$starts)) %>%
  mutate(
    rank             = row_number(),
    cumulative_share = round(100 * cumsum(.data$starts) / sum(.data$starts), 1)
  )

provider_concentration <- provider_ranked %>%
  filter(.data$rank %in% c(10, 25, 50, 100)) %>%
  select(top_n_providers = rank, cumulative_share)

message("\n  Providers with starts in ", ref_year_label, ": ", nrow(provider_ranked))
print(provider_concentration)


# --- SECTION 8: SAVE ----------------------------------------------------------

save_table(standards_distribution,    "standards_distribution.csv")
save_table(standards_concentration,   "standards_concentration.csv")
save_table(standards_delivered_trend, "standards_delivered_trend.csv")
save_table(low_start_trend,           "low_start_trend.csv")
save_table(historical_concentration_trend, "historical_concentration_trend.csv")
save_table(low_start_by_subject,      "low_start_by_subject.csv")
save_table(provider_concentration,    "provider_concentration.csv")

message("\n05_standards_market.R complete.")
