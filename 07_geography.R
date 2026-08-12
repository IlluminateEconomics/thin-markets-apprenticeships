# ==============================================================================
# 07_geography.R
# Geographic distribution of starts, and how it varies by market tier
# ==============================================================================
#
# READS:
#   geo_lad_standard    (03) local authority district x standard x year
#   geo_region_ssa      (03) region x sector subject area x year
#   geo_lad_ssa         (03) local authority district x subject x year
#   ees_geo_population  (02) starts and rates per 100,000, all geographic levels
#   thin_market_is8     (04) classified standards
#   lad_boundaries      (02) local authority district geometry
#   lad_cauth_lookup    (02) local authority district to combined authority
#
# PRODUCES:
#   lsip_rates              starts per 100,000 by LSIP area
#   lsip_variation          spread of rates, by level and by age
#   tier_lad_dispersion     how widely each tier is delivered
#   tier_dispersion_summary the headline dispersion measure by tier
#   region_subject          starts by region and subject
#   lad_priority_subjects   priority-subject starts by district
#   lad_tier_map_base       district-level totals joined to geometry
#
# DEPENDS ON: 00_config.R, 01_functions.R, 02_load_data.R,
#             03_prep_geography.R, 04_source_of_truth.R
#
# Delivery is measured directly from the underlying starts file rather than
# estimated by distributing each provider's total across the areas it operates
# in. A standard's geographic footprint is therefore observed, not inferred.
# ==============================================================================

library(tidyverse)
library(sf)

source("00_config.R")
source("01_functions.R")


# --- SECTION 1: LSIP RATES ----------------------------------------------------
# Local Skills Improvement Plan areas are the natural unit for questions about
# local coordination. Rates per 100,000 come from the published file, which
# supplies a consistent population denominator.

lsip_rates <- ees_geo_population %>%
  filter(
    .data$time_period == ref_year,
    .data$geographic_level == "Local skills improvement plan area",
    .data$apps_level == "Total",
    .data$age_summary == "Total",
    !is.na(.data$starts_rate_per_100000_population)
  ) %>%
  distinct(.data$lsip_code, .keep_all = TRUE) %>%
  select(lsip_code, lsip_name, starts, participation, achievements,
         population_estimate, starts_rate_per_100000_population)

message("LSIP areas: ", nrow(lsip_rates))

#' Spread of starts rates across areas, within a grouping
rate_variation <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(
      areas         = n_distinct(.data$lsip_code),
      median_rate   = median(.data$starts_rate_per_100000_population, na.rm = TRUE),
      min_rate      = min(.data$starts_rate_per_100000_population, na.rm = TRUE),
      max_rate      = max(.data$starts_rate_per_100000_population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(max_min_ratio = round(.data$max_rate / .data$min_rate, 2))
}

lsip_by_level <- ees_geo_population %>%
  filter(
    .data$time_period == ref_year,
    .data$geographic_level == "Local skills improvement plan area",
    .data$apps_level != "Total",
    .data$age_summary == "Total",
    !is.na(.data$starts_rate_per_100000_population)
  ) %>%
  distinct(.data$lsip_code, .data$apps_level, .keep_all = TRUE)

lsip_by_age <- ees_geo_population %>%
  filter(
    .data$time_period == ref_year,
    .data$geographic_level == "Local skills improvement plan area",
    .data$apps_level == "Total",
    .data$age_summary %in% c("Under 19", "19-24", "25+"),
    !is.na(.data$starts_rate_per_100000_population)
  ) %>%
  distinct(.data$lsip_code, .data$age_summary, .keep_all = TRUE)

lsip_variation <- bind_rows(
  rate_variation(lsip_by_level, .data$apps_level) %>%
    rename(category = apps_level) %>%
    mutate(dimension = "Level"),
  rate_variation(lsip_by_age, .data$age_summary) %>%
    rename(category = age_summary) %>%
    mutate(dimension = "Age")
) %>%
  select(dimension, category, everything()) %>%
  arrange(desc(.data$max_min_ratio))

print(lsip_variation)


# --- SECTION 2: DISPERSION BY MARKET TIER -------------------------------------
# How widely each standard is delivered, and whether that varies by tier.
#
# The claim under test is that thinner markets are more geographically
# concentrated. Because thin standards have few starts by definition, some
# concentration is mechanical - a standard with twelve starts cannot appear in
# many places. The measure is reported alongside starts so that the mechanical
# component is visible rather than hidden.

n_lads <- n_distinct(geo_lad_standard$geo_lad)

normalise_geography_standard_name <- function(x) {
  x %>%
    replace_na("") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

market_standard_links <- thin_market_is8 %>%
  mutate(name_key = normalise_geography_standard_name(.data$standard_name))

market_name_key_conflicts <- market_standard_links %>%
  count(.data$name_key, name = "standard_references") %>%
  filter(.data$standard_references > 1)

if (nrow(market_name_key_conflicts) > 0) {
  stop(
    "Normalised dashboard names map to more than one standard reference. ",
    "Inspect market_name_key_conflicts before geographic joins.",
    call. = FALSE
  )
}

geo_lad_standard_reference_year <- geo_lad_standard %>%
  filter(.data$academic_year %in% c(ref_year, ref_year_label, as.character(ref_year))) %>%
  mutate(name_key = normalise_geography_standard_name(.data$std_fwk_name))

geo_standard_name_variants <- geo_lad_standard_reference_year %>%
  distinct(.data$name_key, .data$std_fwk_name) %>%
  group_by(.data$name_key) %>%
  summarise(
    source_name_variants = n(),
    source_names = paste(sort(unique(.data$std_fwk_name)), collapse = "; "),
    .groups = "drop"
  )

standard_footprint <- geo_lad_standard_reference_year %>%
  group_by(.data$name_key) %>%
  summarise(
    districts      = n_distinct(.data$geo_lad),
    starts_in_area = sum(.data$starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_districts = round(100 * .data$districts / n_lads, 1))

tier_lad_dispersion <- market_standard_links %>%
  select(standard_ref, standard_name, name_key, tier, starts, providers) %>%
  left_join(standard_footprint, by = "name_key")

geography_standard_link_audit <- market_standard_links %>%
  select(standard_ref, standard_name, name_key, tier, starts, providers) %>%
  left_join(geo_standard_name_variants, by = "name_key") %>%
  mutate(name_matched = !is.na(.data$source_name_variants)) %>%
  arrange(.data$name_matched, .data$standard_name)

report_match(tier_lad_dispersion, "districts", "tier to geographic footprint")

tier_dispersion_summary <- tier_lad_dispersion %>%
  filter(!is.na(.data$districts)) %>%
  group_by(.data$tier) %>%
  summarise(
    standards           = n(),
    median_districts    = median(.data$districts),
    median_pct_districts = median(.data$pct_districts),
    pct_in_over_quarter = round(100 * mean(.data$pct_districts > 25), 1),
    median_starts       = median(.data$starts),
    .groups = "drop"
  )

print(tier_dispersion_summary)


# --- SECTION 2b: THE SAME MEASURE AT LSIP LEVEL -------------------------------
# Dispersion depends on how many areas exist. England has around 300 local
# authority districts but around 40 LSIP areas, so "present in more than a
# quarter of areas" is a far harder test at district level and the two are not
# comparable. Both are reported.
#
# The underlying data carries district names, not LSIP codes, so districts are
# assigned to LSIP areas by locating each district's centroid within an LSIP
# polygon. A district straddling a boundary is assigned to whichever area
# contains its centroid.

lsip_name_col <- names(lsip_boundaries)[
  str_detect(names(lsip_boundaries), regex("^LSIP[0-9]*NM$|lsip_name|LSIP_NAME", ignore_case = TRUE))
][1]

lad_to_lsip <- NULL

if (!is.na(lad_name_col <- names(lad_boundaries)[
  str_detect(names(lad_boundaries), regex("^LAD[0-9]*NM$|lad_name|LAD_NAME", ignore_case = TRUE))
][1]) && !is.na(lsip_name_col)) {

  lad_centroids <- lad_boundaries %>%
    st_transform(st_crs(lsip_boundaries))

  # Apply point-on-surface to the geometry vector only. Calling the sf method
  # emits an attribute-constancy warning even though no attributes are changed.
  st_geometry(lad_centroids) <- st_point_on_surface(st_geometry(lad_centroids))

  lad_to_lsip <- st_join(
    lad_centroids,
    select(lsip_boundaries, lsip_name = all_of(lsip_name_col)),
    join = st_within
  ) %>%
    st_drop_geometry() %>%
    transmute(
      lad_name  = .data[[lad_name_col]],
      lsip_name = .data$lsip_name
  ) %>%
    filter(!is.na(.data$lsip_name))

  lad_to_lsip_conflicts <- lad_to_lsip %>%
    count(.data$lad_name, name = "lsip_matches") %>%
    filter(.data$lsip_matches > 1)

  if (nrow(lad_to_lsip_conflicts) > 0) {
    stop(
      "One or more districts were assigned to multiple LSIP areas. ",
      "Inspect lad_to_lsip_conflicts before continuing.",
      call. = FALSE
    )
  }

  message("\n  Districts assigned to an LSIP area: ", nrow(lad_to_lsip),
          " of ", nrow(lad_boundaries))
  message("  LSIP areas covered: ", n_distinct(lad_to_lsip$lsip_name))
}

if (!is.null(lad_to_lsip)) {

  n_lsips <- n_distinct(lad_to_lsip$lsip_name)

  standard_lsip_footprint <- geo_lad_standard_reference_year %>%
    inner_join(lad_to_lsip, by = c("geo_lad" = "lad_name")) %>%
    group_by(.data$name_key) %>%
    summarise(lsips = n_distinct(.data$lsip_name), .groups = "drop") %>%
    mutate(pct_lsips = round(100 * .data$lsips / n_lsips, 1))

  tier_lsip_dispersion <- market_standard_links %>%
    select(standard_ref, standard_name, name_key, tier, starts) %>%
    left_join(standard_lsip_footprint, by = "name_key")

  tier_lsip_summary <- tier_lsip_dispersion %>%
    filter(!is.na(.data$lsips)) %>%
    group_by(.data$tier) %>%
    summarise(
      standards            = n(),
      median_lsips         = median(.data$lsips),
      median_pct_lsips     = median(.data$pct_lsips),
      pct_in_over_quarter  = round(100 * mean(.data$pct_lsips > 25), 1),
      median_starts        = median(.data$starts),
      .groups = "drop"
    )

  message("\n  Dispersion by tier, LSIP basis (comparable with the published figures):")
  print(tier_lsip_summary)

  save_table(tier_lsip_summary, "tier_dispersion_summary_lsip.csv")
  save_table(lad_to_lsip,       "lad_to_lsip_lookup.csv")

} else {
  warning(
    "Could not build the district-to-LSIP lookup; dispersion is reported at ",
    "district level only. Check the name columns in lad_boundaries and ",
    "lsip_boundaries.",
    call. = FALSE
  )
}


# --- SECTION 2c: THE SAME MEASURE AT STRATEGIC AUTHORITY LEVEL ----------------
# Mayoral strategic authorities are the third geography of interest, since
# they are the bodies most likely to convene employers locally.
#
# Two features of this geography differ from the others and constrain what can
# be said. Coverage is partial: only districts inside a combined authority
# appear, so a standard delivered entirely outside them registers as reaching
# no strategic authority at all. And Greater London is absent from the ONS
# lookup, so the London boroughs are added here as a single authority.
#
# Reach is therefore reported as a share of authorities covered, alongside the
# proportion of each tier's delivery that falls outside any authority.

lad_code_col <- names(lad_boundaries)[
  str_detect(names(lad_boundaries), regex("^LAD[0-9]*CD$|lad_code", ignore_case = TRUE))
][1]

msa_lookup <- lad_cauth_lookup %>%
  transmute(
    lad_name = .data$LAD25NM,
    msa_name = .data$CAUTH25NM
  )

if (!is.na(lad_code_col)) {
  london <- lad_boundaries %>%
    st_drop_geometry() %>%
    filter(str_starts(.data[[lad_code_col]], "E09")) %>%
    transmute(lad_name = .data[[lad_name_col]], msa_name = "Greater London")

  msa_lookup <- bind_rows(msa_lookup, london) %>% distinct()
}

n_msas <- n_distinct(msa_lookup$msa_name)

message("\n  Districts inside a strategic authority: ", nrow(msa_lookup),
        " of ", nrow(lad_boundaries),
        " (", n_msas, " authorities, including Greater London)")

standard_msa_footprint <- geo_lad_standard_reference_year %>%
  left_join(msa_lookup, by = c("geo_lad" = "lad_name")) %>%
  group_by(.data$name_key) %>%
  summarise(
    msas            = n_distinct(.data$msa_name[!is.na(.data$msa_name)]),
    starts_in_msa   = sum(.data$starts[!is.na(.data$msa_name)], na.rm = TRUE),
    starts_total    = sum(.data$starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_msas          = round(100 * .data$msas / n_msas, 1),
    pct_starts_in_msa = round(100 * .data$starts_in_msa / .data$starts_total, 1)
  )

tier_msa_summary <- market_standard_links %>%
  select(standard_ref, standard_name, name_key, tier, starts, providers) %>%
  left_join(standard_msa_footprint, by = "name_key") %>%
  filter(!is.na(.data$msas)) %>%
  group_by(.data$tier) %>%
  summarise(
    standards            = n(),
    median_msas          = median(.data$msas),
    median_pct_msas      = median(.data$pct_msas),
    pct_in_over_quarter  = round(100 * mean(.data$pct_msas > 25), 1),
    median_pct_starts_in_msa = median(.data$pct_starts_in_msa),
    median_providers     = median(.data$providers),
    .groups = "drop"
  )

message("\n  Dispersion by tier, strategic authority basis:")
print(tier_msa_summary)


# --- SECTION 2d: REACH RELATIVE TO PROVIDER COUNT -----------------------------
# Geographic reach and provider count are not independent. Delivery is
# recorded against a delivery postcode, so a standard cannot appear in more
# places than its providers operate in, and a tier defined partly by provider
# count will differ in reach almost by construction.
#
# The question that is not circular is whether each provider's reach differs
# between tiers: do the providers serving thin markets cover narrower
# geographies than those serving scaled ones, or the same? Areas per provider
# isolates that.

reach_per_provider <- market_standard_links %>%
  select(standard_ref, standard_name, name_key, tier, starts, providers) %>%
  left_join(
    select(standard_lsip_footprint, name_key, lsips),
    by = "name_key"
  ) %>%
  filter(!is.na(.data$lsips), .data$providers > 0) %>%
  mutate(areas_per_provider = .data$lsips / .data$providers) %>%
  group_by(.data$tier) %>%
  summarise(
    standards               = n(),
    median_providers        = median(.data$providers),
    median_lsips            = median(.data$lsips),
    median_areas_per_provider = round(median(.data$areas_per_provider), 2),
    .groups = "drop"
  )

message("\n  Geographic reach per provider, by tier:")
print(reach_per_provider)


# --- SECTION 3: SUBJECT BY PLACE ----------------------------------------------

region_subject <- geo_region_ssa %>%
  filter(.data$academic_year %in% c(ref_year, ref_year_label, as.character(ref_year))) %>%
  group_by(.data$geo_region) %>%
  mutate(region_total = sum(.data$starts, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct_of_region = round(100 * .data$starts / .data$region_total, 1)) %>%
  arrange(.data$geo_region, desc(.data$starts))

lad_priority_subjects <- geo_lad_ssa %>%
  filter(
    .data$academic_year %in% c(ref_year, ref_year_label, as.character(ref_year)),
    .data$ssa_tier_1 %in% priority_ssa_tier_1
  ) %>%
  group_by(.data$geo_lad) %>%
  summarise(
    priority_starts  = sum(.data$starts, na.rm = TRUE),
    suppressed_cells = sum(.data$suppressed_cells, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(.data$priority_starts))

region_priority_concentration <- geo_region_ssa %>%
  filter(
    .data$academic_year %in% c(ref_year, ref_year_label, as.character(ref_year)),
    .data$ssa_tier_1 %in% priority_ssa_tier_1
  ) %>%
  group_by(.data$geo_region) %>%
  summarise(priority_starts = sum(.data$starts, na.rm = TRUE), .groups = "drop") %>%
  geo_concentration(.data$geo_region, .data$priority_starts)

print(region_priority_concentration)


# --- SECTION 4: MAP BASE ------------------------------------------------------
# District geometry joined to district totals. The join is on name, since the
# underlying file supplies district names rather than codes; unmatched
# districts are reported so silent gaps in a map are not mistaken for zeros.

if (is.na(lad_name_col)) {
  warning(
    "Could not identify the district name column in lad_boundaries. ",
    "Columns available: ", paste(names(lad_boundaries), collapse = ", "),
    call. = FALSE
  )
} else {
  message("  Joining boundaries on: ", lad_name_col)

  lad_tier_map_base <- lad_boundaries %>%
    mutate(join_name = .data[[lad_name_col]]) %>%
    left_join(
      lad_priority_subjects %>% rename(join_name = geo_lad),
      by = "join_name"
    )

  unmatched_lads <- setdiff(lad_priority_subjects$geo_lad, lad_tier_map_base$join_name)

  message("  Districts in data with no boundary match: ", length(unmatched_lads))
  if (length(unmatched_lads) > 0) {
    message("    ", paste(head(unmatched_lads, 10), collapse = ", "))
  }
}


# --- SECTION 5: SAVE ----------------------------------------------------------

save_table(tier_msa_summary,        "tier_dispersion_summary_msa.csv")
save_table(reach_per_provider,      "reach_per_provider.csv")
save_table(msa_lookup,              "lad_to_msa_lookup.csv")
save_table(lsip_rates,              "lsip_rates.csv")
save_table(lsip_variation,          "lsip_variation.csv")
save_table(tier_dispersion_summary, "tier_dispersion_summary.csv")
save_table(tier_lad_dispersion,     "tier_lad_dispersion.csv")
save_table(geography_standard_link_audit, "geography_standard_link_audit.csv")
save_table(region_subject,          "region_subject.csv")
save_table(lad_priority_subjects,   "lad_priority_subjects.csv")

message("\n07_geography.R complete.")
