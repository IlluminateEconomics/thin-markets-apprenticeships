# ==============================================================================
# 02_load_data.R
# Read and standardise every data source, once
# ==============================================================================
#
# READS:
#   EES July 2026 release   - subject/standards, provider, learner, geography,
#                             historical  (see ees_files in 00_config.R)
#   Shiny provider dashboard- provider x standard, provider x region,
#                             provider x LAD   (2024/25, unsuppressed)
#   Skills England          - Apprenticeships.csv, standards catalogue
#   APAR                    - provider register
#   Boundaries              - LSIP / LAD / Region simplified sf objects
#   ONS                     - LAD to Combined Authority lookup
#
# PRODUCES (in the global environment, for downstream scripts):
#   ees_subject_standards   ees_routes_standards    ees_subject_detailed
#   ees_provider_starts     ees_learner_detailed    ees_geo_population
#   ees_geo_detailed        ees_historical
#   shiny_provider_standard shiny_provider_region   shiny_provider_lad
#   skills_england          apar
#   lsip_boundaries lad_boundaries region_boundaries lad_cauth_lookup
#
# DEPENDS ON: 00_config.R, 01_functions.R
#
# Nothing is filtered here beyond structural de-duplication. Analytical
# filtering belongs in the scripts that do the analysis.
# ==============================================================================

library(tidyverse)
library(sf)

source("00_config.R")
source("01_functions.R")


# --- SECTION 1: PRE-FLIGHT CHECK ----------------------------------------------
# Fail early and name every missing file, rather than erroring halfway through.

required_paths <- c(
  unlist(ees_files),
  unlist(shiny_files),
  skills_england_path,
  apar_path,
  lad_cauth_path,
  lsip_boundaries_path,
  lad_boundaries_path,
  region_boundaries_path
)

missing_paths <- required_paths[!file.exists(required_paths)]

if (length(missing_paths) > 0) {
  stop(
    "Missing required files:\n  ",
    paste(missing_paths, collapse = "\n  "),
    call. = FALSE
  )
}

message("All ", length(required_paths), " required files found.\n")


# --- SECTION 2: EES — SUBJECT AND STANDARDS -----------------------------------
# apps_17 covers 2019/20 to 2025/26 in a single file, all on the same sector
# subject area mapping, so no year-on-year combining is needed.
#
# Note the file's structure: it stacks several breakdown_topic blocks (Total,
# Age, and others) that each independently partition the same starts, and it
# also carries "Total" subtotal rows inside ssa_tier_1 and std_fwk_name_stcode.
# Summing without filtering will therefore multiply the true figure. Filtering
# is left to the analysis scripts; keep_total_breakdown() and drop_total_rows()
# in 01_functions.R handle both cases.

message("--- EES: subject and standards ---")

ees_subject_standards <- read_ees_csv(ees_files$subject_standards) %>%
  mutate(
    academic_year = make_academic_year(.data$time_period),
    standard_ref  = extract_standard_ref(.data$std_fwk_name_stcode),
    standard_name = strip_standard_ref(.data$std_fwk_name_stcode)
  )

ees_routes_standards <- read_ees_csv(ees_files$routes_standards) %>%
  mutate(
    academic_year = make_academic_year(.data$time_period),
    standard_ref  = extract_standard_ref(.data$std_fwk_name_stcode),
    standard_name = strip_standard_ref(.data$std_fwk_name_stcode)
  )

# Carries a ready-made `stem` column plus route and mandatory-degree flags.
# Prefer this over deriving STEM from SSA names.
ees_subject_detailed <- read_ees_csv(ees_files$subject_detailed) %>%
  mutate(academic_year = make_academic_year(.data$time_period))


# --- SECTION 3: EES — PROVIDER ------------------------------------------------
# Has a real provider_ukprn column, unlike the Shiny exports.

message("\n--- EES: provider ---")

ees_provider_starts <- read_ees_csv(ees_files$provider_starts) %>%
  mutate(
    academic_year   = make_academic_year(.data$time_period),
    apps_level_std  = normalise_apps_level(.data$apps_level)
  )


# --- SECTION 4: EES — LEARNER CHARACTERISTICS ---------------------------------
# Learning difficulty and disability is recorded in the column
# `learning_difficulty_disability`. The assertion below guards against the
# column being renamed in a future release, which would otherwise cause any
# filter on it to fail silently or error some way downstream.

message("\n--- EES: learner characteristics ---")

ees_learner_detailed <- read_ees_csv(ees_files$learner_detailed) %>%
  mutate(
    academic_year  = make_academic_year(.data$time_period),
    apps_level_std = normalise_apps_level(.data$apps_level)
  )

stopifnot(
  "learning_difficulty_disability column missing from learner file" =
    "learning_difficulty_disability" %in% names(ees_learner_detailed)
)


# --- SECTION 5: EES — GEOGRAPHY -----------------------------------------------
# Two geography files with different coverage:
#
#   app-geography-population : 2020/21 to 2025/26, all geographic levels,
#                              level and age, plus population and rates.
#                              NO subject breakdown.
#
#   apps_19_geography_detailed : carries ssa_tier_1, but is a single-year
#                              file covering the current year only.
#
# Regional and local authority analysis BY SUBJECT is therefore only available
# for the year of the current release. Where subject detail is needed for an
# earlier year, the underlying starts file
# (supporting-files/apps_u01_underlying_starts_*.zip) carries region, local
# authority district and sector subject area across all years in the series.
# See METHODOLOGY-NOTE.md, "Geographic coverage".

message("\n--- EES: geography ---")

ees_geo_population <- read_ees_csv(ees_files$geography_population) %>%
  mutate(
    academic_year  = make_academic_year(.data$time_period),
    apps_level_std = normalise_apps_level(.data$apps_level)
  )

ees_geo_detailed <- read_ees_csv(ees_files$geography_detailed) %>%
  mutate(
    academic_year  = make_academic_year(.data$time_period),
    apps_level_std = normalise_apps_level(.data$apps_level)
  )

geo_detailed_years <- sort(unique(ees_geo_detailed$time_period))

if (!ref_year %in% geo_detailed_years) {
  warning(
    "geography_detailed covers ", paste(geo_detailed_years, collapse = ", "),
    " and has no data for the reference year ", ref_year, ".\n",
    "  Regional and local authority analysis by subject must either use ",
    max(geo_detailed_years), ",\n",
    "  or be derived from the underlying starts file.\n",
    "  See METHODOLOGY-NOTE.md, 'Geographic coverage'.",
    call. = FALSE
  )
}


# --- SECTION 6: EES — HISTORICAL ----------------------------------------------
# 2002/03 to 2024/25. Rounded to the nearest 10, or 100 before 2014/15.
# Uses `z` (not applicable) in participation for the earliest years.

message("\n--- EES: historical ---")

ees_historical <- read_ees_csv(ees_files$historical_summary) %>%
  mutate(academic_year = make_academic_year(.data$time_period))


# --- SECTION 7: PROVIDER DASHBOARD --------------------------------------------
# Unrounded, unsuppressed, 2024/25 full year. Reconciles to the published
# total of 353,500 starts.
#
# The exports capitalise provider names inconsistently between files, so a
# join on name will not match. The UKPRN is extracted here and is the key
# used for all provider joins downstream.

message("\n--- Shiny provider dashboard ---")

shiny_provider_standard <- read_shiny_csv(shiny_files$provider_standard_starts) %>%
  rename(
    apps_level            = apps_Level,
    standard_name         = std_fwk_name,
    ssa_tier_1            = ssa_t1_desc,
    ssa_tier_2            = ssa_t2_desc,
    standard_or_framework = std_fwk_flag,
    starts                = values
  ) %>%
  # The suppression flag is named after the source column, so it is renamed
  # alongside it to keep the `<measure>_suppressed` convention intact.
  rename(any_of(c(starts_suppressed = "values_suppressed"))) %>%
  mutate(
    provider_ukprn = extract_ukprn(.data$provider_name),
    apps_level_std = normalise_apps_level(.data$apps_level)
  )

shiny_provider_region <- read_shiny_csv(shiny_files$provider_region) %>%
  rename(apps_level = apps_Level) %>%
  mutate(
    provider_ukprn = extract_ukprn(.data$provider_name),
    apps_level_std = normalise_apps_level(.data$apps_level)
  )

# Column names contain spaces in the source file.
shiny_provider_lad <- read_shiny_csv(shiny_files$provider_lad) %>%
  rename(
    learner_home_lad = `Learner home LAD`,
    delivery_lad     = `Delivery LAD`
  ) %>%
  mutate(provider_ukprn = extract_ukprn(.data$provider_name))


# --- SECTION 8: SKILLS ENGLAND ------------------------------------------------
# Row 1 of the CSV is a title line ("Apprenticeships accurate as of ..."), so
# skip = 1.
#
# One row per standard VERSION, not per standard. Deduplicate before joining
# or starts data will fan out.
#
# Status handling. Skills England distinguishes "Approved for delivery" from
# "Approved for delivery (paused for starts)". A substring match on the former
# captures both, which would count paused standards as open to new starts.
# Three explicit flags are derived instead.

message("\n--- Skills England ---")

skills_england <- read_csv(
  skills_england_path,
  skip = 1,
  show_col_types = FALSE,
  progress = FALSE
) %>%
  rename(
    programme_type             = `Programme Type`,
    standard_name              = Name,
    standard_ref               = Reference,
    version_number             = `Version Number`,
    proposal_approved          = `Proposal Approved`,
    standard_approved          = `Standard Approved`,
    assessment_plan_approved   = `Assessment Plan Approved`,
    funding_approved           = `Funding Approved`,
    status                     = Status,
    approved_for_delivery_date = `Approved for Delivery Date`,
    retired_date               = `Retired date`,
    withdrawn_date             = `Withdrawn date`,
    route                      = Route,
    level                      = Level,
    integrated_degree          = `Integrated Degree`,
    integration                = Integration,
    integrated_apprenticeship  = `Integrated Apprenticeship`,
    max_funding                = `Maximum Funding (£)`,
    typical_duration           = `Typical Duration`,
    min_hours_compliance       = `Minimum Hours For Compliance`,
    core_and_options           = `Core and options`,
    regulated_standard         = `Regulated Standard`,
    trailblazer_contact        = `Trailblazer Contact`,
    lars_code                  = `LARS code for providers only`,
    learning_aim_class_code    = `Learning Aim Class Code`,
    ssa2_code                  = `SSA2 code`,
    eqa_provider               = `EQA Provider`,
    professional_recognition   = `Professional recognition`,
    link                       = Link,
    last_updated               = `Last Updated`,
    job_titles                 = `Job Titles`,
    overview_of_role           = `Overview of role`,
    banners                    = Banners
  ) %>%
  mutate(
    across(c(standard_ref, standard_name, route, status, programme_type), str_trim),
    level                = suppressWarnings(as.numeric(.data$level)),
    max_funding          = suppressWarnings(as.numeric(.data$max_funding)),
    typical_duration     = suppressWarnings(as.numeric(.data$typical_duration)),
    min_hours_compliance = suppressWarnings(as.numeric(.data$min_hours_compliance)),

    # Three distinct states. "Approved for delivery" alone means genuinely
    # open to new starts; the paused variant does not.
    is_available_for_starts = .data$status == "Approved for delivery",
    is_paused               = str_detect(.data$status, fixed("paused")),
    is_approved             = str_detect(.data$status, fixed("Approved for delivery")),
    is_retired              = str_detect(.data$status, fixed("Retired")),
    is_withdrawn            = str_detect(.data$status, fixed("Withdrawn")),

    name_lower = str_to_lower(str_squish(.data$standard_name))
  )

# Standards only — excludes "Apprenticeship unit" (short modular units, max
# funding around £750, no duration) and Foundation apprenticeships.
skills_england_standards <- skills_england %>%
  filter(.data$programme_type == "Apprenticeship standard")

# One row per standard_ref: prefer available, then approved, then latest version.
skills_england_dedup <- skills_england_standards %>%
  filter(!is.na(.data$standard_ref)) %>%
  arrange(
    .data$standard_ref,
    desc(.data$is_available_for_starts),
    desc(.data$is_approved),
    desc(.data$version_number)
  ) %>%
  distinct(.data$standard_ref, .keep_all = TRUE)

assert_unique_key(skills_england_dedup, "standard_ref", "skills_england_dedup")

message(sprintf(
  "  %d standards total | %d approved for delivery | %d paused | %d retired",
  nrow(skills_england_dedup),
  sum(skills_england_dedup$is_available_for_starts, na.rm = TRUE),
  sum(skills_england_dedup$is_paused, na.rm = TRUE),
  sum(skills_england_dedup$is_retired, na.rm = TRUE)
))


# --- SECTION 9: APAR PROVIDER REGISTER ----------------------------------------
# A register of who is permitted to deliver, not activity data. Being on APAR
# does not imply any starts.

message("\n--- APAR ---")

apar <- read_csv(apar_path, show_col_types = FALSE, progress = FALSE) %>%
  rename(provider_ukprn = Ukprn, provider_name_apar = Name) %>%
  mutate(provider_ukprn = as.character(.data$provider_ukprn))


# --- SECTION 10: BOUNDARIES AND LOOKUPS ---------------------------------------
# Simplified sf objects. The full-resolution BFC shapefiles are only needed to
# regenerate these.

message("\n--- Boundaries and lookups ---")

lsip_boundaries   <- readRDS(lsip_boundaries_path)
lad_boundaries    <- readRDS(lad_boundaries_path)
region_boundaries <- readRDS(region_boundaries_path)

# ONS LAD to Combined Authority lookup. Excludes Greater London by design —
# the GLA is added back explicitly in the geography script.
lad_cauth_lookup <- read_csv(lad_cauth_path, show_col_types = FALSE, progress = FALSE)
names(lad_cauth_lookup) <- str_remove(names(lad_cauth_lookup), "^﻿")  # strip BOM

message(sprintf(
  "  LSIP %d | LAD %d | Region %d | CAUTH lookup %d rows",
  nrow(lsip_boundaries), nrow(lad_boundaries),
  nrow(region_boundaries), nrow(lad_cauth_lookup)
))


# --- SECTION 11: VALIDATION ---------------------------------------------------
# Reconcile against the published figure before anything downstream runs.
# Published final 2024/25: 353,500 starts (DfE, November 2025).

message("\n--- Validation ---")

published_starts_202425 <- 353500

# EES national total, all-learners block, no subtotals double-counted.
ees_check <- ees_subject_standards %>%
  filter(
    .data$time_period == ref_year,
    .data$geographic_level == "National",
    .data$breakdown_topic == "Total",
    .data$breakdown == "Total",
    .data$apps_level_detailed == "Total",
    .data$ssa_tier_1 == "Total",
    .data$std_fwk_name_stcode == "Total"
  )

check_total(ees_check, published_starts_202425, "starts")

# Shiny total — should be 353,503, i.e. the same figure unrounded.
check_total(shiny_provider_standard, published_starts_202425, "starts")

# The two Shiny files must agree with each other.
region_total <- sum(shiny_provider_region$starts, na.rm = TRUE)
standard_total <- sum(shiny_provider_standard$starts, na.rm = TRUE)

if (region_total != standard_total) {
  warning(
    "Shiny files disagree: provider_region ", format(region_total, big.mark = ","),
    " vs provider_standard ", format(standard_total, big.mark = ","),
    call. = FALSE
  )
} else {
  message("  Shiny files agree: ", format(standard_total, big.mark = ","), " starts")
}

# UKPRN must join cleanly across the Shiny files.
ukprn_standard <- unique(na.omit(shiny_provider_standard$provider_ukprn))
ukprn_region   <- unique(na.omit(shiny_provider_region$provider_ukprn))

message(sprintf(
  "  UKPRN overlap: %d common, %d standard-only, %d region-only",
  length(intersect(ukprn_standard, ukprn_region)),
  length(setdiff(ukprn_standard, ukprn_region)),
  length(setdiff(ukprn_region, ukprn_standard))
))

rm(ees_check, region_total, standard_total, ukprn_standard, ukprn_region)


message("\n02_load_data.R complete.")

