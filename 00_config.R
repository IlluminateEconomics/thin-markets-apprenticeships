# ==============================================================================
# 00_config.R
# Apprenticeship thin-market analysis — shared configuration
# ==============================================================================
#
# READS:    nothing
# PRODUCES: path objects, analysis parameters, plot theme constants
#
# Source this first in every script:  source("00_config.R")
#
# Everything that a future refresh might need to change lives in this file.
# If you find yourself hardcoding a year, a threshold or a filename anywhere
# else, it belongs here instead.
# ==============================================================================


# --- PROJECT ROOT -------------------------------------------------------------
# Path to the folder containing "Raw Data", relative to this scripts folder.
# Run scripts with the working directory set to the folder they live in.
#
# This is the only path that changes if the scripts are moved: use ".." if the
# scripts folder sits directly inside the project, "../.." if it is one level
# deeper.

project_root <- "../.."

# The Drive R Scripts folder sits directly beside Raw Data. Preserve an
# existing valid root (including the older Scripts-Clean layout), otherwise
# accept only these nearby layouts rather than requiring a manual path edit.
if (!dir.exists(file.path(project_root, "Raw Data"))) {
  nearby_roots <- c("..", ".")
  nearby_roots <- nearby_roots[dir.exists(file.path(nearby_roots, "Raw Data"))]
  if (length(nearby_roots) > 0) project_root <- nearby_roots[[1]]
}

if (!dir.exists(file.path(project_root, "Raw Data"))) {
  stop(
    "No 'Raw Data' folder found at: ",
    normalizePath(project_root, winslash = "/", mustWork = FALSE),
    "\n  Set project_root in 00_config.R, or set the working directory to the ",
    "folder containing these scripts.",
    call. = FALSE
  )
}

raw_data_folder <- file.path(project_root, "Raw Data")
ees_folder      <- file.path(raw_data_folder, "apprenticeships_2025-26", "data")
ees_support     <- file.path(raw_data_folder, "apprenticeships_2025-26", "supporting-files")

output_folder       <- file.path(project_root, "Outputs")
chart_folder        <- file.path(output_folder, "charts")
table_folder        <- file.path(output_folder, "tables")

# Aggregates that are expensive to rebuild are cached here. The cache is
# derived data: safe to delete, and rebuilt automatically if the source file
# is newer than the cache.
derived_folder      <- file.path(output_folder, "derived")

for (d in c(output_folder, chart_folder, table_folder, derived_folder)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
rm(d)


# --- DATA SOURCES: EES July 2026 release --------------------------------------
# Department for Education, "Apprenticeships", academic year 2025/26,
# published 16 July 2026 on Explore Education Statistics.
#
# Filename convention: apps_<table number>_<topic>_<academic year>_<snapshot>
#   _10 = snapshot SN10, covering August to April, published each July
#   _14 = snapshot SN14, the final full academic year, published each November
#
# Sector subject area codes for 139 standards were updated in December 2025.
# Files published from January 2026 onwards use the revised mapping and carry
# a recalculated five-year back series. Earlier downloads use the superseded
# mapping and are not comparable at subject level.
# See METHODOLOGY-NOTE.md, "Sector subject area".

ees_files <- list(
  # Subject and standard detail — the workhorse for standards analysis
  subject_standards   = file.path(ees_folder, "apps_17_subject_standards_202526_10.csv"),
  routes_standards    = file.path(ees_folder, "apps_17a_routes_standards_202526_10.csv"),
  standards_age22     = file.path(ees_folder, "apps_17b_subject_standards_age22_202526_10.csv"),

  # Carries a ready-made `stem` column — use this rather than deriving STEM
  subject_detailed    = file.path(ees_folder, "apps_14_subject_detailed_202526_10.csv"),
  subject_demographic = file.path(ees_folder, "apps_16_subject_demographic_202526_10.csv"),

  # Provider
  provider_starts     = file.path(ees_folder, "apps_23_provider_starts_202526_10.csv"),

  # Learner characteristics
  learner_detailed    = file.path(ees_folder, "apps_09_learner_detailed_202526_10.csv"),

  # Geography
  geography_detailed  = file.path(ees_folder, "apps_19_geography_detailed_202526_10.csv"),
  geography_pcon_lad  = file.path(ees_folder, "apps_21_geography_pcon_lad_202526_10.csv"),
  geography_population= file.path(ees_folder, "app-geography-population-202526-q3.csv"),

  # Headline and historical
  headline_full_year  = file.path(ees_folder, "apps_55_latest_summary_full_year_202526_10.csv"),
  historical_summary  = file.path(ees_folder, "app-historical-summary-to-2425.csv")
)


# --- DATA SOURCES: provider dashboard -----------------------------------------
# Department for Education apprenticeships provider dashboard (R Shiny),
# downloaded 7 June 2026. An official part of the EES release that allows
# direct download of the user's current selection.
#
# These exports are unrounded and carry no small-number suppression, so counts
# of 1 and 2 are real values rather than artefacts. They cover the 2024/25
# academic year only, and reconcile to the published total of 353,500 starts.
# See METHODOLOGY-NOTE.md, "Data sources".

shiny_files <- list(
  provider_standard_starts = file.path(raw_data_folder, "2024_25-starts---subjects-and-standards.csv"),
  provider_region          = file.path(raw_data_folder, "2024_25-alllevels-allagegroups-provider_breakdowns.csv"),
  provider_lad             = file.path(raw_data_folder, "lad-2024_25.csv")
)


# --- DATA SOURCES: external ---------------------------------------------------

skills_england_path <- file.path(raw_data_folder, "Apprenticeships.csv")
apar_path           <- file.path(raw_data_folder, "apar-2026-05-28-02-01-07.csv")
lad_cauth_path      <- file.path(
  raw_data_folder,
  "Local_Authority_District_to_Combined_Authority_(May_2025)_Lookup_in_EN.csv"
)

# Simplified boundaries. These are what the mapping code reads — the
# full-resolution BFC shapefiles are only needed to regenerate them.
lsip_boundaries_path   <- file.path(raw_data_folder, "lsip_boundaries.rds")
lad_boundaries_path    <- file.path(raw_data_folder, "lad_boundaries.rds")
region_boundaries_path <- file.path(raw_data_folder, "region_boundaries.rds")

# Underlying starts files, from the releases' supporting files. The current
# archive carries the recalculated series from 2020/21 onwards; the 2019/20
# archive supplies 2017/18 to 2019/20. Together they provide exact, unsuppressed
# standard-level starts for the full historical charts used in the report.
# The older archive has had more than one folder name in downloaded projects,
# so 03_prep_geography.R resolves it from these candidates, then falls back to
# a recursive filename search under Raw Data.
underlying_starts_zip <- file.path(
  ees_support, "apps_u01_underlying_starts_202526_10.zip"
)

underlying_starts_historical_candidates <- c(
  file.path(raw_data_folder, "underlying_starts_201920.zip"),
  file.path(
    raw_data_folder,
    "apprenticeships_2019-20",
    "supporting-files",
    "underlying_starts_201920.zip"
  ),
  file.path(
    raw_data_folder,
    "apprenticeships_2019-20",
    "supporting-files",
    "underlying-data-apps-start-1920.zip"
  )
)


# --- STANDARD NAME ALIASES ----------------------------------------------------
# The provider dashboard identifies standards by name only, with no reference
# code, so standards are linked to the Skills England catalogue on a
# normalised name. A small number of names differ between the two sources and
# would otherwise be dropped from the analysis entirely.
#
# Each alias maps a dashboard name to the catalogue reference it corresponds
# to. Frameworks are deliberately absent: they have no standard reference by
# definition, and are correctly excluded.
#
# Add to this table if the unmatched list in Outputs/tables grows.

standard_name_aliases <- tibble::tribble(
  ~dashboard_name,                              ~standard_ref, ~catalogue_name,                 ~note,

  "Dental Nurse (GDC 2023)",                    "ST1431",      "Dental nurse",
  "Dashboard appends the GDC 2023 revision; the catalogue does not.",

  "Orthodontic Therapist (GDC 2023)",           "ST1434",      "Orthodontic therapist",
  "As above.",

  "Ecologist (Degree)",                         "ST0577",      "Ecologist (integrated degree)",
  "Catalogue uses 'integrated degree' where the dashboard uses 'Degree'.",

  "Teacher",                                    "ST0490",      "Teacher - Postgraduate",
  paste(
    "The catalogue now splits teaching into Postgraduate (ST0490) and",
    "Undergraduate (ST1502). The dashboard reports a single unsuffixed",
    "'Teacher'. Mapped to the postgraduate route, which is the established",
    "one; the undergraduate standard is at version 1 and newly approved."
  ),

  "Senior Financial Services Customer Adviser", "ST0185",      "Financial services customer adviser",
  paste(
    "The catalogue lists ST0178 and ST0185 under the same name. ST0185 is",
    "the senior variant. Worth confirming if this standard becomes material."
  ),

  "Financial Services Customer Adviser",        "ST0178",      "Financial services customer adviser",
  paste(
    "The dashboard's level 2 title is the retired ST0178 standard. The",
    "catalogue also uses this title for the distinct level 3 ST0185 standard."
  ),

  "Journalist",                                  "ST1324",      "Journalist",
  paste(
    "The 2024/25 dashboard activity predates ST1490, approved in 2026.",
    "It therefore belongs to the retired level 5 standard ST1324."
  ),

  "Junior Journalist",                           "ST0285",      "Junior journalist",
  paste(
    "The dashboard extract is for 2024/25 and therefore predates ST1516,",
    "approved in 2026. Any activity belongs to the retired ST0285 standard."
  )
)


# --- GEOGRAPHIC BASIS ---------------------------------------------------------
# Apprenticeship geography can be measured two ways, and they give different
# answers:
#
#   "delivery"      where the training was delivered, from the provider's
#                   delivery postcode
#   "learner_home"  where the apprentice lives, from their home postcode
#
# Delivery is used here because the analysis is about where provision exists
# and how exposed each area is if it withdraws. Set to "learner_home" to
# switch every geographic cut to a residence basis.

geo_basis <- "delivery"

stopifnot(geo_basis %in% c("delivery", "learner_home"))


# --- REFERENCE YEARS ----------------------------------------------------------
# Change these two lines at each data refresh; nothing else should hardcode
# a year. Chart subtitles build from `ref_year_label`.

ref_year          <- 202425          # numeric time_period for the main analysis
ref_year_label    <- "2024/25"       # display form, used in every chart subtitle
latest_inyear     <- 202526          # provisional, Aug-Apr only
latest_inyear_lab <- "2025/26 (Aug-Apr, provisional)"

# Historical chart window. The year before the levy provides a clean baseline;
# 2017/18 is the first full academic year after the levy began in April 2017.
chart_trend_start_year <- 201516
levy_first_full_year   <- 201718

# Skills England snapshot date, for footnoting
skills_england_asat <- "1 June 2026"


# --- THIN-MARKET TIER THRESHOLDS ----------------------------------------------
# A standard is classified on two axes: annual starts, and the number of
# distinct providers delivering it. Both come from the provider dashboard
# export, so both are measured on the same population in the same year.
#
# Thresholds are fixed rather than derived from medians, so the tiers mean the
# same thing from one year to the next. They are analytical choices, not
# official DfE definitions. Sensitivity to both thresholds is tested in
# 06_quadrant.R.
#
# Defined once here so that the tier logic and the sensitivity sweeps cannot
# drift apart.

tier_thresholds <- list(
  severe            = list(starts = 20,  providers = 2),
  thin              = list(starts = 50,  providers = 3),
  potential         = list(starts = 100, providers = 5),
  provider_dependency = list(starts = 250, providers = 3),  # >= starts, <= providers
  low_volume        = list(starts = 250)
)

tier_levels <- c(
  "Severe thin market",
  "Thin market",
  "Potential thin market",
  "Low-volume review group",
  "Provider dependency market",
  "Scaled market"
)

# Simple low-start cutoff used for the standards distribution work.
# Analytical, not an official DfE definition.
low_start_cutoff <- 50

# Sensitivity sweep ranges
sens_starts_values    <- c(20, 50, 75, 100, 150, 200)
sens_providers_values <- c(1, 2, 3, 4, 5, 7, 10)


# --- SUBJECT AND ROUTE GROUPINGS ----------------------------------------------

# STEM as defined by DfE: "STEM apprenticeships include those in the
# Construction, Planning and the Built Environment, Digital Technology,
# Engineering and Manufacturing Technologies, and Science and Mathematics
# sector subject areas."  (EES data guidance, July 2026.)
#
# Digital Technology was previously labelled "Information and Communication
# Technology". Several EES files carry a ready-made `stem` column; prefer that
# over matching on subject names.
stem_ssa_tier_1 <- c(
  "Construction, Planning and the Built Environment",
  "Digital Technology",
  "Engineering and Manufacturing Technologies",
  "Science and Mathematics"
)

# Technical-priority subjects for this project. Wider than STEM — adds health.
priority_ssa_tier_1 <- c(
  "Engineering and Manufacturing Technologies",
  "Construction, Planning and the Built Environment",
  "Digital Technology",
  "Science and Mathematics",
  "Health, Public Services and Care"
)

# Skills England routes treated as priority. These are route names, a different
# taxonomy from SSA — the two do not nest.
priority_routes <- c(
  "Engineering and manufacturing",
  "Construction and the built environment",
  "Health and science",
  "Digital",
  "Transport and logistics",
  "Agriculture, environmental and animal care"
)


# --- STARTS BANDS -------------------------------------------------------------

starts_band_breaks <- c(-Inf, 50, 100, 250, 500, 1000, 2500, 5000, Inf)
starts_band_labels <- c(
  "<50", "50-99", "100-249", "250-499",
  "500-999", "1,000-2,499", "2,500-4,999", "5,000+"
)

estimated_starts_band_breaks <- c(-Inf, 50, 100, 250, 500, Inf)
estimated_starts_band_labels <- c(
  "<50 starts", "50-99 starts", "100-249 starts", "250-499 starts", "500+ starts"
)


# --- SUPPRESSION TOKENS -------------------------------------------------------
# EES writes one of four text tokens in place of a number:
#
#   low  volume columns: a raw count of 0-4, suppressed for confidentiality
#        percentage columns: a value below 0.5%
#   c    suppressed
#   x    unavailable
#   z    not applicable
#
# In the volume files used here, `low` is the only token that occurs.
#
# These cells are retained as a distinct category rather than converted to
# missing or to zero. For volume measures, `low` can mean either no activity or
# 1-4 starts, so it does not by itself prove delivery. See the DfE FE and skills
# methodology, "Confidentiality and Rounding", and DATA-NOTES.md.

suppression_tokens <- c("low", "c", "x", "z")

low_token <- "low"


# --- CONCENTRATION FLAGS ------------------------------------------------------

concentrated_top1_share_pct   <- 75   # region-level: top 1 region >= this
dispersed_min_regions         <- 5    # region-level: at least this many regions
dispersed_max_top1_share_pct  <- 50   # ...and top 1 below this

localised_top5_lad_share_pct  <- 75   # LAD-level: top 5 LADs >= this
dispersed_min_lads            <- 25
dispersed_max_top5_share_pct  <- 50


# --- PLOT CONSTANTS -----------------------------------------------------------

colour_primary   <- "#2916E0"
colour_secondary <- "#8E84FF"
colour_accent    <- "#6BB5E8"
colour_text      <- "#222222"
colour_panel     <- "#F8F8F8"

colour_blue_mid   <- "#0072BC"
colour_blue_light <- "#4CB3E6"
colour_teal       <- "#00A6A6"
colour_grey       <- "#8A8F98"

# Suppressed values are drawn as hollow points, so a reader can see the
# standard is present while its exact position is not precisely known.
suppressed_point_shape <- 21
suppressed_point_fill  <- NA

chart_width  <- 10
chart_height <- 6
chart_dpi    <- 300


# --- STANDARD SOURCE FOOTNOTES ------------------------------------------------
# Reused across charts so attribution stays consistent.

src_ees <- paste0(
  "Source: DfE, Apprenticeships, July 2026 release. England, ", ref_year_label, "."
)

src_shiny <- paste0(
  "Source: DfE apprenticeships provider dashboard, downloaded 7 June 2026. ",
  "England, ", ref_year_label, ". Figures are unrounded and unsuppressed."
)

src_skills_england <- paste0(
  "Source: Skills England, Search the Apprenticeships, as at ", skills_england_asat, "."
)


config_version <- "2026-10-17-publication-R"

message("00_config.R loaded (version ", config_version, "). ",
        "Reference year: ", ref_year_label, ". ",
        "Project root: ", normalizePath(project_root, winslash = "/"))
