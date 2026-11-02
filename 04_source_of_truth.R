# ==============================================================================
# 04_source_of_truth.R
# Canonical thin-market classification and headline figures
# ==============================================================================
#
# READS (from 02_load_data.R):
#   shiny_provider_standard   provider x standard starts, 2024/25, unsuppressed
#   skills_england_dedup      one row per standard, with status and funding
#
# PRODUCES:
#   standards_market          standards with recorded starts, eligible for market tiers
#   standards_no_recorded_starts  current standards outside the observed market population
#   standards_analytical_universe union of the current offer and 2024/25 delivery population
#   standards_population_summary  size and share of each population
#   market_standards_not_currently_available  market standards absent from the current offer
#   unavailable_market_status_summary  catalogue status of those market standards
#   thin_market_tiered        one row per market standard: starts, providers, tier
#   thin_market_is8           the above plus route, funding and IS8 proxy sector
#   tier_summary              standards and starts by tier
#   is8_summary               thin-market exposure by IS8 proxy sector
#   is8_tier_crosstab         sector x tier counts
#   is8_review_group_standards severe/thin standards mapped to the core IS8
#                              sectors or the foundational category
#   is8_review_group_totals   core-eight and expanded IS8 totals
#   tier_examples             top 20 standards per tier, with delivery context
#   canonical_headlines       every headline figure, in one place
#
# DEPENDS ON: 00_config.R, 01_functions.R, 02_load_data.R
#
# ------------------------------------------------------------------------------
# This is the single place the thin-market classification is computed. Every
# figure quoted downstream traces back to an object created here, so that a
# number appearing in a chart, a table and the text cannot silently diverge.
#
# Both axes - starts and provider count - come from the same provider dashboard
# export. Taking them from different files would mean comparing a current
# measure against a lagged one, and would make the provider count depend on
# which providers happened to have leavers rather than starts.
# ==============================================================================

library(tidyverse)

source("00_config.R")
source("01_functions.R")


# --- SECTION 1: LINK STARTS TO THE STANDARDS CATALOGUE ------------------------
# The dashboard export identifies standards by name only, with no reference
# code, so the link to the Skills England catalogue is made on a normalised
# name. Names that do not match are reported rather than dropped silently:
# they are mostly frameworks, which have no standard reference by definition.

message("--- Linking dashboard starts to the standards catalogue ---")

# Catalogue names are not unique across references: distinct standards are
# occasionally listed under the same title, distinguished by level or approval
# period. Ambiguous names are excluded from the automatic lookup and must be
# resolved explicitly in standard_name_aliases. Selecting the currently
# available reference would misassign historical 2024/25 delivery to standards
# that were not approved until 2026.
duplicate_names <- skills_england_dedup %>%
  count(.data$name_lower, name = "refs") %>%
  filter(.data$refs > 1)

standards_lookup <- skills_england_dedup %>%
  anti_join(duplicate_names, by = "name_lower") %>%
  arrange(
    .data$name_lower,
    desc(.data$is_available_for_starts),
    desc(.data$is_approved),
    desc(.data$version_number)
  ) %>%
  distinct(.data$name_lower, .keep_all = TRUE) %>%
  select(standard_ref, name_lower, route, level, max_funding,
         typical_duration, regulated_standard, status,
         is_available_for_starts, is_paused, is_retired)

if (nrow(duplicate_names) > 0) {
  message("  ", nrow(duplicate_names),
          " catalogue name(s) shared by more than one reference; ",
          "automatic matching disabled for those names.")
}

assert_unique_key(standards_lookup, "name_lower", "standards_lookup")

starts_keyed <- shiny_provider_standard %>%
  filter(.data$starts > 0) %>%
  mutate(name_lower = str_to_lower(str_squish(.data$standard_name))) %>%
  left_join(select(standards_lookup, name_lower, standard_ref), by = "name_lower") %>%
  rename(standard_ref_name_match = .data$standard_ref)

# Apply the alias table to names the catalogue spells differently. Without
# this, several substantial standards are dropped from the classification on a
# formatting difference alone.
alias_lookup <- standard_name_aliases %>%
  transmute(
    name_lower     = str_to_lower(str_squish(.data$dashboard_name)),
    standard_ref_alias = .data$standard_ref
  )

assert_unique_key(alias_lookup, "name_lower", "alias_lookup")

# Every alias must point at a reference that exists in the catalogue,
# otherwise the standard is recovered but arrives with no route, level or
# funding attached.
alias_unknown <- setdiff(alias_lookup$standard_ref_alias, skills_england_dedup$standard_ref)

if (length(alias_unknown) > 0) {
  stop(
    "standard_name_aliases refers to reference(s) not in the catalogue: ",
    paste(alias_unknown, collapse = ", "),
    call. = FALSE
  )
}

starts_keyed <- starts_keyed %>%
  left_join(alias_lookup, by = "name_lower") %>%
  mutate(
    # The explicit audit table is authoritative where present. This also
    # prevents a future catalogue title collision from silently overriding a
    # deliberately researched historical assignment.
    standard_ref = coalesce(
      .data$standard_ref_alias,
      .data$standard_ref_name_match
    ),
    match_method = case_when(
      !is.na(.data$standard_ref_alias) ~ "Explicit audited alias",
      !is.na(.data$standard_ref_name_match) ~ "Unique normalised name",
      TRUE ~ "Unmatched"
    )
  )

unresolved_duplicate_names <- starts_keyed %>%
  filter(
    .data$name_lower %in% duplicate_names$name_lower,
    is.na(.data$standard_ref_alias)
  ) %>%
  distinct(.data$standard_name)

if (nrow(unresolved_duplicate_names) > 0) {
  stop(
    "Dashboard activity uses ", nrow(unresolved_duplicate_names),
    " ambiguous catalogue name(s) without an explicit alias:\n  ",
    paste(unresolved_duplicate_names$standard_name, collapse = "\n  "),
    "\nAdd an audited reference assignment to standard_name_aliases.",
    call. = FALSE
  )
}

standard_link_audit <- starts_keyed %>%
  group_by(
    .data$standard_name,
    .data$standard_ref,
    .data$match_method
  ) %>%
  summarise(
    starts = sum(.data$starts, na.rm = TRUE),
    providers = n_distinct(.data$provider_ukprn[.data$starts > 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(.data$match_method, .data$standard_name)

recovered <- starts_keyed %>%
  filter(.data$match_method == "Explicit audited alias") %>%
  summarise(
    standards = n_distinct(.data$standard_name),
    starts    = sum(.data$starts, na.rm = TRUE)
  )

message("  Aliases recovered ", recovered$standards, " standards, ",
        format(recovered$starts, big.mark = ","), " starts")

match_quality <- starts_keyed %>%
  summarise(
    rows                = n(),
    starts_total        = sum(.data$starts, na.rm = TRUE),
    starts_matched      = sum(.data$starts[!is.na(.data$standard_ref)], na.rm = TRUE),
    standards_unmatched = n_distinct(.data$standard_name[is.na(.data$standard_ref)])
  ) %>%
  mutate(pct_starts_matched = round(100 * .data$starts_matched / .data$starts_total, 1))

print(match_quality)

unmatched_names <- starts_keyed %>%
  filter(is.na(.data$standard_ref)) %>%
  group_by(.data$standard_name, .data$standard_or_framework) %>%
  summarise(starts = sum(.data$starts, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(.data$starts))

message("  ", nrow(unmatched_names), " unmatched names, ",
        format(sum(unmatched_names$starts), big.mark = ","), " starts")


# --- SECTION 2: BUILD THE TWO AXES --------------------------------------------
# Starts are summed across providers; providers are counted as distinct UKPRNs
# with at least one start. Both are measured on the same rows, so a standard
# cannot appear on one axis and not the other.

standard_starts <- starts_keyed %>%
  filter(!is.na(.data$standard_ref)) %>%
  group_by(.data$standard_ref) %>%
  summarise(
    standard_name = first(.data$standard_name),
    ssa_tier_1    = first(.data$ssa_tier_1),
    starts        = sum(.data$starts, na.rm = TRUE),
    .groups = "drop"
  )

standard_providers <- starts_keyed %>%
  filter(!is.na(.data$standard_ref), !is.na(.data$provider_ukprn), .data$starts > 0) %>%
  group_by(.data$standard_ref) %>%
  summarise(providers = n_distinct(.data$provider_ukprn), .groups = "drop")

assert_unique_key(standard_starts, "standard_ref", "standard_starts")
assert_unique_key(standard_providers, "standard_ref", "standard_providers")

# Classified starts can never exceed the source total. If they do, a join has
# duplicated rows somewhere upstream, which inflates every volume figure
# without changing any count of standards.
source_total    <- sum(shiny_provider_standard$starts, na.rm = TRUE)
classified_total <- sum(standard_starts$starts, na.rm = TRUE)

if (classified_total > source_total) {
  stop(
    "Classified starts (", format(classified_total, big.mark = ","),
    ") exceed the source total (", format(source_total, big.mark = ","), ").\n",
    "  A join has fanned out rows. Check that every lookup used on the ",
    "right-hand side of a join has a unique key.",
    call. = FALSE
  )
}

message("  Classified ", format(classified_total, big.mark = ","), " of ",
        format(source_total, big.mark = ","), " starts (",
        round(100 * classified_total / source_total, 1), "%)")

standards_market <- standard_starts %>%
  left_join(standard_providers, by = "standard_ref")


# Every standard entering the market tiers must have positive observed activity
# on both axes. Fail rather than silently shrinking the denominator if the
# provider identifier is unexpectedly missing.
market_population_issues <- standards_market %>%
  filter(
    is.na(.data$starts) | .data$starts <= 0 |
      is.na(.data$providers) | .data$providers <= 0
  )

if (nrow(market_population_issues) > 0) {
  stop(
    "The market population contains ", nrow(market_population_issues),
    " standard(s) without positive starts and provider counts. ",
    "Inspect market_population_issues before classification.",
    call. = FALSE
  )
}

assert_unique_key(standards_market, "standard_ref", "standards_market")


# --- SECTION 3: DEFINE THE CURRENT OFFER AND OBSERVED MARKET POPULATIONS ------
# Standards currently available for starts but with no recorded 2024/25 starts
# are analytically relevant to the offer and proliferation of standards. They
# are kept separate from the market tiers because provider concentration and
# geographic reach cannot be measured for a standard with no observed delivery.
#
# Standards that are retired, withdrawn, paused or still in development are not
# added to the no-start group. Their absence of starts reflects administrative
# status rather than a current delivery outcome.

current_offer_standards <- skills_england_dedup %>%
  filter(.data$is_available_for_starts) %>%
  select(standard_ref, standard_name_se = standard_name)

standards_no_recorded_starts <- current_offer_standards %>%
  anti_join(standards_market, by = "standard_ref") %>%
  transmute(
    standard_ref,
    standard_name = .data$standard_name_se,
    ssa_tier_1    = NA_character_,
    starts        = 0,
    providers     = 0
  )

market_standards_currently_available <- standards_market %>%
  semi_join(current_offer_standards, by = "standard_ref")

market_standards_not_currently_available_base <- standards_market %>%
  anti_join(current_offer_standards, by = "standard_ref")

if (nrow(market_standards_currently_available) +
    nrow(market_standards_not_currently_available_base) != nrow(standards_market)) {
  stop("Current-availability groups do not reconcile to the market population.",
       call. = FALSE)
}

if (nrow(market_standards_currently_available) +
    nrow(standards_no_recorded_starts) != nrow(current_offer_standards)) {
  stop("Recorded-start and no-start groups do not reconcile to the current offer.",
       call. = FALSE)
}

message("\n  ", nrow(standards_market), " standards in the 2024/25 market-tier population | ",
        nrow(current_offer_standards), " currently available for starts | ",
        nrow(standards_no_recorded_starts), " current standards with no starts recorded")

standards_analytical_universe <- bind_rows(
  market_standards_currently_available %>%
    mutate(population = "Current offer: recorded 2024/25 starts"),
  standards_no_recorded_starts %>%
    mutate(population = "Current offer: no recorded 2024/25 starts"),
  market_standards_not_currently_available_base %>%
    mutate(population = "2024/25 delivery: no longer currently available")
)

assert_unique_key(standards_analytical_universe, "standard_ref",
                  "standards_analytical_universe")

standards_population_summary <- standards_analytical_universe %>%
  count(.data$population, name = "standards") %>%
  mutate(
    population = factor(
      .data$population,
      levels = c(
        "Current offer: recorded 2024/25 starts",
        "Current offer: no recorded 2024/25 starts",
        "2024/25 delivery: no longer currently available"
      )
    ),
    pct_of_analytical_universe = round(100 * .data$standards / sum(.data$standards), 1)
  ) %>%
  arrange(.data$population)

print(standards_population_summary)


# Some standards recorded starts during 2024/25 but are no longer available for
# new starts in the current Skills England catalogue. Keep them in the observed
# 2024/25 market population, but report them separately so a current-stock claim
# does not silently treat historical delivery and current availability as the
# same population.

market_standards_not_currently_available <-
  market_standards_not_currently_available_base %>%
  left_join(
    skills_england_dedup %>%
      select(
        standard_ref,
        catalogue_standard_name = standard_name,
        route,
        level,
        status,
        is_approved,
        is_available_for_starts,
        is_paused,
        is_retired
      ),
    by = "standard_ref"
  ) %>%
  mutate(
    availability_group = case_when(
      is.na(.data$status)      ~ "Not matched to current catalogue",
      .data$is_retired         ~ "Retired",
      .data$is_paused          ~ "Paused",
      .data$is_approved        ~ "Approved but not available for starts",
      TRUE                     ~ "Other not available for starts"
    )
  ) %>%
  select(
    standard_ref,
    dashboard_standard_name = standard_name,
    catalogue_standard_name,
    route,
    level,
    starts,
    providers,
    availability_group,
    status,
    is_approved,
    is_available_for_starts,
    is_paused,
    is_retired
  ) %>%
  arrange(.data$availability_group, desc(.data$starts))

unavailable_market_status_summary <- market_standards_not_currently_available %>%
  group_by(
    .data$availability_group,
    .data$status,
    .data$is_approved,
    .data$is_paused,
    .data$is_retired
  ) %>%
  summarise(
    standards   = n(),
    total_starts = sum(.data$starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(.data$standards), desc(.data$total_starts))

message("  ", nrow(market_standards_not_currently_available),
        " market standards are not currently available for new starts")
print(unavailable_market_status_summary, n = Inf)


# --- SECTION 4: TIER CLASSIFICATION -------------------------------------------
# Thresholds are defined in 00_config.R. Tiers are evaluated in order, so each
# market standard falls into the first tier it qualifies for. The denominator
# is the 2024/25 market population only; no-start standards remain outside it.

thin_market_tiered <- standards_market %>%
  assign_thin_market_tier()

tier_summary <- thin_market_tiered %>%
  group_by(.data$tier) %>%
  summarise(
    standards        = n(),
    total_starts     = sum(.data$starts, na.rm = TRUE),
    median_starts    = median(.data$starts, na.rm = TRUE),
    median_providers = median(.data$providers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_standards = round(100 * .data$standards / sum(.data$standards), 1),
    pct_starts    = round(100 * .data$total_starts / sum(.data$total_starts), 1)
  )

print(tier_summary)


# --- SECTION 5: INDUSTRIAL STRATEGY PROXY SECTORS -----------------------------
# Apprenticeship standards are not coded to the Government's eight Industrial
# Strategy sectors, so a proxy mapping is used, based on the Skills England
# route and keywords in the standard title. This is indicative, not official.
#
# Sectors are defined in an explicit ordered list rather than a single nested
# expression, because several keyword sets overlap - aerospace appears under
# both Defence and Advanced Manufacturing, for instance. Order therefore
# determines the outcome for any standard matching more than one sector, and
# stating it plainly makes that visible. The number of standards matching
# multiple sectors is reported below so the extent of the overlap is known.

is8_definitions <- list(
  list(
    sector = "Digital and Technologies",
    title  = "cyber|artificial intelligence|\\bai\\b|machine learning|software|data|digital|telecom|telecommunications|cloud|quantum|semiconductor|coding|programmer|ux|user experience",
    route  = "digital"
  ),
  list(
    # Placed before Clean Energy so that narrow grid and materials terms take
    # precedence over the broader "power" and "electricity" keywords.
    sector = "Foundational Industries and Inputs",
    title  = "steel|steelwork|steelworker|steelmaking|electricity network|national grid|distribution network operator|transmission network|substation|overhead line|grid connection|chemical|chemicals|petrochemical|critical mineral|rare earth|mining|quarrying|mineral extraction|cement|glass manufacturing|glass technician|polymer|ceramics|refractory",
    route  = NA_character_
  ),
  list(
    sector = "Life Sciences",
    title  = "clinical|biotech|bioinformatics|pharma|pharmaceutical|laboratory|lab scientist|healthcare science|medical|genomics|nursing|midwife|dietitian|podiatrist|radiographer|physiotherapist|orthotist|prosthetist|paramedic",
    route  = "health and science"
  ),
  list(
    sector = "Clean Energy Industries",
    title  = "nuclear|wind|hydrogen|low carbon|heat pump|heating|carbon|energy|power|utilities|gas network|electricity|electrical power|smart meter|water process|water network|environmental|sustainability|retrofit",
    route  = NA_character_
  ),
  list(
    sector = "Defence",
    title  = "defence|defense|ordnance|munitions|explosives|aerospace|aircraft|marine|maritime|naval|army|royal navy|royal air force|security|survival equipment",
    route  = NA_character_
  ),
  list(
    sector = "Advanced Manufacturing",
    title  = "manufacturing|manufacturer|engineering|robot|robotics|automotive|aerospace|materials|composites|battery|machining|machinist|welding|welder|fabrication|metal|foundry|casting|toolmaker|maintenance technician|rail engineering|mechatronics|process operative",
    route  = "engineering and manufacturing"
  ),
  list(
    sector = "Financial Services",
    title  = "finance|financial|investment|insurance|actuary|actuarial|mortgage|banking|pensions|tax|audit|accounting|accountancy|risk",
    route  = "legal, finance and accounting"
  ),
  list(
    sector = "Creative Industries",
    title  = "creative|media|broadcast|game|gaming|animation|film|design|designer|advertising|visual|arts|curator|museum|gallery|archive",
    route  = "creative and design"
  ),
  list(
    sector = "Professional and Business Services",
    title  = "consultant|consulting|business analyst|management|manager|project|legal|solicitor|paralegal|\\bhr\\b|human resources|procurement|marketing|sales|operations|leadership",
    route  = "business and administration"
  ),
  list(
    # Construction-route fallback, applied last so it only captures standards
    # that did not match a more specific sector above.
    sector = "Foundational Industries and Inputs",
    title  = NA_character_,
    route  = "construction and the built environment"
  )
)

is8_sectors_ordered <- c(
  "Advanced Manufacturing", "Clean Energy Industries", "Defence",
  "Digital and Technologies", "Life Sciences", "Financial Services",
  "Creative Industries", "Professional and Business Services",
  "Foundational Industries and Inputs"
)

is8_unmapped_label <- "Other / not clearly IS8"

#' Test a standard against one sector definition
matches_is8 <- function(title_lower, route_lower, definition) {
  by_title <- if (!is.na(definition$title)) str_detect(title_lower, definition$title) else FALSE
  by_route <- if (!is.na(definition$route)) str_detect(route_lower, definition$route) else FALSE
  by_title | by_route
}

#' Assign an IS8 proxy sector, and count how many sectors each standard matches
assign_is8 <- function(standard_name, route) {

  title_lower <- str_to_lower(replace_na(standard_name, ""))
  route_lower <- str_to_lower(replace_na(route, ""))

  hits <- map(is8_definitions, ~ matches_is8(title_lower, route_lower, .x))

  # First match in list order wins.
  sector <- rep(is8_unmapped_label, length(standard_name))
  assigned <- rep(FALSE, length(standard_name))

  for (i in seq_along(is8_definitions)) {
    take <- hits[[i]] & !assigned
    sector[take] <- is8_definitions[[i]]$sector
    assigned <- assigned | hits[[i]]
  }

  n_definition_matches <- reduce(map(hits, as.integer), `+`)

  matched_sector_labels <- map_chr(seq_along(standard_name), function(row_index) {
    matched <- map_chr(seq_along(is8_definitions), function(definition_index) {
      if (hits[[definition_index]][[row_index]]) {
        is8_definitions[[definition_index]]$sector
      } else {
        NA_character_
      }
    })
    paste(unique(matched[!is.na(matched)]), collapse = "; ")
  })

  n_sector_matches <- if_else(
    matched_sector_labels == "",
    0L,
    str_count(matched_sector_labels, fixed(";")) + 1L
  )

  tibble(
    is8_sector = sector,
    is8_n_definitions_matched = n_definition_matches,
    is8_n_sectors_matched = n_sector_matches,
    is8_sectors_matched = matched_sector_labels
  )
}

standards_metadata <- skills_england_dedup %>%
  select(standard_ref, route, level, max_funding, typical_duration,
         regulated_standard, status)

thin_market_is8 <- thin_market_tiered %>%
  left_join(standards_metadata, by = "standard_ref") %>%
  bind_cols(assign_is8(.$standard_name, .$route)) %>%
  mutate(
    is8_flag   = .data$is8_sector != is8_unmapped_label,
    is8_sector = factor(.data$is8_sector, levels = c(is8_sectors_ordered, is8_unmapped_label))
  )

message("\n  IS8 proxy: ", sum(thin_market_is8$is8_flag), " of ",
        nrow(thin_market_is8), " standards mapped to a sector")
message("  ", sum(thin_market_is8$is8_n_definitions_matched > 1),
        " standards match more than one definition; ",
        sum(thin_market_is8$is8_n_sectors_matched > 1),
        " match more than one distinct sector; ",
        "the first in list order is used")

is8_mapping_audit <- thin_market_is8 %>%
  transmute(
    standard_ref,
    standard_name,
    route,
    assigned_proxy_sector = as.character(.data$is8_sector),
    is8_n_definitions_matched,
    is8_n_sectors_matched,
    is8_sectors_matched
  ) %>%
  arrange(desc(.data$is8_n_sectors_matched), .data$standard_name)


# --- SECTION 6: DELIVERY CONTEXT ----------------------------------------------
# Some standards are delivered almost entirely by employer-providers in the
# public sector - NHS trusts, the armed forces, police, prisons, fire and
# rescue. A low provider count there reflects how the occupation is organised
# rather than a commercial market failing, so these are flagged separately and
# a headline excluding them is reported alongside the raw figure.

institutional_pattern <- regex(
  paste0(
    "NHS|NATIONAL HEALTH SERVICE|AMBULANCE|HOSPITAL|FOUNDATION TRUST|HEALTHCARE|",
    "ARMY|ROYAL NAVY|ROYAL AIR FORCE|MINISTRY OF DEFENCE|\\bRAF\\b|HM FORCES|",
    "POLICE|CONSTABULARY|HMPPS|PRISON|HER MAJESTY|HIS MAJESTY'S PRISON|",
    "FIRE AND RESCUE|FIRE SERVICE|METROPOLITAN POLICE|BRITISH TRANSPORT POLICE"
  ),
  ignore_case = TRUE
)

dominant_provider <- starts_keyed %>%
  filter(!is.na(.data$standard_ref), .data$starts > 0) %>%
  group_by(.data$standard_ref, .data$provider_name) %>%
  summarise(provider_starts = sum(.data$starts, na.rm = TRUE), .groups = "drop") %>%
  group_by(.data$standard_ref) %>%
  slice_max(.data$provider_starts, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    standard_ref,
    dominant_provider = .data$provider_name,
    institutional     = str_detect(.data$provider_name, institutional_pattern)
  )

thin_market_is8 <- thin_market_is8 %>%
  left_join(dominant_provider, by = "standard_ref") %>%
  mutate(
    institutional    = replace_na(.data$institutional, FALSE),
    delivery_context = if_else(.data$institutional, "Institutional", "Open market")
  )


# --- SECTION 7: SECTOR SUMMARIES ----------------------------------------------

is8_tier_crosstab <- thin_market_is8 %>%
  filter(.data$is8_flag) %>%
  count(.data$is8_sector, .data$tier, name = "standards") %>%
  complete(
    is8_sector = factor(is8_sectors_ordered, levels = levels(thin_market_is8$is8_sector)),
    tier       = factor(tier_levels, levels = tier_levels),
    fill       = list(standards = 0)
  ) %>%
  filter(.data$is8_sector %in% is8_sectors_ordered) %>%
  arrange(.data$is8_sector, .data$tier)

is8_summary <- thin_market_is8 %>%
  filter(.data$is8_flag) %>%
  group_by(.data$is8_sector) %>%
  summarise(
    total_standards     = n(),
    severe_thin         = sum(.data$tier == "Severe thin market"),
    thin                = sum(.data$tier == "Thin market"),
    potential_thin      = sum(.data$tier == "Potential thin market"),
    provider_dependency = sum(.data$tier == "Provider dependency market"),
    .groups = "drop"
  ) %>%
  mutate(
    severe_plus_thin     = .data$severe_thin + .data$thin,
    pct_severe_plus_thin = round(100 * .data$severe_plus_thin / .data$total_standards, 1)
  ) %>%
  arrange(.data$is8_sector)

print(is8_summary)


# --- SECTION 8: TIER EXAMPLES -------------------------------------------------
# The largest standards in each tier, with the dominant provider and whether
# delivery is institutional. Makes the classification auditable.

tier_examples <- thin_market_is8 %>%
  group_by(.data$tier) %>%
  arrange(desc(.data$starts), .by_group = TRUE) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  select(tier, standard_ref, standard_name, route, level, starts, providers,
         max_funding, regulated_standard, is8_sector,
         dominant_provider, delivery_context) %>%
  arrange(.data$tier, desc(.data$starts))

# Annex Table A1: purposive examples, not a representative sample. Use the
# full population, rather than tier_examples (which contains only the largest
# cases). Include the report's severe/thin examples, two one-start cases,
# the severe-tier median of eight starts and Cabin Crew at 19 starts.
# Composites Technician (26 starts, six providers) and the 97-start senior
# rail standard are outside these two tiers and must not be labelled thin.
report_example_refs <- c(
  "ST0656", "ST1338", "ST0817", "ST0535", "ST0593", "ST0052",
  "ST0859", "ST0315", "ST0495", "ST0558",
  "ST0188", "ST0051", "ST0746", "ST0461", "ST1341", "ST0383", "ST0497",
  "ST1311", "ST0791", "ST0995"
)
table_01_severe_thin_examples <- thin_market_is8 %>%
  filter(.data$standard_ref %in% report_example_refs) %>%
  select(tier, standard_ref, standard_name, route, level, starts, providers,
         max_funding, regulated_standard, is8_sector,
         dominant_provider, delivery_context) %>%
  arrange(.data$tier, .data$starts, .data$standard_ref)

stopifnot(
  nrow(table_01_severe_thin_examples) == length(report_example_refs),
  !anyDuplicated(table_01_severe_thin_examples$standard_ref),
  sum(table_01_severe_thin_examples$tier == "Severe thin market") == 10L,
  sum(table_01_severe_thin_examples$tier == "Thin market") == 10L
)

# Report Table 3 follows the native Functions and Owners table in the body.
table_03_market_tier_summary <- tier_summary


# --- SECTION 9: CANONICAL HEADLINE FIGURES ------------------------------------
# Every headline figure in one object, each with the population it is measured
# against. Quoting from here rather than recomputing keeps the text, tables and
# charts consistent.

thin_severe <- filter(thin_market_is8, .data$tier %in% c("Severe thin market", "Thin market"))

core_is8_sectors <- setdiff(
  is8_sectors_ordered,
  "Foundational Industries and Inputs"
)

is8_review_group_standards <- thin_severe %>%
  filter(.data$is8_flag) %>%
  mutate(
    is8_scope = if_else(
      as.character(.data$is8_sector) == "Foundational Industries and Inputs",
      "Additional foundational category",
      "Eight core Industrial Strategy sectors"
    )
  ) %>%
  select(
    standard_ref, standard_name, route, level, starts, providers, tier,
    is8_sector, is8_scope, max_funding, delivery_context
  ) %>%
  arrange(.data$is8_scope, .data$is8_sector, .data$tier, desc(.data$starts))

is8_review_group_totals <- tibble(
  scope = c(
    "Eight core Industrial Strategy sectors",
    "Eight core sectors plus foundational category"
  ),
  standards = c(
    sum(as.character(thin_severe$is8_sector) %in% core_is8_sectors),
    sum(thin_severe$is8_flag)
  )
) %>%
  mutate(
    review_group_standards = nrow(thin_severe),
    pct_of_review_group = round(
      100 * .data$standards / .data$review_group_standards,
      1
    )
  )

print(is8_review_group_totals)

canonical_headlines <- tribble(
  ~figure,                              ~value,                                                    ~population,
  "Standards currently available for starts", nrow(current_offer_standards),                       "current Skills England catalogue",
  "Current standards with recorded starts", nrow(market_standards_currently_available),            "current offer and 2024/25 dashboard",
  "Current standards with no recorded starts", nrow(standards_no_recorded_starts),                 "current offer, outside market tiers",
  "Standards with 2024/25 starts but no longer available", nrow(market_standards_not_currently_available_base), "2024/25 dashboard and current catalogue",
  "Standards in analytical universe",    nrow(standards_analytical_universe),                       "current offer plus 2024/25 delivery",
  "Standards in market-tier population", nrow(standards_market),                                    "dashboard, 2024/25",
  "Standards classified into tiers",     nrow(thin_market_is8),                                     "market-tier population",
  "Severe thin market",                  sum(thin_market_is8$tier == "Severe thin market"),         "market-tier population",
  "Thin market",                         sum(thin_market_is8$tier == "Thin market"),                "market-tier population",
  "Severe + thin",                       nrow(thin_severe),                                         "market-tier population",
  "Severe + thin, core IS8 sectors",     sum(as.character(thin_severe$is8_sector) %in% core_is8_sectors), "eight core Industrial Strategy sectors",
  "Severe + thin, including foundational category", sum(thin_severe$is8_flag),                       "core IS8 plus additional foundational category",
  "Severe + thin, open market only",     sum(!thin_severe$institutional),                           "excludes institutional delivery",
  "Provider dependency market",          sum(thin_market_is8$tier == "Provider dependency market"), "market-tier population",
  "Scaled market",                       sum(thin_market_is8$tier == "Scaled market"),              "market-tier population",
  "Total starts classified",             sum(thin_market_is8$starts, na.rm = TRUE),                 "market-tier population"
) %>%
  mutate(value = as.numeric(.data$value))

print(canonical_headlines, n = Inf)


# --- SECTION 10: SAVE ---------------------------------------------------------

save_table(table_01_severe_thin_examples,
           "table_01_severe_thin_examples.csv")
save_table(table_03_market_tier_summary,
           "table_03_market_tier_summary.csv")
save_table(standards_population_summary, "standards_population_summary.csv")
save_table(standards_no_recorded_starts, "standards_no_recorded_starts.csv")
save_table(market_standards_not_currently_available,
           "market_standards_not_currently_available.csv")
save_table(unavailable_market_status_summary,
           "unavailable_market_status_summary.csv")
save_table(is8_summary,          "is8_summary.csv")
save_table(is8_tier_crosstab,    "is8_tier_crosstab.csv")
save_table(is8_review_group_standards, "is8_review_group_standards.csv")
save_table(is8_review_group_totals, "is8_review_group_totals.csv")
save_table(is8_mapping_audit,    "is8_mapping_audit.csv")
save_table(canonical_headlines,  "canonical_headlines.csv")
save_table(unmatched_names,      "unmatched_standard_names.csv")
save_table(standard_link_audit,  "standard_link_audit.csv")

message("\n04_source_of_truth.R complete.")
