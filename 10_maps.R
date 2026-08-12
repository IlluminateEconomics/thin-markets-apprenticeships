# ==============================================================================
# 10_maps.R
# Direct maps of severe/thin Industrial Strategy apprenticeship delivery
# ==============================================================================
#
# READS:
#   geo_lad_standard            (03) directly observed LAD x standard starts
#   thin_market_is8             (04) market tier and inclusive proxy-sector links
#   is8_review_group_standards  (04) severe/thin IS8 and foundational standards
#   lad_to_lsip                 (07) district-to-LSIP spatial lookup
#   lad_boundaries, lsip_boundaries (02) simplified map geometries
#
# PRODUCES:
#   map_01_review_group_delivery_by_lsip.png
#   map_02_review_group_delivery_by_sector.png
#   chart-data and match-audit CSVs for both maps
#
# DEPENDS ON: 00_config.R to 07_geography.R
#
# These maps use directly observed delivery locations from the DfE underlying
# starts file. They do not allocate a provider's standard-level starts across
# every area in which that provider operates.
# ==============================================================================

library(tidyverse)
library(sf)

source("00_config.R")
source("01_functions.R")


required_source_objects <- c(
  "skills_england_dedup", "lad_boundaries", "lsip_boundaries"
)

if (!all(vapply(required_source_objects, exists, logical(1), inherits = TRUE))) {
  source("02_load_data.R")
}

if (!exists("geo_lad_standard", inherits = TRUE)) {
  source("03_prep_geography.R")
}

if (!exists("is8_review_group_standards", inherits = TRUE)) {
  source("04_source_of_truth.R")
}

if (!exists("lad_to_lsip", inherits = TRUE)) {
  source("07_geography.R")
}


# --- SECTION 1: STANDARD-NAME LINK -------------------------------------------
# The underlying starts file identifies standards by name, not ST reference.
# Exact dashboard names are therefore preferred over catalogue names. This
# ordering matters where two catalogue references share a displayed name.


normalise_map_name <- function(x) {
  x %>%
    replace_na("") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}


standard_name_candidates <- bind_rows(
  thin_market_is8 %>%
    transmute(
      standard_ref,
      candidate_name = .data$standard_name,
      candidate_source = "Provider dashboard name",
      priority = 1L
    ),
  skills_england_dedup %>%
    semi_join(thin_market_is8, by = "standard_ref") %>%
    transmute(
      standard_ref,
      candidate_name = .data$standard_name,
      candidate_source = "Skills England catalogue name",
      priority = 2L
    )
) %>%
  filter(!is.na(.data$candidate_name), .data$candidate_name != "") %>%
  mutate(name_key = normalise_map_name(.data$candidate_name)) %>%
  distinct(
    .data$name_key, .data$standard_ref, .data$candidate_source,
    .keep_all = TRUE
  )


preferred_name_candidates <- standard_name_candidates %>%
  group_by(.data$name_key) %>%
  filter(.data$priority == min(.data$priority)) %>%
  ungroup()


map_name_candidate_conflicts <- preferred_name_candidates %>%
  group_by(.data$name_key) %>%
  summarise(
    candidate_references = n_distinct(.data$standard_ref),
    references = paste(sort(unique(.data$standard_ref)), collapse = "; "),
    candidate_names = paste(sort(unique(.data$candidate_name)), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(.data$candidate_references > 1)


standard_name_lookup <- preferred_name_candidates %>%
  anti_join(map_name_candidate_conflicts, by = "name_key") %>%
  distinct(.data$name_key, .keep_all = TRUE) %>%
  select(
    name_key, standard_ref, matched_candidate_name = candidate_name,
    candidate_source
  )


ref_year_values <- c(
  as.character(ref_year),
  ref_year_label,
  str_replace(as.character(ref_year), "^(\\d{4})(\\d{2})$", "\\1/\\2")
)


geo_standard_reference_year <- geo_lad_standard %>%
  filter(as.character(.data$academic_year) %in% ref_year_values) %>%
  mutate(name_key = normalise_map_name(.data$std_fwk_name))


geo_standard_linked <- geo_standard_reference_year %>%
  left_join(standard_name_lookup, by = "name_key")


map_unmatched_standard_names <- geo_standard_linked %>%
  filter(is.na(.data$standard_ref)) %>%
  group_by(.data$std_fwk_name, .data$name_key) %>%
  summarise(
    starts = sum(.data$starts, na.rm = TRUE),
    suppressed_cells = sum(.data$suppressed_cells, na.rm = TRUE),
    districts = n_distinct(.data$geo_lad),
    .groups = "drop"
  ) %>%
  arrange(desc(.data$starts), desc(.data$suppressed_cells))


geo_review_group <- geo_standard_linked %>%
  inner_join(
    is8_review_group_standards %>%
      select(
        standard_ref, standard_name, tier, is8_sector, is8_scope,
        market_starts = starts, market_providers = providers
      ),
    by = "standard_ref"
  )


review_standard_coverage <- 100 *
  n_distinct(geo_review_group$standard_ref) /
  nrow(is8_review_group_standards)


all_starts_name_coverage <- 100 *
  sum(geo_standard_linked$starts[!is.na(geo_standard_linked$standard_ref)], na.rm = TRUE) /
  sum(geo_standard_linked$starts, na.rm = TRUE)


# --- SECTION 2: DISTRICT, LSIP AND BOUNDARY LINKS ----------------------------


normalise_geo_name <- function(x) {
  x %>%
    replace_na("") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}


lad_name_col_map <- names(lad_boundaries)[
  str_detect(
    names(lad_boundaries),
    regex("^LAD[0-9]*NM$|lad_name|LAD_NAME", ignore_case = TRUE)
  )
][1]


lsip_name_col_map <- names(lsip_boundaries)[
  str_detect(
    names(lsip_boundaries),
    regex("^LSIP[0-9]*NM$|lsip_name|LSIP_NAME", ignore_case = TRUE)
  )
][1]


if (is.na(lad_name_col_map) || is.na(lsip_name_col_map)) {
  stop(
    "Map boundaries do not expose recognisable LAD and LSIP name columns.",
    call. = FALSE
  )
}


lad_to_lsip_map <- lad_to_lsip %>%
  transmute(
    lad_name,
    lad_key = normalise_geo_name(.data$lad_name),
    lsip_name,
    lsip_key = normalise_geo_name(.data$lsip_name)
  ) %>%
  distinct()


lad_to_lsip_conflicts <- lad_to_lsip_map %>%
  count(.data$lad_key, name = "lsip_matches") %>%
  filter(.data$lsip_matches > 1)


if (nrow(lad_to_lsip_conflicts) > 0) {
  stop(
    "One or more districts map to multiple LSIP areas. Inspect lad_to_lsip_conflicts.",
    call. = FALSE
  )
}


geo_review_group <- geo_review_group %>%
  mutate(lad_key = normalise_geo_name(.data$geo_lad)) %>%
  left_join(
    select(lad_to_lsip_map, lad_key, lsip_name, lsip_key),
    by = "lad_key"
  )


map_unmatched_lads_to_lsip <- geo_review_group %>%
  filter(is.na(.data$lsip_key)) %>%
  group_by(.data$geo_lad, .data$lad_key) %>%
  summarise(
    starts = sum(.data$starts, na.rm = TRUE),
    suppressed_cells = sum(.data$suppressed_cells, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(.data$starts), desc(.data$suppressed_cells))


lsip_start_coverage <- 100 *
  sum(geo_review_group$starts[!is.na(geo_review_group$lsip_key)], na.rm = TRUE) /
  sum(geo_review_group$starts, na.rm = TRUE)


review_lad_totals <- geo_review_group %>%
  group_by(.data$geo_lad, .data$lad_key) %>%
  summarise(
    starts = sum(.data$starts, na.rm = TRUE),
    suppressed_cells = sum(.data$suppressed_cells, na.rm = TRUE),
    standards = n_distinct(.data$standard_ref),
    .groups = "drop"
  )


review_lsip_totals <- geo_review_group %>%
  filter(!is.na(.data$lsip_key)) %>%
  group_by(.data$lsip_name, .data$lsip_key) %>%
  summarise(
    starts = sum(.data$starts, na.rm = TRUE),
    suppressed_cells = sum(.data$suppressed_cells, na.rm = TRUE),
    standards = n_distinct(.data$standard_ref),
    districts = n_distinct(.data$geo_lad),
    .groups = "drop"
  )


lad_boundary_base <- lad_boundaries %>%
  mutate(
    boundary_lad_name = .data[[lad_name_col_map]],
    lad_key = normalise_geo_name(.data$boundary_lad_name)
  )


lsip_boundary_base <- lsip_boundaries %>%
  mutate(
    boundary_lsip_name = .data[[lsip_name_col_map]],
    lsip_key = normalise_geo_name(.data$boundary_lsip_name)
  )


map_unmatched_lad_boundaries <- review_lad_totals %>%
  anti_join(st_drop_geometry(lad_boundary_base), by = "lad_key") %>%
  arrange(desc(.data$starts))


map_unmatched_lsip_boundaries <- review_lsip_totals %>%
  anti_join(st_drop_geometry(lsip_boundary_base), by = "lsip_key") %>%
  arrange(desc(.data$starts))


lad_boundary_start_coverage <- 100 *
  sum(
    review_lad_totals$starts[
      review_lad_totals$lad_key %in% lad_boundary_base$lad_key
    ],
    na.rm = TRUE
  ) /
  sum(review_lad_totals$starts, na.rm = TRUE)


lsip_boundary_start_coverage <- 100 *
  sum(
    review_lsip_totals$starts[
      review_lsip_totals$lsip_key %in% lsip_boundary_base$lsip_key
    ],
    na.rm = TRUE
  ) /
  sum(review_lsip_totals$starts, na.rm = TRUE)


map_coverage_summary <- tibble(
  metric = c(
    "Underlying starts matched to a standard reference",
    "Expected review-group standards found in geography",
    "Review-group starts assigned to an LSIP area",
    "Review-group starts matched to a LAD boundary",
    "Review-group starts matched to an LSIP boundary"
  ),
  pct_coverage = round(c(
    all_starts_name_coverage,
    review_standard_coverage,
    lsip_start_coverage,
    lad_boundary_start_coverage,
    lsip_boundary_start_coverage
  ), 1)
)


save_table(map_name_candidate_conflicts, "map_name_candidate_conflicts.csv")
save_table(map_unmatched_standard_names, "map_unmatched_standard_names.csv")
save_table(map_unmatched_lads_to_lsip, "map_unmatched_lads_to_lsip.csv")
save_table(map_unmatched_lad_boundaries, "map_unmatched_lad_boundaries.csv")
save_table(map_unmatched_lsip_boundaries, "map_unmatched_lsip_boundaries.csv")
save_table(map_coverage_summary, "map_coverage_summary.csv")


if (any(map_coverage_summary$pct_coverage < 90)) {
  stop(
    "One or more map links capture less than 90% of the relevant population. ",
    "Inspect map_coverage_summary.csv and the unmatched diagnostics before mapping.",
    call. = FALSE
  )
}


# --- SECTION 3: MAP 1, REVIEW-GROUP DELIVERY BY LSIP -------------------------


map_1_data <- lsip_boundary_base %>%
  left_join(review_lsip_totals, by = "lsip_key") %>%
  mutate(
    starts = replace_na(.data$starts, 0),
    suppressed_cells = replace_na(.data$suppressed_cells, 0),
    standards = replace_na(.data$standards, 0)
  )


map_1 <- ggplot(map_1_data) +
  geom_sf(aes(fill = .data$starts), colour = "white", linewidth = 0.25) +
  scale_fill_gradient(
    low = "#F2F0FF",
    high = colour_primary,
    trans = "sqrt",
    labels = scales::label_comma(),
    name = "Starts"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Severe and thin proxy-mapped apprenticeship delivery by LSIP area",
    subtitle = paste0(
      "Directly observed starts across ",
      scales::comma(nrow(is8_review_group_standards)),
      " standards, ", ref_year_label,
      "\nCore Industrial Strategy sectors plus the additional foundational category"
    ),
    caption = paste0(
      "Source: DfE underlying apprenticeship starts data, ", ref_year_label,
      "; Skills England catalogue as at ", skills_england_asat, ".",
      "\nDelivery location basis. Suppressed starts are excluded from totals but recorded in the audit.",
      "\nSector assignment is an indicative route-and-title proxy, not an official classification. Fill uses a square-root scale."
    )
  ) +
  theme_void(base_size = 11.5) +
  theme(
    plot.title = element_text(face = "bold", colour = colour_text),
    plot.subtitle = element_text(colour = colour_text),
    plot.caption = element_text(colour = colour_text, hjust = 0),
    plot.caption.position = "plot",
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )


save_table(
  map_1_data %>%
    st_drop_geometry() %>%
    select(
      lsip_name = boundary_lsip_name,
      starts, suppressed_cells, standards
    ),
  "map_01_review_group_delivery_by_lsip_data.csv"
)
save_chart(
  map_1,
  "map_01_review_group_delivery_by_lsip.png",
  width = 9.2,
  height = 8.0
)


# --- SECTION 4: MAP 2, REVIEW-GROUP DELIVERY BY SECTOR -----------------------
# Match Chart 10's inclusive, multi-label treatment. A standard is included in
# every sector to which the route-and-title proxy plausibly links it, but only
# once within each sector and location. Sector panels therefore overlap and
# must not be summed. The aggregate maps above remain uniquely counted.
#
# Each panel shows an LSIP area's share of that sector's severe/thin starts.
# Using a within-sector share prevents the larger Advanced Manufacturing and
# Life Sciences sectors from washing out patterns in smaller sectors.


review_tier_order <- c("Severe thin market", "Thin market")


map_sector_links <- thin_market_is8 %>%
  filter(
    .data$tier %in% review_tier_order,
    .data$is8_n_sectors_matched > 0,
    !is.na(.data$is8_sectors_matched),
    .data$is8_sectors_matched != ""
  ) %>%
  select(
    standard_ref, standard_name, tier,
    is8_n_sectors_matched, is8_sectors_matched
  ) %>%
  tidyr::separate_rows(is8_sectors_matched, sep = ";\\s*") %>%
  transmute(
    standard_ref,
    standard_name,
    tier = factor(as.character(.data$tier), levels = review_tier_order),
    is8_sector = str_trim(.data$is8_sectors_matched)
  ) %>%
  filter(.data$is8_sector %in% is8_sectors_ordered) %>%
  distinct(.data$standard_ref, .data$is8_sector, .keep_all = TRUE)


if (anyDuplicated(map_sector_links[c("standard_ref", "is8_sector")]) > 0) {
  stop(
    "Sector maps count a standard more than once within the same sector.",
    call. = FALSE
  )
}


if (!setequal(
  unique(map_sector_links$standard_ref),
  unique(is8_review_group_standards$standard_ref)
)) {
  stop(
    "Sector-map multi-label links do not reconcile to the mapped review group.",
    call. = FALSE
  )
}


geo_review_group_multilabel <- geo_review_group %>%
  select(-any_of(c("is8_sector", "is8_scope"))) %>%
  inner_join(
    map_sector_links %>% select(standard_ref, is8_sector),
    by = "standard_ref",
    relationship = "many-to-many"
  )


review_lsip_sector <- geo_review_group_multilabel %>%
  filter(!is.na(.data$lsip_key)) %>%
  group_by(.data$lsip_key, .data$is8_sector) %>%
  summarise(
    starts = sum(.data$starts, na.rm = TRUE),
    suppressed_cells = sum(.data$suppressed_cells, na.rm = TRUE),
    standards = n_distinct(.data$standard_ref),
    .groups = "drop"
  ) %>%
  group_by(.data$is8_sector) %>%
  mutate(
    sector_total_starts = sum(.data$starts, na.rm = TRUE),
    pct_sector_starts = if_else(
      .data$sector_total_starts > 0,
      100 * .data$starts / .data$sector_total_starts,
      0
    )
  ) %>%
  ungroup()


map_sector_link_summary <- map_sector_links %>%
  group_by(.data$is8_sector) %>%
  summarise(
    linked_review_standards = n_distinct(.data$standard_ref),
    .groups = "drop"
  ) %>%
  left_join(
    geo_review_group_multilabel %>%
      filter(!is.na(.data$lsip_key)) %>%
      group_by(.data$is8_sector) %>%
      summarise(
        geographically_observed_standards = n_distinct(.data$standard_ref),
        geographically_observed_starts = sum(.data$starts, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "is8_sector"
  ) %>%
  mutate(
    geographic_standard_coverage = 100 *
      .data$geographically_observed_standards /
      .data$linked_review_standards
  )


sector_map_list <- lapply(is8_sectors_ordered, function(sector_name) {
  lsip_boundary_base %>%
    select(lsip_key) %>%
    left_join(
      review_lsip_sector %>%
        filter(as.character(.data$is8_sector) == sector_name) %>%
        select(
          lsip_key, starts, suppressed_cells, standards,
          sector_total_starts, pct_sector_starts
        ),
      by = "lsip_key"
    ) %>%
    mutate(
      is8_sector = sector_name,
      starts = replace_na(.data$starts, 0),
      suppressed_cells = replace_na(.data$suppressed_cells, 0),
      standards = replace_na(.data$standards, 0),
      pct_sector_starts = replace_na(.data$pct_sector_starts, 0)
    )
})


map_a3_sector_data <- do.call(rbind, sector_map_list) %>%
  mutate(
    is8_sector = factor(.data$is8_sector, levels = is8_sectors_ordered)
  )


map_a3_sector <- ggplot(map_a3_sector_data) +
  geom_sf(aes(fill = .data$pct_sector_starts), colour = "white", linewidth = 0.08) +
  facet_wrap(vars(.data$is8_sector), ncol = 3) +
  scale_fill_gradient(
    low = "#F2F0FF",
    high = colour_primary,
    labels = scales::label_percent(scale = 1, accuracy = 1),
    name = "Share of sector's\nreview-group starts"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Geographic distribution of severe and thin markets by indicative sector",
    subtitle = paste0(
      "Each panel shows the LSIP share of directly observed starts linked to that sector, ",
      ref_year_label
    ),
    caption = paste(
      str_wrap(
        paste0(
          "Source: DfE underlying apprenticeship starts data, ", ref_year_label,
          "; Skills England catalogue as at ", skills_england_asat, "."
        ),
        width = 125
      ),
      str_wrap(
        paste0(
          "Standards are included in every sector to which the inclusive route-and-title proxy ",
          "plausibly links them, so panels overlap and should not be summed. Each standard is ",
          "counted only once within a sector and location."
        ),
        width = 125
      ),
      str_wrap(
        paste0(
          "The mapping is indicative and is not an official sector crosswalk. Foundational ",
          "Industries and Inputs is an additional analytical category, not a ninth official IS sector."
        ),
        width = 125
      ),
      sep = "\n"
    )
  ) +
  theme_void(base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold", colour = colour_text),
    plot.subtitle = element_text(colour = colour_text),
    strip.text = element_text(face = "bold", colour = colour_text, size = 9),
    plot.caption = element_text(colour = colour_text, hjust = 0),
    plot.caption.position = "plot",
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )


save_table(
  map_sector_links,
  "map_02_review_group_delivery_multilabel_links.csv"
)
save_table(
  map_sector_link_summary,
  "map_02_review_group_delivery_multilabel_link_summary.csv"
)
save_table(
  map_a3_sector_data %>%
    st_drop_geometry() %>%
    select(
      lsip_key, is8_sector, starts, suppressed_cells, standards,
      sector_total_starts, pct_sector_starts
    ),
  "map_02_review_group_delivery_by_sector_data.csv"
)
save_chart(
  map_a3_sector,
  "map_02_review_group_delivery_by_sector.png",
  width = 13.2,
  height = 11.0
)


message(
  "\n10_maps.R complete: two direct-delivery maps saved. Minimum link coverage: ",
  min(map_coverage_summary$pct_coverage), "%"
)
