# Apprenticeship market structure in England

This repository contains the analysis behind our work on thin markets in England's apprenticeship system. England has around 712 approved apprenticeship standards, and they vary enormously in scale: the largest attracts more than 12,000 starts a year, while many attract fewer than ten and some attract none at all. A standard with very few starts, delivered by very few training providers, is fragile — if one provider withdraws it can become effectively unavailable, even though it remains formally approved and employers may still need those skills. The analysis identifies which standards are in that position, where they are concentrated by subject and by place, and how exposed the system is if provision withdraws.

The code produces a six-tier classification of every standard by annual starts and provider count, together with the supporting distribution, concentration, threshold-sensitivity, approval-date and geographic analysis. Two choices shape the results and are worth knowing before reading any output. **The analysis uses the unsuppressed underlying data files rather than the summary tables wherever precision matters.** The published summary tables round counts to the nearest ten and replace small values with a `low` marker that covers zero as well as one to nine, which makes it impossible to count small standards from them; the underlying files carry exact figures, so the historical series here are exact rather than estimated. **Geographic reach is measured directly** from underlying delivery records rather than estimated by distributing each provider's total across the areas it operates in. That materially changes the picture: the most fragile standards are delivered in a median of one local skills area, where a proxy-based method implies far wider spread. Populations are also kept separate and always named — 644 standards with recorded 2024/25 activity, 604 of those still in the current offer, and 712 approved and not retired or withdrawn are different denominators and are never combined.

## Data sources

All inputs are public. Download them into `Raw Data/` before running.

| Source | What it provides | Where |
|---|---|---|
| **DfE apprenticeships statistics, July 2026** | Summary tables: subject and standard, provider, learner characteristics, geography, historical series | [Release page](https://explore-education-statistics.service.gov.uk/find-statistics/apprenticeships/2025-26) · [Download all as ZIP](https://content.explore-education-statistics.service.gov.uk/api/releases/5fa09991-9ced-4ca5-b626-73b747df272d/files) |
| **Underlying starts data (unsuppressed)** | Exact starts by standard, provider, level, subject and local authority district. The basis for every historical and geographic figure | Same release, "Supporting files" section: `apps_u01_underlying_starts_202526_10.zip` |
| **Earlier underlying starts files** | Extends the exact series back to 2017/18 | [Releases in this series](https://explore-education-statistics.service.gov.uk/find-statistics/apprenticeships) — take the equivalent `underlying starts` supporting file from each earlier release |
| **Skills England apprenticeship catalogue** | Standard reference, status, route, level, funding band, approval and retirement dates | [Apprenticeship finder](https://skillsengland.education.gov.uk/apprenticeships/) → "Download a list of apprenticeships" → save as `Apprenticeships.csv` |
| **DfE apprenticeships provider dashboard** | Unsuppressed provider × standard starts for 2024/25, used for provider counts | [Provider dashboard](https://department-for-education.shinyapps.io/apprenticeships-provider-dashboard/) → apply filters → download the current selection |
| **Apprenticeship Provider and Assessment Register** | Registered providers and what they are permitted to deliver | Search gov.uk for "apprenticeship providers and assessment register"; export the register as CSV |
| **ONS boundaries** | Local Skills Improvement Plan areas, local authority districts and regions, for mapping and area assignment | [ONS Open Geography Portal](https://geoportal.statistics.gov.uk/) — search "LSIP", "Local Authority Districts" and "Regions", full-clipped (BFC) versions |
| **ONS local authority to combined authority lookup** | Assigns districts to mayoral strategic authorities | ONS Open Geography Portal — "Local Authority District to Combined Authority (May 2025) Lookup in EN" |

Filenames expected by the scripts are listed in `00_config.R`. If a release supersedes these, update the paths there rather than anywhere else.

Two things to know when collecting the data. The **provider dashboard** exports only the selection currently on screen, so the filters used determine what you get; the exports here are all levels, all age groups, 2024/25. And the **Skills England catalogue is a point-in-time extract** — ours is dated 1 June 2026, and any catalogue-derived time series must not extend beyond that date.

## Running the analysis

Set the working directory to the folder containing the scripts and run:

```r
source("99_run_all.R")
```

This runs the pipeline in order and writes `production_output_manifest.csv`, which records every expected output and whether it was produced. Scripts can also be run individually in numeric order; each documents at the top what it reads and what it produces.

`00_config.R` holds every parameter a refresh might need — file paths, reference year, classification thresholds, subject groupings — so nothing else hardcodes a year or a threshold.

The geography preparation step is slow on first run because it unpacks and aggregates large underlying files. It caches its results, so later runs load in seconds. Set `rebuild_geography <- TRUE` to force a rebuild.

Outputs are written to `Outputs/tables` and `Outputs/charts`, with a data CSV saved alongside each chart so every figure traces back to the table behind it.

## Method and caveats

`METHODOLOGY-NOTE.md` sets out the data sources, definitions, classification thresholds and limitations in full, and is worth reading before quoting any figure. The sections on suppression and on population definitions matter most: several plausible-looking denominators exist and they are not interchangeable.

## Licensing

Source data is published by the Department for Education, Skills England and the Office for National Statistics under the [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/). Figures relate to England only; apprenticeships are devolved and the other UK nations run separate systems. The reference year is 2024/25, using the July 2026 statistical release and a Skills England catalogue extract dated 1 June 2026.
