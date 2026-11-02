# Apprenticeship market structure in England

R analysis supporting Illuminate Economics’ Gatsby Foundation report on employer representative bodies and England’s apprenticeship system. The publication snapshot uses 2024/25 starts, the July 2026 DfE release and the Skills England catalogue extracted on 1 June 2026.

There are **644 standards with recorded starts**, of which **202 are classified as severe thin or thin** (117 severe and 85 thin). Together they account for 3,415 of 353,463 standards-based starts, about 0.97%. Severe-thin standards have medians of eight starts and one active provider. The six tiers describe recorded volume and provider concentration; they do not establish financial unviability, unmet employer demand or a causal effect of standards proliferation.

Keep the populations distinct: 644 delivered standards, 604 delivered and still available in the June catalogue, 675 available for new starts and 712 approved and not retired/withdrawn, including 37 paused. The 71 available standards with no recorded starts are reported separately from the six delivery tiers.

The October publication corrections use stable ST references for geography and a bounded HR keyword for sector matching. The inclusive mapping has **139 unique core-sector standards**, **165 including the additional foundational category**, **191 standard-category links**, **24 standards matching multiple categories** and **23 matching multiple core sectors**. These are indicative rule-based links, not an official classification. Sector bars overlap and must not be summed.

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
| **ONS boundaries** | Boundary files used to construct the analytical areas, districts and regions | [ONS Open Geography Portal](https://geoportal.statistics.gov.uk/) — search "LSIP", "Local Authority Districts" and "Regions", full-clipped (BFC) versions |
| **ONS local authority to combined authority lookup** | Assigns districts to the combined-authority/London geography in the May 2025 lookup | ONS Open Geography Portal — "Local Authority District to Combined Authority (May 2025) Lookup in EN" |

Filenames expected by the scripts are listed in `00_config.R`. For exact publication reproduction retain these releases and extracts. A newly downloaded catalogue will change the snapshot. For a deliberate refresh, update the paths and parameters in `00_config.R`.

Two things to know when collecting the data. The **provider dashboard** exports only the selection currently on screen, so the filters used determine what you get; the exports here are all levels, all age groups, 2024/25. And the **Skills England catalogue is a point-in-time extract** — ours is dated 1 June 2026, and any catalogue-derived time series must not extend beyond that date.

## Running the analysis

Install R and the packages used by the scripts:

```r
install.packages(c("tidyverse", "sf", "lubridate", "jsonlite"))
```

Put the scripts together and set the working directory to that folder. Supported layouts include:

```text
thin-markets-apprenticeships/   # cloned repository, with R scripts at its root
  Raw Data/                   # supply the inputs separately
  Outputs/                    # generated, not committed
```

The existing Drive layout also works: `02 Analysis/R Scripts` beside `02 Analysis/Raw Data`, writing to `02 Analysis/Outputs`. `00_config.R` first retains a valid configured root, then checks the parent and current directory for `Raw Data`. If more than one nearby project contains that folder, set the root explicitly to select the intended inputs.

Run:

```r
source("99_run_all.R")
```

The runner executes `00_config.R` through `11_publication_outputs.R` in order. The new module is required, so keep it beside the other scripts. The first run rebuilds the older geography cache to retain stable ST references; later runs reuse compatible cached aggregates. Set `rebuild_geography <- TRUE` in the configuration to force a rebuild.

The pipeline requires the raw inputs, including the boundary RDS files named in `00_config.R`; it cannot run from code alone. They are not included in this repository. Current downloads may differ from the original point-in-time catalogue and dashboard exports. Keep the publication inputs if reproducing the frozen results.

Outputs go to `Outputs/tables` and `Outputs/charts`. Inspect:

- `production_output_manifest.csv`: expected files must be fresh and non-empty.
- `publication_value_checks.csv`: structural reconciliations and, for the frozen snapshot, publication totals must pass.
- `publication_figure_manifest.csv`: report labels mapped to production filenames and publication aliases.
- `production_script_run_log.csv`: script execution timings.

The runner removes an explicit list of legacy generated outputs. It does not delete raw data. The illustrative table now selects ten severe and ten thin standards from explicit references, covering low, middle and upper severe volumes; it is a purposive illustration, not a representative sample or an exhaustive list of narrative examples.

## Publication figures

`11_publication_outputs.R` exports PNG/SVG publication aliases and supporting CSVs from the existing R plot objects. It retains the Gatsby project’s original colours, themes and absence of gridlines. It does not read audit pictures or require an audit ZIP. Report labels below are the established audit labels; numbering in the final document may differ.

| Audit/report label | Production PNG | Publication alias, also exported as SVG |
|---|---|---|
| Chart 3 | `chart_03_live_stock_of_standards.png` | `chart_03_reconstructed_stock.png` |
| Chart 9 | `chart_10_is8_review_group_links.png` | `chart_09_corrected_inclusive_sector_links.png` |
| Chart 10 | `chart_11_geographic_reach_by_market_tier.png` | `chart_10_corrected_geographic_reach.png` |
| Figure 2 | `figure_02_all_proxy_linked_delivery.png` | `figure_02_direct_all_proxy_linked_starts.png` |
| Map 2 | `map_01_review_group_delivery_by_lsip.png` | `map_02_direct_review_group_starts.png` |
| Map 3 | `map_02_review_group_delivery_by_sector.png` | `map_03_corrected_sector_panels.png` |

Figure 2 includes all proxy-linked delivered standards, not just the severe/thin review group used in Map 2. Geographic coverage is 633 of 637 eligible standards. The 42 analytical areas are 38 non-London LSIP areas plus four London subregions; they are not 42 designated LSIPs. Delivery locations measure recorded activity, not employer demand or access. Regional-strength callouts in the report are separately attributed policy context, not findings generated by the map.

## Method and validation limits

Read [METHODOLOGY-NOTE.md](METHODOLOGY-NOTE.md) before quoting figures. Summary-table suppression includes zero, so a `low` cell does not establish positive activity. Exact headline tiers use the unsuppressed dashboard export; historical and delivery-geography calculations use underlying starts archives.

Saved October verification executed the focused R mapping, geography, plot and export steps and passed 41 publication reconciliations using retained audited inputs. It did not certify a full raw-data rebuild in the user’s Windows environment. This repository update does not claim a fresh full-pipeline run. A successful local `99_run_all.R` run and its manifests provide the rebuild check for the inputs supplied.

## Licensing

Source data from DfE, Skills England and ONS is published under the [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/). The source-data licence does not by itself license this repository’s code. Figures relate to England only.
