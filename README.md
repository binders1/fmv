# Replication package

* Gold, M., Binder, S., and C. Nolte. *Expanding the Coverage and Accuracy of Parcel-Level Land Value Estimates*

## Repository overview

All replication code resides in `~/fmv/code/`. Generated figures will be saved to `~/fmv/output/exhibits/`.

### code/

#### Real Estate Variables

* `~/fmv/code/real_estate/`

`master_real_estate.R` retrieves and cleans FRED HPI and Realtor.com median home value data.

#### Data Cleaning

* `~/fmv/code/clean/`

`master_clean.R` cleans parcel data and aggregates to the sale level by state. Results are saved in `~/fmv/data/cleaned/`.

#### Modeling

* `~/fmv/code/model/`

There are two modeling approaches. `base_models/` runs the main suite of 7 models reported in the paper. A three-letter naming convention is employed to abbreviate and uniquely identify each model. (predictor set)(geographic level)(additional adjustment). 

Predictor set is either "f" for "full" or "n" for "nolte" (this is a historical accident, as the models were named before we began referring to the Nolte (2020) predictor set as the restricted set). 

The "additional adjustment" is restricted ("r") or base ("b") at the FRR level, to denote whether the sample is restricted to only the counties that got modeled by the restricted-predictor county model. At the county level, the "additional adjustment" is whether HPI is included. In the full predictor set models, HPI is always included, so this has no effect. In the restricted predictor set models, there is one "base" version (w/o HPI), and one version with HPI. 

|                  |        | Predictor Set |            |
|------------------|--------|---------------|------------|
|                  |        | Full          | Restricted |
| Geographic level | County | fcb           | ncb, nch   |
|                  | FRR    | ffb, ffr      | nfb, nfr   |

The other modeling approach (`model_all_parcels/`), run at the FRR level with the full predictor set, produces the predictions plotted in [Figure 4](https://github.com/AMGold99/fmv/blob/master/output/exhibits/ffb_pred_all_parcels.png). Instead of aggregating to the sale level, these models use parcel-level observations as their inputs. To ensure the model is predicting only the value of the land (sans built improvements), we filter out any observations with a building footprint when specifying the model.

#### Visualization

* `~/fmv/code/viz/`

`master_viz.R` generates and saves all paper figures. At time of writing, figure generation occurs twice: once to generate the figures as they are appear and are called in the `main.tex` working paper within this repository; and once for figure versions complying with _PLOS One_ submission requirements.

The figure generation approach can look intimidating at first. The `exhibit_tbl` describes every figure uniquely using a [tribble](https://tibble.tidyverse.org/reference/tribble.html). Each column is an argument in the image generation function `save_image_custom()`, including the code of the function that generates a given figure; and each row is a figure. 


## Replication Instructions

* Fork repository
* Open `fmv.Rproj`
* Set FRED API key: `Sys.setenv(FRED_API_KEY = <API-KEY>)`. Generate one [here](https://fred.stlouisfed.org/docs/api/api_key.html).
* Source `master_fmv.R`
* Follow instructions as they appear in the console.

## Data Availability

As Zillow has [ended](https://www.zillow.com/research/ztrax/) academic access to its proprietary ZTRAX database, the parcel- and sale-level data upon which the analysis is built will not be available in full to readers. A data sample may be accessed [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/00WMEO).

## Description of data directory

Within the codebase, scripts often reference folders and files contained within `fmv/data/`. Below is a brief summary of the the data directory's contents.

- `ArcResults/`
   - `parquet/` _parcel-level climate, irrigation, and soil variables by CONUS state_
   - `soilcodes/` _SSURGO farmland suitability classifications present in each CONUS state_

- `Nolte/` _parcel and sale records, curated by [PLACES](https://placeslab.org/), along with crosswalks that link sale IDs to parcel IDs. Organized by CONUS state_
  
- `mhv_impute/`
   - `realtor/` _[Realtor.com](https://www.realtor.com/research/data/) median home value data_

- `helper_data/` _miscellaneous collection of reference tables, crosswalks, housing indicator, and other data_

- `spatial/` _county and state shapefiles_


