# MEA Shiny Analysis

Shiny app for running the MEA organoid spontaneous baseline analysis and browsing its metadata, plots, rendered HTML report, and downloadable output archive.

## Contents

- `app.R` - Shiny UI/server, upload handling, report rendering, and result gallery.
- `analysis/MEA_organoids_spont_basesline_local_paths.Rmd` - parameterized analysis report used by the app.
- `env.archived.yaml` - Conda package list for rebuilding the R environment.
- `input/` - uploaded MEA zip files and extracted data; ignored by git.
- `output/` - rendered reports, plots, and `report.zip`; ignored by git.

## Requirements

R with the packages listed in `env.archived.yaml`, including Shiny, bslib, DT, dplyr, readxl, rmarkdown, stringr, tibble, tidyverse, ggridges, cowplot, and ggforce.

Optional Conda setup:

```sh
conda env create --name mea-shiny --file env.archived.yaml
conda activate mea-shiny
```

## Run

```sh
Rscript deploy.R
```

or from R:

```r
shiny::runApp(".")
```

## Input Format

Upload a `.zip` through the app. The zip should contain top-level metadata `.xlsx` files and MEA plate folders whose names include `plate1`, `plate2`, etc. The app stores the zip under `input/`, extracts it into `input/<zip-name>/`, and requires an Experiment ID before running.

## Output

The analysis writes plots, the rendered HTML report, and `report.zip` to `output/`. The Shiny tabs show metadata, input files, amplitude and firing-rate plots, heatmaps, raster plots, ISI plots, and report download links.
