# Calendars data repository

This repository contains the R code and raw data for the research article:

> **How to build a calendar: A global survey of calendars reveals constraints on their evolution**
> by Janne Yrjö-Koskinen, Helena Miton & Olivier Morin (submitted)

## Repository contents

| File | Description |
|------|-------------|
| `README.md` | This file |
| `all_calendars.csv` | All calendars surveyed, including whether each was included in the final dataset |
| `calendars-code-2026-09-11.R` | R script that runs the full analysis and produces the figures |
| `data_dictionary_all.txt` | Description of the variables in `all_calendars.csv` |
| `data_dictionary_included.txt` | Description of the variables in `included_calendars.csv` |
| `included_calendars.csv` | The calendars included in the study. This is the main dataset used by the R script. |

## Requirements

- R (tested with R 4.5.1; R 4.1 or later is required for current package versions)
- RStudio (recommended)
- The following R packages: `tidyverse` (which includes `dplyr`, `ggplot2`, `forcats`, `stringr`, `tidyr`, and `readr`), `ggridges`, `viridis`, `ggrepel`, `maps`, and `patchwork`

To install any packages you don't already have, run this in R:

```r
pkgs <- c("tidyverse", "ggridges", "viridis", "ggrepel", "maps", "patchwork")
install.packages(setdiff(pkgs, rownames(installed.packages())))
```

## How to run the analysis

1. **Clone the repository**

   ```bash
   git clone https://github.com/janneyk/calendars-data.git
   ```

   Alternatively, click **Code → Download ZIP** on GitHub and unzip the folder.

2. **Download the D-PLACE location data**

   The maps use society coordinates from [D-PLACE](https://d-place.org). Save the file `societies.csv` in the repository folder, either by downloading it from [this link](https://github.com/D-PLACE/dplace-data/blob/master/datasets/EA/societies.csv) or by running this in R from the repository folder:

   ```r
   download.file(
     "https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/societies.csv",
     "societies.csv"
   )
   ```

3. **Open the script**

   Open `calendars-code-2026-09-11.R` in RStudio and set the working directory to the repository folder (**Session → Set Working Directory → To Source File Location**).

4. **Run the script**

   Run the whole script. Figures are saved as PNG files in the repository folder.

## Data

`included_calendars.csv` contains one row per calendar, and the variables are described in `data_dictionary_included.txt`. 

Most calendars are linked to a location through their eHRAF ID in D-PLACE. Calendars that are not in D-PLACE were given approximate coordinates by hand, and these are listed with notes in the R script.

## Citation

The article is currently in preparation. Until it is published, please cite this repository:

> Yrjö-Koskinen, J., Miton, H., &  Morin, O. (submitted). Code and data for "How to build a calendar: A global survey of calendars reveals constraints on their evolution". GitHub. https://github.com/janneyk/calendars-data

This section will be updated with the full reference once the article is published.

## License

(TBC)

## Contact

Olivier Morin (email: morin.olivier AT pm.me)
