# Toronto Neighbourhood Crime Rate Overview

The crime rates in different regions of Toronto ranges largely. The crime rate also has an increasing trend from 2014 to 2019, and a sudden drop in 2020 probably due to covid 19.

## Data

[neighbourhood-crime-rate.rmd](./outputs/paper/neighbourhood-crime-rate.rmd) will download data from online. In case there is no internet connection, a local copy is saved [here](./inputs/data/neighbourhood-crime-rates.geojson).

## Code and Report

[neighbourhood-crime-rate.pdf](./outputs/paper/neighbourhood-crime-rate.pdf) is the paper I wrote to analyze the crime rate in Toronto.

[neighbourhood-crime-rate.rmd](./outputs/paper/neighbourhood-crime-rate.rmd) contains the code to generate a paper. Just open the file with RStudio, and run Knit. Uncomment the library installation code if you don't have the libraries installed.

The [R script](./scripts/neighbourhood-crime-rate.R) can be found in [scripts](./scripts/) folder.

By running [R script](./scripts/neighbourhood-crime-rate.R), the figures used in the [paper](./outputs/paper/neighbourhood-crime-rate.pdf) will be generated
in folder [outputs/images](./outputs/images/).

To run the [R script](./scripts/neighbourhood-crime-rate.R), Run `Rscript ./scripts/neighbourhood-crime-rate.R` within terminal or Run it in RStudio.