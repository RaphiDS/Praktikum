---
editor_options: 
  markdown: 
    wrap: 72
---

# Drogenkonsum in den USA von 2015-2019

**Author:** Denis Oluri, Sarah Puthiaparampil, Raphael Schneider

## Version & Packages

We use R Version 4.4.2. The Packages we use are: tidyverse, usmap and
usdata

## Data

First you have to get the data:

to create the dataset on your own:

-   Load the setup.R file in the codes folder

-   Go to: <https://www.samhsa.gov/data/data-we-collect/nsduh/datafiles>
    and download the data of the years 2015, 2016, 2017, 2018, 2019 in
    the .RData format

    (website is down as of 09.02.25, datasets are in data_edit in the
    .zip file but not on github because of size)

-   Save them inside the data_edit folder

-   Run data_edit/data_generating_code.R (takes some time loading the
    datasets)

OR

-   it already is loaded with the setup.R

## Presentation

To get the Presentation:

To create it manually:

-   The first Code you should run is the setup.R inside the codes folder
    (should already be done)

-   After that you have to run every script manually (except setup.R and
    source.R)

    OR

    use the source.R inside the same folder to save all plots

-   The last step is to render the presentation_final.qmd

OR

-   open presentation.html

## Images

The Plots are saved in the presentation_files/plots folder
