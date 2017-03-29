Mode share and cycling infrastructure in some European countries
================================================================

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.343938.svg)](https://doi.org/10.5281/zenodo.343938)

What we've been doing.

Idea:

-   Gather mode share data from EPOMM and PASTA for cities in several European countries.

-   Get distances of :bicyclist: paths and number of :car: streets, the latter only for PASTA cities.

Note: At the time we started the project [`overpass`](https://github.com/hrbrmstr/overpass) was the only package for accessing the Overpass API from R. Now the package will be replaced by [`osmdata`](https://github.com/osmdatar/osmdata)

Code:

-  ["Makefile"](makefile.R)


Steps:

-   Preparing mode share data from the two information sources [here](code/data_preparation.R) In this code for the PASTA cities information from tems is replaced with PASTA information.


-   [Finding cities boundaries](code/add_boundaries.R) This is done by country, with conditions because of some small issues we noticed along the way, e.g. using "Ciutat de Valencia" as city name for Valencia. For checking boundaries html maps were produced and looked at.

-   [Finding cycle paths](code/make_overpass_queries.R)

-   [Finding streets](code/make_overpass_streets_queries.R)

-   [Join tables](code/bind_tables.R)

-   [Fit growth model and make a plot with flags](code/maelle_growthmodel.R)

Outputs:

- [Figure for the growth model](figures/kitschflags.png)

- [Table with cycle infrastructure distance](tadaaa.csv)

- [Table with streets distance for the PASTA cities](tadaaa_streets.csv)

- [Final data for cycle infrastructure](data/finaldata.csv)
