# README

This directory contains **official, standardized** [election results certified](https://elections.wi.gov/election-result/2024-general-election-results) by the Wisconsin Election Commission.

The original reporting unit file, downloaded from the WEC, is `Ward by Ward Report_November 5 2024 General Election_Federal and State Contests.xlsx`. This file contains 1 sheet for every contest, and a multi-row header for each table.

I clean this file into a single, long format CSV. See `clean-wec-ward-file.R` for details. The output is `wec-original-all-races-long-format.csv`. This file also contains the state assembly, state senate, and US congress district assignments for each reporting unit.

The GIS boundaries for each reporting unit are in `rep-units-simplified.geojson`. To see how this file was created see `integrate-ward-vintages.R` and `aggregate-rep-unit-polygons.R`.

I also (dis)aggregate past election results into the 2024 reporting unit files using registered voter overlap to create allocation factors. The results of that comparison are in `pres-gov-senate_2024-rep-units_1990-2024.geojson`.