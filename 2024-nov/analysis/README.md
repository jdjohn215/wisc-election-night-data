**The files in this directory have been superceded by the release of official, [certified election results](https://elections.wi.gov/election-result/2024-general-election-results) from the Wisconsin Elections Commission on November 29, 2024. I have cleaned and processed those results, providing GIS boundaries and integrated past election results in the [`wec` directory](https://github.com/jdjohn215/wisc-election-night-data/tree/main/2024-nov/wec) of this repository.**

# Wisconsin 2024 reporting unit polygons

The file `election-results-2012-2024.geojson` contains polygons corresponding to the reporting units used in Wisconsin's November 2024 general elections. Reporting units are necessarily entirely within a single minor civil division, so I have added the corresponding `MCD_FIPS` code to each record. The wards include the (to my knowledge) complete, unofficial election night results from each reporting unit for Trump (PREREP24), Harris (PREDEM24), Baldwin (USSDEM24), and Hovde (USSREP24).

I have also added Democratic and Republican candidate totals to each ward for past races for president, governor, and senate from 2012 through 2022. Ward boundaries change between elections. The sources for these ward totals are as follows:

* 2022, a custom reporting unit shapefile of my own creation with official, certified 2022 results
* 2012-2020, a disaggregated ward file created by the Wisconsin Legislative Technology Services Bureau.

Files with the suffice `with-all-party-totals` also include a `TOT` column for each race with the total number of votes cast for any candidate (including 3rd parties).

In both cases, I created allocation weights from the old ward geography to the new one using the addresses of every voter in the L2 Wisconsin voter file. Keep in mind, there is uncertainty associated with this process. Individual ward data, particularly for past elections, may be innaccurate. Vote allocations from past elections include fractions.

Most of the ward boundaries represented by this reporting unit GIS file were collected by the LTSB in July of 2024. In instances where ward boundaries had clearly changed, I obtained new ward files from county clerks. I cannot guarantee that the ward boundaries shown here precisely match those under which the election was administered.

The file `rep-units-with-2020-census-block-stats.csv` contains demographic data from the 2020 census allocated into each reporting unit. I assigned census blocks to reporting units based on centroid intersection. The `pop_per_sq_mi` variable is the result of dividing total 2020 population by the land area of each reporting unit. I calculated the land area by erasing surface water before computing the remaining polygon size.

## Coverage

I provide reporting units for 69/72 counties. For the remaining counties of Green, Sauk, and Iron, I include a row containing their candidate totals.

I confirmed that my 2024 candidate totals match the values from the Associated Press as of Wednesday afternoon, with a few exceptions:

* I have 6 fewer votes in Washington County because AP is double-counting a City of Hartford which is reported by both Washington and Dodge counties
* [Iowa](https://www.iowacounty.org/elections), Trump's total is 6631 not 6571 and Harris' is 7750 not 7730
* [Marquette](https://www.co.marquette.wi.us/home/showpublisheddocument/18546), Hovde's total is 5629 not 5729
* Iron County's correct total for Trump is 2,557. The Town of Pence reported the wrong total on election night, per the Iron Co clerk.

## Citations

You are free to use this data with attribution. Please cite me in my professional capacity: John Johnson, Research Fellow in the Marquette Law School Lubar Center. [Contact me with questions here](https://law.marquette.edu/faculty-and-staff-directory/john-johnson).

## Updates

* on 11/8/2024 I noticed that Marathon County T Berlin Wards 1-2 had been corrected by Marathon County. The initial file I downloaded showed doubled vote values for each contest. See the [original PDF](https://github.com/jdjohn215/wisc-election-night-data/blob/main/2024-nov/raw/Marathon%202024-11-06%2004-00-15.pdf) here and [the correction here](https://github.com/jdjohn215/wisc-election-night-data/blob/main/2024-nov/raw/Marathon%202024-11-08%2012-54-55.pdf).
* on 11/9/2024 I added Sauk County reporting unit data
* on 11/11/2024 I added Green County reporting units, Iron County reporting units and official Shawano County results. The latter includes the Town of Fairbanks which was previously ommitted from the election night reporting unit file.
* on 11/21/2024 I updated the La Crosse County results with the county certified version.