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

* Shawano County's ward file appears to lack any values for the Town of Fairbanks, despite those totals apparently being reflected in the countywide statistics. This affects a very small number of votes.
* I have 6 fewer votes in Washington County because AP is double-counting a City of Hartford which is reported by both Washington and Dodge counties
* [Iowa](https://www.iowacounty.org/elections), Trump's total is 6631 not 6571 and Harris' is 7750 not 7730
* [Marquette](https://www.co.marquette.wi.us/home/showpublisheddocument/18546), Hovde's total is 5629 not 5729

## Citations

You are free to use this data with attribution. Please cite me in my professional capacity: John Johnson, Research Fellow in the Marquette Law School Lubar Center. [Contact me with questions here](https://law.marquette.edu/faculty-and-staff-directory/john-johnson).