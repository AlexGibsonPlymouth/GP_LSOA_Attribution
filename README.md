## Population-weighted attribution of GP data to LSOAs

 <p style="color:red">DRAFT (as of 9/3/23)</p>
 <p style="color:red">Finish README</p>
 <p style="color:red">zipfile to be added</p>
 
 This was developed to attribute GP-level **QOF data** to LSOAs, but in theory should work with any GP-level count & denominator data on the PHE website.

 As usual, the R script (written using R version 4.2.2) is run in the Terminal (Windows/Ubuntu) and takes a user-supplied 'driver file' which provides instructions as to which PHE datasets for which years are to be processed, along with a couple of additional parameters. 

### Run from Terminal
- Download zipfile *GP_LSOA_Attribution.zip* from the github
- Unzip into a local directory (make sure R can access - set PATH if necessary)
- Requires tidyverse and httr packages preloaded into R. It works on my Windows10 & Ubuntu 22.04 PCs.
- Open Terminal and navigate to the local directory with the **GP_LSOA_Attribution.R** script
- To run, type in Terminal: `Rscript GP_LSOA_Attribution.R <driver.file> <attribute.file>`
- The `<driver.file>` and `<attribute.file>` arguments are optional - if left blank *DriverFileExample.csv* and *LSOA_CoastalFlag.csv* will be used.  This is sensible for a first run to illustrate the different outputs that can be produced.
- Progress is reported on screen - which depends on internet access as a variety of data files must be downloaded
- Output is sent to the *./output* directory (which will be created if necessary)
- Every effort has been made to ensure the script fails gracefully and informatively (e.g. because of missing or malformed input data), but .......!

### What it does
Following a variety of data integrity checks, the script:
- Downloads NHS Digital GP-LSOA population lookup files (see below) for 2014/15 to 2021/22 inclusive. The downloaded files are saved locally and, if they remain available, this step will be skipped.
- Reads the first line of the designated driver file for instructions about:
    1. which Public Health England (PHE) indicator dataset is to be used (see below), 
    2. whether the GP-LSOA population lookup should use males, females or total population, 
    3. which year or years are to be analysed, and 
    4. whether case and denominator data are to saved alongside rates and whether the output will be averaged over a given range or output for each individual year (see below).
- Downloads the specified indicator dataset from the PHE website
- Extracts data from the indicator dataset for the year or range of years specified.
- Calculates the GP-level rate for every GP in the PHE dataset and links it with GP-LSOA 'flow' data for the appropriate year, calculating the 'expected' number of cases in each unique GP-LSOA combination as the product of the PHE rate and GP-LSOA population.
- Aggregates all 'expected cases' and populations to LSOA level
- Adds default or user-supplied LSOA attribute data if requested
- Writes output as requested to .csv files in the *./output* directory.
- Moves to the next line of the designated driver file and iterates through the above steps, until all lines are processed.
- Once all lines in the driver file have been processed all temporary files (except *junk.txt*) are deleted from the hard drive and the program terminates. The *junk.txt* file can be ignored as it will get over-written each time the script runs.

##### Formatting the driver file
Getting the .csv driver file correctly formatted is crucial, including column headings precisely as below:

| Indicator | Sex |   Years   | Average |
| :-------: | :-: | :-------: | :-----: |
|    212    |  a  | 2014:2016 |    y    |
|    219    |  a  | 2015:2018 |    n    |
|    241    |  a  |   2016    |    n    |
|    258    |  a  |   2014    |    y    |
|    276    |  a  |   2019    |    n    |
|    280    |  a  |   2020    |    y    |

Each line of the driver file is processed separately and in turn
- The 'Indicator' cell determines which PHE dataset is to be used, and refers to its PHE ID, about which more below. This ID number is used, with 'Years', to create the output filename
- The 'Sex' cell determines whether the NHS Digital GP<>LSOA population data used to attribute from GPs to LSOAs will comprise justs males (m), just females (f) or the whole population (a). [I have yet to come across indicators that refer to other than the whole population - but it is possible!]
- The 'Years' cell determines the single year or range of years ('XXXX:YYYY') for which LSOA data will be attributed, and
- The 'Average' cell has a different effect depending on whether a single or range of years has been specified.  
  - If a single year, then 'n' means only estimated LSOA rates will be output whilst 'y' means estimated case and denominator data will also be output. 
  - If a range of years is selected, the 'n' means estimated rates will **not** be calculated as an average for the period as a whole - instead output will be produced for each year separately. On the other hand, if 'y' then a single rate will be calculated as the sum of cases across the year divided by the sum of denominators.  In this case annual count and denominator data is provided alongside the average rate.
- The file *DriverFileExample.csv* provides a template tat can be followed.


##### Formatting the attribute data
The script will `left-join()` any user-supplied file of LSOA attribute data to the output.  This must be provided as a .csv file with the LSOA column named LSOA_CODE and include all 32844 English LSOAs. The script strips out all non-English LSOAs that appear in the GP<>LSOA lookup file, and will (should!) ignore any non English LSOAs in the attribute file.  **Note** that this uses 2011 LSOAs - it will need to be updated when PHE & NHS Digital start using 2021 LSOAs (or, worse, incorporate a best-fit look up if one starts using 2021 LSOAs whilst the still uses 2021 LSOAs!).

##### Supplied (necessary) files
The script initialises by downloading all NHS Digital population lookup files (unless already saved in the path), and then downloads whichever PHE Indicator datasets are specified.  The script also uses *LSOAMasterList.csv*, which is included in the zipfile.

##### NHS Digital GP-LSOA Population Lookup data
NHS Digital provide quarterly and then monthly 'snapshot' data on "Patients registered at GP Practices (https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice).

This includes information on the number of males, females by 5-year bands and, in recent years, by single-year-of-age.  It also includes the number of males and females in each LSOA who attend each GP.  There is no age-breakdown for this GP<>LSOA flow data.

The PHE indicator data is produced for fiscal years (April-March) so we only require a single set of GP<>LSOA flow data for each year and I (arbitrarily) use NHS Digital GP<>LSOA flow data for each April, such that population data for April 2016 is used with respect to PHE indicator data for 2016/17.

It is difficult to assess the reliability of the NHS Digital GP<>LSOA population data, not least as it will be affected by the constant churn of GPs and their patient lists. There is certainly an issue for 2017 & 2018 as there are a number of LSOAs that don't appear in the dataset - implying that data for the GPs that must be supplying them is missing. (See below for potential upgrade.)

##### PHE (QOF) Data
Public Health England's 'Fingertips' database (https://fingertips.phe.org.uk/) can be accessed using API. The GingertipsAbout the PHE (QOF) data - where to get list of ID's, how to find what is available at GP level

https://fingertips.phe.org.uk/api/available_data?area_type_id=7

##### Observations and warnings
2017 & 2018 and what they tell us about population lookup quality
data 
what might need to do to use other PHE data
principle of allocating non-contiguous data might be useful for other applications - but must be able to assume that no selection bias

##### Troubleshooting
Download speed / responsiveness of the PHE and NHS Digital sites.
When come back to this remember vulnerability to url changes
Can add additional years of PHE / NHS Digital data, but only into  into hard script
Zero PHE data doesn't make the script fall down - but easy to see in output
Zero LSOA data can also happen - does in 2017 & 18
Script is heavily commented - should be possible to adapt
Think about upper/lower estimates.


