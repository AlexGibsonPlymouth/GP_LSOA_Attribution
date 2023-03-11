## Population-weighted attribution of GP data to LSOAs

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
- Output is sent to the *./output* directory (which will be created if necessary). It goes without saying that the output should be checked for NAs and zeros.
- Every effort has been made to ensure the script fails gracefully and informatively (e.g. because of missing or malformed input data), but .......!

### What it does
Following a variety of data integrity checks, the script:
- Downloads NHS Digital GP-LSOA population lookup files (see below) for 2014/15 to 2021/22 inclusive. The downloaded files are saved locally and, if they remain available, this step will be skipped.
- Reads the first line of the designated driver file for instructions about:
    1. which Public Health England (PHE) indicator dataset is to be used (see below), 
    2. whether the GP-LSOA population lookup should use males, females or the total population, 
    3. which year or years are to be analysed, and 
    4. whether case and denominator data are to saved alongside rates and whether the output will be averaged over a given range or output for each individual year (see below).
- Downloads the specified indicator dataset from the PHE website
- Extracts data from the indicator dataset for the year or range of years specified.
- Calculates the GP-level rate for the specified indicator for every GP in the PHE dataset and links it with NHS Digital GP-LSOA 'flow' data for the appropriate year, calculating the 'expected' number of cases in each unique GP-LSOA combination as the product of the PHE rate and GP-LSOA population.
- Aggregates all 'expected cases' and populations to LSOA level and then calculates LSOA-level estimated rates.
- Adds default or user-supplied LSOA attribute data if requested
- Writes the requested output as .csv files in the *./output* directory.
- Moves to the next line of the designated driver file and iterates through the above steps, until all lines are processed.
- Once all lines in the driver file have been processed, all temporary files (except *junk.txt*) are deleted from the hard drive and the program terminates. The *junk.txt* file can be ignored as it will get over-written each time the script runs.

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

Each line of the driver file is processed separately and run in turn.
- The 'Indicator' value determines which PHE dataset is to be used, and refers to its PHE ID, about which more below. This ID number is used, with 'Years', to create the output filename
- The 'Sex' value determines whether the NHS Digital GP<>LSOA population data will refer to males (m), females (f) or the whole population (a). *[I have yet to come across indicators that refer to other than the whole population - but it is possible!]*
- The 'Years' value determines the single year or range of years (as 'XXXX:YYYY') for which LSOA data will be attributed, and
- The 'Average' value has a different effect depending on whether a single year or range of years has been specified.  
  - If a single year, then 'n' means only estimated LSOA rates will be output whilst 'y' means estimated case and denominator data will also be output. 
  - If a range of years is selected, the 'n' means estimated rates will **not** be calculated as an average for the period as a whole - instead output will be produced for each year separately. On the other hand, if 'y' then a single rate will be calculated as the sum of cases across all years divided by the sum of denominators.  In this case annual count and denominator data is provided alongside the average rate.
- The file *DriverFileExample.csv* provides a template that can be followed.


##### Formatting the attribute data
The script will `left-join()` any user-supplied file of LSOA attribute data to the output.  This must be provided as a .csv file with the LSOA column named LSOA_CODE and include all 32844 English LSOAs. The script strips out all non-English LSOAs from the output and should ignore any non English LSOAs in the attribute file.  **Note** that this uses 2011 LSOAs - it will need to be updated when PHE & NHS Digital start using 2021 LSOAs (or, worse, incorporate a best-fit look up if one starts using 2021 LSOAs whilst the still uses 2011 LSOAs!).

##### Supplied (necessary) files
The script initialises by downloading all NHS Digital population lookup files (unless already saved in the path), and then downloads whichever PHE Indicator datasets are specified.  The script also uses *LSOAMasterList.csv*, which is included in the zipfile.

##### NHS Digital GP-LSOA Population Lookup data
NHS Digital provide quarterly and then monthly 'snapshot' data on "Patients registered at GP Practices" (https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice).

This includes information on the number of males, females and total population by 5-year bands as well as, in recent years, by single-year-of-age.  It also includes (form January 2014) the number of males, females and total population in each LSOA who attend each GP.  There is no age-breakdown for this GP<>LSOA flow data so attributed LSOA rates for age-restricted QOF data must assume that all age cohorts are similarly distributed between GPs and LSOAs.

The PHE indicator data is for fiscal years (April-March) so we only require a single set of GP<>LSOA flow data for each year. NHS Digital GP<>LSOA flow data for each April is used, such that, for instance, population data for April 2016 is used with respect to PHE indicator data for 2016/17.

It is difficult to assess the reliability of the NHS Digital GP<>LSOA population data, not least because it must be affected by the constant churn of GPs and their patient lists. There is certainly an issue for 2017 & 2018 as there are a number of LSOAs that don't appear in the dataset (54 & 12 respectively) - implying that data for the GPs that must be supplying them is missing. (See below for potential upgrade.)

##### Public Health England Data (includes QOF)
Public Health England's 'Fingertips' database (https://fingertips.phe.org.uk/) is accessed using API (https://fingertips.phe.org.uk/profile/guidance/supporting-information/api). 

The script, obviously, will only work with indicator datasets which include GP data, which is a relatively small subset of the whole.  The key API command in the script (in this case to download the ID=212 dataset)  is:
`https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=212&child_area_type_id=7&parent_area_type_id=15`          
             
- Where `child_area_type_id=7` refers to GPs and `parent_area_type_id=15` refers to England. 

A listing of all indicator dataset IDs and descriptive names is available from https://fingertips.phe.org.uk/documents/api_annex.ods.

A JSON (but readable) listing of all PHE indicator datasets which include GP data can be obtained by copying the following into your browser address: https://fingertips.phe.org.uk/api/available_data?area_type_id=7. Not all of these datasets will be suitable, though the QOF data should be. 

- If a request is made for a year for which indicator data is unavailable (e.g. a file exists and includes data for, say, 2016-2018, but the request is for 2020 data), then the program will run and zeros will be returned for that year.  All output will need to be checked!

##### Troubleshooting and Observations
- The major risk going forward is that PHE or NHS Digital change the html addresses of their datasets.  If there is no response when the script reports "Downloading ...." then this may be the problem.

- Even if the html addresses are correct, the script is dependant on a reasonably quick response from PHE / NHS Digital servers. I have had no problems with this so far, though response times can sometimes be up to 30 seconds or so.

- Access to the PHE / NHS Digital data does not currently require authorisation, and I haven't met download limits. Both these might change in the future.

- It should be possible to add new NHS Digital GP-LSOA population lookup files, but these are hard-coded so the script itself will need to be edited. The script is extensively commented (and the section with NHS Digital filenames is obvious) so that shouldn't be difficult.

- The script extracts and uses data from PHE indicator dataset from columns named "Indicator Name","Area Code","Area Name","Time period" "Count","Denominator","Area Type". An error will be returned if these columns are unavailable.  It is unlikely that current datasets will be changed, but new datasets may use different column names, which will be annoying!!  

- If I end up needing to use 2017 and/or 2018 data it will be necessary to produce a single GP-LSOA lookup based on all available months for each year.  That should avoid zero population LSOAs arising because lookup data for the GPs which serve those LSOAs is missing. This is probably a sensible **UPDATE** for all years, although it will probably have marginal impact.

- It's worth keeping this technique/script in mind for any other attribution of data to LSOAs, although remember that it rests on the assumption that there is no socio-economic or other bias in the distribution of populations between LSOAs. It would not, therefore, be suitable for attributing, for instance, data relating to pupils at schools to LSOAs.


---
11 March 2023



