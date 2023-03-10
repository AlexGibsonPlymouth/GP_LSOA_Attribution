## Population-weighted attribution of GP data to LSOAs

 This was developed to attribute GP-level **QOF data** to LSOAs, but in theory it should work with any GP-level count & denominator data on the PHE website.

 As usual, the R script (written using R version 4.2.2) is run in the Terminal (Windows) and takes a user-supplied 'driver file' which provides instructions as to which PHE datasets for which years are to be processed, along with a couple of additional parameters. 

### Run from Terminal
- Download zipfile from the github
- Unzip into local directory (make sure R can access - set PATH if necessary)
- Open Terminal and navigate to the local directory with the **GP_Attribution.R** script
- To run, type in Terminal: `Rscript GP_Attribution.R <driver.file> <attribute.file>`
- The `<driver.file>` and `<attribute.file>` areguments are optional - if left blank *DriverFileExample.csv* and *LSOA_CoastalFlag.csv* will be used.  This is sensible for a first run to illustrate the different outputs that can be produced.
- Progress is reported on screen - which depends on internet access as a variety of data files must be downloaded
- Output is sent to the *./output* directory (which will be created if necessary)
- Every effort has been made to ensure the script fails gracefully and informatively (e.g. because of missing or malformed input data), but .......!

### What it does
Following a variety of data integrity checks, the script:
- Downloads NHS Digital GP-LSOA population lookup files (see below) for 2014/15 to 2021/22. The downloaded files are saved locally and, if they remain available, this step will be skipped.
- Reads the first line of the designated driver file for instructions about:
    1. which Public Health England (PHE) indicator dataset is to be used (see below), 
    2. whether the GP-LSOA population lookup should use males, females or total population, 
    3. which year or years are to be analysed, and 
    4. whether, if a single year is selected, case and denominator data are to be included alongside rates or, if a range of years is selected, whether output is to be for (a) each year separately or (b) averaged over the whole date range (in which case annual count and denominator data is provided alongside the average rate).
- Downloads the specified indicator dataset from the PHE website
- Extracts data from the indicator dataset for the year or range of years specified.
- Applying the specified, followinf and, following the instructions in the driver file, 

##### Formatting the driver file
About the driver file

##### NHS Digital GP-LSOA Population Lookup data
About the GP-Population data

##### PHE (QOF) Data
About the PHE (QOF) data




