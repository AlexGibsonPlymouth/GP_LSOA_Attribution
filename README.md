## Population-weighted attribution of GP data to LSOAs

 This was developed to attribute GP-level **QOF data** to LSOAs, but in theory it should work with any GP-level count & denominator data on the PHE website.

 As usual, the R script is run in the Terminal (Windows) and takes a user-supplied 'driver file' which provides instructions as to which PHE datasets for which years are to be processed, along with a couple of additional parameters. 

### Run from Terminal
- Download zipfile from the github
- Unzip into local directory (make sure R can access - set PATH if necessary)
- Open Terminal and navigate to local directory with **GP_Attribution.R** script
- To run script: `Rscript GP_Attribution.R <driver.file> <attribute.file>`

 `<driver.file>` and `<attribute.file>` are optional - if left blank *DriverFileExample.csv* and *LSOA_CoastalFlag.csv* will be used.  



 Instructions and further information to be provided!

 But, for the moment, download the R script by clicking on **GP_LSOA_Attribution.R** above, which will take you to its page, and then right-clicking on the **Raw** button - and save the R script to desired directory on your PC.

Then open up the **Terminal** on your PC and navigate to the directory in which you saved the *GP_LSOA_Attribution.R* file.

Type **Rscript GP_LSOA_Attribution.R** and follow instructions

 This then will download all data needed and tidy up afterwards. LSOA files will be saved to a sub-directory called **output**.



