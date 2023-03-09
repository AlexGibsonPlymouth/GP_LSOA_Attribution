#!/usr/bin/env Rscript

# GP_LSOA_Attribution_BATCH.R

args = commandArgs(trailingOnly=TRUE)

# For manual testing only #####################################################################
# setwd("H:/000_Coastal_Related/03_QOF_Practice_Attribution/000_gitfolders/GP_LSOA_Attribution")
# args=c("DriverFileExample.csv","LSOA_CoastalFlag.csv")
###############################################################################################

# Description----

# Designed for use with QOF data - batch file version

# This script attributes GP-level cases and denominator data to LSOAs using a 
# simple population weighting based on GP<>LSOA patient flow data. It then 
# calculates a rate and, if desired, attaches LSOA attribute data as specified by the user.

# See README in https://github.com/AlexGibsonPlymouth/GP_LSOA_Attribution
# Note: https://fingertips.phe.org.uk/api/available_data?area_type_id=7 will list
#       all PHE denominator that refer to GPs

# Run in Terminal as:
# Rscript GP_LSOA_Attribution_BATCH.R <driver.file> <LSOA.attribute.file>
# where <driverfile> & <LSOA.attribute.file> are user-defined .csv files (see below)

# Sends results to sub-directory "./output" (created if doesn't exist) 

# Most data is downloaded from PHE and NHSDigital (addresses might change!), but the 
# script also uses the following local files: (a) a user-defined 'driver file', 
# (b) a pre-prepared 'master list of LSOAs' ('LSOAMasterList.csv'), and (c) a 
# user-defined set of LSOAattribute data (our classification of LSOAs as Coastal/Inland 
# is supplied as a default). These files must be in the same directory as the R script.

# The .csv 'driver file' is used to define, with one record per line:
# (a) PHE dataset ID number (see https://fingertips.phe.org.uk/documents/api_annex.ods),
# (b) whether, given the PHE dataset, the GP<>LSOA population lookup data should 
#     be for males(m), females(f) or all(a) - all I know of are currently (a), 
# (c) the date range for which rates are to be calculated (which must be either a single
#     year or a contiguous range specified as XXXX:YYYY) - appropriate data will 
#     be extracted from the PHE dataset and then the corresponding GP<>LSOA flow data
#     will be downloaded from NHSDigital, and
# (d) whether an average rate is to be calculated for the selected years (y/n).
#     If not separate output for each year in the given range is produced.

# The format must be as illustrated by "DriverFileExample.csv"
# Indicator	Sex	     Years	Average
#       212	  a	 2018:2020     y
#       241	  a	      2015     n
#       262	  a	      2019     n
#       276	  a	 2014:2021     n

# The master list of 32,844 LSOAs is supplied as 'LSOAList.csv'. It will have to
# be updated when the GP<>LSOA lookup tables start using 2021 LSOAs.

# The user-defined file of LSOA-level attribute data must be specified as a .csv
# file (currently LSOA_CoastalFlag.csv). The file must include all 32,844 LSOAs and
# the first column must be LSOAs - with the column name 'LSOA_CODE'

# The script starts by checking that the required files exist and are correctly 
# formatted.

##----

# Preliminary (incl functions and GP<>LSOA file download)----
# A temporary hack to suppress warnings to neaten output 
options(warn=1)

# suppress all output when run from terminal
if (!interactive() ) {sink(file("./junk.txt", "w"), type="message")} 

# Function to test driver file ONLY
TestDriverOnly <- function(x) {
  DriverDF=read.csv(x)
    if(names(DriverDF)[1]=="Indicator" & names(DriverDF)[2]=="Sex" & 
       names(DriverDF)[3]=="Years" & names(DriverDF)[4]=="Average" & 
       nrow(DriverDF>0) & sum(is.na(as.numeric(DriverDF$Indicator)))==0) {
       cat(" The driver file is ",DriverFile,
           "\n No LSOA attribute data will be added to output.",
           "\n >>> If required, abort at next step and repeat command\n >>> with LSOA attribute data filename as final argument.\n\n")
    } else {
      cat("\n >>>>> The driverfile ",x," is badly formatted!  <<< Check driver file.\n\n")
      stop("", call.=FALSE)
    }
    
}

# Function to test driver file
TestDriver<- function(x) {
  DriverDF=read.csv(x)
  if(names(DriverDF)[1]=="Indicator" & names(DriverDF)[2]=="Sex" & 
       names(DriverDF)[3]=="Years" & names(DriverDF)[4]=="Average" & 
       nrow(DriverDF>0)  & sum(is.na(as.numeric(DriverDF$Indicator)))==0) {
       cat(" The driver file is ",x,"\n")
      
    } else {
      cat("\n >>>>> The driverfile ",x," is badly formatted!  <<< Check driver file.\n\n")
      stop("", call.=FALSE)
    }
}

# Function to test LSOA attribute file
TestLSOAAttributeData <- function(x) {
    LSOAAttributeData=read.csv(x)
    
    invisible(if(nrow(LSOAAttributeData)!=32844) {
      cat(" >>>> There are not 32844 LSOAs in ",x,"! <<<< Check the file!\n\n")
      stop("", call.=FALSE)
    } else if (names(LSOAAttributeData)[1]!="LSOA_CODE") {
      cat(" >>>> The LSOA column is not named 'LSOA_CODE'! <<<< Check the file!\n\n")
      stop("", call.=FALSE)
    } else {
      cat(" LSOA attribute file is ",x,"\n\n")
    }
    )
}

# Function to test default LSOA attribute data
TestDefaultLSOAAttributeData <- function(x) {
    LSOAAttributeData=read.csv(x)
    
    invisible(if(nrow(LSOAAttributeData)!=32844) {
      cat(" There are not the expected 32844 LSOAs!  Something has gone wrong with default",
          x," file.\n Perhaps get a new copy from https://github.com/AlexGibsonPlymouth/GP_LSOA_Attribution?\n\n")
      stop("", call.=FALSE)
    } else if (names(LSOAAttributeData)[1]!="LSOA_CODE") {
      cat(" The LSOA column is not named 'LSOA_CODE'!  Something has gone wrong with default ",
          x," file.\n Perhaps get new copy from https://github.com/AlexGibsonPlymouth/GP_LSOA_Attribution?\n\n")
      stop("", call.=FALSE)
    } else {
      cat(" LSOA attribute file is ",x,"\n\n")
    }
    )
}

# Function for prompt and response
UserInput <- function(msg) {
  if (interactive() ) {
    txt <- readline(paste0(msg," "))
  } else {
    cat(msg);
    txt <- readLines("stdin",n=1);
  }
  return(txt)
}

# Function to prompt <Return> to Continue
UserContinue <- function(msg) {
  if (interactive() ) {
    readline(msg)
  } else {
    cat(msg);
    readLines("stdin",n=1);
  }
  return(1)
}

# function to test whether a file is open prior to attempted write
file.opened <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path, 
               open = "w"), 
          silent = TRUE
      )
    )
  )
}

# Function to tidy up temporary files
TidyUp <- function(x) {
  invisible(if (file.exists(x)){
    unlink(x)
  } else {
   #cat(x, " not found.\n")
  })
}

dir.create(file.path(getwd(),"./output"), showWarnings = FALSE)

library(tidyverse)
library(httr)

# For efficiency, initialise by downloading all NHS Digital GP<>LSOA population files----
# This will not happen if file(s) are already in path
# This can be updated as new years' come online
cat("\n Downloading GP<>LSOA population lookup files if not already in path (once only)\n")
if (!file.exists("./GP_LSOA2014.csv")){ cat(" Downloading 2014/15 GP<>LSOA data.\n");
  download.file("https://files.digital.nhs.uk/publicationimport/pub13xxx/pub13932/gp-reg-patients-04-2014-totals-lsoa-alt.csv","./GP_LSOA2014.csv")
}
if (!file.exists("./GP_LSOA2015.csv")){ cat(" Downloading 2015/16 GP<>LSOA data.\n");
  download.file("https://files.digital.nhs.uk/publicationimport/pub17xxx/pub17356/gp-reg-patients-lsoa-alt-tall.csv","./GP_LSOA2015.csv")
}
if (!file.exists("./GP_LSOA2016.csv")){ cat(" Downloading 2016/17 GP<>LSOA data.\n");
  download.file("https://files.digital.nhs.uk/publicationimport/pub20xxx/pub20480/lsoa-alt-format-tall.csv","./GP_LSOA2016.csv")
}
if (!file.exists("./GP_LSOA2017.zip")) { cat(" Downloading 2017/18 GP<>LSOA data.\n");
  download.file("https://files.digital.nhs.uk/publicationimport/pub23xxx/pub23475/gp-reg-pat-prac-lsoa-all-females-males.zip","./GP_LSOA2017.zip")
}
if (!file.exists("./GP_LSOA2018.zip")) { cat(" Downloading 2018/19 GP<>LSOA data.\n");
  download.file("https://files.digital.nhs.uk/62/638799/gp-reg-pat-prac-lsoa-all-females-males.zip","./GP_LSOA2018.zip")
}
if (!file.exists("./GP_LSOA2019.zip")) { cat(" Downloading 2019/20 GP<>LSOA data.\n");
  download.file("https://files.digital.nhs.uk/16/740C9E/gp-reg-pat-prac-lsoa-male-female-apr-19.zip","./GP_LSOA2019.zip")
}
if (!file.exists("./GP_LSOA2020.zip")) { cat(" Downloading 2020/21 GP<>LSOA data.\n");
  download.file("https://files.digital.nhs.uk/93/714E7D/gp-reg-pat-prac-lsoa-male-female-Apr-20.zip","./GP_LSOA2020.zip")
}
if (!file.exists("./GP_LSOA2021.zip")) { cat(" Downloading 2021/22 GP<>LSOA data.\n");
  download.file("https://files.digital.nhs.uk/52/2D964D/gp-reg-pat-prac-lsoa-male-female-Apr-21.zip","./GP_LSOA2021.zip")
}
if (!file.exists("./GP_LSOA2022.zip")) { cat(" Downloading 2022/23 GP<>LSOA data.\n");
  download.file("https://files.digital.nhs.uk/20/64261B/gp-reg-pat-prac-lsoa-male-female-April-22.zip","./GP_LSOA2022.zip")
}
cat("\n\t>>> All GP<>LSOA population lookup files loaded.\n")
#----


#----


# Initial check of command line arguments, check whether user wants to proceed and read data----
cat(("\n\n"))

if(length(args)==0) {
  # read and test basic structure of the default driver and LSOA attribute files
  cat(" No arguemts supplied.  Using defaults specified on lines 228-29 of the R script.\n")
  DriverFile="DriverFileExample.csv"
  LSOAAttributeFile="LSOA_CoastalFlag.csv"
  if (file.exists(DriverFile)){TestDriver(DriverFile)} else {
    cat(" >>>>> Driver file ",DriverFile,
        " not found.\n >>>>> Check command line correct and/or that file exists in current path.\n\n");
    stop("", call.=FALSE)
    }
  if (file.exists(LSOAAttributeFile)){TestDefaultLSOAAttributeData(LSOAAttributeFile)} else {
    cat(" >>>>> LSOA Attribute file ",LSOAAttributeFile,
        " not found.\n >>>>> Check command line correct and/or that file exists in current path.\n\n");
    stop("", call.=FALSE)
  }
  AddAttribute=1
  
} else if (length(args)==1) {
  # read and test basic structure of the driver file (no LSOA attribute file to be used)
  DriverFile=args[1]
  if (file.exists(DriverFile)){TestDriverOnly(DriverFile)} else {
    cat(" >>>>> Driver file ",DriverFile,
        " not found.\n >>>>> Check command line correct and/or that file exists in current path.\n\n");
    stop("", call.=FALSE)
  }
  AddAttribute=0
  
} else if (length(args)==2) {
  # read and test basic structure of user-supplied driver and LSOA attribute files
  DriverFile=args[1]
  LSOAAttributeFile=args[2]
  if (file.exists(DriverFile)){TestDriver(DriverFile)} else {
    cat(" >>>>> Driver file ",DriverFile,
        " not found.\n >>>>> Check command line correct and/or that file exists in current path.\n\n");
    stop("", call.=FALSE)
  }
  if (file.exists(LSOAAttributeFile)){TestLSOAAttributeData(LSOAAttributeFile)} else {
    cat(" >>>>> LSOA Attribute file ",LSOAAttributeFile,
        " not found.\n >>>>> Check command line correct and/or that file exists in current path.\n\n");
    stop("", call.=FALSE)
  }
  AddAttribute=1

} else {
  # Reject if too many arguments as probably means input error
  cat(" Too many arguments supplied. Only two arguments needed")
  stop("", call.=FALSE)
} 

# Check whether user is happy to proceed
Message=" Are these setting OK? (y/n)"
GoResponse=UserInput(Message)
if(GoResponse!="y" & GoResponse!="Y"){
  cat(" User Abort\n\n")
  stop("", call.=FALSE)
} 
#----  

# Check existence of and read in all files needed----
# Although already read in for checking within functions, now read in the driver file 
# and the provided master list of all LSOAs (currently 2011 LSOAs)
if (file.exists(DriverFile)){ DriverDF=read.csv(DriverFile) } else {
  cat(" >>>>> Driver file ",DriverFile,
      " not found.\n >>>>> Check command line correct and/or that file exists in current path.\n\n");
  stop("", call.=FALSE)
}  

if (file.exists("LSOAMasterList.csv")){ LSOAMasterListDF=read.csv("LSOAMasterList.csv") } else {
  cat(" >>>>> 'LSOAMasterList.csv' not found.\n >>>>> This is an essential file.\n >>>>> Check that file exists in current path.\n");
  cat(" >>>>> Perhaps download clean copy from https://github.com/AlexGibsonPlymouth/GP_LSOA_Attribution.\n")
  stop("", call.=FALSE)
}  

# If AddAttribute==1 we need to read-in the LSOA attribute file (and check that it exists!)
if(AddAttribute==1){
  # We have already checked that this exists and is properly formatted, but not yet read it in to program
  if (file.exists(LSOAAttributeFile)){ LSOAAttributeDataDF=read.csv(LSOAAttributeFile) } else {
    cat(" >>>>> 'User request for LSOA Attribute to be added, but requested file (",LSOAAttributeFile,
    ") not found.\n >>>>> This is an essential file.\n >>>>> Check that file exists in current path.\n")
    stop("", call.=FALSE)
  }  
}
#----

  
# MAIN PROGRAM ###########################################################

# The OuterLoop processes each line of the driver file 
for (OuterLoop in 1:nrow(DriverDF)) {
    cat(paste0("\n Processing line ",OuterLoop," of ",nrow(DriverDF)," lines in the driver file\n"))  
    
    # Extract variables from the appropriate line (=OuterLoop) of the driver file----
    TargetIndicator=DriverDF$Indicator[OuterLoop]
    Sex=DriverDF$Sex[OuterLoop]
    Years = unlist(lapply(strsplit(DriverDF$Years[OuterLoop], ":"), function(x) Reduce(`:`, as.numeric(x))))
    YearsCount=length(Years)
    Ave=DriverDF$Average[OuterLoop]
    #----
  
    # Extract QOF data from PHE Fingertips site----
    URL=paste0("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=",
             TargetIndicator,"&child_area_type_id=7&parent_area_type_id=15");
    cat("\n Downloading PHE data from https://fingertips.phe.org.uk/ ......\n .....")
    Download=GET(URL)
    #----
    
    # Abort just this line in the driver file if download fails (i.e. if http status code !=200)----
    if(status_code(Download)!=200){ 
      cat(paste0("\n\n Attemped download of PHE Indicator",TargetIndicator," failed.\n"))
      cat(paste0(" HTTP Response = ",status_code(Download),"\n"))
      cat(" Could be due to timeout, or check list at https://fingertips.phe.org.uk/documents/api_annex.ods.\n")
      cat(" Proceeding to next line in driver file (if applicable)\n\n")
      next
    }
    #----
    
    # Extract body of downloaded PHE file and save as a data.frame
    DataDF=content(Download,"parsed")
        
    # Test that the file downloaded from PHE contains the columns that will be needed----
    # Should probably put more tests in here to ensure we don't get rubbish!
    ColList=names(DataDF)
    Missing=setdiff(c("Indicator Name","Area Code","Area Name","Time period",
                      "Count","Denominator","Area Type"),ColList)
    Test1=sum(DataDF$Count,na.rm=TRUE)
    Test2=sum(DataDF$Denominator,na.rm=TRUE)
    
    if (length(Missing)!=0){
      cat(paste("\n ERROR: PHE file is missing essential columns: ",Missing))
      cat(" Proceeding to next line in driver file (if applicable)\n\n")
      next
    }
    
    if (Test1==0 | Test2==0 | Test1>Test2){
      cat("\n ERROR: Count and/or denominator data zero or count>denominator!\n")
      cat(" Proceeding to next line in driver file (if applicable)\n\n")
      next
    }
    #----

    # Define output name prefix
    FilenamePrefix = paste0("./output/PHE_",DataDF$'Indicator ID'[1],"_")

    # Report on variable selected and check OK
    cat(paste0(" >>>> Indicator selected = ",TargetIndicator," > ",DataDF$`Indicator Name`[1],"\n\n"))

    # Extract variables in which we are interested (numerator & denominator for GPs
    # and which not 2013/14 - because no GP-LSOA lookup until April 2014)
    ExtractDF = DataDF %>% filter(`Area Type`=="GPs" & `Time period`!="2013/14") %>%
      select(`Area Code`,`Area Name`,`Time period`,Count,Denominator) %>%
      mutate(Year=as.numeric(substring(`Time period`,1,4)),Rate=Count/Denominator)
    names(ExtractDF)[1:2]=c("PRACTICE_CODE","ORG_NAME")

    # Now attribute to LSOA data
    # Loop through years
    NoSave=0
    for (InnerLoop in Years) {
      # Because of different formats (.csv & ..zip) we need to do different stuff for 2014-16 and 2017-2021
      
      # For years 2014-2016
      if (InnerLoop<=2016) {
            
            GPLSOAFILE=paste0("./GP_LSOA",InnerLoop,".csv")
            LongDF=read.csv(GPLSOAFILE)
            
            # Attach GP Rate to all GP<>LSOA lines, aggregate by LSOA and calculate LSOA rates
            cat("\t>>>> Attributing GP data for ",InnerLoop," to LSOAs...\n")
            DateExtractDF = ExtractDF %>% filter(Year==InnerLoop) %>% select(PRACTICE_CODE,Rate)
            
            # Sometimes no data for the selected year, but shall only warn!
            if(nrow(DateExtractDF)==0){cat("WARNING: No data for year ",InnerLoop,"\n")}
            
            LSOADF = LongDF %>% left_join(DateExtractDF,by="PRACTICE_CODE") %>% 
              mutate(MaleCases=Male.Patients*Rate, FemaleCases=Female.Patients*Rate,
                     AllCases=All.Patients*Rate) %>%
              group_by(LSOA_CODE) %>%
              summarise(MaleCases = sum(MaleCases,na.rm=TRUE),MaleDenom=sum(`Male.Patients`,na.rm=TRUE),
                        FemaleCases = sum(FemaleCases,na.rm=TRUE),FemaleDenom=sum(`Female.Patients`,na.rm=TRUE),
                        OverallCases = sum(AllCases,na.rm=TRUE),OverallDenom=sum(`All.Patients`,na.rm=TRUE)) %>%
              arrange(LSOA_CODE)      
            
            if (Sex=="m" | Sex=="M"){
              LSOADF = LSOADF %>% select(LSOA_CODE,MaleCases,MaleDenom)
            } else if (Sex=="f" | Sex=="F"){
              LSOADF = LSOADF %>% select(LSOA_CODE,FemaleCases,FemaleDenom)
            } else if (Sex=="a" | Sex=="A"){
              LSOADF = LSOADF %>% select(LSOA_CODE,OverallCases,OverallDenom)
            } else {
              cat("Wrong value for sex\n")
              # Abort >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            }
            names(LSOADF)[2:3]=c("Cases","Denominator")
      }
      # End of dealing with years 2014-2016
      
      # For PHE files dated 2017 to 2021
      if (InnerLoop>2016) {
           GPLSOAFILE=paste0("./GP_LSOA",InnerLoop,".zip")
        
          # Warning about 2017 & 2018 data
          if (InnerLoop==2017) {
            cat("\t GP<>LSOA lookup data for 2017 is problematic.\n")
            cat("\t See README on https://github.com/AlexGibsonPlymouth/GP_LSOA_Attribution\n\t ....")
          } else if (InnerLoop==2018) {
            cat("\t GP<>LSOA lookup data for 2018 is problematic.\n")
            cat("\t .... See README on https://github.com/AlexGibsonPlymouth/GP_LSOA_Attribution\n\t ....")
          } 
        
          DateExtractDF = ExtractDF %>% filter(Year==InnerLoop) %>%
            select(PRACTICE_CODE,Rate)
          # Sometimes no data for the selected year > so warning to console
          if(nrow(DateExtractDF)==0){cat("WARNING: No data for year ",InnerLoop,"\n")}
          
          unzip(GPLSOAFILE,overwrite=TRUE)
          cat("\t>>>> Attributing GP data for ",InnerLoop," to LSOAs...\n")
          
          if(Sex=="m" | Sex=="M"){
            LongDF=read.csv("./gp-reg-pat-prac-lsoa-male.csv")
            names(LongDF)[7]="Number.of.Patients"
            LSOADF = LongDF %>% left_join(DateExtractDF,by="PRACTICE_CODE") %>% 
              mutate(Cases = Number.of.Patients*Rate) %>% 
              group_by(LSOA_CODE) %>%
              summarise(Cases = sum(Cases,na.rm=TRUE),
                        Denominator=sum(Number.of.Patients,na.rm=TRUE)) 
              select(LSOA_CODE,Cases,Denominator) %>% arrange(LSOA_CODE)      
          } else if (Sex=="f" | Sex=="F") {
            LongDF=read.csv("./gp-reg-pat-prac-lsoa-female.csv")
            names(LongDF)[7]="Number.of.Patients"
            LSOADF = LongDF %>% left_join(DateExtractDF,by="PRACTICE_CODE") %>% 
              mutate(Cases = Number.of.Patients*Rate) %>% 
              group_by(LSOA_CODE) %>%
              summarise(Cases = sum(Cases,na.rm=TRUE),
                        Denominator=sum(Number.of.Patients,na.rm=TRUE)) %>%
              select(LSOA_CODE,Cases,Denominator) %>% arrange(LSOA_CODE)  
           } else if (Sex=="a" | Sex=="A"){
            LongDF=read.csv("./gp-reg-pat-prac-lsoa-all.csv")
            names(LongDF)[7]="Number.of.Patients"
            LSOADF = LongDF %>% left_join(DateExtractDF,by="PRACTICE_CODE") %>% 
              mutate(Cases = Number.of.Patients*Rate) %>% 
              group_by(LSOA_CODE) %>%
              summarise(Cases = sum(Cases,na.rm=TRUE),
                        Denominator=sum(Number.of.Patients,na.rm=TRUE)) %>%
              select(LSOA_CODE,Cases,Denominator) %>% arrange(LSOA_CODE) 
          } else {
            cat("Wrong value for sex\n")
            # Abort >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          }
          
        }
      # End of dealing with >2016  
  

      # Different actions depending on whether user chose to extract an average rate
      if (Ave=="n") { 

        # if want data for single year(s) (i.e. not an average)
        LSOADF = LSOADF %>% mutate(Rate = Cases/Denominator) %>% select(LSOA_CODE,Rate)
        FilenameSuffix=paste0(InnerLoop,".csv")
        
        # Produce required output
        #filter out non-English LSOAs----
        LSOADF$Country=substring(LSOADF$LSOA_CODE,1,1)
        LSOADF = LSOADF %>% filter(LSOADF$Country=="E") %>% select(-Country)

        names(LSOADF)[names(LSOADF) == "Rate"] = DataDF$`Indicator Name`[1]
 

        # Add user-defined LSOA attribute data (if user selected)
        if (AddAttribute==1) {
          LSOADF = left_join(LSOADF,LSOAAttributeDataDF,by="LSOA_CODE")
          cat("\t Producing LSOA counts, denominators & rates and LSOA attribute data.\n")
        } else {
          cat("\t Producing LSOA counts, denominators & rates (no LSOA attribute data).\n")
        }
 
        # save file for just this year, having first checked not open----
        while(1==1) {
          if(file.opened(paste0(FilenamePrefix,FilenameSuffix))) {
            cat("\n\n File to be written (",paste0(FilenamePrefix,FilenameSuffix)," is open - probably in Excel\n")
            Message="\n >>> Close file, then press enter (if stuck Ctrl-C to abort Rscript)"
            Continue = UserContinue(Message)
          } else {
            write.csv(LSOADF,paste0(FilenamePrefix,FilenameSuffix),row.names=FALSE)
            cat("\t >>>>>>>>> Output written to ",paste0(FilenamePrefix,FilenameSuffix),"\n\n")
            break
          }
        }
    
        # Flag then fact that don't need to save for a final time!
        NoSave=1
        
      } else { 

        # if we want an average for a set of years
        # deal differently with first, middle and last years' data 
        
        if (InnerLoop==Years[YearsCount]){                         
          # for the last year in a range of years, so sum and calculate rate
          # note that year used as name suffix for Cases and Denom
          names(LSOADF)[2:3]=c(paste0("Cases",InnerLoop),paste0("Denom",InnerLoop))
          
          if (YearsCount>1){
            # If dealing with last of multiple year to then get 'average'
            FilenameSuffix=paste0("Ave_",min(Years),"_",max(Years),".csv")
            Cases = left_join(Cases,LSOADF[c(1:2)])
            Denoms = left_join(Denoms,LSOADF[c(1,3)])
            # And calculate rate
            Cases = Cases %>% replace(is.na(.), 0) %>%
              mutate(TotCases = rowSums(across(where(is.numeric)))) 
            Denoms = Denoms %>% replace(is.na(.), 0) %>% 
              mutate(TotDenoms = rowSums(across(where(is.numeric)))) 
            LSOADF = left_join(Cases,Denoms) %>% mutate(Rate = TotCases/TotDenoms)
            
          } else {
            # if dealing with a single year for which need to save Cases & Denom as well as Rate
            FilenameSuffix=paste0("WithData_",min(Years),".csv")
            Cases=LSOADF[c(1:2)]
            Denoms=LSOADF[c(1,3)]
            # And calculate rate
            LSOADF = left_join(Cases,Denoms) %>% mutate(Rate = .[[2]]/.[[3]])
          } 
          # And ready to save file
          
        } else if (InnerLoop==Years[1]){                                    
          # for the first year in a range of loop
          # note that year used as name suffix for Cases and Denom
          names(LSOADF)[2:3]=c(paste0("Cases",InnerLoop),paste0("Denom",InnerLoop))
          Cases = left_join(LSOAMasterListDF,LSOADF[c(1:2)])
          Denoms = left_join(LSOAMasterListDF,LSOADF[c(1,3)])
          
        } else {
          # for all years in range except 1st and last
          # note that year used as name suffix for Cases and Denom
          names(LSOADF)[2:3]=c(paste0("Cases",InnerLoop),paste0("Denom",InnerLoop))
          Cases = left_join(Cases,LSOADF[c(1:2)])
          Denoms = left_join(Denoms,LSOADF[c(1,3)])
        }

      }
    }
    # End of looping through years specified by line on driver file (InnerLoop)
    
    # Now produce required output as long as NoSave==0
    if(NoSave==0){
        #filter out non-English LSOAs and extract basic stats for reporting----
        LSOADF$Country=substring(LSOADF$LSOA_CODE,1,1)
        LSOADF = LSOADF %>% filter(LSOADF$Country=="E") %>% select(-Country)
        
        names(LSOADF)[names(LSOADF) == "Rate"] = DataDF$`Indicator Name`[1]
        #----
      
        # Add user-defined LSOA attribute data (if user selected)
          if (AddAttribute==1) {
          LSOADF = left_join(LSOADF,LSOAAttributeDataDF,by="LSOA_CODE")
          cat("\n\t Producing annual counts, denominator and average rates for LSOAs for ",DriverDF$Years[OuterLoop]," - plus LSOA attribute data\n")
          } else {
          cat("\n\t Producing annual counts, denominator and average rates for LSOAs for ",DriverDF$Years[OuterLoop]," (no LSOA attribute data)\n")
          }
    
        # save file for just this year, having first checked not open----
        while(1==1) {
          if(file.opened(paste0(FilenamePrefix,FilenameSuffix))) {
            cat("\n\n File to be written (",paste0(FilenamePrefix,FilenameSuffix)," is open - probably in Excel\n")
            Message="\n >>> Close file, then press enter (if stuck Ctrl-C to abort Rscript)"
            Continue = UserContinue(Message)
          } else {
            write.csv(LSOADF,paste0(FilenamePrefix,FilenameSuffix),row.names=FALSE)
            cat("\t >>>>>>>>> Output written to ",paste0(FilenamePrefix,FilenameSuffix),"\n\n")
            break
          }
        }
        #----
    }
    
#  }
#  # End of looping through years specified by line on driver file (InnerLoop)
    
} # end of OuterLoop that processes each line of the driver file 

# Tidy up all temporary files that might have been produced----
cat(" >>> Tidying files\n\n")
cat("  junk.txt cannot be deleted.\n")
cat("  It will get over-written so doesn't cause problems.\n  Delete manually if bothered by it!\n\n")

TidyUp("./Long.csv")
TidyUp("./Long.zip")
TidyUp("./gp-reg-pat-prac-lsoa-all.csv")
TidyUp("./gp-reg-pat-prac-lsoa-male.csv")
TidyUp("./gp-reg-pat-prac-lsoa-female.csv")
TidyUp("./Long2019.zip")
TidyUp("./Long2020.zip")
TidyUp("./Long2021.zip")
#----


# SCRIPT ENDS



