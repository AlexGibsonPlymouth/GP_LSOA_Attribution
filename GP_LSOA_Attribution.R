
# GP_LSOA_Attribution.R

# To run from terminal is easiest (assuming $PATH for R has been set)
# Thus, in Windows terminal:

#   cd <to directory in which GP_LSOA_Attribution.R resides 
#   Rscript GP_LSOA_Attribution.R

#   [All output sent to subdirectory ./output > which will 
#    be created in not already present.]


# Description ----
# This script requests user input defining which PHE Fingertips dataset is to be
# processed. Note that a full list of PHE indicators and their codes is listed in
# the 'annex' at https://fingertips.phe.org.uk/profile/guidance/supporting-information/api
# (Direct Link = https://fingertips.phe.org.uk/documents/api_annex.ods)

# The script should work with any GP indicator dataset with count 
# and Denominator columns, as long as it also has `Area Code`,`Area Name`,
# and `Time period` columns.  The script might need to be tweaked to work with
# other GP Indicator datasets if they have a different format, but 
# I know it works with the following PHE Indicator datasets.

# Risk Factors: 91280, 219, 91248, 91262, 90616, 90619,  
# Diabetes: 241, 
# Heart: 273, 262, 90999, 91001, 91005,
# Kidney: 258
# Stroke: 212, 280, 91013, 92594, 
# Cancer: 276,

# It then links GP<>LSOA patient flow data with the GP-level dataset and calculates 
# a population-weighted attribution to 2011 LSOAs. (Note it will have to be updated  
# to use 2021 LSOAs.)

# Depending on user input, the script produces output for all years 2014/15 to
# 2021/22 or just for individual years.

# Finally, the script attaches a variety of LSOA attributes to each LSOA-level 
# output file, including whether the LSOA is coastal - using the definition used 
# in the 2021 CMO Report (https://tinyurl.com/yn847swd)

# The script has been written to minimise the amount of harddisk space used. This 
# does mean it is slower than it might be and will have to be change if I write a
# batch script to deal with multple PHS indicators as it would make unnecessary
# multiple calls to re-download the GP<>LSOA population data from the ONS


# Note that the fingertips.R package doesn't seem to be available anymore

#----

### Preliminary ----

# Initial hack to suppress all output if run from terminal
if (!interactive() ) {
  sink(file("./junk.txt", "w"), type="message")
  cat("\n\n This script links GP<>LSOA patient flow data with user-specified\n")
  cat(" GP-level PHE data and calculates population-weighted statitsics\n")
  cat(" for all 2011 LSOAs. (It will need updateing to use 2021 LSOAs\n")
  cat("\n See comments in R script for more details.\n ....")
  dir.create("./output", showWarnings = FALSE)
  #cat("Not interactive!!!!\n\n")
} else {
  # Define working directory
  setwd("H:/000_Coastal_Related/03_QOF_Practice Attribution")
  dir.create(file.path(getwd(),"./output"), showWarnings = FALSE)
  #cat("Interactive!!!!\n\n")
  #Sys.sleep(10)
}  

library(tidyverse)
library(httr)

  #----
  
  # Define list of http address for GP<>LSOA population files ----
  # There isn't an API for downloading NHS Digital data. so we will downloaded
  # .csv and .zip files directly and then extracting required data
  
  GPLSOAFileList=data.frame(Year=c(2014,2015,2016,2017,2018,2019,2020,2021,2022),
                            URL=c("https://files.digital.nhs.uk/publicationimport/pub13xxx/pub13932/gp-reg-patients-04-2014-totals-lsoa-alt.csv",
                                  "https://files.digital.nhs.uk/publicationimport/pub17xxx/pub17356/gp-reg-patients-lsoa-alt-tall.csv",
                                  "https://files.digital.nhs.uk/publicationimport/pub20xxx/pub20480/lsoa-alt-format-tall.csv",
                                  "https://files.digital.nhs.uk/publicationimport/pub23xxx/pub23475/gp-reg-pat-prac-lsoa-all-females-males.zip",
                                  "https://files.digital.nhs.uk/62/638799/gp-reg-pat-prac-lsoa-all-females-males.zip",
                                  "https://files.digital.nhs.uk/16/740C9E/gp-reg-pat-prac-lsoa-male-female-apr-19.zip",
                                  "https://files.digital.nhs.uk/93/714E7D/gp-reg-pat-prac-lsoa-male-female-Apr-20.zip",
                                  "https://files.digital.nhs.uk/52/2D964D/gp-reg-pat-prac-lsoa-male-female-Apr-21.zip",
                                  "https://files.digital.nhs.uk/20/64261B/gp-reg-pat-prac-lsoa-male-female-April-22.zip"))
  
  # End of GPLSOAFileList definition----
  
  # Define function to manage terminal vs interactive user input----
  UserInput <- function(msg) {
    if (interactive() ) {
      txt <- readline(paste0(msg," "))
    } else {
      cat(msg);
      txt <- readLines("stdin",n=1);
    }
    return(txt)
  }
 
  # Define functions to manage terminal vs interactive user input----
  UserContinue <- function(msg) {
    if (interactive() ) {
      readline(msg)
    } else {
      cat(msg);
      readLines("stdin",n=1);
    }
    return(1)
  }
  
  # function to test whether a file is open (in which case the write would fail)
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
  
  # function to tidy up temporary files
  TidyUp <- function(x) {
    invisible(if (file.exists(x)){
    unlink(x)
  } else {
    #cat(x, " not found.\n")
  })
  }
  
  #----
  
  
  # Extract QOF data from PHE Fingertips site
  
  
  # Request user input
  cat("\n\n The PHE Indicator ID needs to be specified. (e.g. 212, 258 or 276)")
  TargetIndicator=0
  
  while (TargetIndicator!=-9) {
    Message="\n\n Enter PHE Indicator ID (-9 to to Finish):"
    TargetIndicator=UserInput(Message)
    if(TargetIndicator==-9){
      cat("\n\n Program Finished.")
      break
    } else {
        cat("\n Searching PHE for Indictor ",TargetIndicator, " dataset\n ....\n")
    }
    URL=paste0("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=",
               TargetIndicator,"&child_area_type_id=7&parent_area_type_id=15");
    cat("\n Downloading from https://fingertips.phe.org.uk/ ......\n .....")
    Download=GET(URL)
    
      if(status_code(Download)!=200){  # Only proceed if download was successful
        cat(paste0("\n\n Attemped download of PHE Indicator",TargetIndicator," failed.\n"))
        cat(paste0("\n\n HTTP Response = ",status_code(Download),"\n"))
        cat(paste0("\n Check list at https://fingertips.phe.org.uk/documents/api_annex.ods.\n"))
        cat("\n Try again!\n")
        break
      } else {
        # Extract body from download   
        DataDF=content(Download,"parsed")
        
        # Test that file contains the columns that will be needed
        # Should probably put more tests in here to ensure we don't get rubbish!
        ColList=names(DataDF)
        #cat(ColList[6])
        Missing=setdiff(c("Indicator Name","Area Code","Area Name","Time period",
                          "Count","Denominator","Area Type"),ColList)
        Test1=sum(DataDF$Count,na.rm=TRUE)
        Test2=sum(DataDF$Denominator,na.rm=TRUE)
        if (length(Missing)!=0){
          cat(paste("\n ERROR: PHE file is missing essential columns: ",Missing))
          break
        }
        if (Test1==0 | Test2==0 | Test1>Test2){
          cat("\n ERROR: Count and/or denominator data zero or count>denominator!\n")
          break
        }
        
        cat("\n\n Note: GP>LSOA population data does not include age data so if the indicator\n")
        cat("       is for a specific age range it is presumed that this GP cohort is distributed\n")
        cat("       between LSOAs in the same way as the population as a whole!\n")
        cat("       THIS COULD CAUSE PROBLEMS IN E.G. AREAS WITH STUDENT PRACTICES\n")
        # Create Output filename prefix based on PHE ID
        FilenamePrefix = paste0("./output/PHE_",DataDF$'Indicator ID'[1],"_")
        
        # Report on variable selected and check OK
        cat(paste0("\n\n Indicator selected = ",TargetIndicator," > ",DataDF$`Indicator Name`[1],"\n"))
       
        Response="Waiting"
        while (Response!="Y" & Response!="y" & Response!="N" & Response!="n"){
          Message="\n\n Happy with this indicator? (y/n):"
          Response = UserInput(Message)
           if(Response!="Y" & Response!="y" & Response!="N" & Response!="n"){
            cat("\n Answer y/n!\n")
          }
        }
        
        if (Response=="N" | Response=="n"){
          cat("\n\n User Abort!!!\n\n")
          break                            # aborts loop if use says n or N
        }
      
        cat("\n\n Some indicators relate to just males or females.\n")
        cat(" The GP<>LSOA population data does differentiate between males and females.\n")
        cat(" Select appropriate GP<>LSOA look up data.\n\n")
        Message=" Male (m), female (f) or total population (just return):"
        Sex = UserInput(Message)
        if(Sex=="m" | Sex=="M"){
          cat("\n >>>> Working with *male* only population data")
        } else if (Sex=="f" | Sex=="F") {
          cat("\n >>>> Working with *female* only population data")
        } else {
          cat("\n >>>> Working with default *total* population data")
        }
      
    # Extract variables in which we are interested (numerator & denominator for GPs
    # and which not 2013/14 - because no GP-LSOA lookuuntil April 2014)
    ExtractDF = DataDF %>% filter(`Area Type`=="GPs" & `Time period`!="2013/14") %>%
      select(`Area Code`,`Area Name`,`Time period`,Count,Denominator) %>%
      mutate(Year=as.numeric(substring(`Time period`,1,4)),Rate=Count/Denominator)
    names(ExtractDF)[1:2]=c("PRACTICE_CODE","ORG_NAME")

    # Now attribute to LSOA data
    # Download NHS Digital data on patients registered at GP Practices
    # We will download April data starting with April 2014, and will
    # link April 2014 pop data with QOF data for 2014/15, etc.
    
    # User defines year (or all)
    BreakFlag=0
    AveFlag=0
    while (1==1) {
        Message="\n\n Select Year between 2014 and 2021,\n   or 'All' for all individual years (2014/15 - 2021/22)\n   or 'Ave' for average of 2019/20 - 2021/22\n   or -9 to abort:"
        YearSelect = UserInput(Message)
        cat("\n Got .....", YearSelect,"\n\n")
        if (YearSelect=="All"){
          YearSelect=c(2014:2021)
          break
        } else if (YearSelect=="Ave" | YearSelect=="AVE" | YearSelect=="ave"){
          YearSelect=c(2019:2021)
          AveFlag=1
          break          
        } else if (YearSelect!=2014 & YearSelect!=2015 & YearSelect!=2016 &
                   YearSelect!=2017 & YearSelect!=2018 & YearSelect!=2019 &
                   YearSelect!=2020 & YearSelect!=2021) {
          cat("\t Input Error!")
        } else if (YearSelect==-9){
          BreakFlag=1
          break
        } else {
          break
        }
    }
    if (BreakFlag==1){
      cat(" User Abort!\n")
      break
    } else if (AveFlag==1){
      #cat(" User selected Ave!\n")
      #break
    }
    
    if (AveFlag==0){
    # WHAT TO DO IF NOT AVE
    Loops=length(YearSelect)
    for (j in 1:Loops){
      FilenameSuffix=paste0(YearSelect[j],".csv")
      if (YearSelect[j]>2016) {
        cat(paste0("\n Downloading GP<>LSOA lookup for ",YearSelect[j],"...\n ...."))
        if (YearSelect[j]==2017) {
          cat(" GP<>LSOA lookup data for 2017 is problematic.\n")
          cat(" See README on https://github.com/AlexGibsonPlymouth/GP_LSOA_Attribution\n")
        } else if (YearSelect[j]==2018) {
          cat(" GP<>LSOA lookup data for 2018 is problematic.\n")
          cat(" See README on https://github.com/AlexGibsonPlymouth/GP_LSOA_Attribution\n")
        }
        DateExtractDF = ExtractDF %>% filter(Year==as.numeric(YearSelect[j])) %>%
          select(PRACTICE_CODE,Rate)
        
        YearSelectDF=data.frame(Year=as.numeric(YearSelect[j]))
        URL=left_join(YearSelectDF,GPLSOAFileList,by="Year")
        download.file(URL$URL[1],"./Long.zip")
        
        unzip("./Long.zip",overwrite=TRUE)
        cat("\t ... Attributing to LSOAs...\n ....")
        
        if(Sex=="m" | Sex=="M"){
          LongDF=read.csv("./gp-reg-pat-prac-lsoa-male.csv")
          names(LongDF)[7]="Number.of.Patients"
          LSOADF = LongDF %>% left_join(DateExtractDF,by="PRACTICE_CODE") %>% 
            mutate(MaleCases = Number.of.Patients*Rate) %>% 
            group_by(LSOA_CODE) %>%
            summarise(MaleCases = sum(MaleCases,na.rm=TRUE),
                      MaleDenom=sum(Number.of.Patients,na.rm=TRUE)) %>%
            mutate(MaleRate = MaleCases/MaleDenom) %>% 
            select(LSOA_CODE,MaleRate) %>% arrange(LSOA_CODE)      
        } else if (Sex=="f" | Sex=="F") {
          LongDF=read.csv("./gp-reg-pat-prac-lsoa-female.csv")
          names(LongDF)[7]="Number.of.Patients"
          LSOADF = LongDF %>% left_join(DateExtractDF,by="PRACTICE_CODE") %>% 
            mutate(FemaleCases = Number.of.Patients*Rate) %>% 
            group_by(LSOA_CODE) %>%
            summarise(FemaleCases = sum(FemaleCases,na.rm=TRUE),
                      FemaleDenom=sum(Number.of.Patients,na.rm=TRUE)) %>%
            mutate(FemaleRate = FemaleCases/FemaleDenom) %>% 
            select(LSOA_CODE,FemaleRate) %>% arrange(LSOA_CODE)      
        } else {
          LongDF=read.csv("./gp-reg-pat-prac-lsoa-all.csv")
          names(LongDF)[7]="Number.of.Patients"
          LSOADF = LongDF %>% left_join(DateExtractDF,by="PRACTICE_CODE") %>% 
            mutate(OverallCases = Number.of.Patients*Rate) %>% 
            group_by(LSOA_CODE) %>%
            summarise(OverallCases = sum(OverallCases,na.rm=TRUE),
                      OverallDenom=sum(Number.of.Patients,na.rm=TRUE)) %>%
            mutate(OverallRate = OverallCases/OverallDenom) %>%
            select(LSOA_CODE,OverallRate) %>% arrange(LSOA_CODE)      
        }
      } else {
        # For years 2014-2016
        cat(paste0(" Downloading GP<>LSOA lookup for ",YearSelect[j]," ...\n...."))
        YearSelectDF=data.frame(Year=as.numeric(YearSelect[j]))
        URL=left_join(YearSelectDF,GPLSOAFileList,by="Year")
        download.file(URL$URL[1],"./Long.csv")
        LongDF=read.csv("./Long.csv")
        #head(LongDF)
        # Attach GP Rate to all GP<>LSOA lines, aggregate by LSOA and calculate LSOA rates
        cat("\t Attributing to LSOAs...\n....")
        DateExtractDF = ExtractDF %>% filter(Year==as.numeric(YearSelect[j])) %>% select(PRACTICE_CODE,Rate)
        LSOADF = LongDF %>% left_join(DateExtractDF,by="PRACTICE_CODE") %>% 
          mutate(MaleCases = `Male.Patients`*Rate, FemaleCases = `Female.Patients`*Rate,
                 AllCases = `All.Patients`*Rate) %>% 
          group_by(LSOA_CODE) %>%
          summarise(MaleCases = sum(MaleCases,na.rm=TRUE),MaleDenom=sum(`Male.Patients`,na.rm=TRUE),
                    FemaleCases = sum(FemaleCases,na.rm=TRUE),FemaleDenom=sum(`Female.Patients`,na.rm=TRUE),
                    OverallCases = sum(AllCases,na.rm=TRUE),OverallDenom=sum(`All.Patients`,na.rm=TRUE)) %>%
          mutate(MaleRate = MaleCases/MaleDenom, FemaleRate = FemaleCases/FemaleDenom,
                 OverallRate = OverallCases/OverallDenom) %>% arrange(LSOA_CODE)      
        
        if (Sex=="m" | Sex=="M"){
            LSOADF = LSOADF %>% select(LSOA_CODE,MaleRate)
        } else if (Sex=="f" | Sex=="F"){
            LSOADF = LSOADF %>% select(LSOA_CODE,FemaleRate)
        } else {
            LSOADF = LSOADF %>% select(LSOA_CODE,OverallRate)
        }
      }
        
      names(LSOADF)[2]=DataDF$`Indicator Name`[1]
              
        # filter out non-English LSOAs
        LSOADF$Country=substring(LSOADF$LSOA_CODE,1,1)
        LSOADF = LSOADF %>% filter(LSOADF$Country=="E") %>% select(c(1,2))
        
        # Report on how many LSOAs have data
        LSOAVals = LSOADF %>% filter(!is.na(.[[2]]) & .[[2]]>0) %>% select(2)
        LSOACount = nrow(LSOAVals)
        LSOAMinVal = min(LSOAVals)
        LSOAMaxVal = max(LSOAVals)
 
        
        # save file. having first checked not open!
        while(1==1)
          if(file.opened(paste0(FilenamePrefix,FilenameSuffix))) {
             cat("\n\n File to be written (",paste0(FilenamePrefix,FilenameSuffix)," is open - probably in Excel\n")
             Message="\nClose file, then press enter (if stuck Ctrl-C to abort Rscript) "
             Continue = UserContinue(Message)
          } else {
             write.csv(LSOADF,paste0(FilenamePrefix,FilenameSuffix),row.names=FALSE)
             cat("\t written to ",paste0(FilenamePrefix,FilenameSuffix),"\n")
             cat(paste0(" Data contains ",LSOACount,
                        " LSOAs (expected 32844) with range ",
                        sprintf(LSOAMinVal,fmt='%#.4f')," - ",
                        sprintf(LSOAMaxVal,fmt='%#.4f'),"\n\n"))
             break
          }

    } # end j loop
    } else {
      # WHAT TO DO IF AVE
      
        FilenameSuffix="RecentAve.csv"
        cat(paste0(" Downloading GP<>LSOA lookup files\n ...."))
        ExtractDF2019 = ExtractDF %>% filter(Year==2019) %>%
          select(PRACTICE_CODE,Rate)
        ExtractDF2020 = ExtractDF %>% filter(Year==2020) %>%
          select(PRACTICE_CODE,Rate)
        ExtractDF2021 = ExtractDF %>% filter(Year==2021) %>%
          select(PRACTICE_CODE,Rate)
        
        # Download 2019 data
        cat(paste0(" . 1 ."))
        YearSelectDF2019=data.frame(Year=2019)
        URL2019=left_join(YearSelectDF2019,GPLSOAFileList,by="Year")
        download.file(URL2019$URL[1],"./Long2019.zip")
        unzip("./Long2019.zip",overwrite=TRUE)
        if(Sex=="m" | Sex=="M"){
          LongDF2019=read.csv("./gp-reg-pat-prac-lsoa-male.csv")
          names(LongDF2019)[7]="Number.of.Patients"
        } else if (Sex=="f" | Sex=="F") {
          LongDF2019=read.csv("./gp-reg-pat-prac-lsoa-female.csv")
          names(LongDF2019)[7]="Number.of.Patients"
        } else {
          LongDF2019=read.csv("./gp-reg-pat-prac-lsoa-all.csv")
          names(LongDF2019)[7]="Number.of.Patients"
        }
        
        # Download 2020 data
        cat(paste0(" . 2 ."))
        YearSelectDF2020=data.frame(Year=2020)
        URL2020=left_join(YearSelectDF2020,GPLSOAFileList,by="Year")
        download.file(URL2020$URL[1],"./Long2020.zip")
        unzip("./Long2020.zip",overwrite=TRUE)
        if(Sex=="m" | Sex=="M"){
          LongDF2020=read.csv("./gp-reg-pat-prac-lsoa-male.csv")
          names(LongDF2020)[7]="Number.of.Patients"
        } else if (Sex=="f" | Sex=="F") {
          LongDF2020=read.csv("./gp-reg-pat-prac-lsoa-female.csv")
          names(LongDF2020)[7]="Number.of.Patients"
        } else {
          LongDF2020=read.csv("./gp-reg-pat-prac-lsoa-all.csv")
          names(LongDF2020)[7]="Number.of.Patients"
        }
        
        # Download 2021 data
        cat(paste0(" . 3 ."))
        YearSelectDF2021=data.frame(Year=2021)
        URL2021=left_join(YearSelectDF2021,GPLSOAFileList,by="Year")
        download.file(URL2021$URL[1],"./Long2021.zip")
        unzip("./Long2021.zip",overwrite=TRUE)
        if(Sex=="m" | Sex=="M"){
          LongDF2021=read.csv("./gp-reg-pat-prac-lsoa-male.csv")
          names(LongDF2021)[7]="Number.of.Patients"
        } else if (Sex=="f" | Sex=="F") {
          LongDF2021=read.csv("./gp-reg-pat-prac-lsoa-female.csv")
          names(LongDF2021)[7]="Number.of.Patients"
        } else {
          LongDF2021=read.csv("./gp-reg-pat-prac-lsoa-all.csv")
          names(LongDF2021)[7]="Number.of.Patients"
        }
        
        cat("\t ... Attributing to LSOAs...\n ....")
        
        # Aggregate Case and Denoms for each year 
        # 2019
        LSOADF2019 = LongDF2019 %>% left_join(ExtractDF2019,by="PRACTICE_CODE") %>% 
          mutate(Cases = Number.of.Patients*Rate) %>% 
          group_by(LSOA_CODE) %>%
          summarise(Cases19 = sum(Cases,na.rm=TRUE),
                    Denom19=sum(Number.of.Patients,na.rm=TRUE)) %>%
           select(LSOA_CODE,Cases19,Denom19) %>% arrange(LSOA_CODE)    
        # 2020
        LSOADF2020 = LongDF2020 %>% left_join(ExtractDF2020,by="PRACTICE_CODE") %>% 
          mutate(Cases = Number.of.Patients*Rate) %>% 
          group_by(LSOA_CODE) %>%
          summarise(Cases20 = sum(Cases,na.rm=TRUE),
                    Denom20=sum(Number.of.Patients,na.rm=TRUE)) %>%
          select(LSOA_CODE,Cases20,Denom20) %>% arrange(LSOA_CODE)    
        # 2021
        LSOADF2021 = LongDF2021 %>% left_join(ExtractDF2021,by="PRACTICE_CODE") %>% 
          mutate(Cases = Number.of.Patients*Rate) %>% 
          group_by(LSOA_CODE) %>%
          summarise(Cases21 = sum(Cases,na.rm=TRUE),
                    Denom21=sum(Number.of.Patients,na.rm=TRUE)) %>%
          select(LSOA_CODE,Cases21,Denom21) %>% arrange(LSOA_CODE)    
        
        # Aggregate the three years
        LSOADFpt1=left_join(LSOADF2019,LSOADF2020,by="LSOA_CODE")
        LSOADF=left_join(LSOADFpt1,LSOADF2021,by="LSOA_CODE")
        
        LSOADF = LSOADF %>% mutate(Cases=Cases19+Cases20+Cases21,
                                   Denom=Denom19+Denom20+Denom21) %>%
          mutate(Rate=Cases/Denom) %>% select(LSOA_CODE,Rate) %>%
          arrange(LSOA_CODE)
        
        names(LSOADF)[2]=DataDF$`Indicator Name`[1]
        
        # filter out non-English LSOAs
        LSOADF$Country=substring(LSOADF$LSOA_CODE,1,1)
        LSOADF = LSOADF %>% filter(LSOADF$Country=="E") %>% select(c(1,2))
        
        # Report on how many LSOAs have data
        LSOAVals = LSOADF %>% filter(!is.na(.[[2]]) & .[[2]]>0) %>% select(2)
        LSOACount = nrow(LSOAVals)
        LSOAMinVal = min(LSOAVals)
        LSOAMaxVal = max(LSOAVals)
        
        
        # save file. having first checked not open!
        while(1==1)
          if(file.opened(paste0(FilenamePrefix,FilenameSuffix))) {
            cat("\n\n File to be written (",paste0(FilenamePrefix,FilenameSuffix)," is open - probably in Excel\n")
            Message="\nClose file, then press enter (if stuck Ctrl-C to abort Rscript)"
            Continue = UserContinue(Message)
          } else {
            write.csv(LSOADF,paste0(FilenamePrefix,FilenameSuffix),row.names=FALSE)
            cat("\t written to ",paste0(FilenamePrefix,FilenameSuffix),"\n")
            cat(paste0(" Data contains ",LSOACount,
                       " LSOAs (expected 32844) with range ",
                       sprintf(LSOAMinVal,fmt='%#.4f')," - ",
                       sprintf(LSOAMaxVal,fmt='%#.4f'),"\n\n"))
            break
          }
        
        
        } 
        # end of AveFlag block
    

   }  
      # end status == 200 if block
  }   
  # end while
  
  # Tidy up
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

#################################################################################  
#################################################################################  
#################################################################################  

#DateExtractDF %>% filter(PRACTICE_CODE=="K82014" | PRACTICE_CODE=="K82018" |
#                           PRACTICE_CODE=="K82019" | PRACTICE_CODE=="K82038" |
#                           PRACTICE_CODE=="K82040" | PRACTICE_CODE=="K82073")

