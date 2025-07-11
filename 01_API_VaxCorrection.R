########################### IMMUNIZATION DQA: ECW INTERFACE DASHBOARD ###########################
##################################   Libraries   ************************************

library(stringr)
library(openxlsx)
library(readxl)
library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(glue)
library(ggplot2)
library(data.table)
library(lubridate)
library(plyr)
library(reshape2)
library(anytime)
library(readr)

here::here()


#Specify the month and month year: its being run every day so month and year should match this date.
month <- format((Sys.Date()), "%B")
month <- substr(month,1,3)
yr <- format((Sys.Date()), "%Y")
monthyr <- paste(month,yr)
today <- format(Sys.time(),"_ran_on_%m_%d_%Y %H_%M_%S")

################################ Code to Create Stacked Barplots ****************************
################################  for Visual Analysis purposes  *****************************

files.to.read = list.files(path = here::here("HL7_RAW_IMMZ"),
                           pattern = "*\\.xls$",
                           full.names = T)

# Read each file and write it to csv. Now lets change the xls to csv, to make the file more standard for our archive  

lapply(files.to.read, function(f) {
  df = read_excel(f, sheet=1)
  write.csv(df, gsub("xls", "csv", f), row.names=FALSE)
})

#obtaining the date from file to rename the cvs later.
file_name <- list.files(path = here::here("HL7_RAW_IMMZ"), pattern = "\\.csv$")%>%map_df(~file.info(.)) %>% 
  select(1)
file_name <- cbind(file_name = rownames(file_name), file_name) 
file_name <-  substring(file_name[1,1], 14, 21)

#Now as a csv file, lets read it.
my_data<-
  list.files(path = here::here("HL7_RAW_IMMZ"), pattern = "\\.csv$", full.names = TRUE) %>%
  map_df(~read_csv(., id = "filename", col_names = F, show_col_types = FALSE))

# Line of code to trip and error if there are no observations in the report folder
if (nrow(my_data) == 0){stop("There are no observations in the report data frame. Please go 
                            verify that the newest DQA report has been placed in the correct folder, 
                            then restart the code from the beginning.")}


colnames(my_data)[2] <- "Patname"
colnames(my_data)[3] <- "DOB"
colnames(my_data)[4] <- "Acct"
colnames(my_data)[5] <- "Senton"
colnames(my_data)[6] <- "Facility"
colnames(my_data)[7] <- "Desc"
colnames(my_data)[8] <- "Vax"
colnames(my_data)[9] <- "CVX"
colnames(my_data)[10] <- "Location"
colnames(my_data)[11] <- "MVX"
colnames(my_data)[12] <- "History"
colnames(my_data)[13] <- "Vaxdate"
colnames(my_data)[14] <- "Givenby"

my_data <- my_data[-1, -1]

# my_data$DOB <- as.Date((my_data$DOB), format = "%m/%d/%Y")
# my_data$Vaxdate <- as.Date((my_data$Vaxdate), format = "%m/%d/%Y")

my_data <- my_data %>% 
  fill(Patname, .direction = "down") %>% 
  fill(DOB, .direction = "down") %>% 
  fill(Acct, .direction = "down") %>% 
  fill(Senton, .direction = "down") %>% 
  fill(Facility, .direction = "down") %>% 
  fill(Desc, .direction = "down")

#Let's rename only the columns that really matter
colnames(my_data)[6] <- "ErrorMes"
colnames(my_data)[7] <- "VaxName"
colnames(my_data)[12] <- "VaxDate"
colnames(my_data)[13] <- "Givenby"


# Line of code to trip if any failure in BIDX connection happened
failed_sent <- my_data[grepl("Failed due to connection error", my_data$ErrorMes),]
if (nrow(failed_sent) != 0){stop("There was a Failure in the Bidirectional Feed with ImmTrac2. Please review BIDX Interface.")}

#Parsing Desc (Error Msg) since they all come clumped in a single cell. First, cleaning up all that were sent.
my_data <- my_data[grepl("Sent Successfully -|Message Rejected -", my_data$ErrorMes),]


#Now im adding a double ; , so I can split the string based on that and assign it its own row
my_data$ErrorMes <- str_replace_all(my_data$ErrorMes, "IMR", ";;IMR")
my_data$ErrorMes <- str_replace_all(my_data$ErrorMes, "CLR", ";;CLR")
my_data$ErrorMes <- str_replace_all(my_data$ErrorMes, "FIR", ";;FIR")
my_data$ErrorMes <- str_replace_all(my_data$ErrorMes, "IEE", ";;IEE")
my_data$ErrorMes <- str_replace_all(my_data$ErrorMes, "MER", ";;MER")

my_data<- my_data %>% 
  mutate(ErrorMes = strsplit(as.character(my_data$ErrorMes), ";;")) %>%
  unnest(ErrorMes)


#keeping only those rows that start with IMR, CLR, FIR, IEE and MER
my_data <- my_data[grep("IMR-|CLR-|FIR-|IEE-|MER-", my_data$ErrorMes), ]
 

#only sending consentclientreject, warnings
my_data <- my_data %>% 
  mutate(
    #we are just keeping dupeck for historical purposes: with BIDX now a duplicate immz error message is more of an FYI than a thing to fix
    dupeck = str_detect(`ErrorMes`, "IMR-109|IMR-110|IEE-513"),
    addimzck2 = str_detect(`ErrorMes`, "IEE-575"),
    dobck = str_detect(`ErrorMes`, "birth date"),
    nameck = str_detect(`ErrorMes`, "cannot be empty|one character|BABY"),
    ptaddressck = str_detect(`ErrorMes`, "IEE-998|PID-11 missing|PID-11-3 missing"),
    consentclientrejectck = str_detect(`ErrorMes`, "CLR-100|CLR-101|CLR-105|CLR-106|IMR-132|IMR-133"),
    tradenameck = str_detect(`ErrorMes`, "Trade Name"),
    manufactck = str_detect(`ErrorMes`, "Manufacturer Code|IMR-111"),
    warningck = str_detect(`ErrorMes`, "IEE-319|IEE-320|IEE-321|IEE-413|IEE-519|IEE-539|IEE-540|IEE-541|IEE-409|IEE-104|IEE-533|IEE-563|IEE-570"),
   #review are those errors that should be looked at by Data science, not a nurse.
     review = str_detect(`ErrorMes`, "IMR-100|IEE-402|IEE-407|IEE-411|IEE-515"),
   #warnings that are known, but we can't do anything about them or are just fyi
    ignore = str_detect(`ErrorMes`, "IEE-200|IEE-201|IEE-251|IEE-296|IEE-297|IEE-298|IEE-299|IEE-313|IEE-514|IEE-521|IEE-526|IEE-565|IEE-594|IEE-596")
        )
# no desc - what category?

# IMR-149 review (later) manufact?
#   IMR-144 - review (later) manufact?
#   IMR-139 -review (later)
# IMR-131 -review (later)

# 

#adding a new column to have a more anthropomorphic description of the error so the end user knows how to correct it.
my_data <- my_data %>% 
  mutate(errordesc = case_when(startsWith(my_data$ErrorMes, "CLR-100") == TRUE~ 'No consent was registered or is on file on eCW. Please make sure a scanned consent is on file before inputting the type and date in the patients profile.',

startsWith(my_data$ErrorMes, "IEE-402") == TRUE~ 'Please review by Data science', 
startsWith(my_data$ErrorMes, "IEE-407") == TRUE~ 'Please review by Data science', 
startsWith(my_data$ErrorMes, "IEE-411") == TRUE~ 'Please review by Data science', 
startsWith(my_data$ErrorMes, "IEE-515") == TRUE~ 'Please review by Data science', 

startsWith(my_data$ErrorMes, "IEE-200") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-201") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-251") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-296") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-297") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-298") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-299") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-313") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-514") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-521") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-526") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-565") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-594") == TRUE~ 'Ignore.', 
startsWith(my_data$ErrorMes, "IEE-596") == TRUE~ 'Ignore.', 
                               
startsWith(my_data$ErrorMes, "CLR-101") == TRUE~ 'The type of consent is wrong according to the patients age. Please review and select the right type again for children or adult.', 
startsWith(my_data$ErrorMes, "CLR-105") == TRUE~ 'The date of the consent is wrong. Please review the date on when the consent was given on the patients profile.',
startsWith(my_data$ErrorMes, "CLR-106") == TRUE~ 'A future date was put for the consent in relation to the appointment date. Please go back and correct it to the actual date consent was given.',
startsWith(my_data$ErrorMes, "IMR-132") == TRUE~ 'A standard consent (not a disaster-type) is required for transmitting this immunization. Please check that a standard consent was signed, scanned and then selected on the patients profile.',
startsWith(my_data$ErrorMes, "IMR-133") == TRUE~ 'For this vaccine and the date it was given, its not considered linked to any disaster event/pandemic. Therefore, it needs a standard consent to be transmitted to ImmTrac2.',
startsWith(my_data$ErrorMes, "IEE-319") == TRUE~ 'Please select ONE valid ethnicity on the patients profile on eCW from these options: Hispanic or Latino, Not Hispanic or Latino, Unknown).',
startsWith(my_data$ErrorMes, "IEE-320") == TRUE~ 'Please select ONE valid race on the patients profile on eCW from these options: American Indian or Alaska Native, Asian, Black or African American, Native Hawaiian or Other Pacific Islander, White, Other Race).',
startsWith(my_data$ErrorMes, "IEE-321") == TRUE~ 'Please select ONE valid ethnicity on the patients profile on eCW from these options: Hispanic or Latino, Not Hispanic or Latino, Unknown).',
startsWith(my_data$ErrorMes, "IEE-409") == TRUE~ 'If the Resp Party is SELF (Relation is 1, Self-patient is the insured), the Resp Part section must match the patients name. Please correct it on Patient information.',
startsWith(my_data$ErrorMes, "IEE-413") == TRUE~ 'Please input a valid phone number for this patient, not a made-up/placeholder one.',
startsWith(my_data$ErrorMes, "IEE-519") == TRUE~ 'The Resp Party section was left empty: please complete it on the Patients profile.',
startsWith(my_data$ErrorMes, "IEE-539") == TRUE~ 'In Patient Information>Additional Information, in VFC Eligibility please select an adult code, not an adult one of the choice that applies to the patient.',
startsWith(my_data$ErrorMes, "IEE-540") == TRUE~ 'In Patient Information>Additional Information, in VFC Eligibility please select a child code, not an adult one of the choice that applies to the patient.',
startsWith(my_data$ErrorMes, "IEE-541") == TRUE~ 'IF the patient is really UNDERinsured (not the same as Uninsured) In Patient Information>Additional Information, in VFC Eligibility select Underinsured, Not FQHC/RHC/Deputized.',
startsWith(my_data$ErrorMes, "IEE-570") == TRUE~ 'The Administration Route is incorrect: please go back and make sure to select an acceptable option.',
startsWith(my_data$ErrorMes, "IEE-571") == TRUE~ 'The Administration Body Site code is incorrect: please go back and make sure to select an acceptable option.',
startsWith(my_data$ErrorMes, "IEE-575") == TRUE~ 'The two Beyfortus doses reported have the same body site of administration: please verify that the vaccine was administered in two different sites or that this information is correct.',
startsWith(my_data$ErrorMes, "IEE-998") == TRUE~ 'The patients address is not valid. Please go to the patients profile, enter a correct, complete address and validate it.',
startsWith(my_data$ErrorMes, "IEE-104") == TRUE~ 'Please change to a corresponding Vaccine elegibility code in both Patient profile and vaccine documentation from this list: 
Private Pay/Insurance: Patient is < 19 years old
Medicaid: Patient is < 19 years old
No Insurance: Patient is < 19 years old
American Indian/Alaskan Native: Patient is < 19 years old
V05 for Underinsured, FQHC/RHC/Deputized: Patient is < 19 years old
CHIP: Patient is < 19 years old
Underinsured, Not FQHC/RHC/Deputized: Patient is < 19 years old
19-Year-Old Completing Series: Patient = 19 years old
Adult, No Insurance: Patient is >= 19 years old
Adult, Underinsured: Patient is >= 19 years old
Adult, Private Pay/Insurance: Patient is >= 19 years old.',

startsWith(my_data$ErrorMes, "IEE-533") == TRUE~ 'Please change to a corresponding Vaccine elegibility code in both Patient profile and vaccine documentation from this list: 
Private Pay/Insurance: Patient is < 19 years old
Medicaid: Patient is < 19 years old
No Insurance: Patient is < 19 years old
American Indian/Alaskan Native: Patient is < 19 years old
V05 for Underinsured, FQHC/RHC/Deputized: Patient is < 19 years old
CHIP: Patient is < 19 years old
Underinsured, Not FQHC/RHC/Deputized: Patient is < 19 years old
19-Year-Old Completing Series: Patient = 19 years old
Adult, No Insurance: Patient is >= 19 years old
Adult, Underinsured: Patient is >= 19 years old
Adult, Private Pay/Insurance: Patient is >= 19 years old.',

startsWith(my_data$ErrorMes, "IEE-563") == TRUE~ 'Please change to a corresponding Vaccine elegibility code in both Patient profile and vaccine documentation from this list: 
Private Pay/Insurance: Patient is < 19 years old
Medicaid: Patient is < 19 years old
No Insurance: Patient is < 19 years old
American Indian/Alaskan Native: Patient is < 19 years old
V05 for Underinsured, FQHC/RHC/Deputized: Patient is < 19 years old
CHIP: Patient is < 19 years old
Underinsured, Not FQHC/RHC/Deputized: Patient is < 19 years old
19-Year-Old Completing Series: Patient = 19 years old
Adult, No Insurance: Patient is >= 19 years old
Adult, Underinsured: Patient is >= 19 years old
Adult, Private Pay/Insurance: Patient is >= 19 years old.'))



#Im going to leave these categories here just to be able to track how we are doing with them and being able to compare
#to the past
my_data$Errorcod <- NA
my_data$Errorcod[my_data$dupeck=='TRUE'] <- 'Duplicate Immz'    
my_data$Errorcod[my_data$addimzck2=='TRUE'] <- 'Immz to Add'    
my_data$Errorcod[my_data$dobck=='TRUE'] <- 'Birthdate error' 
my_data$Errorcod[my_data$nameck=='TRUE'] <- 'Name error' 
my_data$Errorcod[my_data$ptaddressck=='TRUE'] <- 'Patient Address' 
my_data$Errorcod[my_data$consentclientrejectck=='TRUE'] <- 'Consent Client Rejected'
my_data$Errorcod[my_data$tradenameck=='TRUE'] <- 'Trade Name error'
my_data$Errorcod[my_data$manufactck=='TRUE'] <- 'Manufacturer error'
my_data$Errorcod[my_data$warningck=='TRUE'] <- 'Warnings'    

my_data <- data.frame(my_data)
my_data <-my_data%>% arrange(my_data$Patname)

#openxlsx::write.xlsx(my_data, "my_datatest2.xlsx")

#Discarding the Warning code glitch and 'not really count as an error'
#IEE-200::Warning. The incoming client information has been saved (ID XXXXXXX) for review by a State BA, who will decide the appropriate existing client to match to..
#IEE-201::Warning. The incoming client matches more than one existing candidate. Existing candidate client ids include XXXXX YYYYY . 
#IEE-251::Informational Message. Duplicate SSN. No value stored.
#IEE-296:: Warning. You are attempting to update the FirstName of ImmTrac2 Client ID: XXXXX, who was created by the state's vital statistics unit. This update is not allowed via data exchange. Please contact ImmTrac2@dshs.texas.gov for assistance with the update. 
#IEE-297::Warning. You are attempting to update the LastName of ImmTrac2 Client ID: XXXXXXXX, who was created by the state's vital statistics unit. This update is not allowed via data exchange. Please contact ImmTrac2@dshs.texas.gov for assistance with the update. 
#IEE-298::Warning. You are attempting to update the MiddleName of ImmTrac2 Client ID: xxxxxxxxx, who was created by the state's vital statistics unit. This update is not allowed via data exchange. Please contact ImmTrac2@dshs.texas.gov for assistance with the update.
#IEE-299::Warning. You are attempting to update the DOB of ImmTrac2 Client ID: xxxxx, who was created by the state's vital statistics unit. This update is not allowed via data exchange. Please contact ImmTrac2@dshs.texas.gov for assistance with the update.
#IEE-313::Warning. You are attempting to update the Gender of ImmTrac2 Client ID: xxxxxxxx, who was created by the state's vital statistics unit. This update is not allowed via data exchange. Please contact ImmTrac2@dshs.texas.gov for assistance with the update.
#IEE-513::Warning. Incoming Immunization already exists in the system.
#IEE-526:: Warning. The license number (or NPI number) provided in ORC-12.1 is not associated to the prescribing authority in lmmTrac2. License number is preferred, but individual NPI for prescribing authority is acceptable. Please be sure you are entering in the correct value. If the Issue persists, please fill out a renewal, and add this prescribing provider to the list of allowed providers. 
#IEE-594::Warning. The incoming delete immunization does not match an existing immunization in ImmTrac2. This delete was not processed. 
#IEE-596::Warning. The sending provider organization does not own the existing matched immunization in ImmTrac2. This delete was not processed. 


my_data <- my_data %>% 
  filter(!grepl('IEE-200|IEE-201|IEE-251|IEE-296|IEE-297|IEE-298|IEE-299|IEE-313|IEE-513|IEE-526|IEE-594|IEE-596' , ErrorMes))


#Deduping in general: sometimes the exact same error gets sent twice in a day
my_data <- my_data[!duplicated(my_data[,c("Patname", "DOB", "Acct", "ErrorMes", "VaxName", "VaxDate")]),]


#deduping CCR: some immz get a double CCR error, keep only one copy of CLR-105 since its an error with one blanket solution for all vaccines that get it
ccronly <- dplyr::filter(my_data, grepl('Consent Client Rejected', Errorcod))
ccronly <- ccronly[!duplicated(ccronly[,c("Patname", "DOB", "Acct", "VaxName", "VaxDate")]),]

ccr105only <- dplyr::filter(ccronly, grepl('CLR-105', ErrorMes))
ccr105only <- ccr105only[!duplicated(ccr105only[,c("Acct")]),]
ccronly <- dplyr::filter(ccronly, !grepl('CLR-105', ErrorMes))

ccr101only <- dplyr::filter(ccronly, grepl('CLR-101', ErrorMes))
ccr101only <- ccr101only[!duplicated(ccr101only[,c("Acct")]),]
ccronly <- dplyr::filter(ccronly, !grepl('CLR-101', ErrorMes))

ccr100only <- dplyr::filter(ccronly, grepl('CLR-100', ErrorMes))
ccr100only <- ccr100only[!duplicated(ccr100only[,c("Acct")]),]
ccronly <- dplyr::filter(ccronly, !grepl('CLR-100', ErrorMes))

imr132only <- dplyr::filter(ccronly, grepl('IMR-132', ErrorMes))
imr132only <- imr132only[!duplicated(imr132only[,c("Acct")]),]
ccronly <- dplyr::filter(ccronly, !grepl('IMR-132', ErrorMes))

ccronly <- rbind(ccronly, ccr101only, ccr105only, ccr100only, imr132only)


#deduping duplicate immz: some immz get a double dupeck error.
dupeckonly <- dplyr::filter(my_data, grepl('Duplicate Immz', Errorcod))
dupeckonly <- dupeckonly[!duplicated(dupeckonly[,c("Patname", "DOB", "Acct", "VaxName", "VaxDate")]),]

#deduping warnings immz. If a patient doesnt have a proper code all vaccines get the same error for one single solution
warnonly <- dplyr::filter(my_data, grepl('IEE-319|IEE-320|IEE-321', ErrorMes))

#10/15/2024: LAST TIME added ADD IEE-299|IEE-313|IEE-402|IEE-404|IEE-411|IEE-413|IEE-519|IEE-998 for next of kin/responsible party warnings and have more submitted
warnonly <- warnonly[!duplicated(warnonly[,c("Patname", "DOB", "Acct", "ErrorMes")]),]


## check here for new rejection errors
newreject<-my_data%>%
  mutate(
    newrejection = ifelse(dupeck == FALSE & addimzck2 == FALSE & dobck == FALSE & nameck == FALSE & ptaddressck == FALSE &
                            consentclientrejectck == FALSE & tradenameck == FALSE & manufactck == FALSE & warningck == FALSE, "New Rejection", "Matches Other"))%>%
  subset(newrejection == "New Rejection")

my_data<-warnonly

#making the main df ccr-less, to now add the deduped ccr and dupeck and warning
my_data <-dplyr::filter(my_data, !grepl('Consent Client Rejected', Errorcod))
my_data <-dplyr::filter(my_data, !grepl('Duplicate Immz', Errorcod))
my_data <-dplyr::filter(my_data, !grepl('Warnings', Errorcod))


my_data <- rbind(my_data, ccronly, warnonly)
my_data <- rbind(my_data, dupeckonly)


## exclude those immz that were not administered
my_data <- my_data %>% 
  filter(!(History == "Current" & Givenby == "NA"))

#In case there is a duplicate of same new rejection, deduping.
newreject$key2 = paste0(newreject$Acct, newreject$VaxName, newreject$VaxDate)
newreject <- newreject%>%
  distinct(key2, .keep_all = TRUE)

#Sometimes same immz events from new rejections are also CCR rejections, so we want to exclude them from new rejections df.
dedupe_NR <- ccronly %>% 
  mutate(newrejection = 'NA')
dedupe_NR$key2 = paste0(dedupe_NR$Acct, dedupe_NR$VaxName, dedupe_NR$VaxDate)
dedupe_NR <- rbind(dedupe_NR, newreject)
dedupe_NR$key2 = paste0(dedupe_NR$Acct, dedupe_NR$VaxName, dedupe_NR$VaxDate)

dedupe_NR <- dedupe_NR[!(duplicated(dedupe_NR[,c("key2")]) | duplicated(dedupe_NR[,c("key2")], fromLast = TRUE)), ]
newreject <- dplyr::filter(dedupe_NR, grepl ('New Rejection', newrejection))



## this little trick is to detect if there is a new rejection in the file and bring it to our attention
if (nrow(newreject) != 0){
  if(askYesNo(paste("There is a new rejection, should I continue?")) == FALSE){
    stop()
  }
} else{
  table(newreject$newrejection)
}

#this is just temporary for the typhoid rejections
#openxlsx::write.xlsx(newreject, "testtest2.xlsx")
# done!



## Now adding the section for the API. 
library(httr)
library(jsonlite)
library(httr2)
library(keyring)


i <- 1 
enddf <- nrow(my_data)

repeat {
  PatName <- my_data[i,1]
  DOB <- my_data [i,2]
  MRN <- my_data [i,3]
  Clinic <- my_data[i,5]
  ErrorCod <- my_data [i,6]
  VaxName <- my_data [i,7]
  Manufac <- my_data [i,10]
  VaxDate <- my_data [i,12]
  Nurse <- my_data [i,13]
  ErrorMes <- my_data [i,23]
  
  body <- paste('{"to":"btk8zdess",
              "data":[
              {
                "6":{
                  "value":"',PatName,'"
                  }, 
                "7":{ 
                  "value":"',DOB,'"
                  }, 
                "8":{ 
                  "value":"',"U",'"
                  }, 
                "9":{ 
                  "value":"',"NA",'"
                  },
                "17":{
                  "value":"',"NA",'"
                  }, 
                "18":{
                  "value":"',Nurse,'"
                  },
                "19":{
                  "value":"',"NA",'"
                  },
                "20":{
                  "value":"',VaxDate,'"
                  },
                "21":{
                  "value":"',MRN,'"
                  },
                "22":{
                  "value":"',Clinic,'"
                  },
                "23":{
                  "value":"',"NA",'"
                  },
                "24":{
                  "value":"',Manufac,'"
                  },
                "25":{
                  "value":"',ErrorCod,'"
                  },
                "26":{
                  "value":"',ErrorMes,'"
                  },
                "40":{
                  "value":"',"No",'"
                  },
                "41":{
                  "value":"',"No comments",'"
                  },
                "44":{
                  "value":"',VaxName,'"
                  },
                "45":{
                  "value":"',"NA",'"
                  }
                }]}', sep= "")
  
  
  
  ## This makes the actual POST Request with our data
  report <- POST(url = paste0("https://api.quickbase.com/v1/records"),
                 add_headers("Authorization" = paste("QB-USER-TOKEN", keyring::key_get("Secret_QB_Token")),
                             "QB-Realm-Hostname" = "https://quickbase.com",#add acronym and period after slashes.
                             "Content-Type" = "Application/json") , 
                 body = body)
  i <- i + 1
  
  if (i > enddf) {
    break
  }
}



##This lets you see the response

report_text <- fromJSON(rawToChar(report$content))


