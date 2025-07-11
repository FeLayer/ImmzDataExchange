### INDEX

### BASED ON REJECTED IMMZ (BIDX)

# -   Total Rejections by Vaccination Date
# -   Vaccination Dates Processed by Message Date
# -   Number of Patients with Rejection Issues per Vax Date

### BASED ON EHR REPORTS (SENT)

# -   Number of Immunizations by Date
# -   Count of Immz per vaccine group given by Date
# -   Number of Patients vaccinated by Date
# -   Type of vaccines given by clinic

### BASED ON BIDX/SENT

# -   Immunizations Rejected/Accepted by Date
# -   Number of Patients Rejected/Accepted by Date
############ BASED ON REJECTED IMMZ (BIDX)
################################## Libraries ************************************

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

#################################### Prepping ***********************************************

#Specify the month and month year: its being run every day so month and year should match this date. 
month <- format((Sys.Date()), "%B")
month <- substr(month,1,3)
yr <- format((Sys.Date()), "%Y")
monthyr <- paste(month,yr)
today <- format(Sys.time(),"_ran_on_%m_%d_%Y %H_%M_%S")


################################ Code to Create Stacked Barplots ****************************
################################  for Visual Analysis purposes  *****************************

#read in the XLS file. This is a "bucket". where we should always drop the daily XLS BIDX file from eCW. 
#After, it makes a df out of it.
files.to.read = list.files(path = here::here("HL7_RAW_IMMZ"),
                           pattern = "*\\.xls$",
                           full.names = T)

# Read each file and write it to csv. Now lets change the xls to csv, to make the file more standard for our archive  
lapply(files.to.read, function(f) {
  df = read_excel(f, sheet=1)
  write.csv(df, gsub("xls", "csv", f), row.names=FALSE)
})

#Now as a csv file, lets read it.
my_data<-
  list.files(path = here::here("HL7_RAW_IMMZ"), pattern = "\\.csv$", full.names = TRUE) %>%
  map_df(~read_csv(., id = "filename", col_names = F, show_col_types = FALSE))

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

my_data <- my_data %>% 
  fill(Patname, .direction = "down") %>% 
  fill(DOB, .direction = "down") %>% 
  fill(Acct, .direction = "down") %>% 
  fill(Senton, .direction = "down") %>% 
  fill(Facility, .direction = "down") %>% 
  fill(Desc, .direction = "down")

#Let's rename only the columns that really matter
colnames(my_data)[1] <- "Patname"
colnames(my_data)[2] <- "DOB"
colnames(my_data)[3] <- "Acct"
colnames(my_data)[5] <- "Facility"
colnames(my_data)[6] <- "ErrorMes"
colnames(my_data)[7] <- "VaxName"
colnames(my_data)[10] <- "MVX"
colnames(my_data)[12] <- "VaxDate"
colnames(my_data)[13] <- "Givenby"

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

my_data$ErrorMes <- trimws(my_data$ErrorMes)

#keeping only those rows that start with IMR, CLR, FIR, IEE and MER
my_data <- my_data[grep("IMR-|CLR-|FIR-|IEE-|MER-", my_data$ErrorMes), ]

my_data <- my_data %>% 
  mutate(
    dupeck = str_detect(`ErrorMes`, "This immunization matches another immunization in incoming file|Duplicate immunization Identified"),
    addimzck2 = str_detect(`ErrorMes`, "IMR-100::Immunization rejected.|Required field OBX-14 missing|Required field MSH-5 missing|WARNING|Future Date|Administrative Code invalid"),
    dobck = str_detect(`ErrorMes`, "birth date"),
    nameck = str_detect(`ErrorMes`, "cannot be empty|one character|BABY"),
    ptaddressck = str_detect(`ErrorMes`, "PID-11 missing|PID-11-3 missing"),
    consentclientrejectck = str_detect(`ErrorMes`, ":Client rejected. consent flag not valid for client age|Client rejected. Invalid Affirmation date format.|Client Rejected. No existing consent on file. Please verify the client submitted matches an existing record in ImmTrac2.|Immunization Rejected. Standard consent required for retaining non-disaster immunizations.|active disaster"),
    tradenameck = str_detect(`ErrorMes`, "Trade Name"),
    manufactck = str_detect(`ErrorMes`, "Manufacturer Code"),
    warningck = str_detect(`ErrorMes`, "Warning|Phone|Informational|Potential client|Beyfortus doses reported have the same body"),)


my_data$Errorcod <- NA
my_data$Errorcod[my_data$addimzck2=='TRUE'] <- 'Immz to Add'    
my_data$Errorcod[my_data$dobck=='TRUE'] <- 'Birthdate error' 
my_data$Errorcod[my_data$nameck=='TRUE'] <- 'Name error' 
my_data$Errorcod[my_data$ptaddressck=='TRUE'] <- 'Patient Address error' 
my_data$Errorcod[my_data$consentclientrejectck=='TRUE'] <- 'Consent Client Rejected'
my_data$Errorcod[my_data$tradenameck=='TRUE'] <- 'Trade Name error'
my_data$Errorcod[my_data$manufactck=='TRUE'] <- 'Manufacturer error'
my_data$Errorcod[my_data$warningck=='TRUE'] <- 'Warnings'    
my_data$Errorcod[my_data$dupeck=='TRUE'] <- 'Duplicate Immz'    

my_data <- data.frame(my_data)
my_data <-my_data%>% arrange(my_data$Patname)

#openxlsx::write.xlsx(my_data, "my_datatest.xlsx")


#Discarding the Warning code glitch and 'not really count as an error'
#IEE-200::Warning. The incoming client information has been saved (ID XXXXXXX) for review by a State BA, who will decide the appropriate existing client to match to..
#IEE-201::Warning. The incoming client matches more than one existing candidate. Existing candidate client ids include XXXXX YYYYY . 
#IEE-251::Informational Message. Duplicate SSN. No value stored.
#IEE-296:: Warning. You are attempting to update the FirstName of ImmTrac2 Client ID: XXXXX, who was created by the state's vital statistics unit. This update is not allowed via data exchange. Please contact ImmTrac2@dshs.texas.gov for assistance with the update. 
#IEE-297::Warning. You are attempting to update the LastName of ImmTrac2 Client ID: XXXXXXXX, who was created by the state's vital statistics unit. This update is not allowed via data exchange. Please contact ImmTrac2@dshs.texas.gov for assistance with the update. 
#IEE-298::Warning. You are attempting to update the MiddleName of ImmTrac2 Client ID: xxxxxxxxx, who was created by the state's vital statistics unit. This update is not allowed via data exchange. Please contact ImmTrac2@dshs.texas.gov for assistance with the update.
#IEE-299::Warning. You are attempting to update the DOB of ImmTrac2 Client ID: xxxxx, who was created by the state's vital statistics unit. This update is not allowed via data exchange. Please contact ImmTrac2@dshs.texas.gov for assistance with the update.
#IEE-313::Warning. You are attempting to update the Gender of ImmTrac2 Client ID: xxxxxxxx, who was created by the state's vital statistics unit. This update is not allowed via data exchange. Please contact ImmTrac2@dshs.texas.gov for assistance with the update.
#IEE-411::Warning. Neither last name, address, nor telephone specified. 
#IEE-513::Warning. Incoming Immunization already exists in the system.
#IEE-526:: Warning. The license number (or NPI number) provided in ORC-12.1 is not associated to the prescribing authority in lmmTrac2. License number is preferred, but individual NPI for prescribing authority is acceptable. Please be sure you are entering in the correct value. If the Issue persists, please fill out a renewal, and add this prescribing provider to the list of allowed providers. 
#IEE-594::Warning. The incoming delete immunization does not match an existing immunization in ImmTrac2. This delete was not processed. 
#IEE-596::Warning. The sending provider organization does not own the existing matched immunization in ImmTrac2. This delete was not processed. 

#also duplicates since with BIDX, these are just ''bouncing'' off vaccines, no fixable thing on our side now
#IMR-109::Immunization rejected. Duplicate immunization Identified.
#IMR-110::Immunization rejected. This immunization matches another immunization in incoming file.

my_data <- my_data %>% 
  filter(!grepl('IEE-200|IEE-201|IEE-251|IEE-296|IEE-297|IEE-298|IEE-299|IEE-411|IEE-513|IEE-526|IEE-594|IEE-596|IMR-109|IMR-110|IMR-131' , ErrorMes))


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

#deduping warnings immz (ethnic/race error). If a patient doesnt have a proper code all vaccines get the same error for one single solution
warnonly <- dplyr::filter(my_data, grepl('IEE-319|IEE-320|IEE-321', ErrorMes))
warnonly <- warnonly[!duplicated(warnonly[,c("Acct", "Patname", "DOB", "ErrorMes")]),]

#making the main df ccr-less, to now add the deduped ccr and dupeck and warning
my_data <-dplyr::filter(my_data, !grepl('Consent Client Rejected', Errorcod))
my_data <-dplyr::filter(my_data, !grepl('Duplicate Immz', Errorcod))
my_data <-dplyr::filter(my_data, !grepl('Warnings', Errorcod))

my_data <- rbind(my_data, ccronly, warnonly)
my_data <- rbind(my_data, dupeckonly)

## exclude those immz that were not administered
my_data <- my_data %>% 
  filter(!(History == "Current" & Givenby == "NA"))

## check here for new rejection errors
newreject<-my_data%>%
  mutate(
    newrejection = ifelse(dupeck == FALSE & addimzck2 == FALSE & dobck == FALSE & nameck == FALSE & ptaddressck == FALSE & consentclientrejectck == FALSE & tradenameck == FALSE & manufactck == FALSE & warningck == FALSE, "New Rejection", "Matches Other"))%>%
  subset(newrejection == "New Rejection")

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


########################### Creation of XLSX and archiving the og XLS ******************************
# All fill print outs and working directories will have the months automatically included

#Now that its all processed, we should archive the xls file based on the vaxdate (not title of file nor the
#day it was ran)

dayoforigin <- my_data[1,4] %>% substring(1,10)
dayoforigin <- as.character(dayoforigin)
dayoforigin <- glue(dayoforigin, today)

#It's better organization practice to create a folder for each day of each HL7 message we download from FTP, to have less clutter.
#This line moves the HL7 file from the "bucket", creates a new folder for it with the date of the HL7 files and drops the file in it.
daypath1 = here::here(glue("{monthyr} DQA Files"))
filename1 = glue("BIDX message {dayoforigin}")
dir.create(file.path(daypath1, filename1), recursive = TRUE)

#Moving the XLS original version to its assigned folder, now that we have a cvs copy to utilize for our script. 
files.to.read = list.files(path = here::here("HL7_RAW_IMMZ"),
                           pattern = "*\\.xls$",
                           full.names = F)

file.rename(from = here::here("HL7_RAW_IMMZ", files.to.read),
            to = here::here(glue("{monthyr} DQA Files/BIDX message {dayoforigin}/", files.to.read)))

#It's better organization practice to create a folder for each day of each XLS message we download from FTP, to have less clutter.
#This line creates a new folder for it with the date of the XLS files and drops the file in the month/yr folder.
daypath2 = here::here(glue("Data Entry Warnings and Rejections/{yr}/{monthyr}"))
dir.create(file.path(daypath2), recursive = TRUE, showWarnings = F)

#Now saving an xlsx copy of the cleaned report.
filename2 = daypath2 + glue('\\{monthyr} Warnings and Rejections {dayoforigin} .xlsx')
openxlsx::write.xlsx(my_data, filename2, overwrite = F)


################ CHARTS START HERE. REJECTED IMMZ ###########################
# Total Rejections by Vaccination Date
# Vaccination Dates Processed by Message Date
# Number of Patients with Rejection Issues per Vaccination Date
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
library(plotly)
library(htmlwidgets)
library(echarts4r)

#Specify the month and month year: its being run every day so month and year should match this date.
month <- format((Sys.Date()), "%B")
month <- substr(month,1,3)
yr <- format((Sys.Date()), "%Y")
monthyr <- paste(month,yr)
today <- format(Sys.time(),"_ran_on_%m_%d_%Y %H_%M_%S")

firstdayyear <- as.Date("2025-01-01")

today <- format(Sys.time(),"_ran_on_%m_%d_%Y %H_%M_%S")
maxrangerej <- as_date(format((Sys.Date()-1), "%Y/%m/%d"))

################################ Code to Create Stacked Barplots ****************************
################################  for Visual Analysis purposes  *****************************

#making a list of the excel files of rejections into R
readOneBook <- function(fn) {
  shtnms <- openxlsx::getSheetNames(fn)
  sheets <- lapply(setNames(nm = shtnms), 
                   openxlsx::readWorkbook, 
                   xlsxFile = fn)
  sheets
}

# reading the Data Entry Reports excels, and making them in a single list.
files <- lapply(setNames(nm= list.files(here::here(glue("Data Entry Warnings and Rejections/{yr}")),
                                        pattern = "\\.xlsx$", 
                                        full.names = TRUE,
                                        recursive = TRUE)), 
                readOneBook)

lapply(files, rbindlist, idcol = "sheet", fill=TRUE)

rbindlist(lapply(files, rbindlist, idcol = "sheet", fill=TRUE),
          idcol = "workbook"
)
commonsheets <- Reduce(intersect, lapply(files, names))

#Safety barrier in case a day of XLS files is missing (Monday-Fri) to avoid having incomplete daily data, starting 08/01/2024
files_daysrej <- as.data.table(names(files)) %>% 
  mutate(
    `V1` = str_sub(`V1`, 136, 145)) %>% 
  mutate_all(as.Date)

files_daysrej$weekday = weekdays.Date(files_daysrej$V1)

date_range_rej <- seq(min(firstdayyear), max(maxrangerej), by = 1)


#this next line is to know what dates are missing. Since its every day that the sent BIDX DQA is uploaded, 
#we are just interested in knowing that no weekday is missing (only weekends should be here)
missrej <- as.data.table(date_range_rej[!date_range_rej %in% files_daysrej$V1])
missrej$weekday = weekdays.Date(missrej)

missrej<- missrej%>% 
  mutate(number = ifelse(str_detect(weekday, "Saturday"), 1, 
                         if_else(str_detect(weekday, "Sunday"), 1,0)))

# All will start from 01/01/2025.
missrej <- subset(missrej,missrej$V1!= "2025-01-01")
missrej <- subset(missrej,missrej$V1!= "2025-01-10")
missrej <- subset(missrej,missrej$V1!= "2025-01-20")
missrej <- subset(missrej,missrej$V1!= "2025-02-17")
missrej <- subset(missrej,missrej$V1!= "2025-03-31")
missrej <- subset(missrej,missrej$V1!= "2025-04-18")
missrej <- subset(missrej,missrej$V1!= "2025-05-26")
missrej <- subset(missrej,missrej$V1!= "2025-06-19")
missrej <- subset(missrej,missrej$V1!= "2025-07-04")
missrej <- subset(missrej,missrej$V1!= "2025-09-01")

missrej <- subset(missrej,missrej$V1!= "2025-11-27")
missrej <- subset(missrej,missrej$V1!= "2025-11-28")
missrej <- subset(missrej,missrej$V1!= "2025-12-24")
missrej <- subset(missrej,missrej$V1!= "2025-12-25")
missrej <- subset(missrej,missrej$V1!= "2026-01-01")
missrej <- subset(missrej,missrej$V1!= "2026-01-19")
missrej <- subset(missrej,missrej$V1!= "2026-02-16")
missrej <- subset(missrej,missrej$V1!= "2026-03-31")
missrej <- subset(missrej,missrej$V1!= "2026-04-03")
missrej <- subset(missrej,missrej$V1!= "2026-05-25")
missrej <- subset(missrej,missrej$V1!= "2026-06-19")
missrej <- subset(missrej,missrej$V1!= "2026-07-03")
missrej <- subset(missrej,missrej$V1!= "2026-09-07")





#01/01/2025 New Years
#01/10/2025 SNOW DAY
#01/20/2025 Martin Luther King Jr Day
#02/17/2025 Presidents Day
#03/31/2025 Cesar Chavez Day
#04/18/2025 Good Friday
#05/26/2025 Memorial Day
#06/19/2025 Juneteenth
#07/04/2025 Independence Day
#09/01/2025 Labor Day


#here we add the exceptions to holidays that fall in a weekday. Check for the days with a 0, those days are missing.
if (0 %in% missrej$number){
  if(askYesNo(paste("At least 1 business day HL7 file is missing (DQA report). 
Have you checked 'misssrej' Data file in Global Environment to see what day/file is missing?")) == FALSE){
    stop()
  }
}

#creating the ''raw'' df with the excels merged from rejections
raw_df<- lapply(setNames(nm = commonsheets),
                function(sht) rbindlist(lapply(files, `[[`, sht), idcol = "workbook")) 
raw_df[raw_df == ""] <- NA
raw_df <- as.data.table(raw_df)

colnames(raw_df)[which(names(raw_df) == "Sheet.1.workbook")] <- "workbook"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.Patname")] <- "Last.Name.First.Name.MI"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.DOB")] <- "DOB"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.Acct")] <- "MRN"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.Senton")] <- "Senton"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.Facility")] <- "Clinic.Location"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.ErrorMes")] <- "ErrorMes"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.VaxName")] <- "Vaccine.Name"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.CVX")] <- "CVX"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.Location")] <- "Location"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.MVX")] <- "MVX"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.History")] <- "History"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.VaxDate")] <- "Vaccination.Date"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.Givenby")] <- "Givenby"
colnames(raw_df)[which(names(raw_df) == "Sheet.1.Errorcod")] <- "Error.code"


# Deduping raw_df. Each excel when processed was deduped within in. This dedupes immz that may be repeated in different excel workbooks ergo, raw_df (since raw_df is a merged df of all the excels)

raw_df <- raw_df[!duplicated(raw_df[,c("MRN", "Vaccination.Date", "Vaccine.Name", "Error.code")]),]


CCR_df <- raw_df[raw_df$Error.code == "Consent Client Rejected",]%>% 
  select(1:14) %>% 
  mutate(
    `workbook` = str_sub(`workbook`, 71))
CCR_df <- CCR_df[!duplicated(CCR_df[,c("workbook","MRN", "Vaccination.Date", "Vaccine.Name", "ErrorMes")]),]
CCR_df <- CCR_df %>% 
  mutate_all(as.character)


NE_df <- raw_df[raw_df$Error.code == "Name error",]%>% 
  select(1:14) %>% 
  mutate(
    `workbook` = str_sub(`workbook`, 71))
NE_df <- NE_df[!duplicated(NE_df[,c("workbook","MRN", "Vaccination.Date", "Vaccine.Name", "ErrorMes")]),]
NE_df <- NE_df %>% 
  mutate_all(as.character)


BE_df <- raw_df[raw_df$Error.code == "Birthdate error",]%>% 
  select(1:14) %>% 
  mutate(
    `workbook` = str_sub(`workbook`, 71))
BE_df <- BE_df[!duplicated(BE_df[,c("workbook","MRN", "Vaccination.Date", "Vaccine.Name", "ErrorMes")]),]
BE_df <- BE_df %>% 
  mutate_all(as.character)


PAE_df <- raw_df[raw_df$Error.code == "Patient Address error",]%>% 
  select(1:14) %>% 
  mutate(
    `workbook` = str_sub(`workbook`, 71))
PAE_df <- PAE_df[!duplicated(PAE_df[,c("workbook","MRN", "Vaccination.Date", "Vaccine.Name", "ErrorMes")]),]
PAE_df <- PAE_df %>% 
  mutate_all(as.character)


TNE_df <- raw_df[raw_df$Error.code == "Trade Name error",]%>% 
  select(1:14) %>% 
  mutate(
    `workbook` = str_sub(`workbook`, 71))
TNE_df <- TNE_df[!duplicated(TNE_df[,c("workbook","MRN", "Vaccination.Date", "Vaccine.Name", "ErrorMes")]),]
TNE_df <- TNE_df %>% 
  mutate_all(as.character)


ME_df <- raw_df[raw_df$Error.code == "Manufacturer error",]%>% 
  select(1:14) %>% 
  mutate(
    `workbook` = str_sub(`workbook`, 71))
ME_df <- ME_df[!duplicated(ME_df[,c("workbook","MRN", "Vaccination.Date", "Vaccine.Name", "ErrorMes")]),]
ME_df <- ME_df %>% 
  mutate_all(as.character)


DI_df <- raw_df[raw_df$Error.code == "Duplicate Immz",]%>% 
  select(1:14) %>% 
  mutate(
    `workbook` = str_sub(`workbook`, 71))
DI_df <- DI_df[!duplicated(DI_df[,c("workbook","MRN", "Vaccination.Date", "Vaccine.Name", "ErrorMes")]),]
DI_df <- DI_df %>% 
  mutate_all(as.character)


WA_df <- raw_df[raw_df$Error.code == "Warnings",]%>% 
  select(1:14) %>% 
  mutate(
    `workbook` = str_sub(`workbook`, 71))
WA_df <- WA_df[!duplicated(WA_df[,c("workbook","MRN", "Vaccination.Date", "Vaccine.Name", "ErrorMes")]),]
WA_df <- WA_df %>% 
  mutate_all(as.character)


proc_df <- list("Consent Client Rejected" = CCR_df, "Name error" = NE_df, "Birthdate error" = BE_df,
                "Patient Address error"= PAE_df, "Trade Name error"= TNE_df,
                "Manufacturer error" = ME_df, "Duplicate Immz" = DI_df, "Warnings"= WA_df)

#creating the folder where we can drop the procdf file for future processing, and have less clutter. This line creates a new folder and drops the file there.
path_procdf = here::here(glue("Charts_and_Plots/Based_on_BIDX/{yr}/{monthyr}"))
dir.create(file.path(path_procdf), recursive = TRUE, showWarnings = F)

#Now saving an xlsx copy of the cleaned report.
file_procdf = path_procdf + glue('\\{monthyr} Plots.xlsx')
openxlsx::write.xlsx(proc_df, file_procdf, overwrite = T)


################################ Stacked Barplot for ****************************************
################################ Visual Analysis     ****************************************
# Total Rejections by Vaccination Date
# Vaccination Dates Processed by Message Date
# Number of Patients with Rejection Issues per Vaccination Date

#################### Total Rejections by Vaccination Date #################### 

#creating the df removing the fields that are for data-entry team (data validation dropdown) and
# removing in case 100% duplicates slipped through

# removing all historical vaccines (those that are classified as 'Past' under 'History' column) since
# we are mostly interested in what we actually administer as a provider.

# Checking the number of observations per type of errors 

CCR_tot <- proc_df$`Consent Client Rejected`
CCR_tot <- CCR_tot[!(CCR_tot$History== "Past")]
CCR_tot <- CCR_tot[,13] 
CCR_tot <- CCR_tot %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n())
colnames(CCR_tot)[2] <- "Consent Client Rejected"

NE_tot <- proc_df$`Name error`
NE_tot <- NE_tot[!(NE_tot$History=="Past")]
NE_tot <- NE_tot[,13]
NE_tot <- NE_tot %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n()) 
colnames(NE_tot)[2] <- "Name Error"

BE_tot <- proc_df$`Birthdate error`
BE_tot <- BE_tot[!(BE_tot$History =="Past")]
BE_tot <- BE_tot[,13]
BE_tot <- BE_tot %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n()) 
colnames(BE_tot)[2] <- "Birthdate Error"

PAE_tot <- proc_df$`Patient Address error`
PAE_tot <- PAE_tot[!(PAE_tot$History =="Past")]
PAE_tot <- PAE_tot[,13]
PAE_tot <- PAE_tot %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n()) 
colnames(PAE_tot)[2] <- "Patient Address Error"

TNE_tot <- proc_df$`Trade Name error`
TNE_tot <- TNE_tot[!(TNE_tot$History =="Past")]
TNE_tot <- TNE_tot[,13]
TNE_tot <- TNE_tot %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n())
colnames(TNE_tot)[2] <- "Trade name Error"

ME_tot <- proc_df$`Manufacturer error`
ME_tot <- ME_tot[!(ME_tot$History =="Past")]
ME_tot <- ME_tot[,13]
ME_tot <- ME_tot %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n()) 
colnames(ME_tot)[2] <- "Manufacturer Error"

DI_tot <- proc_df$`Duplicate Immz`
DI_tot <- DI_tot[!(DI_tot$History =="Past")]
DI_tot <- DI_tot[,13]
DI_tot <- DI_tot %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n()) 
colnames(DI_tot)[2] <- "Duplicate Immunization"

WA_tot <- proc_df$`Warnings`
WA_tot <- WA_tot[!(WA_tot$History =="Past")]
WA_tot <- WA_tot[,13]
WA_tot <- WA_tot %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n()) 
colnames(WA_tot)[2] <- "Warnings"

#putting them in a single list
joined_tot <- list("Consent Client Rejected Totals" = CCR_tot, "Name errors Total" = NE_tot,
                   "Birthdate errors Total" = BE_tot, "Patient Address errors Total"= PAE_tot, 
                   "Trade Name errors Total"= TNE_tot,"Manufacturer errors Total" = ME_tot,
                   "Duplicate Immz Total" = DI_tot, "Warnings Total" = WA_tot)

# joining all the merged sheets into a single table with Vaccination Date on Y and Errors on X. 
joined_tot <- list(CCR_tot, NE_tot, BE_tot, PAE_tot, TNE_tot, ME_tot, DI_tot, WA_tot) %>% 
  reduce(full_join, by = "Vaccination.Date")

# setting the cells blanks/NA to 0
joined_tot[is.na(joined_tot)] <- 0

# Changing the format from wide to long, so echarts can actually read the info
joined_tot <- as.data.table(joined_tot)
joined_tot$Vaccination.Date <-  as.Date(joined_tot$Vaccination.Date)
#joined_tot <- melt(setDT(joined_tot), id.vars = "Vaccination.Date")

# To add the totals for each day, we'll be attaching a total columns and avoid melting the data (such as the previous line of melt)
joined_tot$total <- rowSums(joined_tot[,2:9])
joined_tot$zero <- 0

openxlsx::write.xlsx(joined_tot, glue("Charts_and_Plots/Based_on_BIDX/{yr}/{monthyr}/{monthyr} joined_tot.xlsx"))

# Use this lines if you want to subset based on dates or last days
joined_tot<- joined_tot[joined_tot$Vaccination.Date >= "2025-01-01",]

## echarts4r
### Bar-Chart Stacked ###
colors = c("#5D7E82",
           "#4C3113",
           "#2292D0",
           "#A6E2BE",
           "#284E5F",
           "#CA6A53",
           "#DCB59E",
           "#803B21",
           "#23513C")
joined_tot_plot <- joined_tot%>% 
  e_chart(Vaccination.Date) %>% 
  e_bar(serie = `Consent Client Rejected`, name = "Consent Client Rejected", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Name Error`, name = "Name Error", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Birthdate Error`, name = "Birthdate Error", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Patient Address Error`, name = "Patient Address Error", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Trade name Error`, name = "Trade name Error", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Manufacturer Error`, name = "Manufacturer Error", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Duplicate Immunization`, name = "Duplicate Immunization", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Warnings`, name = "Warnings", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(
    serie = zero, bind = total, name = "Total",
    stack = "stack",
    label = list(
      show = TRUE,
      formatter = "{b}",
      position = "top"
    ),
    legend = NULL
  ) %>% 
  e_color(colors)%>% 
  e_tooltip(trigger = "item") %>% 
  e_title("Rejections by Vaccination date") %>% 
  e_axis_labels(y = "Count", x ="Vaccination date") %>% 
  e_x_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = FALSE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted")
    )
  ) %>% 
  e_y_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = TRUE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted")
    )
  ) %>% 
  e_legend (top = 40,
            align = "right",
            orient = "vertical",
            right = 40,
            height = 800) %>% 
  e_datazoom(x_index = 0, type = "slider") %>%  
  e_datazoom(y_index = 0, type = "slider") 

joined_tot_plot

#saving it as html
saveWidget(widget = joined_tot_plot, 
           file = glue("Charts_and_Plots/Based_on_BIDX/{yr}/{monthyr}/Type and Number of Rejections per Vaccination Date.html"))


#################### Vaccination Dates Processed by Message Date #################### 

# We want to count how many times vaccination dates are processed per daily HL7 
# message to see how up to date is the vax info we are receiving and processing in our daily DQA report.

# Vaccines Administered per visit grouped by age [PENDING IDEA]  

# removing all historical vaccines (those that are classified as 'Past' under 'History' column) since
# we are mostly interested in what we actually administer as a provider.

#making a new dataframe with only the vaccination date and the date of the file 
CCR_msg <- CCR_df[!(CCR_df$History== "Past")]
CCR_msg <- CCR_msg %>% 
  select(5, 13) %>% 
  mutate(`Senton` = str_sub(`Senton`, 0, 10))


NE_msg <- NE_df[!(NE_df$History== "Past")]
NE_msg <- NE_msg %>% 
  select(5, 13) %>% 
  mutate(`Senton` = str_sub(`Senton`, 0, 10))


BE_msg <- BE_df[!(BE_df$History== "Past")]
BE_msg <- BE_msg %>% 
  select(5, 13) %>% 
  mutate(`Senton` = str_sub(`Senton`, 0, 10))


PAE_msg <- PAE_df[!(PAE_df$History== "Past")]
PAE_msg <- PAE_msg %>% 
  select(5, 13) %>% 
  mutate(`Senton` = str_sub(`Senton`, 0, 10))


TNE_msg <- TNE_df[!(TNE_df$History== "Past")]
TNE_msg <- TNE_msg %>% 
  select(5, 13) %>% 
  mutate(`Senton` = str_sub(`Senton`, 0, 10))


ME_msg <- ME_df[!(ME_df$History== "Past")]
ME_msg <- ME_msg %>% 
  select(5, 13) %>% 
  mutate(`Senton` = str_sub(`Senton`, 0, 10))


DI_msg <- DI_df[!(DI_df$History== "Past")]
DI_msg <- DI_msg %>% 
  select(5, 13) %>% 
  mutate(`Senton` = str_sub(`Senton`, 0, 10))

WA_msg <- WA_df[!(WA_df$History== "Past")]
WA_msg <- WA_msg %>% 
  select(5, 13) %>% 
  mutate(`Senton` = str_sub(`Senton`, 0, 10))

proc_msg <- list("Consent Client Rejected" = CCR_msg, "Name error" = NE_msg,"Birthdate error" = BE_msg,
                 "Patient Address error"= PAE_msg, "Trade Name error"= TNE_msg,
                 "Manufacturer error" = ME_msg,"Duplicate Immz" = DI_msg, "Warnings" = WA_msg)

#Join all of these sheets into one
joined_msg <- bind_rows(proc_msg)

#vaccination date as date in order to sort it, then transform to character to plot it.
joined_msg$Vaccination.Date <- as.Date(joined_msg$Vaccination.Date)
joined_msg[rev(order(as.Date(joined_msg$Vaccination.Date, format="%m/%d/%Y"))),] 

#Checking the number of observations (of Vaccine dates) per xls message ran from BIDX
joined_msg <- joined_msg[, Vaccination.Date := as.character(Vaccination.Date)]
joined_msg <- as.data.table(table(joined_msg[ , c("Senton","Vaccination.Date")]))

# Transforming senton to date. Use this lines if you want to subset based on dates or last x days
joined_msg$Senton <-  as.Date(joined_msg$Senton)
#joined_msg<- joined_msg[joined_msg$Vaccination.Date >= "2025-01-01"]

#GGPLOT as a bar chart. Change position to "stack" for stacked bar chart
joined_msg_plot<- 
  ggplot(joined_msg, aes(x = Senton, fill = Vaccination.Date)) + 
  geom_bar(stat = "identity", 
           aes(y = N), 
           position = "stack")+
  scale_fill_manual(values=c(
    "#1E2E2B",
    "#333630",
    "#333631",
    "#333632",
    "#333633",
    "#333634",
    "#493D35",
    "#493D36",
    "#493D37",
    "#493D38",
    "#493D39",
    "#5E453A",
    "#744C3F",
    "#895444",
    "#895445",
    "#895446",
    "#895447",
    "#895448",
    "#9F5B49",
    "#B4634E",
    "#ca6a53",
    "#cb6e56",
    "#cc7259",
    "#cc7279",
    "#cd775d",
    "#ce7b60",
    "#cf7f64",
    "#d08367",
    "#d08378",
    "#d08379",
    "#d08380",
    "#d08381",
    "#d08382",
    "#d08383",
    "#d08384",
    "#d08385",
    "#d08386",
    "#d08387",
    "#d08388",
    "#d08389",
    "#d08390",
    "#d1876b",
    "#d28b6f",
    "#d38f73",
    "#d38f74",
    "#d49377",
    "#d5977b",
    "#d69a7f",
    "#d79e83",
    "#d8a287",
    "#d8a288",
    "#d8a289",
    "#d9a68c",
    "#d9aa90",
    "#d9aa92",
    "#d9aa93",
    "#d9aa94",
    "#daae95",
    "#daae96",
    "#dbb199",
    "#dbb299",
    "#dbb399",
    "#dcb59e",
    "#d8ad9c",
    "#d3a59a",
    "#ce9e99",
    "#c79798",
    "#c79799",
    "#c09197",
    "#c09198",
    "#c09199",
    "#c09200",
    "#c09201",
    "#c09202",
    "#b78a96",
    "#ae8595",
    "#a47f94",
    "#a47f95",
    "#9a7a92",
    "#8f7690",
    "#8f7695",
    "#83718d",
    "#786d8a",
    "#6c6986",
    "#606481",
    "#606491",
    "#54607b",
    "#485c75",
    "#3d576e",
    "#325367",
    "#325369",
    "#325379",
    "#284e5f",
    "#2b5566",
    "#2b5576",
    "#2b5586",
    "#2b5568",
    "#2e5d6c",
    "#326473", 
    "#326474", 
    "#326475", 
    "#326476",
    "#366c79",
    "#3a737f",
    "#3f7b85",
    "#3f7b86",
    "#3f7b87",
    "#3f7b88",
    "#44838a",
    "#498b90",
    "#498b91",
    "#498b92",
    "#498b93",
    "#4f9394",
    "#4f9395",
    "#4f9396",
    "#569b9a",
    "#569b9b",
    "#569b9c",
    "#5da39f",
    "#64aba3",
    "#64aca3",
    "#64aca4",
    "#64aca5",
    "#64aca6",
    "#64aca7",
    "#64aca8",
    "#64aca9",
    "#6cb3a6",
    "#6cb3a7",
    "#6cb3a8",
    "#6cb3a9",
    "#75bbab",
    "#7ec2af",
    "#87cab3",
    "#91d2c7",
    "#91d2b2",
    "#91d2b3",
    "#91d2b4",
    "#91d2b8",
    "#91d2b9",
    "#9bdaba",
    "#a6e2be",
    "#a6e2bf"
  )) +
  labs(title = "Vaccination Dates by Rejections Message Date", x = "Message Date", y = "Count")

joined_msg_plot<- joined_msg_plot%>% ggplotly(tooltip = c("fill", "y"))
joined_msg_plot

#saving it as html
saveWidget(ggplotly(joined_msg_plot), file = glue("Charts_and_Plots/Based_on_BIDX/{yr}/{monthyr}/Vaccination Dates processed by Day INTERN Analysis.html"))


#################### Number of Patients with Rejection Issues per Vax Date #################### 

# Make a df with only distinct patients (based on name and DOB, since we can have duplicate ecw accounts) and vax date
# to know unique patients per day will need a further dedupe because the same patient can have more than one rejection in a day.

CCR_rejpt <- CCR_df[!(CCR_df$History == "Past")]
CCR_rejpt <- CCR_rejpt %>% 
  select(2, 3, 13) %>% 
  mutate("Consent Client Rejected")
CCR_rejpt <- CCR_rejpt[!duplicated(CCR_rejpt[,c("Last.Name.First.Name.MI", "DOB", "Vaccination.Date")]),]


NE_rejpt <- NE_df[!(NE_df$History == "Past")]
NE_rejpt <- NE_rejpt %>% 
  select(2, 3, 13) %>% 
  mutate("Name error")
NE_rejpt <- NE_rejpt[!duplicated(NE_rejpt[,c("Last.Name.First.Name.MI", "DOB", "Vaccination.Date")]),]


BE_rejpt <- BE_df[!(BE_df$History == "Past")]
BE_rejpt <- BE_rejpt %>% 
  select(2, 3, 13) %>% 
  mutate("Birthdate error")
BE_rejpt <- BE_rejpt[!duplicated(BE_rejpt[,c("Last.Name.First.Name.MI", "DOB", "Vaccination.Date")]),]


PAE_rejpt <- PAE_df[!(PAE_df$History == "Past")]
PAE_rejpt <- PAE_rejpt %>% 
  select(2, 3, 13) %>% 
  mutate("Patient Address error")
PAE_rejpt <- PAE_rejpt[!duplicated(PAE_rejpt[,c("Last.Name.First.Name.MI", "DOB", "Vaccination.Date")]),]


TNE_rejpt <- TNE_df[!(TNE_df$History == "Past")]
TNE_rejpt <- TNE_rejpt %>% 
  select(2, 3, 13) %>% 
  mutate("Trade Name error")
TNE_rejpt <- TNE_rejpt[!duplicated(TNE_rejpt[,c("Last.Name.First.Name.MI", "DOB", "Vaccination.Date")]),]


ME_rejpt <- ME_df[!(ME_df$History == "Past")]
ME_rejpt <- ME_rejpt %>% 
  select(2, 3, 13) %>% 
  mutate("Manufacturer error")
ME_rejpt <- ME_rejpt[!duplicated(ME_rejpt[,c("Last.Name.First.Name.MI", "DOB", "Vaccination.Date")]),]


DI_rejpt <- DI_df[!(DI_df$History == "Past")]
DI_rejpt <- DI_rejpt %>% 
  select(2, 3, 13) %>% 
  mutate("Duplicate Immz")
DI_rejpt <- DI_rejpt[!duplicated(DI_rejpt[,c("Last.Name.First.Name.MI", "DOB", "Vaccination.Date")]),]


WA_rejpt <- WA_df[!(WA_df$History == "Past")]
WA_rejpt <- WA_rejpt %>% 
  select(2, 3, 13) %>% 
  mutate("Warnings")
WA_rejpt <- WA_rejpt[!duplicated(WA_rejpt[,c("Last.Name.First.Name.MI", "DOB", "Vaccination.Date")]),]


proc_rejpt <- list("Consent Client Rejected" = CCR_rejpt, "Name error" = NE_rejpt,"Birthdate error" = BE_rejpt,
                   "Patient Address error"= PAE_rejpt, "Trade Name error"= TNE_rejpt,
                   "Manufacturer error" = ME_rejpt,"Duplicate Immz" = DI_rejpt, "Warnings" = WA_rejpt) 

# joining all the merged sheets into a single table to further dedupe same patients that may have multiple rejection errors.
# we only care about individual patients per DAY, regardless of number or type of errors they may share
joined_rejpt <- bind_rows(proc_rejpt)
joined_rejpt <- joined_rejpt[!duplicated(joined_rejpt[,c("Last.Name.First.Name.MI", "DOB", "Vaccination.Date")]),]

#vaccination date as date in order to sort it, then transform to character to plot it
joined_rejpt$Vaccination.Date <-  as.Date(joined_rejpt$Vaccination.Date)
joined_rejpt <- joined_rejpt[rev(order(as.Date(joined_rejpt$Vaccination.Date, format="%m/%d/%Y"))),] 

#counting how many patients are per vaccination date, an adding it as a column to be able to plot it
tot_rejpt <- joined_rejpt %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n()) 

# This line is for having a excel of all Number of Patients Rejected by Vaccination Dates for future use in other code
openxlsx::write.xlsx(tot_rejpt, glue("Charts_and_Plots/Based_on_BIDX/{yr}/{monthyr}/{monthyr} tot_rejpt.xlsx"))

# Use this lines if you want to subset based on dates or last days
tot_rejpt<- tot_rejpt[tot_rejpt$Vaccination.Date >= "2025-01-01",]


## echarts4r
### Bar-Chart ###
tot_rejpt_plot <- tot_rejpt%>% 
  e_charts(x = Vaccination.Date)%>% 
  e_bar(serie = count, 
        color = '#DCB59E',
        legend = FALSE,
        itemStyle = list(
          borderWidth = 1.5,
          borderColor = '#CA6A53')
  )%>% 
  e_tooltip(trigger = "item") %>% 
  e_title("Patients with Rejections by Vaccination Date") %>% 
  e_axis_labels(y = "Number of Patients", x ="Vaccination date") %>% 
  e_x_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = FALSE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted")
    )
  ) %>% 
  e_y_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = FALSE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted")
    )
  ) %>% 
  e_datazoom(x_index = 0, type = "slider") %>%  
  e_datazoom(y_index = 0, type = "slider") 

tot_rejpt_plot

#save as html
saveWidget(widget = tot_rejpt_plot,
           file = glue("Charts_and_Plots/Based_on_BIDX/{yr}/{monthyr}/Number of Patients Rejected by Vaccination Dates.html"))



############ BASED ON EHR REPORTS (SENT) ###############################
################################## Libraries ********************************************

library(stringr)
library(readxl)
library(openxlsx)
require(openxlsx)
library(here)
library(tidyr)
library(dplyr)
library(purrr)
library(readr)
library(here)
library(glue)
library(ggplot2)
library(data.table)
library(lubridate)
library(reshape2)
library(plotly)
library(htmlwidgets)
library(echarts4r)

here::here("HL7_RAW_IMMZ")

#Specify the month and month year: its being run every day so month and year should match this date.
month <- format((Sys.Date()), "%B")
month <- substr(month,1,3)
yr <- format((Sys.Date()), "%Y")
monthyr <- paste(month,yr)

today <- format(Sys.time(),"_ran_on_%m_%d_%Y %H_%M_%S")
maxrangerej <- as_date(format((Sys.Date()-1), "%Y/%m/%d"))

# List all Excel files in the directory
excel_files <- list.files(here::here("HL7_RAW_IMMZ"),
                          pattern = "\\.xlsx$", 
                          full.names = TRUE)


# If the directory contains files, find the most recent one
if (length(excel_files) > 0) {
  # Get the most recently modified file
  recent_file <- excel_files[which.max(file.info(excel_files)$mtime)]
  
  # Read the most recent Excel file
  report_adm <- read.xlsx(recent_file)
  
  # View the first few rows of the data
  head(report_adm)
} else {
  cat("No Excel files found in the specified directory.")
}


# Line of code to trip and error if there are no observations in the report folder
if (nrow(report_adm) == 0){stop("There are no observations in the report data frame. Please go 
                            verify that the newest DQA report has been placed in the correct folder, 
                            then restart the code from the beginning.")}

report_adm$Given.Date <- as.Date(report_adm$Given.Date, origin = "1899-12-30")
report_adm$Patient.DOB <- as.Date(report_adm$Patient.DOB, origin = "1899-12-30")

#Since this will be a daily report (1 file per day, and thus we can only run ONE file at a time), 
#in the script (since now this variable is based on the actual date of the EBO report and not the day that the script was ran)
dayoforigin_adm <- report_adm[2,15]
dayoforigin_adm <- as.character(dayoforigin_adm)
dayoforigin_adm <- glue(dayoforigin_adm, today)

#It's better organization practice to create a folder for each day of each EBO Immz admin report, to have less clutter.
#This line moves the xlsx file from the "bucket", creates a new folder for it with the date of the admin vax and drops it in it.
daypath3 = here::here(glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Sent Data"))
dir.create(file.path(daypath3), recursive = TRUE, showWarnings = F)

my_EHRfile <- list.files(
  path = here::here("HL7_RAW_IMMZ"),
  pattern = "\\.xlsx$",
  full.names = FALSE)


file.rename(from = here::here("HL7_RAW_IMMZ", my_EHRfile),
            to = here::here(glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Sent Data/EHR sent message {dayoforigin_adm}", my_EHRfile)))



################ CHARTS START HERE: EHR REPORTS ############
# Number of Immunizations by Date
# Count of immz per vaccine group given by Date
# Number of Patients vaccinated by Date
# Type of immunizations given by clinic

#making a list of the excel files into R
readOneBook <- function(fn) {
  shtnms <- openxlsx::getSheetNames(fn)
  sheets <- lapply(setNames(nm = shtnms), 
                   openxlsx::readWorkbook, 
                   xlsxFile = fn)
  sheets
}

# reading the excels of SENT IMMZ DATA, and putting them together
files_sentdata <- lapply(setNames(nm= list.files(here::here(glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/")),
                                                 pattern = "^EHR sent message.*\\.xlsx$", 
                                                 full.names = TRUE,
                                                 recursive = TRUE)), 
                         readOneBook)
lapply(files_sentdata, rbindlist, idcol = "sheet", fill=TRUE)

rbindlist(lapply(files_sentdata, rbindlist, idcol = "sheet", fill=TRUE),
          idcol = "workbook"
)

commonsheets_sentdata <- Reduce(intersect, lapply(files_sentdata, names))

#Safety barrier in case a day of HL7 files is missing to avoid having incomplete daily data
maxrangesen <- as_date(format((Sys.Date()-1), "%Y/%m/%d"))

files_dayssent <- as.data.table(names(files_sentdata)) %>% 
  mutate(
    `V1` = str_sub(`V1`, 141, 152)) %>% 
  mutate_all(as.Date)

files_dayssent$weekday = weekdays.Date(files_dayssent$V1)

#this next line is to know what dates are missing. Since now its every Friday that the HL7 DQA is uploaded, 
#we are just interested in knowing that no Friday is missing
date_range_sen <- seq(min(firstdayyear), max(maxrangesen), by = 1)

misssent <- as.data.table(date_range_sen[!date_range_sen %in% files_dayssent$V1])
misssent$weekday = weekdays.Date(misssent)

misssent <- misssent%>% 
  mutate(number = ifelse(str_detect(weekday, "Saturday"), 1, 
                         if_else(str_detect(weekday, "Sunday"), 1,0)))


# All will start from 01/01/2025.
misssent <- subset(misssent,misssent$V1!= "2025-01-01")
misssent <- subset(misssent,misssent$V1!= "2025-01-10")
misssent <- subset(misssent,misssent$V1!= "2025-01-20")
misssent <- subset(misssent,misssent$V1!= "2025-02-17")
misssent <- subset(misssent,misssent$V1!= "2025-03-31")
misssent <- subset(misssent,misssent$V1!= "2025-04-18")
misssent <- subset(misssent,misssent$V1!= "2025-05-23") #I was out but included it in the next business day
misssent <- subset(misssent,misssent$V1!= "2025-05-26")
misssent <- subset(misssent,misssent$V1!= "2025-06-19")
misssent <- subset(misssent,misssent$V1!= "2025-07-04")
misssent <- subset(misssent,misssent$V1!= "2025-09-01")

misssent <- subset(misssent,misssent$V1!= "2025-11-27")
misssent <- subset(misssent,misssent$V1!= "2025-11-28")
misssent <- subset(misssent,misssent$V1!= "2025-12-24")
misssent <- subset(misssent,misssent$V1!= "2025-12-25")
misssent <- subset(misssent,misssent$V1!= "2026-01-01")
misssent <- subset(misssent,misssent$V1!= "2026-01-19")
misssent <- subset(misssent,misssent$V1!= "2026-02-16")
misssent <- subset(misssent,misssent$V1!= "2026-03-31")
misssent <- subset(misssent,misssent$V1!= "2026-04-03")
misssent <- subset(misssent,misssent$V1!= "2026-05-25")
misssent <- subset(misssent,misssent$V1!= "2026-06-19")
misssent <- subset(misssent,misssent$V1!= "2026-07-03")
misssent <- subset(misssent,misssent$V1!= "2026-09-07")


#01/01/2025 New Years
#01/10/2025 SNOW DAY
#01/20/2025 Martin Luther King Jr Day
#02/17/2025 Presidents Day
#03/31/2025 Cesar Chavez Day
#04/18/2025 Good Friday
#05/26/2025 Memorial Day
#06/19/2025 Juneteenth
#07/04/2025 Independence Day
#09/01/2025 Labor Day

if (0 %in% misssent$number){
  if(askYesNo(paste("At least 1 business day HL7 file is missing. 
Have you checked 'misssent' Data file in Global Environment to see what day/file is missing?")) == FALSE){
    stop()
  }
}

#creating the ''raw'' df with the SENT IMMZ excels merged
raw_sentdata<- lapply(setNames(nm = commonsheets_sentdata),
                      function(sht) rbindlist(lapply(files_sentdata, `[[`, sht), idcol = "workbook")) 
raw_sentdata[raw_sentdata == ""] <- NA
raw_sentdata <- as.data.table(raw_sentdata)

raw_sentdata <- raw_sentdata %>% 
  select(1, 2, 11:17, 19, 20, 25, 26)

colnames(raw_sentdata)[which(names(raw_sentdata) == "page.workbook")] <- "workbook"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Appointment.Facility.Name")] <- "Facility"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Patient.Name")] <- "Last.Name..First.Name..MI"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Patient.Acct.No")] <- "MRN"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Patient.DOB")] <- "DOB"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Patient.Age")] <- "Age"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Patient.Age.as.of.Given.Date")] <- "Age as of encounter date"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Given.Date")] <- "Vaccination.Date"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Lot.Number")] <- "Lot"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Immunization...Vaccine.Name")] <- "Vaccine.Name"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Dosage.No")] <- "Dose number"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Appointment.Provider.Name")] <- "OrderedBy"
colnames(raw_sentdata)[which(names(raw_sentdata) == "page.Primary.Insurance.Name")] <- "Insurance"


#Deduping raw_sentdata. We deduped before all immz contained within each excel workbook. This dedupes immz that may be repeated in different
# excel workbooks ergo, raw_sentdata (since raw_sentdata is a merged df of all the excels)

raw_sentdata <- raw_sentdata[!duplicated(raw_sentdata[,c("Last.Name..First.Name..MI", "DOB", "Vaccination.Date", "Vaccine.Name")]),]
raw_sentdata$Vaccination.Date <- as.Date(raw_sentdata$Vaccination.Date, origin = "1899-12-30")
raw_sentdata$DOB <- as.Date(raw_sentdata$DOB, origin = "1899-12-30")


#################### Number of Immunizations by Date ##### 
#this is ONLY for vaccines that we ADMINISTER! Does not include historical
raw_sentdata1 <- raw_sentdata[!(is.na(raw_sentdata$OrderedBy) | raw_sentdata$OrderedBy=="")]

ct_by_vaxdate <- as.data.frame(raw_sentdata1)

ct_by_vaxdate <- ct_by_vaxdate %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(count=n())
ct_by_vaxdate[rev(order(as.Date(ct_by_vaxdate$Vaccination.Date, format="%Y/%m/%d"))),] 

#ct_by_vaxdate$Vaccination.Date <-as.character(ct_by_vaxdate$Vaccination.Date)
colnames(ct_by_vaxdate)[2] <- "Immunizations"

# This line is for having a excel of all number of immunizations by date, for future use in other code
openxlsx::write.xlsx(ct_by_vaxdate, glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/immzbydate.xlsx"))

# Use this lines if you want to subset based on dates or days
ct_by_vaxdate <- ct_by_vaxdate[ct_by_vaxdate$Vaccination.Date >= "2025-01-01",] 

## echarts4r
### Bar-Chart ###
ct_by_vaxdate_plot <- ct_by_vaxdate%>% 
  e_charts(x = Vaccination.Date)%>% 
  e_bar(serie = Immunizations, 
        color = '#DCB59E',
        legend = FALSE,
        itemStyle = list(
          borderWidth = 1.5,
          borderColor = '#CA6A53')
  )%>% 
  e_tooltip(trigger = "item") %>% 
  e_title("Immunizations by Vaccination Date") %>% 
  e_axis_labels(y = "Immunizations", x ="Vaccination date") %>% 
  e_x_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = FALSE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted")
    )
  ) %>% 
  e_y_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = TRUE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted")
    )
  )%>% 
  e_datazoom(x_index = 0, type = "slider") %>%  
  e_datazoom(y_index = 0, type = "slider") 

ct_by_vaxdate_plot
#saving it as html

daypath4 = here::here(glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots"))
dir.create(file.path(daypath4), recursive = TRUE, showWarnings = F)

saveWidget(widget = ct_by_vaxdate_plot, 
           file = glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/Immunizations by Vaccination Date.html"))



#################### Count of Immz per vaccine group given by Date ####


#First, reading the Data Entry Reports excels, for SENT DATA and discarding all historical and duplicates
ct_vax_vaxgrp <- raw_sentdata[!(is.na(raw_sentdata$OrderedBy) | raw_sentdata$OrderedBy=="")]
ct_vax_vaxgrp <- ct_vax_vaxgrp[!(is.na(ct_vax_vaxgrp$Facility) | ct_vax_vaxgrp$Facility=="")]

colnames(ct_vax_vaxgrp)[10] <- "Vaccine"

# 7)No commas, no signs punctuation. KEEP SPACING.
ct_vax_vaxgrp$Vaccine <- sapply(ct_vax_vaxgrp$Vaccine, function(x) gsub("[[:punct:]]", "", x))

#to get the list of vaccines and be sure that everything was cleaned up
ct_vax_vaxgrp_vac2 <- unique(ct_vax_vaxgrp$Vaccine)
ct_vax_vaxgrp_vac2 <- as.data.frame(ct_vax_vaxgrp_vac2)


#cleaning name in case we need more analysis on them later.
#COVID19, Flu

ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Covid SpikeVax 12yrs\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Covid Comirnaty\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Covid Novavax\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Moderna SpikeVax 12 years\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Novavax 19 years\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPfizer Comirnaty 12 years\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Covid Comirnaty 12yrs\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Covid SpikeVax 12yrs\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Moderna Spikevax 12 years\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Pfizer Comirnaty 12 years single dose vial\\b", "COVID")

ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Covid Comirnaty 12yrs\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Covid Moderna 6mo11yrs\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Covid SpikeVax 12yrs\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Covid Pfizer 511 yrs\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Covid Pfizer 6mo4yrs\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Moderna 6mo11years\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Moderna Spikevax 12 years\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Novavax 12 years\\b", "COVID")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Pfizer Comirnaty 12 years\\b", "COVID")


ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Influenza PFS 05ml\\b", "Flu")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Influenza MDV 05ml\\b", "Flu")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Influenza MDV 025ml\\b", "Flu")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Influenza MDV 025ml\\b", "Flu")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Influenza MDV 05ml\\b", "Flu")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Influenza PFS 05ml\\b", "Flu")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Influenza PFS High Dose 65yrs\\b", "Flu")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Influenza MDV Cell Cultured\\b", "Flu")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Influenza PFS Cell Cultured\\b", "Flu")


ct_vax_vaxgrp <- dplyr::filter(ct_vax_vaxgrp, !grepl('COVID', Vaccine))
ct_vax_vaxgrp <- dplyr::filter(ct_vax_vaxgrp, !grepl('Flu', Vaccine))
ct_vax_vaxgrp <- dplyr::filter(ct_vax_vaxgrp, !grepl('TCPH TB Skin Test PPDTST', Vaccine))
ct_vax_vaxgrp <- dplyr::filter(ct_vax_vaxgrp, !grepl('Cabenuva 600900mg kit', Vaccine))
ct_vax_vaxgrp <- dplyr::filter(ct_vax_vaxgrp, !grepl('Valacyclovir10', Vaccine))




# VFC
# ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bMMR\\b", "MMR VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC DTaPHep BIPV Pediarix\\b", "DTaP-HepB-IPV VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC DTaPHIBIPV Pentacel\\b", "DTaP-Hib-IPV VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC DTaPIPV Kinrix\\b", "DTaP-IPV VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC DTaPIPV Quadracel\\b", "DTaP-IPV VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC DTaP Daptacel\\b", "DTaP VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC DTaP Infanrix\\b", "DTaP VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC DTaPIPVHIBHEP B Vaxelis\\b", "DTaP-IPV-Hib-HePB VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC DTaP\\b", "DTaP VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Hep A Vaqta 05mL\\b", "HepA VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Hep A Havrix 05mL\\b", "HepA VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Hepatitis A\\b", "HepA VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Hep B EngerixB 05mL\\b", "HepB VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Hep B RecombivaxB 05mL\\b", "HepB VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Hepatitis B\\b", "HepB VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Hep AB Twinrix\\b", "HepA-HepB VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Hemophilus Influenza B ActHIB\\b", "Hib VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Hemophilus Influenza BHIB\\b", "Hib VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC HPV9 Gardisil 9\\b", "HPV9 VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Polio IPOL\\b", "IPV VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC PENBRAYA\\b", "Meningo 5 VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC MCV4 Menveo\\b", "MCV4 VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC MCV4 MenQuadfi\\b", "MCV4 VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Men B Bexsero\\b", "MenB VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Men B Trumenba\\b", "MenB VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC MMRV ProQuad\\b", "MMRV VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC MMR MMR II\\b", "MMR VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC MMR Priorix\\b", "MMR VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC PCV13 Prevnar 13\\b", "PCV13 VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Vaxneuvance\\b", "PCV15 VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC PCV15 Vaxneuvance\\b", "PCV15 VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC PCV20 Prevnar 20\\b", "PCV20 VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC PPSV Pneumovax23\\b", "PPSV VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Rotavirus RotaTeq\\b", "Rotavirus VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Rotavirus Rotarix 15mL\\b", "Rotavirus VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Rotavirus Rotarix 1mL\\b", "Rotavirus VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC RSV Beyfortus 1ml\\b", "RSV VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC RSV Beyfortus 05 ml\\b", "RSV VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC TDaP Boostrix\\b", "Tdap VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC TDaP Adacel\\b", "Tdap VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Tetanus Diphtheria Acellular PertusisTdap\\b", "Tdap VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC TD Tenivac\\b", "Td VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC TD TdVax\\b", "Td VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Tetanus DiphtheriaTd\\b", "Td VFC")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bVFC Varicella Varivax\\b", "Varicella VFC")


# ASN
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Hep A Vaqta 1mL\\b", "HepA ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Hep A Havrix 1mL\\b", "HepA ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Hepatitis A\\b", "HepA ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Hep B EngerixB 1mL\\b", "HepB ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Hepatitis B 3 dose\\b", "HepB ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Hep B Recombivax HB 1mL\\b", "HepB ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Hep AB Twinrix\\b", "HepA-HepB ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN HPV9 Gardisil 9\\b", "HPV9 ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN MCV4 MenQuadfi\\b", "MCV4 ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN MCV4 Menveo\\b", "MCV4 ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN MMR MMR II\\b", "MMR ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN PCV20 Prevnar 20\\b", "PCV20 ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN PPSV Pneumovax23\\b", "PPSV ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Tetanus Diphtheria Acellular PertusisTdap\\b", "Tdap ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN TDaP Boostrix\\b", "Tdap ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN TDaP Adacel\\b", "Tdap ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN TD TdVax\\b", "Td ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN TD Tenivac\\b", "Td ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Tetanus DiphtheriaTd\\b", "Td ASN")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bASN Varicella Varivax\\b", "Varicella ASN")


#MPOX
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bMPX Jynneos SQ\\b", "MPX")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bMPX JYNNEOS 1st Dose ID\\b", "MPX")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bMPX JYNNEOS 2nd Dose SB\\b", "MPX")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bMPX JYNNEOS 1st Dose\\b", "MPX")


#Private Pay
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Chikungunya Ixchiq\\b", "Chikungunya PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hep A Havrix 1mL\\b", "HepA PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hep A Havrix 05mL\\b", "HepA PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hepatitis A Adult\\b", "HepA PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hepatitis A Pediatric\\b", "HepA PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hep A Vaqta 1mL\\b", "HepA PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hep B EngerixB 1mL\\b", "HepB PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hepatitis B Adult 3 dose\\b", "HepB PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hep B HeplisavB\\b", "HepB PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hep B PreHevbrio\\b", "HepB PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hep B Recombivax HB 1mL\\b", "HepB PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Hep AB Twinrix\\b", "HepA-HepB PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP HPV9 Gardisil 9\\b", "HPV9 PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Japanese Encephalitis Ixiaro\\b", "Jap. Encephalitis PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP MMR MMR II\\b", "MMR PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP MMR Priorix\\b", "MMR PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP MCV4 MenQuadfi\\b", "MCV4 PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Men B BEXSERO\\b", "MCV4 PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP MCV4 Menveo\\b", "MCV4 PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bMCV4 PP 2vial\\b", "MCV4 PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP PCV20 Prevnar 20\\b", "PCV20 PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Pneumococcal conjugate PCV 20\\b", "PCV20 PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP PPSV Pneumovax23\\b", "PPSV PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Polio IPOL\\b", "IPV PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Rabies Imovax\\b", "Rabies PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Rabies\\b", "Rabies PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP RSV Arexvy\\b", "RSV PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP RSV Abrysvo\\b", "RSV PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Tetanus Diphtheria Acellular PertusisTdap\\b", "Tdap PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP TDaP Boostrix\\b", "Tdap PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP TDaP Adacel\\b", "Tdap PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP TD Tenivac\\b", "Td PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Tetanus DiphtheriaTd\\b", "Td PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP TickBorn Enceph Ticovac 05mL\\b", "Tick-borne enceph. PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Typhoid Injectable Typhim Vi\\b", "Typhoid Inj PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Typhoid Oral Vivotif\\b", "Typhoid Oral PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Varicella Varivax\\b", "Varicella PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Yellow Fever YFVax\\b", "Yellow Fever PP")
ct_vax_vaxgrp$Vaccine <- str_replace(ct_vax_vaxgrp$Vaccine, "\\bPP Zoster Shingrix\\b", "Zoster PP")



#to get the list of vaccines and be sure that everything was cleaned up
ct_vax_vaxgrp_vac <- unique(ct_vax_vaxgrp$Vaccine)
ct_vax_vaxgrp_vac <- as.data.frame(ct_vax_vaxgrp_vac)

# Line of code to trip and error if there are different variations in vaccine names
# The most variations we had of vaccines names once cleaned up, was 58. 
# As of 01/02/2025, for all 2025, this was up to 31. Change this number accordingly
if (nrow(ct_vax_vaxgrp_vac) != 50) {stop("There are changes to the vaccines reported! Check ct_vax_vaxgrp_vac.")}


#### script to create count of immz per vax group

ct_vax_vaxgrp <- ct_vax_vaxgrp %>% 
  mutate(
    Chikungunya = str_detect(`Vaccine`, "Chikungunya"),
    DTaP = str_detect(`Vaccine`, "DTaP"),
    HepA = str_detect(`Vaccine`, "HepA"),
    HepB = str_detect(`Vaccine`, "HepB"),
    Hib = str_detect(`Vaccine`, "Hib"),
    HPV9 = str_detect(`Vaccine`, "HPV9"),
    IPV = str_detect(`Vaccine`, "IPV"),
    Jap_Encephalitis = str_detect(`Vaccine`, "Jap. Encephalitis PP"),
    MCV4 = str_detect(`Vaccine`, "MCV4"),
    MenB = str_detect(`Vaccine`, "MenB"),
    Meningo_5 = str_detect(`Vaccine`, "Meningo 5 VFC"),
    MMR = str_detect(`Vaccine`, "MMR"),
    MPX = str_detect(`Vaccine`, "MPX"),
    Pneumo = str_detect(`Vaccine`, "PCV|PPSV"),
    Rabies = str_detect(`Vaccine`, "Rabies"),
    Rotavirus = str_detect(`Vaccine`, "Rotavirus"),
    RSV = str_detect(`Vaccine`, "RSV"),
    Td_Tdap = str_detect(`Vaccine`, "Td"),
    Tick_borne_enceph = str_detect(`Vaccine`, "Tick-borne enceph"),
    Typhoid = str_detect(`Vaccine`, "Typhoid"),
    Varicella = str_detect(`Vaccine`, "Varicella"),
    Yellow_Fever = str_detect(`Vaccine`, "Yellow"),
    Zoster = str_detect(`Vaccine`, "Zoster"),)

#changing TRUE to 1 so we can add them up. Rather do it by name of columns than position for safety.
ct_vax_vaxgrp$Chikungunya <- ct_vax_vaxgrp$Chikungunya %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$DTaP <- ct_vax_vaxgrp$DTaP %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$HepA <- ct_vax_vaxgrp$HepA %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$HepB <- ct_vax_vaxgrp$HepB %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Hib <- ct_vax_vaxgrp$Hib %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$HPV9 <- ct_vax_vaxgrp$HPV9 %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$IPV <- ct_vax_vaxgrp$IPV %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Jap_Encephalitis <- ct_vax_vaxgrp$Jap_Encephalitis %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$MCV4 <- ct_vax_vaxgrp$MCV4 %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$MenB <- ct_vax_vaxgrp$MenB %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Meningo_5 <- ct_vax_vaxgrp$Meningo_5 %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$MMR <- ct_vax_vaxgrp$MMR %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$MPX <- ct_vax_vaxgrp$MPX %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Pneumo <- ct_vax_vaxgrp$Pneumo %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Rabies <- ct_vax_vaxgrp$Rabies %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Rotavirus <- ct_vax_vaxgrp$Rotavirus %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$RSV <- ct_vax_vaxgrp$RSV %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Td_Tdap <- ct_vax_vaxgrp$Td_Tdap %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Tick_borne_enceph <- ct_vax_vaxgrp$Tick_borne_enceph %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Typhoid <- ct_vax_vaxgrp$Typhoid %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Varicella <- ct_vax_vaxgrp$Varicella %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Yellow_Fever <- ct_vax_vaxgrp$Yellow_Fever %>% 
  sapply(., \(x) +as.logical(x))
ct_vax_vaxgrp$Zoster <- ct_vax_vaxgrp$Zoster %>% 
  sapply(., \(x) +as.logical(x))

#Creating the chart for the plot
CHIK_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(Chikungunya=sum(Chikungunya))
DTAP_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(DTaP=sum(DTaP))
HEPA_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(HepA=sum(HepA))
HEPB_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(HepB=sum(HepB))
HIBB_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(Hib=sum(Hib))
HPV9_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(HPV9=sum(HPV9))
IPVV_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(IPV=sum(IPV))
JAPE_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(`Jap. Encephalitis` = sum(Jap_Encephalitis))
MCV4_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(MCV4=sum(MCV4))
MENB_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(MenB=sum(MenB))
MEN5_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(`Meningo 5`=sum(Meningo_5))
MMRR_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(MMR=sum(MMR))
MPOX_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(MPX=sum(MPX))
PNEU_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(Pneumo=sum(Pneumo))
RABI_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(Rabies=sum(Rabies))
ROTA_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(Rotavirus=sum(Rotavirus))
RSVV_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(RSV=sum(RSV))
TDAP_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(`Td/Tdap` = sum(Td_Tdap))
TICK_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(`Tick-borne enceph.` = sum(Tick_borne_enceph))
TYPH_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(Typhoid=sum(Typhoid))
VARI_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(Varicella=sum(Varicella))
YEFE_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(`Yellow Fever`=sum(Yellow_Fever))
ZOST_vaxgrp <- ct_vax_vaxgrp %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(Zoster=sum(Zoster))


#putting them in a single list
ct_vax_vaxgrpchart <- list("Chikungunya" = CHIK_vaxgrp, "DTaP" = DTAP_vaxgrp, "HepA" = HEPA_vaxgrp, "HepB"= HEPB_vaxgrp, "Hib"= HIBB_vaxgrp, "HPV9" = HPV9_vaxgrp, "IPV" = IPVV_vaxgrp, "Jap. Encephalitis" = JAPE_vaxgrp, "MCV4"= MCV4_vaxgrp, "MenB" = MENB_vaxgrp, "Meningo 5" = MEN5_vaxgrp, "MMR" = MMRR_vaxgrp, "MPX"= MPOX_vaxgrp, "PCV/PPSV" = PNEU_vaxgrp, "Rabies" = RABI_vaxgrp, "Rotavirus" = ROTA_vaxgrp,"RSV"= RSVV_vaxgrp, "Td/Tdap" = TDAP_vaxgrp, "Tick-borne enceph."= TICK_vaxgrp, "Typhoid" = TYPH_vaxgrp, "Varicella" = VARI_vaxgrp, "Yellow Fever" = YEFE_vaxgrp, "Zoster"= ZOST_vaxgrp)


# joining all the merged sheets into a single table with Vaccination Date on Y and Errors on X. 
ct_vax_vaxgrpchart <- list(CHIK_vaxgrp, DTAP_vaxgrp, HEPA_vaxgrp, HEPB_vaxgrp, HIBB_vaxgrp, HPV9_vaxgrp, IPVV_vaxgrp, JAPE_vaxgrp, MCV4_vaxgrp, MENB_vaxgrp, MEN5_vaxgrp, MMRR_vaxgrp, MPOX_vaxgrp, PNEU_vaxgrp, RABI_vaxgrp, ROTA_vaxgrp, RSVV_vaxgrp, TDAP_vaxgrp,TICK_vaxgrp, TYPH_vaxgrp, VARI_vaxgrp, YEFE_vaxgrp, ZOST_vaxgrp) %>% 
  reduce(full_join, by = "Vaccination.Date")

# setting the cells blanks/NA to 0
ct_vax_vaxgrpchart[is.na(ct_vax_vaxgrpchart)] <- 0

#changing the format from wide to long, so we can plot it
ct_vax_vaxgrpchart <- as.data.table(ct_vax_vaxgrpchart)
ct_vax_vaxgrpchart$Vaccination.Date <-  as.Date(ct_vax_vaxgrpchart$Vaccination.Date)
#ct_vax_vaxgrpchart <- melt(setDT(ct_vax_vaxgrpchart), id.vars = "Vaccination.Date")

# To add the totals for each day, we'll be attaching a total columns and avoid melting the data (such as the previous line of melt)
ct_vax_vaxgrpchart$total <- rowSums(ct_vax_vaxgrpchart[,2:24])
ct_vax_vaxgrpchart$zero <- 0

openxlsx::write.xlsx(ct_vax_vaxgrpchart, glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/{monthyr} ct_vax_vaxgrpchart.xlsx"))

# Use this lines if you want to subset based on dates or last days
ct_vax_vaxgrpchart<- ct_vax_vaxgrpchart[ct_vax_vaxgrpchart$Vaccination.Date >= "2025-01-01",]


## echarts4r
### Bar-Chart Stacked ###
colors = c("#5D7E82",
           "#7D7E82",
           "#4C3113",
           "#8C3113",
           "#6C7113",
           "#6C3113",
           "#9C3113",
           "#2292D0",
           "#5292D0",
           "#7292D0",
           "#A6E2BE",
           "#A6E5FE",
           "#284E5F",
           "#281E5F",
           "#282E5F",
           "#484E5F",
           "#489E5F",
           "#CA6A53",
           "#FA6A53",
           "#DCB59E",
           "#DCD59E",
           "#803B21",
           "#23513C",
           "#23713C",
           "#23913C"
)
ct_vax_vaxgrpplot <- ct_vax_vaxgrpchart %>%
  e_chart(Vaccination.Date) %>% 
  e_bar(serie = `Chikungunya`, name = "Chikungunya", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `DTaP`, name = "DTaP", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `HepA`, name = "HepA", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `HepB`, name = "HepB", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Hib`, name = "Hib", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `HPV9`, name = "HPV9", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `IPV`, name = "IPV", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Jap. Encephalitis`, name = "Jap. Encephalitis", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `MCV4`, name = "MCV4", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `MenB`, name = "MenB", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Meningo 5`, name = "Meningo 5", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `MMR`, name = "MMR", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `MPX`, name = "MPX", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Pneumo`, name = "Pneumo", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Rabies`, name = "Rabies", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Rotavirus`, name = "Rotavirus", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `RSV`, name = "RSV", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Td/Tdap`, name = "Td/Tdap", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Tick-borne enceph.`, name = "Tick-borne enceph.", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Typhoid`, name = "Typhoid", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Varicella`, name = "Varicella", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Yellow Fever`, name = "Yellow Fever", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Zoster`, name = "Zoster", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(
    serie = zero, bind = total, name = "Total",
    stack = "stack",
    label = list(
      show = TRUE,
      formatter = "{b}",
      position = "top"
    ),
    legend = NULL
  ) %>%
  e_color(colors)%>% 
  e_tooltip(trigger = "item") %>% 
  e_title("Vaccine groups given by Vaccination date") %>% 
  e_axis_labels(y = "Count", x ="Vaccination date") %>% 
  e_x_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = FALSE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted")
    )
  ) %>% 
  e_y_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = TRUE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted")
    )
  ) %>% 
  e_legend(top = 40,
           align = "right",
           orient = "vertical",
           right = 40,
           height = 800) %>% 
  e_datazoom(x_index = 0, type = "slider") %>%  
  e_datazoom(y_index = 0, type = "slider") 

ct_vax_vaxgrpplot


#saving it as html
saveWidget(widget = ct_vax_vaxgrpplot, 
           file = glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/Count of Vaccine groups administered by Vaccination date.html"))

#################### Number of Patients vaccinated by Date ####

# Only administered vaccines, and getting how many unique patients are vaccinated per day.
raw_sentdata3 <- raw_sentdata[!(is.na(raw_sentdata$OrderedBy) | raw_sentdata$OrderedBy=="")]
ptct_by_vaxdate <- raw_sentdata3 %>% 
  select(4, 8)

ptct_by_vaxdate <- ptct_by_vaxdate %>% 
  distinct(MRN, Vaccination.Date)

#counting how many patients are per vaccination date, an adding it as a column to be able to plot it
ptct_by_vaxdate <- ptct_by_vaxdate %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n()) 
ptct_by_vaxdate <- as.data.table(ptct_by_vaxdate)
ptct_by_vaxdate$Vaccination.Date <-  as.Date(ptct_by_vaxdate$Vaccination.Date, format="%m/%d/%Y")

colnames(ptct_by_vaxdate)[2] <- "Patients"

# This line is for having a excel of all number of patients vaccinated by date, for future use in other code
openxlsx::write.xlsx(ptct_by_vaxdate, glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/ptbyvaxdate.xlsx"))

# Use this lines if you want to subset based on dates or last days
ptct_by_vaxdate <- ptct_by_vaxdate[ptct_by_vaxdate$Vaccination.Date >= "2025-01-01",]

## echarts4r
### Bar-Chart ###
ptct_by_vaxdate_plot <- ptct_by_vaxdate%>% 
  e_charts(x = Vaccination.Date)%>% 
  e_bar(serie = Patients, 
        color = '#DCB59E',
        legend = FALSE,
        itemStyle = list(
          borderWidth = 1,
          borderColor = '#CA6A53')
  )%>% 
  e_tooltip(trigger = "item") %>% 
  e_title("Patients Vaccinated by Day") %>% 
  e_axis_labels(y = "Patients", x ="Vaccination date") %>% 
  e_x_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = FALSE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted")
    )
  ) %>% 
  e_y_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = TRUE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted")
    )
  )%>% 
  e_datazoom(x_index = 0, type = "slider") %>%  
  e_datazoom(y_index = 0, type = "slider") 
ptct_by_vaxdate_plot

saveWidget(widget = ptct_by_vaxdate_plot, 
           file = glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/Patients Vaccinated by Date.html"))



#################### Vaccines given by clinic per program #####








































############ BIDX/SENT IMMZ: DQA-RELATED CHARTS #################################

# Immunizations Rejected/Accepted by Date
# Number of Patients Rejected/Accepted by Date 

##################################   Libraries   ************************************

here::here()

################ CHARTS START HERE: BIDX/SENT ####
#################### Immunizations Rejected/Accepted by Date #################### 

# Re-using raw_df, since thats already a deduped df of all REJECTIONS.Just changing type of column from chr to date.

raw_df$DOB <- as.Date(raw_df$DOB)
raw_df$Senton <- as.Date(raw_df$Senton)
raw_df$Vaccination.Date <- as.Date(raw_df$Vaccination.Date)

# Changing the format of Vaccination date to Date. Checking the number of observations per type of errors 
# we are NOT including past or historical vaccines. only current/administered by us, which is
# something joined_tot already has (Deduped and filtered by only current and starting 07/30/2024)

# 08/20/2024

joined_tot


#changing the format from wide to long, so it can be plotted later
rejdf <- as.data.table(joined_rejls)
rejdf <- melt(setDT(rejdf), id.vars = "Vaccination.Date")

rejdf %>% mutate(variable = "Rejected")
rejdf <- rejdf %>% group_by(`Vaccination.Date`) %>% dplyr::summarize(value=sum(value))
rejdf <- rejdf %>% mutate(variable = "Rejected")

# For reading the Data Entry Reports excels, "ct_by_vaxdate" is the equivalent, done beforehand and already 
# filtered by provider (Orderedby) to only have administered and not historical vaccines.
# we'll add a column named "Sent Immz" 
ct_by_vaxdateviz <- ct_by_vaxdate %>% 
  mutate(variable = "SentImmz") 
colnames(ct_by_vaxdateviz)[which(names(ct_by_vaxdateviz) == "Immunizations")] <- "value"

#merging both df and getting the number of accepted immz
jt_rej_ctbyvaxdate <- rbind(rejdf, ct_by_vaxdateviz)

jt_rej_ctbyvaxdate <- jt_rej_ctbyvaxdate %>% 
  dcast(Vaccination.Date ~ variable, value.var = "value", fill = 0) %>% 
  mutate(Accepted = SentImmz - Rejected) %>% 
  mutate(pctage = round((Rejected/SentImmz)*100))

# This line is for having a excel of all immunizations rej/accp by vax date for future use
#openxlsx::write.xlsx(jt_rej_ctbyvaxdate, glue("Charts_and_Plots/Based on Immz Sent Data/{yr}/{monthyr}/{monthyr} Plots/immzrejaccep_byday.xlsx"))
openxlsx::write.xlsx(jt_rej_ctbyvaxdate, glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/immzrejaccep_byday.xlsx"))

#subsetting for 2025 and onwards. Change here if you want a different date range
jt_rej_ctbyvaxdate <- jt_rej_ctbyvaxdate[jt_rej_ctbyvaxdate$Vaccination.Date >= "2025-01-01",]

# If column Number = 0, means that there is a mismatch (More Rejected immz than Sent for same day, which is impossible), will stop script.
jt_rej_ctbyvaxdate$number <- ifelse(jt_rej_ctbyvaxdate$SentImmz >= jt_rej_ctbyvaxdate$Rejected, 1, 0)

if (0 %in% jt_rej_ctbyvaxdate$number){
  if(askYesNo(paste("Oops! There are more Rejections than Sent Immz in at least 1 day. How is that possible? Have you checked 'jt_rej_ctbyvaxdate'? Find the 0 in the 'number' column")) == FALSE){
    stop()
  }
}

#this is the percentage of rejection/accep to have on a single column to be plotted
pctage <- jt_rej_ctbyvaxdate[,c("Vaccination.Date", "pctage")]


## echarts4r 
### Stacked Bar-Chart ###
colors = c("#CA6A53","#A6E2BE")

rej_ctbyvaxdate_plot <- jt_rej_ctbyvaxdate %>% 
  e_charts(Vaccination.Date) %>%
  e_bar(Rejected, stack = "grp",
        itemStyle = list(
          borderWidth = 1,
          borderColor = '#404040',
          bind = pctage)) %>%
  e_bar(Accepted, stack = "grp",
        itemStyle = list(
          borderWidth = 1,
          borderColor = '#404040',
          bind = pctage)) %>%
  e_scatter(pctage,
            label=list(
              show=TRUE,
              formatter = htmlwidgets::JS(
                "(params) => params.value[1] + '%';
                "),
              color = "#151515",
              fontSize = 14,
              fontWeight = 'bold',
              fontStyle = 'oblique',
              position = "insideBottom",
              distance = 150
            )
  ) %>% 
  e_color(colors)%>% 
  e_title("Immunizations Rejected/Accepted given by day") %>% 
  e_axis_labels(y = "Count", x ="Vaccination date") %>% 
  e_x_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = TRUE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted"))
  ) %>% 
  e_y_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = FALSE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted"))
  ) %>% 
  e_legend(right= TRUE)%>% 
  e_datazoom(x_index = 0, type = "slider") %>%  
  e_datazoom(y_index = 0, type = "slider")

rej_ctbyvaxdate_plot

#saveWidget(widget = rej_ctbyvaxdate_plot, 
#           file = glue("Charts_and_Plots/Based on Immz Sent Data/{yr}/{monthyr}/{monthyr} Plots/Immunizations RejAcc given by Date.html"))

saveWidget(widget = rej_ctbyvaxdate_plot, 
           file = glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/Immunizations RejAcc given by Date.html"))


#################### Number of Patients Rejected/Accepted by Date  ######################

#Getting the df for only number of Patients REJECTED by Vaccination Date: This is already
# deduped and with the counts, and already filtered by provider (Orderedby) so it doesnt contained historical vaccines
#ratio_rejpt <- read_excel(here::here(glue("Charts_and_Plots/Based on Immz Sent Data/{yr}/{monthyr}/{monthyr} Plots/tot_rejpt.xlsx")))

ratio_rejpt <- read_excel(here::here(glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/tot_rejpt.xlsx")))


ratio_rejpt <- ratio_rejpt %>% 
  mutate(variable = "Rejected") 
ratio_rejpt$Vaccination.Date <-  as.Date(ratio_rejpt$Vaccination.Date, format="%m/%d/%Y")


#Getting the df for only number of Patients SENT by Vaccination Date
ratio_senpt <- raw_sentdata[,c("Vaccination.Date", "Last.Name..First.Name..MI", "DOB", "Patient.Address")]

#counting per vaccination date and adding tag of SenPts.
ratio_senpt <- ratio_senpt %>% group_by(Vaccination.Date) %>% dplyr::summarize(count=n()) 
ratio_senpt <- ratio_senpt %>% 
  mutate(variable = "SenPts") 
ratio_senpt$Vaccination.Date <-  as.Date(ratio_senpt$Vaccination.Date, format="%m/%d/%Y")

#merging both df to be plotted
ratio_rejacceppts <- rbind(ratio_senpt, ratio_rejpt)

ratio_rejacceppts<- ratio_rejacceppts %>% 
  dcast(Vaccination.Date ~ variable, value.var = "count", fill = 0) %>% 
  mutate(Accepted = SenPts - Rejected) %>% 
  mutate(pctage_pt = round((Rejected/SenPts)*100))


# This line is for having a excel of all NUMBER OF PATIENTS REJECTED/ACCEPTED BY VAX DATE, for future use in other code
#openxlsx::write.xlsx(ratio_rejacceppts, glue("Charts_and_Plots/Based on Immz Sent Data/{yr}/{monthyr}/{monthyr} Plots/ratio_rejacceppts.xlsx"))

openxlsx::write.xlsx(ratio_rejacceppts, glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/ratio_rejacceppts.xlsx"))


#subsetting to 2025
ratio_rejacceppts<- ratio_rejacceppts[ratio_rejacceppts$Vaccination.Date >= "2025-01-01",]


# If column Number = 0, means that there is a mismatch (More Rejected immz than Sent for same day, which is impossible), will stop script.
ratio_rejacceppts$number <- ifelse(ratio_rejacceppts$SenPts >= ratio_rejacceppts$Rejected, 1, 0)

if (0 %in% ratio_rejacceppts$number){
  if(askYesNo(paste("Oops! There are more Rejections than Sent Immz in at least 1 day. How is that possible? Have you checked 'ratio_rejacceppts'? Find the 0 in the 'number' column")) == FALSE){
    stop()
  }
}
pctage_pt <- ratio_rejacceppts[,c("Vaccination.Date", "pctage_pt")]


## echarts4r 
### Stacked Bar-Chart ###
colors = c("#CA6A53","#A6E2BE")

ratio_rejacceppts_plot <- ratio_rejacceppts%>% 
  e_charts(Vaccination.Date) %>%
  e_bar(Rejected, stack = "grp",
        itemStyle = list(
          borderWidth = 1,
          borderColor = '#404040')) %>%
  e_bar(Accepted, stack = "grp",
        itemStyle = list(
          borderWidth = 1,
          borderColor = '#404040')) %>% 
  e_scatter(pctage_pt,
            label=list(
              show=TRUE,
              formatter = htmlwidgets::JS(
                "(params) => params.value[1] + '%';
                "),
              color = "#151515",
              fontSize = 14,
              fontWeight = 'bold',
              fontStyle = 'oblique',
              position = "insideBottom",
              distance = 150
            )
  ) %>% 
  e_color(colors)%>% 
  e_title("Patients Rejected/Accepted by day") %>% 
  e_axis_labels(y = "Patient Count", x ="Vaccination date") %>% 
  e_x_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = FALSE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted"))
  ) %>% 
  e_y_axis(
    nameLocation = "center", 
    splitArea = list(show = FALSE),
    axisLabel = list(margin = 3),
    axisPointer = list(
      show = FALSE, 
      lineStyle = list(
        color = "#999999",
        width = 1.75,
        type = "dotted"))
  ) %>% 
  e_legend(right= TRUE)%>% 
  e_datazoom(x_index = 0, type = "slider") %>%  
  e_datazoom(y_index = 0, type = "slider") %>% 
  e_tooltip(trigger = "item")

ratio_rejacceppts_plot

#saveWidget(widget = ratio_rejacceppts_plot, 
#           file = glue("Charts_and_Plots/Based on Immz Sent Data/{yr}/{monthyr}/{monthyr} Plots/Patients RejAcc given by Date.html"))

saveWidget(widget = ratio_rejacceppts_plot, 
           file = glue("Charts_and_Plots/Based_on_EHR_reports/{yr}/{monthyr}/{monthyr} Plots/Patients RejAcc given by Date.html"))

