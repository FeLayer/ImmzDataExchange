######## Libraries

library(stringr)
library(readxl)
library(openxlsx)
require(openxlsx)
library(here)
library(tidyr)
library(dplyr)
library(purrr)
library(readr)
library(glue)
library(data.table)
library(lubridate)
library(ggplot2)
library(plyr)
library(reshape2)
library(anytime)
library(readr)
library(plotly)
library(htmlwidgets)
library(echarts4r)

#Specify the month and month year: its being run every day so month and year should match this date.
month <- format((Sys.Date()), "%B")
month <- substr(month,1,3)
yr <- format((Sys.Date()), "%Y")
monthyr <- paste(month,yr)
today <- format(Sys.time(),"_ran_on_%m_%d_%Y %H_%M_%S")

here::here()
#
# #read in the HL7 file. This is a "bucket".
# #After, it makes a df out of it.
# here::here("ARCHIVE/FTP/RSV")
#
# reportcsv_rsv<-
# list.files(path = here::here("ARCHIVE/FTP/RSV"), pattern = "\\.csv$", full.names = TRUE) %>%
# map_df(~read_csv(., id = "filename", col_names = F, show_col_types = FALSE))
#
# colnames(reportcsv_rsv) <- reportcsv_rsv[1,]
# reportcsv_rsv <- reportcsv_rsv[-1,]
#
# #Making it into a dataframe and just filtering those who are for TCPH as of now.
# #Maybe later we can process the rest of the data
# rawreport_rsv <- as.data.table(reportcsv_rsv)
# rawreport_rsv[is.na(rawreport_rsv)] <- "0"
#
# #We need to save as much space as possible. Filtering and removing irrelevant stuff 10/17/2024.
# rawreport_rsv <- rawreport_rsv %>% filter(grepl('RSV' , VACCINE_GROUP))
#
# rawreport_rsv = subset(rawreport_rsv, select = -c(STATECODE,PROVIDER_STATE,VACCINE_ID,IMMUN_FACT_ID,IMMUNIZATION_ID,SHORT_NAME))
#
# rawreport_rsv$CVX_CODE <- sub("\\.\\d+$", "", rawreport_rsv$CVX_CODE)
#
# #change this according to month
# colnames(rawreport_rsv)[1] <- glue("{monthyr}")
# rawreport_rsv$BIRTH_DATE  <- as.Date(rawreport_rsv$BIRTH_DATE, "%Y-%m-%d")
# rawreport_rsv$VACCINATION_DATE <- as.Date(rawreport_rsv$VACCINATION_DATE, "%Y-%m-%d")
# write.csv(rawreport_rsv, glue("ARCHIVE/FTP/RSV/FTP_{monthyr}_rsv.csv"))

############
############
# Start here if you already processed the csv report from FTP

rawreport_rsv <-
  list.files(path = here::here("ARCHIVE/FTP/RSV"), pattern = "\\.csv$", full.names = TRUE) %>%
  map_df(~read_csv(., id = "filename", col_names = F, show_col_types = FALSE))

colnames(rawreport_rsv) <- rawreport_rsv[1,]
rawreport_rsv <- rawreport_rsv[-1,]
rawreport_rsv <- rawreport_rsv[, -c(1:3)]

# Correcting some duplicate accounts

rawreport_rsv$GIVEN_BY_ORG_ID <- str_replace_all(rawreport_rsv$GIVEN_BY_ORG_ID, "25300306", "1502116016")
rawreport_rsv$GIVEN_BY_ORG_ID <- str_replace_all(rawreport_rsv$GIVEN_BY_ORG_ID, "25300302", "1502116005")
rawreport_rsv$GIVEN_BY_ORG_ID <- str_replace_all(rawreport_rsv$GIVEN_BY_ORG_ID, "25289635", "25277918")
rawreport_rsv$PROVIDER_NAME <- str_replace_all(rawreport_rsv$PROVIDER_NAME, "Kashif Anwar, M.D., P.A. - DUPLICATE", "KASHIF ANWAR MD PA")


# unique values that we could categorize
# Count of total immz Vaccination date (day)
VAX_DATE_rsv <- rawreport_rsv %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(count=n())

# Vaccine group
VACCINE_GROUP_rsv <- rawreport_rsv %>% group_by(`VACCINE_GROUP`) %>% dplyr::summarize(count=n())

# Unique patient
UNIQUE_PT_rsv <- rawreport_rsv %>% group_by(`CLIENT_ID`) %>% dplyr::summarize(count=n())

# Org ID
PROVIDER_ID_rsv <- rawreport_rsv %>% group_by(`GIVEN_BY_ORG_ID`) %>% dplyr::summarize(count=n())

# PROVIDER name: we'll use provider name since its more possible that instead of renewing, 
# some clinic forgot to do so and created a new org on ImmTrac2
PROVIDER_NAME_rsv <- rawreport_rsv %>% group_by(`PROVIDER_NAME`) %>% dplyr::summarize(count=n())

# ZIP code: patient
PATIENT_ZIP_rsv <- rawreport_rsv %>% group_by(`ZIP_CODE`) %>% dplyr::summarize(count=n())
#write.csv(PATIENT_ZIP_rsv, glue("ARCHIVE/FTP/RSV/PATIENT_ZIP_rsv.csv"))

# ZIP code: provider
PROVIDER_ZIP_rsv <- rawreport_rsv %>% group_by(`PROVIDER_ZIP_CODE`) %>% dplyr::summarize(count=n())

# CVX code
CVX_rsv <- rawreport_rsv %>% group_by(`CVX_CODE`) %>% dplyr::summarize(count=n())


#########################################   Plots   ###########################################



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ██████╗ ██╗      ██████╗ ████████╗███████╗
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ██╔══██╗██║     ██╔═══██╗╚══██╔══╝██╔════╝
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ██████╔╝██║     ██║   ██║   ██║   ███████╗
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ██╔═══╝ ██║     ██║   ██║   ██║   ╚════██║
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ██║     ███████╗╚██████╔╝   ██║   ███████║
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  ╚═╝     ╚══════╝ ╚═════╝    ╚═╝   ╚══════╝



# Immz count by vaccination date #############################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  _____                   _     _           
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% /  __ \                 | |   | |          
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% | /  \/ ___  _   _ _ __ | |_  | |__  _   _ 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% | |    / _ \| | | | '_ \| __| | '_ \| | | |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% | \__/\ (_) | |_| | | | | |_  | |_) | |_| |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  \____/\___/ \__,_|_| |_|\__| |_.__/ \__, |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                       __/ |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                     |___/ 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                     _       _              
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                    | |     | |             
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% __   ____ ___  ____| | __ _| |_ ___        
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \ \ / / _` \ \/ / _` |/ _` | __/ _ \       
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  \ V / (_| |>  < (_| | (_| | ||  __/       
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   \_/ \__,_/_/\_\__,_|\__,_|\__\___|       
                         
                         
VAX_DATE_rsv[rev(order(as.Date(VAX_DATE_rsv$VACCINATION_DATE, format="%Y-%m-%d"))),] 
  
## echarts4r
### Bar-Chart ###
IMMZ_VAXDATE_plot_rsv <- VAX_DATE_rsv%>% 
  e_charts(x = VACCINATION_DATE)%>% 
  e_bar(serie = count,
        color = '#DCB59E',
        legend = FALSE,
        itemStyle = list(
          borderWidth = 1.5,
          borderColor = '#CA6A53')
  )%>% 
  e_tooltip(trigger = "item") %>% 
  e_title("RSV Immz by Vaccination Date") %>% 
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

IMMZ_VAXDATE_plot_rsv

#saving it as html
 saveWidget(widget = IMMZ_VAXDATE_plot_rsv, 
            file = glue("Charts_and_Plots/FTP/Immz_by_Vaxdate_{monthyr}_rsv.html"))
# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



# Number of Patients vaxed by date ########################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   ____       _   _            _         _           
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  |  _ \ __ _| |_(_) ___ _ __ | |_ ___  | |__  _   _ 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  | |_) / _` | __| |/ _ \ '_ \| __/ __| | '_ \| | | |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  |  __/ (_| | |_| |  __/ | | | |_\__ \ | |_) | |_| |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  |_|   \__,_|\__|_|\___|_| |_|\__|___/ |_.__/ \__, |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% __   ____ ___  ____| | __ _| |_ ___           |___/ 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \ \ / / _` \ \/ / _` |/ _` | __/ _ \               
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  \ V / (_| |>  < (_| | (_| | ||  __/               
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   \_/ \__,_/_/\_\__,_|\__,_|\__\___|               
                         
                         
# How many unique patients are vaccinated per day.

UNIQUE_VAXDATE_rsv <- rawreport_rsv %>% 
  select(CLIENT_ID, VACCINATION_DATE) %>% 
  distinct(CLIENT_ID, VACCINATION_DATE)

#counting how many patients are per vaccination date, an adding it as a column to be able to plot it
UNIQUE_VAXDATE_rsv <- UNIQUE_VAXDATE_rsv %>% group_by(VACCINATION_DATE) %>% dplyr::summarize(count=n()) 
UNIQUE_VAXDATE_rsv <- as.data.table(UNIQUE_VAXDATE_rsv)
UNIQUE_VAXDATE_rsv$VACCINATION_DATE <-  as.Date(UNIQUE_VAXDATE_rsv$VACCINATION_DATE, format="%Y-%m-%d")

colnames(UNIQUE_VAXDATE_rsv)[2] <- "Patients"


## echarts4r
### Bar-Chart ###
UNIQUE_VAXDATE_plot_rsv <- UNIQUE_VAXDATE_rsv%>% 
  e_charts(x = VACCINATION_DATE)%>% 
  e_bar(serie = Patients, 
        color = '#DCB59E',
        legend = FALSE,
        itemStyle = list(
          borderWidth = 1,
          borderColor = '#CA6A53')
  )%>% 
  e_tooltip(trigger = "item") %>% 
  e_title("RSV Patients Vaccinated by Day") %>% 
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
UNIQUE_VAXDATE_plot_rsv


 saveWidget(widget = UNIQUE_VAXDATE_plot_rsv, 
            file = glue("Charts_and_Plots/FTP/Patients Vaccinated by Date {monthyr}_rsv.html"))
 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Number of Providers that vaxed by date ########################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   ____                 _     _                 _           
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   |  _ \ _ __ _____   _(_) __| | ___ _ __ ___  | |__  _   _ 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   | |_) | '__/ _ \ \ / / |/ _` |/ _ \ '__/ __| | '_ \| | | |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   |  __/| | | (_) \ V /| | (_| |  __/ |  \__ \ | |_) | |_| |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   |_|   |_|  \___/ \_/_|_|\__,_|\___|_|  |___/ |_.__/ \__, |
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   __   ____ ___  ____| | __ _| |_ ___                 |___/ 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   \ \ / / _` \ \/ / _` |/ _` | __/ _ \                      
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    \ V / (_| |>  < (_| | (_| | ||  __/                      
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     \_/ \__,_/_/\_\__,_|\__,_|\__\___|                      


# How many providers gave vaccines per day first, to know who are our biggest vaccinators.

PROVIDER_CT_rsv <- PROVIDER_NAME_rsv
PROVIDER_CT_rsv <- PROVIDER_CT_rsv %>%  
  mutate(total = sum(count),
         pctage = (count/total)*100)

PROVIDER_CT_rsv <- PROVIDER_CT_rsv[rev(order(PROVIDER_CT_rsv$count)),]%>% 
  mutate(cum = cumsum(pctage))












# Vax by Pharma vs non-pharma #########################################################


#%%%%%%%%%%%%%%%%%%%%%%%% __     __           _             ____  _                                   
#%%%%%%%%%%%%%%%%%%%%%%%% \ \   / /_ ___  __ | |__  _   _  |  _ \| |__   __ _ _ __ _ __ ___   __ _    
#%%%%%%%%%%%%%%%%%%%%%%%%  \ \ / / _` \ \/ / | '_ \| | | | | |_) | '_ \ / _` | '__| '_ ` _ \ / _` |   
#%%%%%%%%%%%%%%%%%%%%%%%%   \ V / (_| |>  <  | |_) | |_| | |  __/| | | | (_| | |  | | | | | | (_| |   
#%%%%%%%%%%%%%%%%%%%%%%%%    \_/ \__,_/_/\_\ |_.__/ \__, | |_|   |_| |_|\__,_|_|  |_| |_| |_|\__,_|   
#%%%%%%%%%%%%%%%%%%%%%%%%  __   _____   _ __   ___  _|___/      _ __ | |__   __ _ _ __ _ __ ___   __ _ 
#%%%%%%%%%%%%%%%%%%%%%%%%  \ \ / / __| | '_ \ / _ \| '_ \ _____| '_ \| '_ \ / _` | '__| '_ ` _ \ / _` |
#%%%%%%%%%%%%%%%%%%%%%%%%   \ V /\__ \ | | | | (_) | | | |_____| |_) | | | | (_| | |  | | | | | | (_| |
#%%%%%%%%%%%%%%%%%%%%%%%%    \_/ |___/ |_| |_|\___/|_| |_|     | .__/|_| |_|\__,_|_|  |_| |_| |_|\__,_|
#%%%%%%%%%%%%%%%%%%%%%%%%                                      |_|                                     

#script to see what % of pharmacies administer vaccines vs non-pharmacies. Creating the dfs first.

CT_PHARMA_NONPHARMA_rsv <- rawreport_rsv %>% 
  mutate(TYPE_PROVIDER = str_detect(PROVIDER_NAME, "Pharma|PHARMA|pharma|pHARMA|Sams Club 6781"),)

CT_PHARMA_NONPHARMA_rsv$TYPE_PROVIDER[CT_PHARMA_NONPHARMA_rsv$TYPE_PROVIDER=='TRUE'] <- 'Pharma'
CT_PHARMA_NONPHARMA_rsv$TYPE_PROVIDER[CT_PHARMA_NONPHARMA_rsv$TYPE_PROVIDER=='FALSE'] <- 'Non-pharma'

CT_PHARMA_rsv <- dplyr::filter(CT_PHARMA_NONPHARMA_rsv, grepl('Pharma', TYPE_PROVIDER))
CT_NONPHARMA_rsv <- dplyr::filter(CT_PHARMA_NONPHARMA_rsv, grepl('Non-pharma', TYPE_PROVIDER))


#Summarizing and changing the format of dates for Pharma
CT_PHARMA_rsv <- CT_PHARMA_rsv %>% group_by(VACCINATION_DATE) %>% dplyr::summarize(count=n()) 
CT_PHARMA_rsv <- merge(CT_PHARMA_rsv, VAX_DATE_rsv, by = "VACCINATION_DATE", all = TRUE)
CT_PHARMA_rsv[is.na(CT_PHARMA_rsv)] <- 0

CT_PHARMA_rsv <- CT_PHARMA_rsv %>% 
  mutate(variable = "Pharma")

colnames(CT_PHARMA_rsv)[2] <- "Pharmacy"
colnames(CT_PHARMA_rsv)[3] <- "ct_total"

CT_PHARMA_rsv <- CT_PHARMA_rsv %>% 
  mutate(diff_pharma = ct_total - Pharmacy) %>% 
  mutate(pctage_pharma = round((Pharmacy/ct_total)*100))

CT_PHARMA_rsv <- as.data.table(CT_PHARMA_rsv)
CT_PHARMA_rsv$VACCINATION_DATE <-  as.Date(CT_PHARMA_rsv$VACCINATION_DATE, format="%Y-%m-%d")


#Summarizing and changing the format of dates for Non-Pharma
CT_NONPHARMA_rsv <- CT_NONPHARMA_rsv %>% group_by(VACCINATION_DATE) %>% dplyr::summarize(count=n()) 
CT_NONPHARMA_rsv <- merge(CT_NONPHARMA_rsv, VAX_DATE_rsv, by = "VACCINATION_DATE", all = TRUE)
CT_NONPHARMA_rsv[is.na(CT_NONPHARMA_rsv)] <- 0

CT_NONPHARMA_rsv <- CT_NONPHARMA_rsv %>% 
  mutate(variable = "Non-pharma")

colnames(CT_NONPHARMA_rsv)[2] <- "Non_Pharmacy"
colnames(CT_NONPHARMA_rsv)[3] <- "ct_total"

CT_NONPHARMA_rsv <- CT_NONPHARMA_rsv %>% 
  mutate(diff_nonpharma = ct_total - Non_Pharmacy) %>% 
  mutate(percentage_non_pharmacy = round((Non_Pharmacy/ct_total)*100))


CT_NONPHARMA_rsv <- as.data.table(CT_NONPHARMA_rsv)
CT_NONPHARMA_rsv$VACCINATION_DATE <-  as.Date(CT_NONPHARMA_rsv$VACCINATION_DATE, format="%Y-%m-%d")

PHARMA_NONPHARMA_chart_rsv <- merge(CT_NONPHARMA_rsv, CT_PHARMA_rsv, by = "VACCINATION_DATE", all = TRUE)
PHARMA_NONPHARMA_chart_rsv$zero <- 0


## echarts4r 
### Stacked Bar-Chart ###
colors = c("#CA6A53","#A6E2BE","#00000000")

PHARMA_NONPHARMA_plot_rsv <- PHARMA_NONPHARMA_chart_rsv %>% 
  e_charts(VACCINATION_DATE) %>%
  e_bar(serie = `Pharmacy`, name = "Pharmacy", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Non_Pharmacy`, name = "Non_Pharmacy", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(
    serie = percentage_non_pharmacy, bind = zero, name = "Percentage: Non-pharmacy",
    stack = "stack",
    label = list(
      show = TRUE,
      formatter = htmlwidgets::JS(
          "(params) => params.value[1] + '%';"),
      position = "top"
    ),
    legend = NULL
  )%>% 
e_color(colors)%>% 
  e_title("All RSV administered by Pharmacies/Non-Pharmacies(% Non-pharmacies)") %>% 
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

PHARMA_NONPHARMA_plot_rsv

# saving it as html
 saveWidget(widget = PHARMA_NONPHARMA_plot_rsv, 
            file = glue("Charts_and_Plots/FTP/All Vax by Pharma-nonpharma by Vaxdate {monthyr}_rsv.html"))
















 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 