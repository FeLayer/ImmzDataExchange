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

#read in the HL7 file. This is a "bucket".
#After, it makes a df out of it.
here::here("ARCHIVE/FTP")

# reportcsv<-
# list.files(path = here::here("ARCHIVE/FTP"), pattern = "\\.csv$", full.names = TRUE) %>%
# map_df(~read_csv(., id = "filename", col_names = F, show_col_types = FALSE))
# 
# colnames(reportcsv) <- reportcsv[1,]
# reportcsv <- reportcsv[-1,]
# 
# #Making it into a dataframe and just filtering those who are for TCPH as of now.
# #Maybe later we can process the rest of the data
# rawreport <- as.data.table(reportcsv)
# rawreport[is.na(rawreport)] <- "0"
# 
# #We need to save as much space as possible. Filtering and removing irrelevant stuff 10/17/2024. UNCOMMENT next line and comment RSV.  Uncomment line 53
# rawreport <- rawreport %>% filter(grepl('Tarrant CO Public Health Dept' , `RESPONSIBLE_ENTITY`))
# #rawreport <- rawreport %>% filter(grepl('RSV' , VACCINE_GROUP))
# 
# rawreport = subset(rawreport, select = -c(STATECODE,PROVIDER_STATE,VACCINE_ID,IMMUN_FACT_ID,IMMUNIZATION_ID,SHORT_NAME,RESPONSIBLE_ENTITY))
# 
# rawreport$CVX_CODE <- sub("\\.\\d+$", "", rawreport$CVX_CODE)
# 
# #change this according to month
# colnames(rawreport)[1] <- glue("{monthyr}")
# rawreport$BIRTH_DATE  <- as.Date(rawreport$BIRTH_DATE, "%Y-%m-%d")
# rawreport$VACCINATION_DATE <- as.Date(rawreport$VACCINATION_DATE, "%Y-%m-%d")
# #remove the 1 for getting it back normal. The 1 is for the RSV immunizations.
# here::here()
# write.csv(rawreport, here::here(glue("ARCHIVE/FTP/FTP_{monthyr}.csv")))

############
############
# Start here if you already processed the csv report from FTP

rawreport <-
  list.files(path = here::here("ARCHIVE/FTP"), pattern = "\\.csv$", full.names = TRUE) %>%
  map_df(~read_csv(., id = "filename", col_names = F, show_col_types = FALSE))

colnames(rawreport) <- rawreport[1,]
rawreport <- rawreport[-1,]
rawreport <- rawreport[, -c(1:3)]

# Correcting some duplicate accounts

rawreport$GIVEN_BY_ORG_ID <- str_replace_all(rawreport$GIVEN_BY_ORG_ID, "25300306", "1502116016")
rawreport$GIVEN_BY_ORG_ID <- str_replace_all(rawreport$GIVEN_BY_ORG_ID, "25300302", "1502116005")
rawreport$GIVEN_BY_ORG_ID <- str_replace_all(rawreport$GIVEN_BY_ORG_ID, "25289635", "25277918")
rawreport$PROVIDER_NAME <- str_replace_all(rawreport$PROVIDER_NAME, "Kashif Anwar, M.D., P.A. - DUPLICATE", "KASHIF ANWAR MD PA")


# unique values that we could categorize
# Count of total immz Vaccination date (day)
VAX_DATE <- rawreport %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(count=n())

# Vaccine group
VACCINE_GROUP <- rawreport %>% group_by(`VACCINE_GROUP`) %>% dplyr::summarize(count=n())

# Unique patient
UNIQUE_PT <- rawreport %>% group_by(`CLIENT_ID`) %>% dplyr::summarize(count=n())

# Org ID
PROVIDER_ID <- rawreport %>% group_by(`GIVEN_BY_ORG_ID`) %>% dplyr::summarize(count=n())

# PROVIDER name: we'll use provider name since its more possible that instead of renewing, 
# some clinic forgot to do so and created a new org on ImmTrac2
PROVIDER_NAME <- rawreport %>% group_by(`PROVIDER_NAME`) %>% dplyr::summarize(count=n())


# ZIP code: patient
PATIENT_ZIP <- rawreport %>% group_by(`ZIP_CODE`) %>% dplyr::summarize(count=n())
#write.csv(PATIENT_ZIP, glue("ARCHIVE/FTP/PATIENT_ZIP_rsv.csv"))

# ZIP code: provider
PROVIDER_ZIP <- rawreport %>% group_by(`PROVIDER_ZIP_CODE`) %>% dplyr::summarize(count=n())

# CVX code
CVX <- rawreport %>% group_by(`CVX_CODE`) %>% dplyr::summarize(count=n())


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
                         
                         
VAX_DATE[rev(order(as.Date(VAX_DATE$VACCINATION_DATE, format="%Y-%m-%d"))),] 
  
## echarts4r
### Bar-Chart ###
IMMZ_VAXDATE_plot <- VAX_DATE%>% 
  e_charts(x = VACCINATION_DATE)%>% 
  e_bar(serie = count,
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

IMMZ_VAXDATE_plot

#saving it as html
saveWidget(widget = IMMZ_VAXDATE_plot,
           file = glue("Charts_and_Plots/FTP/Immz_by_Vaxdate_{monthyr}.html"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Immz per vaxgroup given by date #########################################################
#%%%%%%%%%%%%%%%%%%%%%%%%   _   _                                                         
#%%%%%%%%%%%%%%%%%%%%%%%%  | | | |                                                        
#%%%%%%%%%%%%%%%%%%%%%%%%  | | | | __ ___  ____ _ _ __ ___  _   _ _ __    _ __   ___ _ __ 
#%%%%%%%%%%%%%%%%%%%%%%%%  | | | |/ _` \ \/ / _` | '__/ _ \| | | | '_ \  | '_ \ / _ \ '__|
#%%%%%%%%%%%%%%%%%%%%%%%%  \ \_/ / (_| |>  < (_| | | | (_) | |_| | |_) | | |_) |  __/ |   
#%%%%%%%%%%%%%%%%%%%%%%%%   \___/ \__,_/_/\_\__, |_|  \___/ \__,_| .__/  | .__/ \___|_|   
#%%%%%%%%%%%%%%%%%%%%%%%%                    __/ |               | |     | |              
#%%%%%%%%%%%%%%%%%%%%%%%%                   |___/                |_|     |_|              
#%%%%%%%%%%%%%%%%%%%%%%%%                        _       _                                  
#%%%%%%%%%%%%%%%%%%%%%%%%                       | |     | |                                 
#%%%%%%%%%%%%%%%%%%%%%%%%    __   ____ ___  ____| | __ _| |_ ___                            
#%%%%%%%%%%%%%%%%%%%%%%%%    \ \ / / _` \ \/ / _` |/ _` | __/ _ \                           
#%%%%%%%%%%%%%%%%%%%%%%%%     \ V / (_| |>  < (_| | (_| | ||  __/                           
#%%%%%%%%%%%%%%%%%%%%%%%%      \_/ \__,_/_/\_\__,_|\__,_|\__\___|                           
                         
#### script to create count of immz per vax group: vax group here means component, so
#if patient gets Vaxelis, it will have 4 different immz events/rows for each component/vax group:
# 1 for DTAP, POL (or IPV), HIB, HEPB

VAXGRP_VAXDATE <- rawreport %>% 
  mutate(
    DTaP = str_detect(`VACCINE_GROUP`, "DTAP"),
    HepA = str_detect(`VACCINE_GROUP`, "HEPA"),
    HepB = str_detect(`VACCINE_GROUP`, "HEPB"),
    Hib = str_detect(`VACCINE_GROUP`, "HIB"),
    HPV9 = str_detect(`VACCINE_GROUP`, "HPV"),
    IPV = str_detect(`VACCINE_GROUP`, "POL"),
    MCV4 = str_detect(`VACCINE_GROUP`, "MENI"),
    MenB = str_detect(`VACCINE_GROUP`, "MENB"),
    MMR = str_detect(`VACCINE_GROUP`, "MMR"),
    MPX = str_detect(`VACCINE_GROUP`, "VAC"),
    Pneumo = str_detect(`VACCINE_GROUP`, "PNCN"),
    Rotavirus = str_detect(`VACCINE_GROUP`, "RV"),
    RSV = str_detect(`VACCINE_GROUP`, "RSV"),
    Td_Tdap = str_detect(`VACCINE_GROUP`, "TD"),
    Varicella = str_detect(`VACCINE_GROUP`, "VAR"),
    Zoster = str_detect(`VACCINE_GROUP`, "HZ"),
    COVID = str_detect(`VACCINE_GROUP`, "COV"),
    Flu = str_detect(`VACCINE_GROUP`, "FLU"),)

#changing TRUE to 1 so we can add them up. Rather do it by name of columns than position for safety.
VAXGRP_VAXDATE$DTaP <- VAXGRP_VAXDATE$DTaP %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$HepA <- VAXGRP_VAXDATE$HepA %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$HepB <- VAXGRP_VAXDATE$HepB %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$Hib <- VAXGRP_VAXDATE$Hib %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$HPV9 <- VAXGRP_VAXDATE$HPV9 %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$IPV <- VAXGRP_VAXDATE$IPV %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$MCV4 <- VAXGRP_VAXDATE$MCV4 %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$MenB <- VAXGRP_VAXDATE$MenB %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$MMR <- VAXGRP_VAXDATE$MMR %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$MPX <- VAXGRP_VAXDATE$MPX %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$Pneumo <- VAXGRP_VAXDATE$Pneumo %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$Rotavirus <- VAXGRP_VAXDATE$Rotavirus %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$RSV <- VAXGRP_VAXDATE$RSV %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$Td_Tdap <- VAXGRP_VAXDATE$Td_Tdap %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$Varicella <- VAXGRP_VAXDATE$Varicella %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$Zoster <- VAXGRP_VAXDATE$Zoster %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$COVID <- VAXGRP_VAXDATE$COVID %>% 
  sapply(., \(x) +as.logical(x))
VAXGRP_VAXDATE$Flu <- VAXGRP_VAXDATE$Flu %>% 
  sapply(., \(x) +as.logical(x))

#Creating the chart for the plot
DTAP_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(DTaP=sum(DTaP))
HEPA_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(HepA=sum(HepA))
HEPB_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(HepB=sum(HepB))
HIBB_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(Hib=sum(Hib))
HPV9_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(HPV9=sum(HPV9))
IPVV_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(IPV=sum(IPV))
MCV4_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(MCV4=sum(MCV4))
MENB_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(MenB=sum(MenB))
MMRR_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(MMR=sum(MMR))
MPOX_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(MPX=sum(MPX))
PNEU_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(Pneumo=sum(Pneumo))
ROTA_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(Rotavirus=sum(Rotavirus))
RSVV_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(RSV=sum(RSV))
TDAP_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(`Td/Tdap` = sum(Td_Tdap))
VARI_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(Varicella=sum(Varicella))
ZOST_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(Zoster=sum(Zoster))
FLUV_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(COVID=sum(COVID))
COVI_vaxgrp <- VAXGRP_VAXDATE %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(Flu=sum(Flu))


#putting them in a single list
VAXGRP_VAXDATEchart <- list("DTaP" = DTAP_vaxgrp, "HepA" = HEPA_vaxgrp, "HepB"= HEPB_vaxgrp, "Hib"= HIBB_vaxgrp, "HPV9" = HPV9_vaxgrp, "IPV" = IPVV_vaxgrp, "MCV4"= MCV4_vaxgrp, "MenB" = MENB_vaxgrp, "MMR" = MMRR_vaxgrp, "MPX"= MPOX_vaxgrp, "PCV/PPSV" = PNEU_vaxgrp, "Rotavirus" = ROTA_vaxgrp,"RSV"= RSVV_vaxgrp, "Td/Tdap" = TDAP_vaxgrp, "Varicella" = VARI_vaxgrp, "Zoster"= ZOST_vaxgrp, "Flu"= FLUV_vaxgrp, "COVID"= COVI_vaxgrp)


# joining all the merged sheets into a single table with Vaccination Date on Y and Errors on X. 
VAXGRP_VAXDATEchart <- list(DTAP_vaxgrp, HEPA_vaxgrp, HEPB_vaxgrp, HIBB_vaxgrp, HPV9_vaxgrp, IPVV_vaxgrp, MCV4_vaxgrp, MENB_vaxgrp, MMRR_vaxgrp, MPOX_vaxgrp, PNEU_vaxgrp, ROTA_vaxgrp, RSVV_vaxgrp, TDAP_vaxgrp, VARI_vaxgrp, ZOST_vaxgrp, FLUV_vaxgrp, COVI_vaxgrp) %>% 
  reduce(full_join, by = "VACCINATION_DATE")

#changing the format from wide to long, so we can plot it
VAXGRP_VAXDATEchart <- as.data.table(VAXGRP_VAXDATEchart)
VAXGRP_VAXDATEchart$VACCINATION_DATE <-  as.Date(VAXGRP_VAXDATEchart$VACCINATION_DATE)
#VAXGRP_VAXDATEchart <- melt(setDT(VAXGRP_VAXDATEchart), id.vars = "VACCINATION_DATE")

# To add the totals for each day, we'll be attaching a total columns and avoid melting the data (such as the previous line of melt)
VAXGRP_VAXDATEchart$total <- rowSums(VAXGRP_VAXDATEchart[,-1])
VAXGRP_VAXDATEchart$zero <- 0


## echarts4r
### Bar-Chart Stacked ###
colors = c("#5D7E82",
           "#4C3113",
           "#8C3113",
           "#6C7113",
           "#6C3113",
           "#9C3113",
           "#2292D0",
           "#7292D0",
           "#A6E2BE",
           "#284E5F",
           "#281E5F",
           "#484E5F",
           "#489E5F",
           "#CA6A53",
           "#FA6A53",
           "#DCB59E",
           "#DCD59E",
           "#803B21",
           "#23513C",
           "#23913C"
)

VAXGRP_VAXDATE_plot <- VAXGRP_VAXDATEchart %>%
  e_chart(VACCINATION_DATE) %>% 
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
  e_bar(serie = `MCV4`, name = "MCV4", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `MenB`, name = "MenB", 
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
  e_bar(serie = `Varicella`, name = "Varicella", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Zoster`, name = "Zoster", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `COVID`, name = "COVID", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
  e_bar(serie = `Flu`, name = "Flu", 
        stack = "stack",
        borderWidth = 1,
        borderColor = '#404040') %>% 
    e_bar(
    serie = zero, bind = total, name = "Total",
    stack = "stack"
    # label = list(
    #   show = TRUE,
    #   formatter = "{b}",
    #   position = "top"
    # ),
    # legend = NULL
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

VAXGRP_VAXDATE_plot

# 
# #saving it as html
saveWidget(widget = VAXGRP_VAXDATE_plot,
           file = glue("Charts_and_Plots/FTP/Count of Vaxgrps by Vaxdate {monthyr}.html"))



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

UNIQUE_VAXDATE <- rawreport %>% 
  select(CLIENT_ID, VACCINATION_DATE) %>% 
  distinct(CLIENT_ID, VACCINATION_DATE)

#counting how many patients are per vaccination date, an adding it as a column to be able to plot it
UNIQUE_VAXDATE <- UNIQUE_VAXDATE %>% group_by(VACCINATION_DATE) %>% dplyr::summarize(count=n()) 
UNIQUE_VAXDATE <- as.data.table(UNIQUE_VAXDATE)
UNIQUE_VAXDATE$VACCINATION_DATE <-  as.Date(UNIQUE_VAXDATE$VACCINATION_DATE, format="%Y-%m-%d")

colnames(UNIQUE_VAXDATE)[2] <- "Patients"


## echarts4r
### Bar-Chart ###
UNIQUE_VAXDATE_plot <- UNIQUE_VAXDATE%>% 
  e_charts(x = VACCINATION_DATE)%>% 
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
UNIQUE_VAXDATE_plot

# 
saveWidget(widget = UNIQUE_VAXDATE_plot,
           file = glue("Charts_and_Plots/FTP/Patients Vaccinated by Date {monthyr}.html"))

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

PROVIDER_CT <- PROVIDER_NAME
PROVIDER_CT <- PROVIDER_CT %>%  
  mutate(total = sum(count),
         pctage = (count/total)*100)

PROVIDER_CT <- PROVIDER_CT[rev(order(PROVIDER_CT$count)),]%>% 
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

CT_PHARMA_NONPHARMA <- rawreport %>% 
  mutate(TYPE_PROVIDER = str_detect(PROVIDER_NAME, "Pharma|PHARMA|pharma|pHARMA|Sams Club 6781"),)

CT_PHARMA_NONPHARMA$TYPE_PROVIDER[CT_PHARMA_NONPHARMA$TYPE_PROVIDER=='TRUE'] <- 'Pharma'
CT_PHARMA_NONPHARMA$TYPE_PROVIDER[CT_PHARMA_NONPHARMA$TYPE_PROVIDER=='FALSE'] <- 'Non-pharma'

CT_PHARMA <- dplyr::filter(CT_PHARMA_NONPHARMA, grepl('Pharma', TYPE_PROVIDER))
CT_NONPHARMA <- dplyr::filter(CT_PHARMA_NONPHARMA, grepl('Non-pharma', TYPE_PROVIDER))


#Summarizing and changing the format of dates for Pharma
CT_PHARMA <- CT_PHARMA %>% group_by(VACCINATION_DATE) %>% dplyr::summarize(count=n()) 
CT_PHARMA <- merge(CT_PHARMA, VAX_DATE, by = "VACCINATION_DATE", all = TRUE)
CT_PHARMA[is.na(CT_PHARMA)] <- 0

CT_PHARMA <- CT_PHARMA %>% 
  mutate(variable = "Pharma")

colnames(CT_PHARMA)[2] <- "Pharmacy"
colnames(CT_PHARMA)[3] <- "ct_total"

CT_PHARMA <- CT_PHARMA %>% 
  mutate(diff_pharma = ct_total - Pharmacy) %>% 
  mutate(pctage_pharma = round((Pharmacy/ct_total)*100))

CT_PHARMA <- as.data.table(CT_PHARMA)
CT_PHARMA$VACCINATION_DATE <-  as.Date(CT_PHARMA$VACCINATION_DATE, format="%Y-%m-%d")


#Summarizing and changing the format of dates for Non-Pharma
CT_NONPHARMA <- CT_NONPHARMA %>% group_by(VACCINATION_DATE) %>% dplyr::summarize(count=n()) 
CT_NONPHARMA <- merge(CT_NONPHARMA, VAX_DATE, by = "VACCINATION_DATE", all = TRUE)
CT_NONPHARMA[is.na(CT_NONPHARMA)] <- 0

CT_NONPHARMA <- CT_NONPHARMA %>% 
  mutate(variable = "Non-pharma")

colnames(CT_NONPHARMA)[2] <- "Non_Pharmacy"
colnames(CT_NONPHARMA)[3] <- "ct_total"

CT_NONPHARMA <- CT_NONPHARMA %>% 
  mutate(diff_nonpharma = ct_total - Non_Pharmacy) %>% 
  mutate(percentage_non_pharmacy = round((Non_Pharmacy/ct_total)*100))


CT_NONPHARMA <- as.data.table(CT_NONPHARMA)
CT_NONPHARMA$VACCINATION_DATE <-  as.Date(CT_NONPHARMA$VACCINATION_DATE, format="%Y-%m-%d")

PHARMA_NONPHARMA_chart <- merge(CT_NONPHARMA, CT_PHARMA, by = "VACCINATION_DATE", all = TRUE)
PHARMA_NONPHARMA_chart$zero <- 0


## echarts4r 
### Stacked Bar-Chart ###
colors = c("#CA6A53","#A6E2BE","#00000000")

PHARMA_NONPHARMA_plot <- PHARMA_NONPHARMA_chart%>% 
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
  e_title("All Vaccines administered by Pharmacies/Non-Pharmacies(% Non-pharmacies)") %>% 
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

PHARMA_NONPHARMA_plot

# 
# #saving it as html
saveWidget(widget = PHARMA_NONPHARMA_plot,
           file = glue("Charts_and_Plots/FTP/All Vax by Pharma-nonpharma by Vaxdate {monthyr}.html"))



# Flu by Pharma vs non-pharma #########################################################

#%%%%%%%%%%%%%%%%%%%%%%%%   _____ _         _             ____  _                                      
#%%%%%%%%%%%%%%%%%%%%%%%%  |  ___| |_   _  | |__  _   _  |  _ \| |__   __ _ _ __ _ __ ___   __ _       
#%%%%%%%%%%%%%%%%%%%%%%%%  | |_  | | | | | | '_ \| | | | | |_) | '_ \ / _` | '__| '_ ` _ \ / _` |      
#%%%%%%%%%%%%%%%%%%%%%%%%  |  _| | | |_| | | |_) | |_| | |  __/| | | | (_| | |  | | | | | | (_| |      
#%%%%%%%%%%%%%%%%%%%%%%%%  |_|   |_|\__,_| |_.__/ \__, | |_|   |_| |_|\__,_|_|  |_| |_| |_|\__,_|      
#%%%%%%%%%%%%%%%%%%%%%%%%  __   _____   _ __   ___|___/_        _ __ | |__   __ _ _ __ _ __ ___   __ _ 
#%%%%%%%%%%%%%%%%%%%%%%%%  \ \ / / __| | '_ \ / _ \| '_ \ _____| '_ \| '_ \ / _` | '__| '_ ` _ \ / _` |
#%%%%%%%%%%%%%%%%%%%%%%%%   \ V /\__ \ | | | | (_) | | | |_____| |_) | | | | (_| | |  | | | | | | (_| |
#%%%%%%%%%%%%%%%%%%%%%%%%    \_/ |___/ |_| |_|\___/|_| |_|     | .__/|_| |_|\__,_|_|  |_| |_| |_|\__,_|
#%%%%%%%%%%%%%%%%%%%%%%%%                                      |_|                                     

#script to see what % of pharmacies administer FLU vaccines vs non-pharmacies. Creating the dfs first.

CT_PHARMA2 <- dplyr::filter(CT_PHARMA_NONPHARMA, grepl('Pharma', TYPE_PROVIDER))
CT_NONPHARMA2 <- dplyr::filter(CT_PHARMA_NONPHARMA, grepl('Non-pharma', TYPE_PROVIDER))

#Filtering COVID and Flu
CT_PHARMA2 <- dplyr::filter(CT_PHARMA2, grepl('FLU', VACCINE_GROUP))
CT_NONPHARMA2 <- dplyr::filter(CT_NONPHARMA2, grepl('FLU', VACCINE_GROUP))

# Filtering by Flu
VAX_DATE_FLU <- dplyr::filter(CT_PHARMA_NONPHARMA, grepl('FLU', VACCINE_GROUP))
VAX_DATE_FLU <- VAX_DATE_FLU %>% group_by(`VACCINATION_DATE`) %>% dplyr::summarize(count=n())

#Summarizing and changing the format of dates for Pharma
CT_PHARMA2 <- CT_PHARMA2 %>% group_by(VACCINATION_DATE) %>% dplyr::summarize(count=n()) 
CT_PHARMA2 <- merge(CT_PHARMA2, VAX_DATE_FLU, by = "VACCINATION_DATE", all = TRUE)
CT_PHARMA2[is.na(CT_PHARMA2)] <- 0

CT_PHARMA2 <- CT_PHARMA2 %>% 
  mutate(variable = "Pharma")

colnames(CT_PHARMA2)[2] <- "Pharmacy"
colnames(CT_PHARMA2)[3] <- "ct_total"

CT_PHARMA2 <- CT_PHARMA2 %>% 
  mutate(diff_pharma = ct_total - Pharmacy) %>% 
  mutate(pctage_pharma = round((Pharmacy/ct_total)*100))

CT_PHARMA2 <- as.data.table(CT_PHARMA2)
CT_PHARMA2$VACCINATION_DATE <-  as.Date(CT_PHARMA2$VACCINATION_DATE, format="%Y-%m-%d")


#Summarizing and changing the format of dates for Non-Pharma
CT_NONPHARMA2 <- CT_NONPHARMA2 %>% group_by(VACCINATION_DATE) %>% dplyr::summarize(count=n()) 
CT_NONPHARMA2 <- merge(CT_NONPHARMA2, VAX_DATE_FLU, by = "VACCINATION_DATE", all = TRUE)
CT_NONPHARMA2[is.na(CT_NONPHARMA2)] <- 0

CT_NONPHARMA2 <- CT_NONPHARMA2 %>% 
  mutate(variable = "Non-pharma")

colnames(CT_NONPHARMA2)[2] <- "Non_Pharmacy"
colnames(CT_NONPHARMA2)[3] <- "ct_total"

CT_NONPHARMA2 <- CT_NONPHARMA2 %>% 
  mutate(diff_nonpharma = ct_total - Non_Pharmacy) %>% 
  mutate(percentage_non_pharmacy = round((Non_Pharmacy/ct_total)*100))


CT_NONPHARMA2 <- as.data.table(CT_NONPHARMA2)
CT_NONPHARMA2$VACCINATION_DATE <-  as.Date(CT_NONPHARMA2$VACCINATION_DATE, format="%Y-%m-%d")

PHARMA_NONPHARMA2_chart <- merge(CT_NONPHARMA2, CT_PHARMA2, by = "VACCINATION_DATE", all = TRUE)
PHARMA_NONPHARMA2_chart$zero <- 0


## echarts4r 
### Stacked Bar-Chart ###
colors = c("#CA6A53","#A6E2BE","#00000000")

PHARMA_NONPHARMA2_plot <- PHARMA_NONPHARMA2_chart%>% 
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
    serie = pctage_pharma, bind = zero, name = "Percentage: Pharmacy",
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
  e_title("Flu administered by Pharmacies/Non-Pharmacies(% Pharmacies)") %>% 
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

PHARMA_NONPHARMA2_plot

# #saving it as html
saveWidget(widget = PHARMA_NONPHARMA2_plot,
           file = glue("Charts_and_Plots/FTP/Flu by Pharma-nonpharma by Vaxdate {monthyr}.html"))











