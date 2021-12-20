## ---------------------------
##
## Script name: functions_base_code
##
## Purpose of script: Contains master functiosn for processesing excel file,
##                    perform data cleaning, housekeeping and processing.
##                    Also contains functions supporting static features for charts.
##
## Author: Dhyey Bhatpuria
##
## Date Created: 2021-10-11
##
## Email: dhyey.bhatpuria@sei.org
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------


if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("sqldf")) {
  install.packages("sqldf")
  library(sqldf)
}
if (!require("stringr")) {
  install.packages("stringr")
  library(stringr)
}
if (!require("openxlsx")) {
  install.packages("openxlsx")
  library(openxlsx)
}
if (!require("stringi")) {
  install.packages("stringi")
  library(stringi)
}
if (!require("tidyr")) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("networkD3")) {
  install.packages("networkD3")
  library(networkD3)
}
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}
if (!require("htmlwidgets")) {
  install.packages("htmlwidgets")
  library(htmlwidgets)
}
if (!require("ggalluvial")) {
  install.packages("ggalluvial")
  library(ggalluvial)
}
if (!require("gridExtra")) {
  install.packages("gridExtra")
  library(gridExtra)
}
if (!require("grid")) {
  install.packages("grid")
  library(grid)
}
if (!require("webshot")) {
  install.packages("webshot")
  install_phantomjs(version = "2.1.1",
                    baseURL = "https://github.com/wch/webshot/releases/download/v0.3.1/",
                    force = FALSE)
  library(webshot)
}


data_filtering_olddata = function(inputdata) {
  masterdata <- read_excel(inputdata) #
  masterdata$ID <- seq.int(nrow(masterdata))
  study_countries = subset(
    masterdata,
    subset = Recipient %in% c(
      'India',
      "China (People's Republic of)",
      'Viet Nam',
      'Pakistan',
      'Philippines',
      'Indonesia',
      'Thailand',
      'South Africa',
      'Brazil',
      'Mexico'
    )
  )
  # China, India, Brazil, Indonesia, Mexico, South Africa, Pakistan, Thailand, Vietnam and the Philippines.
  
  study_countries = subset(
    study_countries,
    subset = Provider %in% c(
      'AsDB',
      #Asian Development Bank',
      'EIB',
      #"EU Institutions (EIB)",
      "EU Institutions (excl. EIB)",
      'GCF',
      #'Green Climate Fund',
      'WB',
      # 'World Bank'
      'Australia',
      'Canada',
      'Denmark',
      'France',
      'Germany',
      'Japan',
      'Korea',
      'Netherlands',
      'Sweden',
      'Switzerland',
      'United Kingdom',
      'United States'
    )
  )
  # IRENA, UNDP, UNEP MISSING
  # the European Union, the Green Climate Fund, IRENA, UNDP, UNEP,
  # World Bank, Asian Development Bank and European Investment Bank
  # United Kingdom, Germany, France, The Netherlands, Denmark,
  # Switzerland, Sweden, USA, Canada, Australia, Japan and South Korea
  
  # transport and storage; Energy; Industry, construction and mining; General environment protection; and Other multisector
  with_transport = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%transport%'")
  with_storage = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%storage%'")
  with_energy = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%Energy%'")
  with_industry = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%Industry%'")
  with_mining = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%mining%'")
  with_protection = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%protection%'")
  with_multi = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%multisector%'")
  
  key_sector_data_alpha <-
    rbind(
      with_transport,
      with_storage,
      with_energy,
      #with_const,
      with_industry,
      with_mining,
      with_protection,
      with_multi
    )
  study_countries <-
    key_sector_data_alpha[!duplicated(key_sector_data_alpha$ID), ]
  # Changing GCF Significant to Principal
  study_countries$`Climate objective (applies to Rio-marked data only) or climate component`[which(study_countries$Provider == "GCF")] <-
    "Principal"
  # Remove rows with Significant
  study_countries_selected <-
    study_countries[!(
      study_countries$`Climate objective (applies to Rio-marked data only) or climate component` ==
        "Significant"
    ), ]
  
  study_countries_selected$`Adaptation-related development finance - Commitment - 2018 USD thousand`[study_countries_selected$`Overlap - Commitment - 2018 USD thousand` > 0] <- 0
  study_countries_selected$`Mitigation-related development finance - Commitment - 2018 USD thousand`[study_countries_selected$`Overlap - Commitment - 2018 USD thousand` > 0] <- 0
  study_countries_selected$`Overlap - Commitment - 2018 USD thousand`[is.na(study_countries_selected$`Overlap - Commitment - 2018 USD thousand`)] <- 0
  study_countries_selected$`Adaptation-related development finance - Commitment - 2018 USD thousand`[is.na(
    study_countries_selected$`Adaptation-related development finance - Commitment - 2018 USD thousand`)] <- 0
  study_countries_selected$`Mitigation-related development finance - Commitment - 2018 USD thousand`[is.na(
    study_countries_selected$`Mitigation-related development finance - Commitment - 2018 USD thousand`)] <- 0
  
  
  # Search Strings: ‘capacity’, policy’, ‘technical assistance’, ‘TA  ’, ‘ energy access’, ‘energy efficiency’ and ‘climate finance’
  with_capacity = sqldf("select * from study_countries_selected where `Short Description` LIKE '%capacity%'")
  with_policy = sqldf("select * from study_countries_selected where `Short Description` LIKE '%policy%'")
  with_techassist = sqldf(
    "select * from study_countries_selected where `Short Description` LIKE '%technical assistance%'"
  )
  with_ta = sqldf("select * from study_countries_selected where `Short Description` LIKE '% TA %'")
  with_energyaccess = sqldf(
    "select * from study_countries_selected where `Short Description` LIKE '%energy access%'"
  )
  with_energyeffic = sqldf(
    "select * from study_countries_selected where `Short Description` LIKE '%energy efficiency%'"
  )
  with_climfin = sqldf(
    "select * from study_countries_selected where `Short Description` LIKE '%climate finance%'"
  )
  
  with_capacity_d = sqldf("select * from study_countries_selected where Description LIKE '%capacity%'")
  with_policy_d = sqldf("select * from study_countries_selected where Description LIKE '%policy%'")
  with_techassist_d = sqldf(
    "select * from study_countries_selected where Description LIKE '%technical assistance%'"
  )
  with_ta_d = sqldf("select * from study_countries_selected where Description LIKE '% TA %'")
  with_energyaccess_d = sqldf("select * from study_countries_selected where Description LIKE '%energy access%'")
  with_energyeffic_d = sqldf(
    "select * from study_countries_selected where Description LIKE '%energy efficiency%'"
  )
  with_climfin_d = sqldf("select * from study_countries_selected where Description LIKE '%climate finance%'")
  
  keyword_data <- rbind(
    with_capacity,
    with_policy,
    with_climfin,
    with_techassist,
    with_ta,
    with_energyaccess,
    with_energyeffic
    ,
    with_capacity_d,
    with_policy_d,
    with_climfin_d,
    with_techassist_d,
    with_ta_d,
    with_energyaccess_d,
    with_energyeffic_d
  )
  codebased_data = subset(
    study_countries_selected,
    subset = `Sub-sector Code` %in% c(
      21010,
      21050,
      23110,
      23181,
      23182,
      23183,
      32110,
      32120,
      32210,
      32310,
      32182,
      41010,
      41081,
      43030
    )
  )
  
  merged_ds <- rbind(keyword_data, codebased_data)
  data_cleaned <- merged_ds[!duplicated(merged_ds$ID), ]
  return(data_cleaned)
}

data_filtering = function(inputdata) {
  masterdata <- read_excel(inputdata) #
  masterdata$ID <- seq.int(nrow(masterdata))
  study_countries = subset(
    masterdata,
    subset = Recipient %in% c(
      'India',
      "China (People's Republic of)",
      'Viet Nam',
      'Finland',
      'Pakistan',
      'Philippines',
      'Indonesia',
      'Thailand',
      'South Africa',
      'Brazil',
      'Mexico'
    )
  )
  study_countries = subset(
    study_countries,
    subset = Provider %in% c(
      'AsDB',
      #Asian Development Bank',
      'EIB',
      #"EU Institutions (EIB)",
      "EU Institutions (excl. EIB)",
      'GCF',
      #'Green Climate Fund',
      'WB',
      # 'World Bank'
      'Australia',
      'Canada',
      'Denmark',
      'France',
      'Germany',
      'Japan',
      'Korea',
      'Netherlands',
      'Sweden',
      'Switzerland',
      'United Kingdom',
      'United States'
    )
  )
  
  # transport and storage; Energy; Industry, construction and mining; General environment protection; and Other multisector
  with_transport = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%transport%'")
  with_storage = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%storage%'")
  with_energy = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%Energy%'")
  # with_const = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%construction%'")
  with_industry = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%Industry%'")
  with_mining = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%mining%'")
  with_protection = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%protection%'")
  with_multi = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%multisector%'")
  
  key_sector_data_alpha <-
    rbind(
      with_transport,
      with_storage,
      with_energy,
      #with_const,
      with_industry,
      with_mining,
      with_protection,
      with_multi
    )
  study_countries <-
    key_sector_data_alpha[!duplicated(key_sector_data_alpha$ID), ]
  # Changing GCF Significant to Principal
  study_countries$`Climate objective (applies to Rio-marked data only) or climate component`[which(study_countries$Provider == "GCF")] <-
    "Principal"
  # Remove rows with Significant
  study_countries_selected <-
    study_countries[!(
      study_countries$`Climate objective (applies to Rio-marked data only) or climate component` ==
        "Significant"
    ), ]
  study_countries_selected <-
    study_countries_selected[!(
      study_countries_selected$`Mitigation objective (applies to Rio-marked data only)` ==
        "Significant"
    ), ]
  
  study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand`[study_countries_selected$`Overlap - Commitment - 2019 USD thousand` > 0] <-
    0
  study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand`[study_countries_selected$`Overlap - Commitment - 2019 USD thousand` > 0] <-
    0
  study_countries_selected$`Overlap - Commitment - 2019 USD thousand`[is.na(study_countries_selected$`Overlap - Commitment - 2019 USD thousand`)] <-
    0
  study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand`[is.na(
    study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand`
  )] <- 0
  study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand`[is.na(
    study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand`
  )] <- 0
  study_countries_selected$USD.thousand = study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand` + study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand` + study_countries_selected$`Overlap - Commitment - 2019 USD thousand`
  
  # Search Strings: ‘capacity’, policy’, ‘technical assistance’, ‘TA  ’, ‘ energy access’, ‘energy efficiency’ and ‘climate finance’
  with_capacity = sqldf("select * from study_countries_selected where `Project Title` LIKE '%capacity%'")
  with_policy = sqldf("select * from study_countries_selected where `Project Title` LIKE '%policy%'")
  with_techassist = sqldf(
    "select * from study_countries_selected where `Project Title` LIKE '%technical assistance%'"
  )
  with_ta = sqldf("select * from study_countries_selected where `Project Title` LIKE '% TA %'")
  with_energyaccess = sqldf("select * from study_countries_selected where `Project Title` LIKE '%energy access%'")
  with_energyeffic = sqldf(
    "select * from study_countries_selected where `Project Title` LIKE '%energy efficiency%'"
  )
  with_climfin = sqldf(
    "select * from study_countries_selected where `Project Title` LIKE '%climate finance%'"
  )
  
  with_capacity_d = sqldf("select * from study_countries_selected where Description LIKE '%capacity%'")
  with_policy_d = sqldf("select * from study_countries_selected where Description LIKE '%policy%'")
  with_techassist_d = sqldf(
    "select * from study_countries_selected where Description LIKE '%technical assistance%'"
  )
  with_ta_d = sqldf("select * from study_countries_selected where Description LIKE '% TA %'")
  with_energyaccess_d = sqldf("select * from study_countries_selected where Description LIKE '%energy access%'")
  with_energyeffic_d = sqldf(
    "select * from study_countries_selected where Description LIKE '%energy efficiency%'"
  )
  with_climfin_d = sqldf("select * from study_countries_selected where Description LIKE '%climate finance%'")
  
  
  keyword_data <- rbind(
    with_capacity,
    with_policy,
    with_climfin,
    with_techassist,
    with_ta,
    with_energyaccess,
    with_energyeffic
    ,
    with_capacity_d,
    with_policy_d,
    with_climfin_d,
    with_techassist_d,
    with_ta_d,
    with_energyaccess_d,
    with_energyeffic_d
  )
  codebased_data = subset(
    study_countries_selected,
    subset = `Purpose Code` %in% c(
      21010,
      21050,
      23110,
      23181,
      23182,
      23183,
      32110,
      32120,
      32210,
      32310,
      32182,
      41010,
      41081,
      43030
    )
  )
  
  merged_ds <- rbind(keyword_data, codebased_data)
  data_cleaned <- merged_ds[!duplicated(merged_ds$ID), ]
  
  data_cleaned$Recipient[data_cleaned$Recipient == "China (People's Republic of)"] <-
    "China"
  data_cleaned$Recipient[data_cleaned$Recipient == "Viet Nam"] <-
    "Vietnam"
  
  data_cleaned$`Financial Instrument`[data_cleaned$`Financial Instrument` == "Debt instrument"] <-
    "Debt"
  data_cleaned$`Financial Instrument`[data_cleaned$`Financial Instrument` == "Mezzanine finance instrument"] <-
    "Mezzanine"
  data_cleaned$`Financial Instrument`[data_cleaned$`Financial Instrument` == "Equity and shares in collective investment vehicles"] <-
    "Equity & shares"
  
  data_cleaned$Provider[data_cleaned$Provider == "EU Institutions (excl. EIB)"] <-
    "EU"
  
  temprows = subset(data_cleaned,
                    data_cleaned$`Sector (detailed)` == "II.1. Transport & Storage")
  temprows$`Sector (detailed)` <-
    gsub(
      "II.1. Transport & Storage",
      "II.1. Transport and Storage",
      temprows$`Sector (detailed)`
    )
  permarows <-
    subset(data_cleaned,
           `Sector (detailed)` != "II.1. Transport & Storage")
  total <- rbind(permarows, temprows)
  # Rename Sectors
  total$`Sector (detailed)`[grepl("II.1. Transport and Storage",
                                  total$`Sector (detailed)`,
                                  fixed = TRUE)] <- "Transport and Storage"
  total$`Sector (detailed)`[grepl("II.3. Energy", total$`Sector (detailed)`, fixed = TRUE)] <-
    "Energy"
  total$`Sector (detailed)`[grepl("IV.1. General Environment Protection",
                                  total$`Sector (detailed)`,
                                  fixed = TRUE)] <- "Gen. Environment Protection"
  total$`Sector (detailed)`[grepl("IV.2. Other Multisector", total$`Sector (detailed)`, fixed = TRUE)] <-
    "Other Multisector"
  total$`Sector (detailed)`[grepl("III.2.a. Industry", total$`Sector (detailed)`, fixed = TRUE)] <-
    "Industry"
  total$`Sector (detailed)`[grepl("III.2. Industry, Mining, Construction",
                                  total$`Sector (detailed)`,
                                  fixed = TRUE)] <- "Industry, Mining, Construction"
  total$`Sector (detailed)`[grepl("III.2.b. Mineral Resources and Mining",
                                  total$`Sector (detailed)`,
                                  fixed = TRUE)] <- "Mineral Resources and Mining"
  
  
  total = total %>%
    rename(
      Finance.Instrument = `Financial Instrument`,
      Overlap.Commitment = `Overlap - Commitment - 2019 USD thousand`,
      Adaptation = `Adaptation-related development finance - Commitment - 2019 USD thousand`,
      Mitigation = `Mitigation-related development finance - Commitment - 2019 USD thousand`,
      Sector = `Sector (detailed)`
    )
  
  return(total)
}


data_basic_cleaning = function(filename) {
  inputdata = read.csv(filename, header = TRUE)
  inputdata = inputdata %>%
    rename(
      Finance.Instrument = Financial.Instrument,
      Overlap.Commitment = Overlap...Commitment...2018.USD.thousand,
      Adaptation = Adaptation.related.development.finance...Commitment...2018.USD.thousand,
      Mitigation = Mitigation.related.development.finance...Commitment...2018.USD.thousand,
      Sector = Sector..detailed.,
      Year =  ï..Year
    )
  
  
  inputdata$Recipient[inputdata$Recipient == "China (People's Republic of)"] <-
    "China"
  inputdata$Recipient[inputdata$Recipient == "Viet Nam"] <-
    "Vietnam"
  
  inputdata$Finance.Instrument[inputdata$Finance.Instrument == "Debt instrument"] <-
    "Debt"
  inputdata$Finance.Instrument[inputdata$Finance.Instrument == "Mezzanine finance instrument"] <-
    "Mezzanine"
  inputdata$Finance.Instrument[inputdata$Finance.Instrument == "Equity and shares in collective investment vehicles"] <-
    "Equity & shares"
  
  inputdata$Provider[inputdata$Provider == "EU Institutions (excl. EIB)"] <-
    "EU"
  
  temprows = subset(inputdata, inputdata$Sector == "II.1. Transport & Storage")
  temprows$Sector <-
    gsub("II.1. Transport & Storage",
         "II.1. Transport and Storage",
         temprows$Sector)
  permarows <- subset(inputdata, Sector != "II.1. Transport & Storage")
  total <- rbind(permarows, temprows)
  # Rename Sectors
  total$Sector[grepl("II.1. Transport and Storage", total$Sector, fixed = TRUE)] <-
    "Transport and Storage"
  total$Sector[grepl("II.3. Energy", total$Sector, fixed = TRUE)] <-
    "Energy"
  total$Sector[grepl("IV.1. General Environment Protection", total$Sector, fixed = TRUE)] <-
    "Gen. Environment Protection"
  total$Sector[grepl("IV.2. Other Multisector", total$Sector, fixed = TRUE)] <-
    "Other Multisector"
  total$Sector[grepl("III.2.a. Industry", total$Sector, fixed = TRUE)] <-
    "Industry"
  total$Sector[grepl("III.2. Industry, Mining, Construction", total$Sector, fixed = TRUE)] <-
    "Industry, Mining, Construction"
  total$Sector[grepl("III.2.b. Mineral Resources and Mining", total$Sector, fixed = TRUE)] <-
    "Mineral Resources and Mining"
  
  total$USD.thousand = total$Mitigation + total$Adaptation + total$Overlap.Commitment
  return(total)
}



select_2015_2019 = function(inputdata) {
  masterdata <- read_excel(inputdata)
  masterdata <- subset(masterdata, masterdata$Year > 2014)
  write.xlsx(masterdata,
             file = "crux_data_2015-2019.xlsx",
             sheetName = "newData",
             append = FALSE)
  return("Export Done")
}


read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      readxl::read_excel(filename, sheet = X))
  if (!tibble)
    x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1, ]))
  df[-1, ]
}

donor_perspective_analysis <- function(filename, donordb = TRUE) {
  mysheets <- read_excel_allsheets(filename)
  tablenames = names(mysheets)
  datalist = list()
  for (i in 1:length(tablenames)) {
    # print(i)
    # print(tablenames[i])
    df = mysheets[i]
    df <- do.call(rbind.data.frame, df)
    
    if (isTRUE(donordb)) {
      df = df[-1, ] #df = df[-1:-2,]
    } else {
      df = df[-1:-2, ]
    }
    # df = df[-1,] #df = df[-1:-2,]
    df = header.true(df)
    df = df[-1, ]
    df$DonorName = tablenames[i]
    datalist[[i]] <- df # add it to a list
  }
  big_data = do.call(rbind, datalist)
  big_data[big_data == ".."] <- NA
  
  big_data = big_data[!grepl(", Total", big_data$`Sector(s)`), ]
  
  long_DF <-
    big_data %>% gather(FlowType,
                        Value,
                        `Commitments-Total 2015`:`Disbursements 2019`)
  long_DF <- long_DF %>%
    separate(FlowType, c("flow_type", "year"), " ")
  long_DF = long_DF %>%
    rename(Recipient = `Recipient(s)`,
           Sector = `Sector(s)`)
  
  long_DF$Recipient[long_DF$Recipient == "China (People's Republic of)"] <-
    "China"
  long_DF$Recipient[long_DF$Recipient == "Viet Nam"] <- "Vietnam"
  long_DF$flow_type[long_DF$flow_type == "Commitments-Total"] <-
    "Commitments"
  
  long_DF <- long_DF[!is.na(long_DF$Value),]
  return(long_DF)
}

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n / 1e3), ' k'),  # in thousands
                          ifelse(
                            n < 1e9, paste0(round(n / 1e6), ' M'),  # in millions
                            ifelse(
                              n < 1e12,
                              paste0(round(n / 1e9), ' B'),
                              # in billions
                              ifelse(n < 1e15, paste0(round(n /
                                                              1e12), ' T'), # in trillions
                                     'too big!')
                            )
                          )))
  return(labels)
}

addUnits_neo <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n / 1e3, 2), ' k'),  # in thousands
                          ifelse(
                            n < 1e9, paste0(round(n / 1e6, 2), ' M'),  # in millions
                            ifelse(
                              n < 1e12,
                              paste0(round(n / 1e9, 2), ' B'),
                              # in billions
                              ifelse(n < 1e15, paste0(round(n /
                                                              1e12, 2), ' T'), # in trillions
                                     'too big!')
                            )
                          )))
  return(labels)
}

rounded_integer <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(
                     n < 1e6,
                     round(n / 1e3, 2) * 1e3,
                     # in thousands
                     ifelse(
                       n < 1e9,
                       round(n / 1e6, 2) * 1e6,
                       # in millions
                       ifelse(
                         n < 1e12,
                         round(n / 1e9, 2) * 1e9,
                         # in billions
                         ifelse(n < 1e15, round(n / 1e12, 2) * 1e12, # in trillions
                                'too big!')
                       )
                     )
                   ))
  return(labels)
}
theme_Publication <-
  function(base_size = 14,
           base_family = "Arial") {
    # https://rpubs.com/Koundy/71792
    library(grid)
    library(ggthemes)
    (
      theme_foundation(base_size = base_size, base_family = base_family)
      + theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1.2),
          hjust = 0.5
        ),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = 'gray80'),
        axis.title = element_text(face = "bold", size = rel(0.9)),
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(),
        axis.line = element_line(colour = "gray80"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(size = rel(1.1), colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.4, "cm"),
        legend.margin = unit(0, "cm"),
        legend.title = element_text(size = rel(0.9), face = "bold"),
        plot.margin = unit(c(10, 5, 5, 5), "mm"),
        strip.background = element_rect(colour = "#f0f0f0", fill = "#bold"),
        strip.text = element_text()#(face="bold")
      )
    )
  }
scale_colour_Publication <- function(...) {
  library(scales)
  discrete_scale("colour", "Publication", manual_pal(
    values = c(
      "#386cb0",
      "#ef3b2c",
      "#fdb462",
      "#7fc97f",
      "#662506",
      "#a6cee3",
      "#fb9a99",
      "#984ea3",
      "#ffff33"
    )
  ), ...)
  
}
theme_Publication_facet <-
  function(base_size = 16,
           base_family = "helvetica") {
    library(grid)
    library(ggthemes)
    (
      theme_foundation(base_size = base_size, base_family = base_family)
      + theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1.2),
          hjust = 0.5
        ),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = NA),
        axis.title = element_text(face = "bold", size = rel(1)),
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.2, "cm"),
        legend.margin = unit(0, "cm"),
        # legend.title = element_text(face="italic"),
        plot.margin = unit(c(10, 5, 5, 5), "mm"),
        strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
        strip.text = element_text(face = "bold")
      )
    )
  }

theme_Publication_stackedbars <-
  function(base_size = 14,
           base_family = "Arial") {
    # https://rpubs.com/Koundy/71792
    library(grid)
    library(ggthemes)
    (
      theme_foundation(base_size = base_size, base_family = base_family)
      + theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1.2),
          hjust = 0.5
        ),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = 'gray80'),
        axis.title = element_text(face = "bold", size = rel(0.9)),
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(),
        axis.line = element_line(colour = "gray80"),
        axis.ticks = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "#f0f0f0"),
        legend.key = element_rect(size = rel(1.1), colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.4, "cm"),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank(),
        # legend.title = element_text(size = rel(0.9), face="bold"),
        plot.margin = unit(c(10, 5, 5, 5), "mm"),
        strip.background = element_rect(colour = "#f0f0f0", fill = "#bold"),
        strip.text = element_text()#(face="bold")
      )
    )
  }

theme_Publication_lollipop <-
  function(base_size = 14,
           base_family = "Arial") {
    # https://rpubs.com/Koundy/71792
    library(grid)
    library(ggthemes)
    (
      theme_foundation(base_size = base_size, base_family = base_family)
      + theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1.2),
          hjust = 0.5
        ),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = 'gray80'),
        axis.title = element_text(face = "bold", size = rel(0.9)),
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(),
        axis.line = element_line(colour = "gray80"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.key = element_rect(size = rel(1.1), colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.4, "cm"),
        legend.margin = unit(0, "cm"),
        legend.title = element_text(size = rel(0.9), face = "bold"),
        plot.margin = unit(c(10, 5, 5, 5), "mm"),
        strip.background = element_rect(colour = "#f0f0f0", fill = "#bold"),
        strip.text = element_text()#(face="bold")
      )
    )
  }

country.colors <- c(
  "Brazil" = "#71d0f5",
  "China" = "#47732e",
  "India" = "#f05c3b",
  "Indonesia" = "#197ec0",
  "Mexico" = "#d5e4a2",
  "Pakistan" = "#1ec31e",
  "Philippines" = "#d2af81",
  "South Africa" = "#8a9197",
  "Thailand" = "#709ae1",
  "Vietnam" = "#fed439"
)

sector.colors <- c(
  "Energy" = "#71d0f5",
  "Gen. Environment Protection" = "#47732e",
  "Transport and Storage" = "#f05c3b",
  "Mineral Resources and Mining" = "#197ec0",
  "Industry" = "#d5e4a2",
  "Industry, Mining, Construction" = "#fd7446",
  "Other Multisector" = "#d2af81"
)

fund.colors <-
  c("Commitments" = "#005596",
    "Disbursements" = "#ca6b18")

alloc.colors <- c(
  "Debt" =  "#4c8fba" ,
  "Grant" = "#f4b810",
  "Mezzanine" = "#2ac2c2",
  "Equity & shares" = "#de663e"
)

donor_sector.colors <-
  c(
    "Transport policy and administrative management" = "#fabed4",
    "Energy policy and administrative management" = "#ffd8b1",
    "Urban development and management" = "#dcbeff",
    "Environmental education/training" = "#f032e6",
    "Construction policy and administrative management" = "#911eb4",
    "Technological research and development" = "#42d4f4",
    "Industrial policy and administrative management" = "#bfef45",
    "Mineral/mining policy and administrative management" = "#f58231",
    "Air transport" = "#e6194B",
    "Environmental policy and administrative management" = "#000075",
    "Industrial development" = '#469990',
    "Energy research" = "#808000" ,
    "Energy education/training" = "#9A6324",
    "Energy conservation and demand-side efficiency" = "#800000"
  )


donor_country.colors <- c(
  "Australia" = "#800000",
  "Canada" = "#9A6324",
  "Denmark" = "#dcbeff",
  "France" = "#f032e6",
  "Germany" = "#911eb4",
  "Japan" = "#4363d8",
  "Korea" = "#bfef45",
  "Sweden" = "#f58231",
  "Switzerland" = "#e6194B",
  "UK" = "#000075",
  "USA" = '#469990',
  "Netherlands" = "#808000"
)

donor_count_multilat_colors <-
  c(
    "Asian Development Bank" = "#800000",
    "Canada" = "#9A6324",
    "Denmark" = "#dcbeff",
    "France" = "#f032e6",
    "Germany" = "#911eb4",
    "Japan" = "#4363d8",
    "Korea" = "#bfef45",
    "EIB" = "#f58231",
    "Switzerland" = "#e6194B",
    "United Kingdom" = "#000075",
    "United States" = '#469990',
    "GCF" = "#808000",
    "World Bank" = "#b74a70"
  )

export_formattable <-
  function(f,
           file,
           width = "100%",
           height = NULL,
           background = "white",
           delay = 0.2)
  {
    w <- as.htmlwidget(f, width = width, height = height)
    path <- html_print(w, background = background, viewer = NULL)
    url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
    webshot(url,
            file = file,
            selector = ".formattable_widget",
            delay = delay)
  }