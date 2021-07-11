
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
# if (!require("ggbeeswarm")) {
#   install.packages("ggbeeswarm")
#   library(ggbeeswarm)
# }
# if (!require("awtools")) {
#   devtools::install_github('awhstin/awtools')
#   library(awtools)
# }

data_filtering_olddata = function(inputdata){
  masterdata <- read_excel(inputdata) # 
  masterdata$ID <- seq.int(nrow(masterdata))
  study_countries = subset(masterdata, subset = Recipient %in% c(
    'India',"China (People's Republic of)",'Viet Nam',
    'Pakistan','Philippines','Indonesia',
    'Thailand','South Africa','Brazil','Mexico'))
  # China, India, Brazil, Indonesia, Mexico, South Africa, Pakistan, Thailand, Vietnam and the Philippines. 
  
  study_countries = subset(study_countries, subset = Provider %in% c(
    'AsDB', #Asian Development Bank', 
    'EIB',#"EU Institutions (EIB)",
    "EU Institutions (excl. EIB)", 
    'GCF', #'Green Climate Fund',
    'WB', # 'World Bank'
    'Australia', 'Canada','Denmark','France','Germany', 'Japan', 'Korea', 
    'Netherlands', 'Sweden', 'Switzerland', 'United Kingdom','United States'
  ))
  # IRENA, UNDP, UNEP MISSING
  # the European Union, the Green Climate Fund, IRENA, UNDP, UNEP, 
  # World Bank, Asian Development Bank and European Investment Bank
  # United Kingdom, Germany, France, The Netherlands, Denmark, 
  # Switzerland, Sweden, USA, Canada, Australia, Japan and South Korea
  
  
  
  # transport and storage; Energy; Industry, construction and mining; General environment protection; and Other multisector
  with_transport = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%transport%'")
  with_storage = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%storage%'")
  with_energy = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%Energy%'")
  # with_const = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%construction%'")
  with_industry = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%Industry%'")
  with_mining = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%mining%'")
  with_protection = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%protection%'")
  with_multi = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%multisector%'")
  
  key_sector_data_alpha <- rbind(with_transport, with_storage, with_energy, #with_const, 
                                 with_industry, with_mining, with_protection, with_multi)
  study_countries <- key_sector_data_alpha[!duplicated(key_sector_data_alpha$ID),]
  # Changing GCF Significant to Principal
  study_countries$`Climate objective (applies to Rio-marked data only) or climate component`[which(study_countries$Provider == "GCF")] <- "Principal"
  # Remove rows with Significant
  study_countries_selected<-study_countries[!(study_countries$`Climate objective (applies to Rio-marked data only) or climate component`=="Significant"),]
  
  study_countries_selected$`Adaptation-related development finance - Commitment - 2018 USD thousand`[study_countries_selected$`Overlap - Commitment - 2018 USD thousand` > 0] <- 0 
  study_countries_selected$`Mitigation-related development finance - Commitment - 2018 USD thousand`[study_countries_selected$`Overlap - Commitment - 2018 USD thousand` > 0] <- 0 
  study_countries_selected$`Overlap - Commitment - 2018 USD thousand`[is.na(study_countries_selected$`Overlap - Commitment - 2018 USD thousand`)] <- 0
  study_countries_selected$`Adaptation-related development finance - Commitment - 2018 USD thousand`[is.na(study_countries_selected$`Adaptation-related development finance - Commitment - 2018 USD thousand`)] <- 0
  study_countries_selected$`Mitigation-related development finance - Commitment - 2018 USD thousand`[is.na(study_countries_selected$`Mitigation-related development finance - Commitment - 2018 USD thousand`)] <- 0

  
  # Search Strings: ‘capacity’, policy’, ‘technical assistance’, ‘TA  ’, ‘ energy access’, ‘energy efficiency’ and ‘climate finance’ 
  with_capacity = sqldf("select * from study_countries_selected where `Short Description` LIKE '%capacity%'")
  with_policy = sqldf("select * from study_countries_selected where `Short Description` LIKE '%policy%'")
  with_techassist = sqldf("select * from study_countries_selected where `Short Description` LIKE '%technical assistance%'")
  with_ta = sqldf("select * from study_countries_selected where `Short Description` LIKE '% TA %'")
  with_energyaccess = sqldf("select * from study_countries_selected where `Short Description` LIKE '%energy access%'")
  with_energyeffic = sqldf("select * from study_countries_selected where `Short Description` LIKE '%energy efficiency%'")
  with_climfin = sqldf("select * from study_countries_selected where `Short Description` LIKE '%climate finance%'")
  
  with_capacity_d = sqldf("select * from study_countries_selected where Description LIKE '%capacity%'")
  with_policy_d = sqldf("select * from study_countries_selected where Description LIKE '%policy%'")
  with_techassist_d = sqldf("select * from study_countries_selected where Description LIKE '%technical assistance%'")
  with_ta_d = sqldf("select * from study_countries_selected where Description LIKE '% TA %'")
  with_energyaccess_d = sqldf("select * from study_countries_selected where Description LIKE '%energy access%'")
  with_energyeffic_d = sqldf("select * from study_countries_selected where Description LIKE '%energy efficiency%'")
  with_climfin_d = sqldf("select * from study_countries_selected where Description LIKE '%climate finance%'")
  
  
  keyword_data <- rbind(with_capacity, with_policy, with_climfin, 
                        with_techassist, with_ta, with_energyaccess, with_energyeffic
                        ,
                        with_capacity_d, with_policy_d, with_climfin_d, 
                        with_techassist_d, with_ta_d, with_energyaccess_d, with_energyeffic_d)
  codebased_data = subset(study_countries_selected, subset = `Sub-sector Code` %in% c(
    21010, 21050, 
    23110, 23181, 23182, 23183, 
    32110, 32120, 32210, 32310, 32182,
    41010, 41081,
    43030))
  
  merged_ds <- rbind(keyword_data, codebased_data)
  data_cleaned <- merged_ds[!duplicated(merged_ds$ID),]
  return(data_cleaned)
}

data_filtering = function(inputdata){
  masterdata <- read_excel(inputdata) # 
  # masterdata <- subset(masterdata, masterdata$Year > 2014)
  masterdata$ID <- seq.int(nrow(masterdata))
  study_countries = subset(masterdata, subset = Recipient %in% c(
    'India',"China (People's Republic of)",'Viet Nam','Finland','Pakistan','Philippines','Indonesia','Thailand','South Africa','Brazil','Mexico'))
  study_countries = subset(study_countries, subset = Provider %in% c(
    'AsDB', #Asian Development Bank', 
    'EIB',#"EU Institutions (EIB)",
    "EU Institutions (excl. EIB)", 
    'GCF', #'Green Climate Fund',
    'WB', # 'World Bank'
    'Australia', 'Canada','Denmark','France','Germany', 'Japan', 'Korea', 
    'Netherlands', 'Sweden', 'Switzerland', 'United Kingdom','United States'
  ))
  
  # transport and storage; Energy; Industry, construction and mining; General environment protection; and Other multisector
  with_transport = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%transport%'")
  with_storage = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%storage%'")
  with_energy = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%Energy%'")
  # with_const = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%construction%'")
  with_industry = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%Industry%'")
  with_mining = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%mining%'")
  with_protection = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%protection%'")
  with_multi = sqldf("select * from study_countries where `Sector (detailed)` LIKE '%multisector%'")
  
  key_sector_data_alpha <- rbind(with_transport, with_storage, with_energy, #with_const, 
                                 with_industry, with_mining, with_protection, with_multi)
  study_countries <- key_sector_data_alpha[!duplicated(key_sector_data_alpha$ID),]
  # Changing GCF Significant to Principal
  study_countries$`Climate objective (applies to Rio-marked data only) or climate component`[which(study_countries$Provider == "GCF")] <- "Principal"
  # Remove rows with Significant
  study_countries_selected<-study_countries[!(study_countries$`Climate objective (applies to Rio-marked data only) or climate component`=="Significant"),]
  
  study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand`[study_countries_selected$`Overlap - Commitment - 2019 USD thousand` > 0] <- 0 
  study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand`[study_countries_selected$`Overlap - Commitment - 2019 USD thousand` > 0] <- 0 
  study_countries_selected$`Overlap - Commitment - 2019 USD thousand`[is.na(study_countries_selected$`Overlap - Commitment - 2019 USD thousand`)] <- 0
  study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand`[is.na(study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand`)] <- 0
  study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand`[is.na(study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand`)] <- 0
  study_countries_selected$USD.thousand = study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand` + study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand` + study_countries_selected$`Overlap - Commitment - 2019 USD thousand` 
  
  # Search Strings: ‘capacity’, policy’, ‘technical assistance’, ‘TA  ’, ‘ energy access’, ‘energy efficiency’ and ‘climate finance’ 
  with_capacity = sqldf("select * from study_countries_selected where `Project Title` LIKE '%capacity%'")
  with_policy = sqldf("select * from study_countries_selected where `Project Title` LIKE '%policy%'")
  with_techassist = sqldf("select * from study_countries_selected where `Project Title` LIKE '%technical assistance%'")
  with_ta = sqldf("select * from study_countries_selected where `Project Title` LIKE '% TA %'")
  with_energyaccess = sqldf("select * from study_countries_selected where `Project Title` LIKE '%energy access%'")
  with_energyeffic = sqldf("select * from study_countries_selected where `Project Title` LIKE '%energy efficiency%'")
  with_climfin = sqldf("select * from study_countries_selected where `Project Title` LIKE '%climate finance%'")
  
  with_capacity_d = sqldf("select * from study_countries_selected where Description LIKE '%capacity%'")
  with_policy_d = sqldf("select * from study_countries_selected where Description LIKE '%policy%'")
  with_techassist_d = sqldf("select * from study_countries_selected where Description LIKE '%technical assistance%'")
  with_ta_d = sqldf("select * from study_countries_selected where Description LIKE '% TA %'")
  with_energyaccess_d = sqldf("select * from study_countries_selected where Description LIKE '%energy access%'")
  with_energyeffic_d = sqldf("select * from study_countries_selected where Description LIKE '%energy efficiency%'")
  with_climfin_d = sqldf("select * from study_countries_selected where Description LIKE '%climate finance%'")
  
  
  keyword_data <- rbind(with_capacity, with_policy, with_climfin, 
                        with_techassist, with_ta, with_energyaccess, with_energyeffic
                        ,
                        with_capacity_d, with_policy_d, with_climfin_d, 
                        with_techassist_d, with_ta_d, with_energyaccess_d, with_energyeffic_d)
  codebased_data = subset(study_countries_selected, subset = `Purpose Code` %in% c(
    21010, 21050, 23110, 23181, 23182, 23183, 32110, 32120, 32210, 32310, 32182,41010, 41081,43030))
  
  merged_ds <- rbind(keyword_data, codebased_data)
  data_cleaned <- merged_ds[!duplicated(merged_ds$ID),]
  
  data_cleaned$Recipient[data_cleaned$Recipient == "China (People's Republic of)"] <- "China"
  data_cleaned$Recipient[data_cleaned$Recipient == "Viet Nam"] <- "Vietnam"
  
  data_cleaned$`Financial Instrument`[data_cleaned$`Financial Instrument` == "Debt instrument"] <- "Debt"
  data_cleaned$`Financial Instrument`[data_cleaned$`Financial Instrument` == "Mezzanine finance instrument"] <- "Mezzanine"
  data_cleaned$`Financial Instrument`[data_cleaned$`Financial Instrument` == "Equity and shares in collective investment vehicles"] <- "Equity & shares"
  
  data_cleaned$Provider[ data_cleaned$Provider == "EU Institutions (excl. EIB)"] <- "EU"
  
  temprows= subset(data_cleaned, data_cleaned$`Sector (detailed)` == "II.1. Transport & Storage")
  temprows$`Sector (detailed)` <- gsub("II.1. Transport & Storage","II.1. Transport and Storage",temprows$`Sector (detailed)`)
  permarows<-subset(data_cleaned, `Sector (detailed)` !="II.1. Transport & Storage")
  total <- rbind(permarows,temprows)
  
  return(total)
}


select_2015_2019 = function(inputdata){
  masterdata <- read_excel(inputdata)
  masterdata <- subset(masterdata, masterdata$Year > 2014)
  write.xlsx(masterdata, file = "crux_data_2015-2019.xlsx",sheetName = "newData", append = FALSE)
  return("Export Done")
}


addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), ' k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), ' M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), ' B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), ' T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

addUnits_neo <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3,2), ' k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6,2), ' M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9,2), ' B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12,2), ' T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}
 
theme_Publication <- function(base_size=14, base_family="Arial") {# https://rpubs.com/Koundy/71792
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = 'gray80'),
            axis.title = element_text(face = "bold", size = rel(0.9)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="gray80"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(size = rel(1.1), colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.4, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(size = rel(0.9), face="bold"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#bold"),
            strip.text = element_text()#(face="bold")
    ))
}


# country.colors <- c("Brazil" = "#0C5BB0FF", # 09488C 
#                     "China" = "#B02D0E", # BE000D
#                     "India" ="#FF9900", # 118070  
#                     "Indonesia" = "#FA6B09FF", #107930 
#                     "Mexico" = "#149BEDFF", # D44E8A     
#                     "Pakistan" = "#15983DFF", #E16008 
#                     "Philippines" = "#9A703EFF", #149bed
#                     "South Africa" = "#EC579AFF", # 809F19
#                     "Thailand" = "#FEC10BFF",  # E4AD09
#                     "Viet Nam" = "#A1C720FF")# 7B5931
# 
# country.colors <- c("Brazil" = "#09488C", # 09488C 
#                     "China" = "#CB323D", # BE000D
#                     "India" ="#383867", # 118070  
#                     "Indonesia" = "#33431e", #107930 
#                     "Mexico" = "#92462f", # D44E8A     
#                     "Pakistan" = "#584c77", #E16008 
#                     "Philippines" = "#a36629", #149bed
#                     "South Africa" = "#b63e36", # 809F19
#                     "Thailand" = "#946943",  # E4AD09
#                     "Vietnam" = "#b74a70")# 7B5931

country.colors <- c("Brazil" = "#71d0f5", # 09488C 
                    "China" = "#47732e", # BE000D
                    "India" ="#f05c3b", # 118070  
                    "Indonesia" = "#197ec0", #107930 
                    "Mexico" = "#d5e4a2", # D44E8A     
                    "Pakistan" = "#fd7446", #E16008 
                    "Philippines" = "#d2af81", #149bed
                    "South Africa" = "#8a9197", # 809F19
                    "Thailand" = "#709ae1",  # E4AD09
                    "Vietnam" = "#fed439")# 7B5931

# colors adopted from library(yarrr) par(mfrow=c(1,1)) piratepal(palette = "all") basel


# ColourScal ='d3.scaleOrdinal() 
# .range(["#383867","#33431e","#92462f", "#584c77",
# "#a36629", "#b63e36","#946943", "#b74a70"])'

sector.colors <- c("Energy" = "#71d0f5", # 09488C 
                    "Gen. Environment Protection" = "#47732e", # BE000D
                    "Transport and Storage" ="#f05c3b", # 118070  
                    "Mineral Resources and Mining" = "#197ec0", #107930 
                    "Industry" = "#d5e4a2", # D44E8A     
                    "Industry, Mining, Construction" = "#fd7446", #E16008 
                    "Other Multisector" = "#d2af81")

