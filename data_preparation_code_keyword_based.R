
masterdata <- read_excel("crux_data_2015-2019.xlsx") #

inputdata =  read_excel("Search_terms_MTA_Crux_CT_R_version.xlsx")
inputdata = inputdata %>% 
  rename(
    Finance.Instrument = `Financial Instrument`,
    Overlap.Commitment = `Overlap - Commitment - 2018 USD thousand`,
    Adaptation = `Adaptation-related development finance - Commitment - 2018 USD thousand`,
    Mitigation = `Mitigation-related development finance - Commitment - 2018 USD thousand`,
    `Climate-related development finance - Commitment - 2019 USD thousand` = `Climate-related development finance - Commitment - 2018 USD thousand`,
    Sector = `Sector (detailed)`,
    `Purpose Code` = `Sub-sector Code`,
    `Development Cooperation Modality` = `Type of Aid`,
    `Project Title` = `Short Description`)

inputdata$Recipient[inputdata$Recipient == "China (People's Republic of)"] <- "China"
inputdata$Recipient[inputdata$Recipient == "Viet Nam"] <- "Vietnam"

inputdata$Finance.Instrument[inputdata$Finance.Instrument == "Debt instrument"] <- "Debt"
inputdata$Finance.Instrument[inputdata$Finance.Instrument == "Mezzanine finance instrument"] <- "Mezzanine"
inputdata$Finance.Instrument[inputdata$Finance.Instrument == "Equity and shares in collective investment vehicles"] <- "Equity & shares"

inputdata$Provider[ inputdata$Provider == "EU Institutions (excl. EIB)"] <- "EU"

temprows= subset(inputdata, inputdata$Sector == "II.1. Transport & Storage")
temprows$Sector <- gsub("II.1. Transport & Storage","II.1. Transport and Storage",temprows$Sector)
permarows<-subset(inputdata, Sector !="II.1. Transport & Storage")
total <- rbind(permarows,temprows)
# Rename Sectors
total$Sector[grepl("II.1. Transport and Storage", total$Sector, fixed = TRUE)] <- "Transport and Storage"
total$Sector[grepl("II.3. Energy", total$Sector, fixed = TRUE)] <- "Energy"
total$Sector[grepl("IV.1. General Environment Protection", total$Sector, fixed = TRUE)] <- "Gen. Environment Protection"
total$Sector[grepl("IV.2. Other Multisector", total$Sector, fixed = TRUE)] <- "Other Multisector"
total$Sector[grepl("III.2.a. Industry", total$Sector, fixed = TRUE)] <- "Industry"
total$Sector[grepl("III.2. Industry, Mining, Construction", total$Sector, fixed = TRUE)] <- "Industry, Mining, Construction"
total$Sector[grepl("III.2.b. Mineral Resources and Mining", total$Sector, fixed = TRUE)] <- "Mineral Resources and Mining"

total$USD.thousand = total$Mitigation + total$Adaptation + total$Overlap.Commitment
keywords_based = total
rm(total)


# keywords_based <- data_basic_cleaning("Search_terms_MTA_Crux_CT_R_version.csv")
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
study_countries_selected<-study_countries_selected[!(study_countries_selected$`Mitigation objective (applies to Rio-marked data only)`=="Significant"),]

study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand`[study_countries_selected$`Overlap - Commitment - 2019 USD thousand` > 0] <- 0 
study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand`[study_countries_selected$`Overlap - Commitment - 2019 USD thousand` > 0] <- 0 
study_countries_selected$`Overlap - Commitment - 2019 USD thousand`[is.na(study_countries_selected$`Overlap - Commitment - 2019 USD thousand`)] <- 0
study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand`[is.na(study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand`)] <- 0
study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand`[is.na(study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand`)] <- 0
study_countries_selected$USD.thousand = study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand` + study_countries_selected$`Adaptation-related development finance - Commitment - 2019 USD thousand` + study_countries_selected$`Overlap - Commitment - 2019 USD thousand` 

study_countries_selected = subset(study_countries_selected, study_countries_selected$`Mitigation-related development finance - Commitment - 2019 USD thousand` > 0)

codebased_data = subset(study_countries_selected, subset = `Purpose Code` %in% c(
  21010, 21050, 23110, 23181, 23182, 23183, 32110, 32120, 32210, 32310, 32182,41010, 41081,43030))

# merged_ds <- rbind(keyword_data, codebased_data)
# data_cleaned <- merged_ds[!duplicated(merged_ds$ID),]

data_cleaned <- codebased_data

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
# Rename Sectors
total$`Sector (detailed)`[grepl("II.1. Transport and Storage", total$`Sector (detailed)`, fixed = TRUE)] <- "Transport and Storage"
total$`Sector (detailed)`[grepl("II.3. Energy", total$`Sector (detailed)`, fixed = TRUE)] <- "Energy"
total$`Sector (detailed)`[grepl("IV.1. General Environment Protection", total$`Sector (detailed)`, fixed = TRUE)] <- "Gen. Environment Protection"
total$`Sector (detailed)`[grepl("IV.2. Other Multisector", total$`Sector (detailed)`, fixed = TRUE)] <- "Other Multisector"
total$`Sector (detailed)`[grepl("III.2.a. Industry", total$`Sector (detailed)`, fixed = TRUE)] <- "Industry"
total$`Sector (detailed)`[grepl("III.2. Industry, Mining, Construction", total$`Sector (detailed)`, fixed = TRUE)] <- "Industry, Mining, Construction"
total$`Sector (detailed)`[grepl("III.2.b. Mineral Resources and Mining", total$`Sector (detailed)`, fixed = TRUE)] <- "Mineral Resources and Mining"


total = total %>% 
  rename(
    Finance.Instrument =`Financial Instrument`,
    Overlap.Commitment = `Overlap - Commitment - 2019 USD thousand`,
    Adaptation = `Adaptation-related development finance - Commitment - 2019 USD thousand`,
    Mitigation = `Mitigation-related development finance - Commitment - 2019 USD thousand`,
    Sector = `Sector (detailed)`
  )

codebased <- total
rm(
  codebased_data,data_cleaned,key_sector_data_alpha,permarows,study_countries,study_countries_selected,temprows,
  total,with_energy,with_industry,with_mining,with_multi,with_protection,with_storage,with_transport
)

aaa = setdiff( colnames(keywords_based), colnames(codebased))
print(aaa)
aab = setdiff( colnames(codebased),  colnames(keywords_based))
print(aab)

merged_ds <- rbind(keywords_based, codebased)
write.xlsx(merged_ds, file = "crux_2015-2019_04082021_newmethod.xlsx",sheetName = "newData", append = FALSE)

#NOTE NOW ALL DUPLICATE ROWS ARE REMOVED MANUALLY IN EXCEL

a_date = read_excel("crux_2015-2019_04082021_newmethod.xlsx")
a_date = subset(a_date, a_date$Mitigation > 0)
write.xlsx(a_date, file = "crux_2015-2019_master.xlsx",sheetName = "newData", append = FALSE)
