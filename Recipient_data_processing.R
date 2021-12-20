# # MASTER DATA PROCESSING
# d1 = read.csv("./raw_dataset/CRS_17aug/c_2015.csv")
# d2 = read.csv("./raw_dataset/CRS_17aug/c_2016.csv")
# d3 = read.csv("./raw_dataset/CRS_17aug/c_2017.csv")
# d4 = read.csv("./raw_dataset/CRS_17aug/c_2018.csv")
# d5 = read.csv("./raw_dataset/CRS_17aug/c_2019.csv")
# masterdata <- rbind(d1,d2,d3,d4,d5)
# rm(d1,d2,d3,d4,d5)
# 
# study_countries = subset(masterdata, subset = RecipientName %in% c(
#   'India',"China (People's Republic of)",
#   'Viet Nam','Pakistan','Philippines',
#   'Indonesia','Thailand','South Africa','Brazil','Mexico'))
# study_countries = subset(study_countries, subset = DonorName %in% c(
#   'Asian Development Bank', #Asian Development Bank', 
#   'EU Institutions',#"EU Institutions (EIB)",
#   "EU Institutions (excl. EIB)", 
#   'Green Climate Fund', #'Green Climate Fund',
#   'WB','International Bank for Reconstruction and Development', # 'World Bank'
#   'UNDP', 'UNEP','International Development Association',
#   'Australia', 'Canada','Denmark','France','Germany', 'Japan',
#   'Korea', 
#   'Netherlands', 'Sweden', 'Switzerland', 'United Kingdom','United States' 
# ))
# 
# codebased_data = subset(study_countries, subset = `PurposeCode` %in% c(
#   21010, 21050, 23110, 23181, 23182, 23183, 32110, 32120, 32210,
#   32310, 32182,41010, 41081,43030))
# rm(study_countries)
# # Only Rio marker is 2 i.e. principal. (1 is significant)
# codebased_data = subset(codebased_data, codebased_data$ClimateMitigation == 2)
# codebased_data$ID <- seq.int(nrow(codebased_data))
# 
# # codebased_data1[is.na(codebased_data1)] = 0
# codebased_data[is.na(codebased_data)] = 0
# 
# codebased_data$SectorName[grepl("II.1. Transport & Storage", codebased_data$SectorName, fixed = TRUE)] <- "Transport and Storage"
# codebased_data$SectorName[grepl("II.3.a. Energy Policy", codebased_data$SectorName, fixed = TRUE)] <- "Energy"
# codebased_data$SectorName[grepl("IV.1. General Environment Protection", codebased_data$SectorName, fixed = TRUE)] <- "Gen. Environment Protection"
# codebased_data$SectorName[grepl("IV.2. Other Multisector", codebased_data$SectorName, fixed = TRUE)] <- "Other MultiSector"
# codebased_data$SectorName[grepl("III.2.a. Industry", codebased_data$SectorName, fixed = TRUE)] <- "Industry"
# codebased_data$SectorName[grepl("III.2. Industry, Mining, Construction", codebased_data$SectorName, fixed = TRUE)] <- "Industry, Mining, Construction"
# codebased_data$SectorName[grepl("III.2.b. Mineral Resources and Mining", codebased_data$SectorName, fixed = TRUE)] <- "Mineral Resources and Mining"
# 
# codebased_data$RecipientName[codebased_data$RecipientName == "China (People's Republic of)"] <- "China"
# codebased_data$RecipientName[codebased_data$RecipientName == "Viet Nam"] <- "Vietnam"
# 
# # write.csv(codebased_data,"recipient_aspect_cleaned.csv")


inputdata = read.csv("recipient_aspect_cleaned.csv")

# inputdata$DonorName[grepl("United Kingdom", inputdata$DonorName, fixed = TRUE)] <- "UK"
# inputdata$DonorName[grepl("United States", inputdata$DonorName, fixed = TRUE)] <- "US"
inputdata$DonorName[grepl("EU Institutions", inputdata$DonorName, fixed = TRUE)] <- "EU"
inputdata$DonorName[grepl("Asian Development Bank", inputdata$DonorName, fixed = TRUE)] <- "AsDB"

output_com <- inputdata %>% group_by(Year,DonorName, RecipientName, SectorName) %>% summarise(Commitments = sum(USD_Commitment))
output_disb <- inputdata %>% group_by(Year,DonorName, RecipientName, SectorName) %>% summarise(Disbursements = sum(USD_Disbursement))
data_merged <- inner_join(output_com, output_disb, by = c("Year", "DonorName", "SectorName","RecipientName"))

# values are in millions 
data_merged$Commitments <- data_merged$Commitments * 1000000
data_merged$Disbursements <- data_merged$Disbursements * 1000000

data_merged <- data_merged %>% gather(flow_type, Value, Commitments:Disbursements)

data_merged = data_merged %>% 
  rename(
    year = Year,
    Recipient = RecipientName,
    Sector = SectorName
  )

rm(masterdata, codebased_data)
rm(output_com,output_disb, inputdata)
