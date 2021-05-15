library(stringr)
library(dplyr)
require(sqldf)

if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}

start.time <- Sys.time()
# Read Master Datafile
masterdata <- read_excel("raw_dataset_oecd.xlsx")
masterdata$ID <- seq.int(nrow(masterdata))
study_countries = subset(masterdata, subset = Recipient %in% c('India',
                                                  "China (People's Republic of)", 
                                                  'Viet Nam',
                                                  'Finland',
                                                  'Pakistan',
                                                  'Indonesia',
                                                  'Thailand',
                                                  'Brazil', 'Mexico'))
study_countries = subset(study_countries, subset = Provider %in% c(
  'AsDB', #Asian Development Bank', 
  'EIB',#"EU Institutions (EIB)",
  "EU Institutions (excl. EIB)", 
  'GCF', #'Green Climate Fund',
  'WB', # 'World Bank'
  'Australia', 'Canada','Denmark','France','Germany', 'Japan', 'Korea', 
  'Netherlands', 'Sweden', 'Switzerland', 'United Kingdom'
))

# Changing GCF Significant to Principal
study_countries$`Climate objective (applies to Rio-marked data only) or climate component`[which(study_countries$Provider == "GCF")] <- "Principal"
# Remove rows with Significant
study_countries_selected<-study_countries[!(study_countries$`Climate objective (applies to Rio-marked data only) or climate component`=="Significant"),]
# aaa = unique(study_countries_selected[c("Climate objective (applies to Rio-marked data only) or climate component")])

study_countries_selected$`Adaptation-related development finance - Commitment - 2018 USD thousand`[study_countries_selected$`Overlap - Commitment - 2018 USD thousand` > 0] <- 0 
study_countries_selected$`Mitigation-related development finance - Commitment - 2018 USD thousand`[study_countries_selected$`Overlap - Commitment - 2018 USD thousand` > 0] <- 0 
study_countries_selected$`Overlap - Commitment - 2018 USD thousand`[is.na(study_countries_selected$`Overlap - Commitment - 2018 USD thousand`)] <- 0
study_countries_selected$`Adaptation-related development finance - Commitment - 2018 USD thousand`[is.na(study_countries_selected$`Adaptation-related development finance - Commitment - 2018 USD thousand`)] <- 0
study_countries_selected$`Mitigation-related development finance - Commitment - 2018 USD thousand`[is.na(study_countries_selected$`Mitigation-related development finance - Commitment - 2018 USD thousand`)] <- 0
# study_countries_selected_a <- study_countries_selected[!is.na(study_countries_selected$B), ]

write.csv(study_countries_selected,"oecd_cleaned_data_without_keyword_search.csv", row.names = FALSE)

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
rm(with_capacity)
rm(with_energyaccess)
rm(with_climfin)
rm(with_policy)
rm(with_techassist)
rm(with_ta)
rm(with_energyeffic)
rm(with_capacity_d)
rm(with_energyaccess_d)
rm(with_climfin_d)
rm(with_policy_d)
rm(with_techassist_d)
rm(with_ta_d)
rm(with_energyeffic_d)
# rm(study_countries_selected)

codebased_data = subset(study_countries_selected, subset = `Sub-sector Code` %in% c(
  21010, 21050, 23110, 23181, 23182, 23183, 32110, 32120, 32120, 32310, 32182, 41010, 41081, 43030))

merged_ds <- rbind(keyword_data, codebased_data)
# temp3 <- unique(temp3[,list(V1, V2)])
data_cleaned <- merged_ds[!duplicated(merged_ds$ID),]
# ff1 = merged_ds[duplicated(merged_ds$ID),]

write.csv(data_cleaned,"oecd_cleaned_data_with_keyword_search.csv", row.names = FALSE)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
print(time.taken)
