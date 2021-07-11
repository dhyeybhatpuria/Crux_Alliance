library(stringr)
library(dplyr)
require(sqldf)

if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}

start.time <- Sys.time()
# Read Master Datafile
masterdata <- read_excel("CRDF-RP-2019.xlsx") # raw_dataset_oecd_15_19.xlsx
masterdata$ID <- seq.int(nrow(masterdata))
study_countries = subset(masterdata, subset = Recipient %in% c('India',
                                                               "China (People's Republic of)", 
                                                               'Viet Nam',
                                                               'Finland',
                                                               'Pakistan',
                                                               'Philippines',
                                                               'Indonesia',
                                                               'Thailand',
                                                               'South Africa',
                                                               'Brazil', 'Mexico'))
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

rm(with_transport)
rm(with_storage)
rm(with_energy)
rm(with_industry)
rm(with_mining)
rm(with_protection)
rm(with_multi)
rm(key_sector_data_alpha)


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

write.csv(study_countries_selected,"oecd_till_step7_only_2019_data.csv", row.names = FALSE)

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

codebased_data = subset(study_countries_selected, subset = `Purpose Code` %in% c(
  21010, 21050, 
  23110, 23181, 23182, 23183, 
  32110, 32120, 32210, 32310, 32182,
  41010, 41081,
  43030))

merged_ds <- rbind(keyword_data, codebased_data)
# temp3 <- unique(temp3[,list(V1, V2)])
data_cleaned <- merged_ds[!duplicated(merged_ds$ID),]
# ff1 = merged_ds[duplicated(merged_ds$ID),]

write.csv(data_cleaned,"oecd_all_steps_only_2019_data.csv", row.names = FALSE)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
print(time.taken)
