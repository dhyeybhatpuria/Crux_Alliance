library(ggplot2)
if (!require("ggalluvial")) {
  install.packages("ggalluvial")
  library(ggalluvial)
}

if(!exists("data_filtering", mode="function")) source("base_functions.R")
#just first time
# master <- select_2015_2019("CRDF-RP-2000-2019.xlsx")

master <- data_filtering("crux_data_2015-2019.xlsx")

master = master %>% 
  rename(
    Finance.Instrument =`Financial Instrument`,
    Overlap.Commitment = `Overlap - Commitment - 2019 USD thousand`,
    Adaptation = `Adaptation-related development finance - Commitment - 2019 USD thousand`,
    Mitigation = `Mitigation-related development finance - Commitment - 2019 USD thousand`,
    Sector = `Sector (detailed)`
  )
master$Sector[grepl("II.1. Transport and Storage", master$Sector, fixed = TRUE)] <- "Transport and Storage"
master$Sector[grepl("II.3. Energy", master$Sector, fixed = TRUE)] <- "Energy"
master$Sector[grepl("IV.1. General Environment Protection", master$Sector, fixed = TRUE)] <- "Gen. Environment Protection"
master$Sector[grepl("IV.2. Other Multisector", master$Sector, fixed = TRUE)] <- "Other Multisector"
master$Sector[grepl("III.2.a. Industry", master$Sector, fixed = TRUE)] <- "Industry"
master$Sector[grepl("III.2. Industry, Mining, Construction", master$Sector, fixed = TRUE)] <- "Industry, Mining, Construction"
master$Sector[grepl("III.2.b. Mineral Resources and Mining", master$Sector, fixed = TRUE)] <- "Mineral Resources and Mining"


countryname = 'Switzerland'
master_selected = subset(master, master$Provider == countryname)
# master_selected = master
# In case only one type of finance
output <- master_selected %>% group_by(Year, Sector) %>% summarise(Commitment = sum(USD.thousand))
output$Commitment <- output$Commitment * 1000

titletext = paste("Year-wise commitment by", countryname ,"for each sector (2015-2019)")

plt = ggplot(output, aes(fill=Sector, y=Commitment, x=Year)) + # 
  geom_bar(stat="identity") +#stat="identity" position="dodge",
  # geom_line(aes(x=Year, y=Commitment),stat="identity")+
  scale_y_continuous(labels = addUnits)+
  labs(title = titletext) +
  scale_fill_manual(values=sector.colors)
# +facet_wrap(~ Recipient,scales="free")

plt + theme_Publication(base_size=14, base_family="helvetica")
