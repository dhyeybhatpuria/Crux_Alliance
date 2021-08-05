

if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
#just first time
# master <- select_2015_2019("CRDF-RP-2000-2019.xlsx")

# master <- data_filtering("crux_data_2015-2019.xlsx")
# master <- data_basic_cleaning("Search_terms_MTA_Crux_CT_R_version.csv")
master = read_excel("crux_2015-2019_master.xlsx")

sectors = unique(master$Sector)
for (sectorname in sectors) {
  print(sectorname)
  master_selected = subset(master, master$Sector == sectorname)
  output <- master_selected %>% group_by(Year, Recipient) %>% summarise(Commitment = sum(USD.thousand))
  output$Commitment <- output$Commitment * 1000
  
  titletext = paste("Year-wise commitment to", sectorname ,"(2015-2019)")
  
  plt = ggplot(output, aes(fill=Recipient, y=Commitment, x=Year)) + # 
    geom_bar(stat="identity") +#stat="identity" position="dodge",
    # geom_line(aes(x=Year, y=Commitment),stat="identity")+
    scale_y_continuous(labels = addUnits_neo)+
    labs(title = titletext) +
    scale_fill_manual(values=country.colors)
  # +facet_wrap(~ Recipient,scales="free")

  output_name_cumul = paste0("fig2_sector_commitment_",sectorname,".png")
  png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
    print(plt + theme_Publication(base_size=14, base_family="helvetica")) # scale_colour_Publication() +
  Sys.sleep(3)  
  dev.off()
  
}







if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
#just first time
# master <- select_2015_2019("CRDF-RP-2000-2019.xlsx")

master <- data_filtering("crux_data_2015-2019.xlsx")

# All sector names:
# "Transport and Storage"          "Energy"                         "Gen. Environment Protection"   
# "Other Multisector"              "Industry, Mining, Construction" "Industry"                      
# "Mineral Resources and Mining"  

sectorname = 'Energy'

# master = master %>% 
#   rename(
#     Finance.Instrument =`Financial Instrument`,
#     Overlap.Commitment = `Overlap - Commitment - 2019 USD thousand`,
#     Adaptation = `Adaptation-related development finance - Commitment - 2019 USD thousand`,
#     Mitigation = `Mitigation-related development finance - Commitment - 2019 USD thousand`,
#     Sector = `Sector (detailed)`
#   )


master_selected = subset(master, master$Sector == sectorname)

# In case only one type of finance
output <- master_selected %>% group_by(Year, Recipient) %>% summarise(Commitment = sum(USD.thousand))
output$Commitment <- output$Commitment * 1000

titletext = paste("Year-wise commitment to", sectorname ,"(2015-2019)")

plt = ggplot(output, aes(fill=Recipient, y=Commitment, x=Year)) + # 
  geom_bar(stat="identity") +#stat="identity" position="dodge",
  # geom_line(aes(x=Year, y=Commitment),stat="identity")+
  scale_y_continuous(labels = addUnits_neo)+
  labs(title = titletext) +
  scale_fill_manual(values=country.colors)
# +facet_wrap(~ Recipient,scales="free")

plt + theme_Publication(base_size=14, base_family="helvetica")
