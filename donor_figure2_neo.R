
if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
#just first time
# master <- select_2015_2019("CRDF-RP-2000-2019.xlsx")

# master <- data_filtering("crux_data_2015-2019.xlsx")
# master <- data_basic_cleaning("Search_terms_MTA_Crux_CT_R_version.csv")
master = read_excel("crux_2015-2019_master.xlsx")

countries = unique(master$Provider)
for (countryofinterest in countries) {
  print(countryofinterest)
  master_selected = subset(master, master$Provider == countryofinterest)
  # master_selected = master
  # In case only one type of finance
  output <- master_selected %>% group_by(Year, Recipient) %>% summarise(Commitment = sum(USD.thousand))
  output$Commitment <- output$Commitment * 1000
  titletext = paste("Year-wise commitment by", countryofinterest ,"to recipient country (2015-2019)")
  
  plt = ggplot(output, aes(fill=Recipient, y=Commitment, x=Year)) + # 
    geom_bar(stat="identity") +#stat="identity" position="dodge",
    # geom_line(aes(x=Year, y=Commitment),stat="identity")+
    scale_y_continuous(labels = addUnits)+
    labs(title = titletext) +
    scale_fill_manual(values=country.colors)
  # +facet_wrap(~ Recipient,scales="free")
  
  output_name_cumul = paste0("fig2_donor_commit2country_",countryofinterest,".png")
  png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
    print(plt + theme_Publication(base_size=14, base_family="helvetica")) # scale_colour_Publication() +
  Sys.sleep(3)  
  dev.off()
  
}




countryname = 'Denmark'
master_selected = subset(master, master$Provider == countryname)
# master_selected = master
# In case only one type of finance
output <- master_selected %>% group_by(Year, Recipient) %>% summarise(Commitment = sum(USD.thousand))
output$Commitment <- output$Commitment * 1000
titletext = paste("Year-wise commitment by", countryname ,"to recipient country (2015-2019)")

plt = ggplot(output, aes(fill=Recipient, y=Commitment, x=Year)) + # 
  geom_bar(stat="identity", width = 0.9) +#stat="identity" position="dodge",
  # geom_line(aes(x=Year, y=Commitment),stat="identity")+
  scale_y_continuous(labels = addUnits)+
  labs(title = titletext) + #scale_x_continuous(labels=2018:2019, breaks = 2018:2019) + 
  scale_fill_manual(values=country.colors)
# +facet_wrap(~ Recipient,scales="free")

plt + theme_Publication(base_size=14, base_family="helvetica")

output_name_cumul = paste0("fig2_donor_commit2country_",countryname,".png")
png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
  print(plt + theme_Publication(base_size=14, base_family="helvetica")) # scale_colour_Publication() +
Sys.sleep(3)  
dev.off()

# 
# plt = ggplot(output, aes(fill=Year, y=Commitment, x=Recipient)) + # 
#   geom_bar(stat="identity") +#stat="identity" position="dodge",
#   # geom_line(aes(x=Year, y=Commitment),stat="identity")+
#   scale_y_continuous(labels = addUnits)+
#   labs(title = titletext) +
#   scale_fill_manual(values=country.colors)+facet_wrap(~ Year,scales="free")
# 
# plt + theme_Publication(base_size=14, base_family="helvetica")


# 
# gga = ggplot(output, aes(x=Year,y=Recipient,color=Recipient))+
#   ggbeeswarm::geom_quasirandom(
#     alpha=.75,aes(size=Commitment),
#     groupOnX = FALSE,
#     show.legend = TRUE) +
#   a_plex_theme(plot_title_size = 16) + a_flat_color() +
#   scale_x_discrete(name ="Year",
#                    limits=c(2015,2016,2017,2018,2019)) +
#   labs(title='"Natural" Disasters in Myanmar',
#        subtitle='The people affected by natural disasters ',
#        y='', caption='Data:EMDAT')
# 
# gg <- gga + guides(color=guide_legend("Disaster Sub-group"), 
#                    size=guide_legend("No of Affected")) #+ scale_y_continuous(labels = scales::comma)
# plot(gg)
