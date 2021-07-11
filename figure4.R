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

master$Recipient[master$Recipient == "China (People's Republic of)"] <- "China"
master$Finance.Instrument[master$Finance.Instrument == "Mezzanine finance instrument"] <- "Mezzanine"

# master$Mitigation

countryname = 'Switzerland'
master_selected = subset(master, master$Provider == countryname)
# In case only one type of finance
output <- master_selected %>% group_by(Year, Sector) %>% summarise(Commitment = sum(Mitigation))
output['Finance.Instrument'] = unique(master_selected$Finance.Instrument)


titletext = paste("Year-wise commitment to sector by ", countryname ," (2015-2019)")
gg = ggplot(output, aes(x=Year, y=Commitment, group=Sector, color=Sector)) + #geom_line(data=dfr[!is.na(dfr$y),])
  # geom_line(size = 1, color =Recipient) +
  geom_line(size = 1)+
  geom_point(shape = 16, size = 2.5)+ ylab("Commitment in USD thousand") + xlab("Years") +
  scale_y_continuous(labels = addUnits) + 
  # scale_x_continuous(breaks = seq(1965,2020,by=10), labels = xaxislabels) +
  labs(title = titletext
       # subtitle = "Number of people whose house is destroyed or heavily damaged and therefore \nneed shelter after an event.",
       # caption = "Data source: EMDAT"
  ) +
  theme_minimal() +#t heme_light() #theme_minimal()
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 10)
  )
plot(gg)
