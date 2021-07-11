library(ggplot2)
if (!require("ggalluvial")) {
  install.packages("ggalluvial")
  library(ggalluvial)
}
library(ggfittext)
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
# output <- master %>% 
# group_by(`Financial Instrument`, `Overlap - Commitment - 2019 USD thousand`, Recipient) %>% tally()

output <- master_selected %>% group_by(Finance.Instrument, Sector) %>% summarise(Commitment = sum(USD.thousand))

# In case only one type of finance
# output <- master_selected %>% group_by(Recipient) %>% summarise(Commitment = sum(USD.thousand))
# output['Finance.Instrument'] = unique(master_selected$Finance.Instrument)

titletext = paste("Sector-wise commitment by ", countryname ," (2015-2019)")
ggplot(as.data.frame(output),
       aes(y = Commitment, axis1 =  Finance.Instrument, axis2 =  Sector)) +
  geom_alluvium(aes(fill = Finance.Instrument), width = 1/14, ) +
  geom_stratum(width = 1/12, fill = "grey70", color = "grey10", alpha = .5) +
  geom_flow() +
  # geom_stratum(alpha = .3) +
  geom_text(stat = "stratum", size = 3.5,aes(label = after_stat(stratum))) +#+ geom_fit_text(reflow = TRUE)+ 
  # geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Finance Instrument", "Sector"), expand = c(.05, .05) ) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_fill_discrete(name = "Finance Instrument") +
  ggtitle(titletext)

grid.table(output)

