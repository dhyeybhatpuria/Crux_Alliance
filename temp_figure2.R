library(ggplot2)
if (!require("ggalluvial")) {
  install.packages("ggalluvial")
  library(ggalluvial)
}
library(gridExtra)
library(grid)
library(data.table)
library(dplyr)
library(networkD3)
library(tidyr)
library(htmlwidgets)

countryname = 'EIB'
if(!exists("data_filtering", mode="function")) source("base_functions.R")
#just first time
# master <- select_2015_2019("CRDF-RP-2000-2019.xlsx")

master <- data_filtering("crux_data_2015-2019.xlsx")
# write.xlsx(master, file = "crux.xlsx",sheetName = "newData", append = FALSE)
master = master %>% 
  rename(
    Finance.Instrument =`Financial Instrument`,
    Overlap.Commitment = `Overlap - Commitment - 2019 USD thousand`,
    Adaptation = `Adaptation-related development finance - Commitment - 2019 USD thousand`,
    Mitigation = `Mitigation-related development finance - Commitment - 2019 USD thousand`
  )
# master$Recipient[master$Recipient == "China (People's Republic of)"] <- "China"
# master$Finance.Instrument[master$Finance.Instrument == "Mezzanine finance instrument"] <- "Mezzanine"
# master$Finance.Instrument[master$Finance.Instrument == "Equity and shares in collective investment vehicles"] <- "Equity & shares"

master_selected = subset(master, master$Provider == countryname)
master_switz = master_selected[, c('Provider', 'Recipient', 'Finance.Instrument','USD.thousand', 'Year')]

output <- master_selected %>% group_by(Finance.Instrument, Year) %>% summarise(Commitment = sum(USD.thousand))

titletext = paste("Sector-wise commitment by ", countryname ," (2015-2019)")
# gg = ggplot(output, aes(x=Year, y=Commitment, group=Finance.Instrument, color=Finance.Instrument)) + #geom_line(data=dfr[!is.na(dfr$y),])
#   # geom_line(size = 1, color =Recipient) +
#   geom_line(size = 1)+
#   geom_point(shape = 16, size = 2.5)+ ylab("Commitment in US$ ") + xlab("Years") +
#   scale_y_continuous(labels = addUnits_neo) + 
#   # scale_x_continuous(breaks = seq(1965,2020,by=10), labels = xaxislabels) +
#   labs(title = titletext
#        # subtitle = "Number of people whose house is destroyed or heavily damaged and therefore \nneed shelter after an event.",
#        # caption = "Data source: EMDAT"
#   ) +
#   theme_minimal() +#t heme_light() #theme_minimal()
#   theme(
#     plot.title = element_text(size = 14),
#     plot.subtitle = element_text(size = 10)
#   )
# gg


ggplot(output, aes(fill=Finance.Instrument, y=Commitment, x=Year)) + 
  geom_bar( stat="identity") +# position="dodge",
scale_y_continuous(labels = addUnits)

library(viridis)
library(hrbrthemes)
ggplot(output, aes(fill=Finance.Instrument, y=Commitment, x=Year)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Studying 4 species..") +
  facet_wrap(~Finance.Instrument, scales = "free") +
  # theme_ipsum() +
  # theme(legend.position="none") +
  xlab("")
