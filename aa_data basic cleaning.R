
if (!require("readxl")) {
  install.packages("readxl")
  library(readxl)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("readr")) {
  install.packages("readr")
  library(readr)
}
if (!require("reshape2")) {
  install.packages("reshape2")
  library(reshape2)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# library(ggsci)
# library(pals)
# library(awtools)
if (!require("ggsci")) {
  install.packages("ggsci")
  library(ggsci)
}
if (!require("pals")) {
  install.packages("pals")
  library(pals)
}
if (!require("devtools")) {
  install.packages("devtools")
  library(devtools)
}
if (!require("awtools")) {
  devtools::install_github('awhstin/awtools')
  library(awtools)
}
if (!require("ggalluvial")) {
  install.packages("ggalluvial")
  library(ggalluvial)
}
if (!require("ggforce")) {
  install.packages("ggforce")
  library(ggforce)
}
if (!require("ggbeeswarm")) {
  install.packages("ggbeeswarm")
  library(ggbeeswarm)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("rvest")) {
  install.packages("rvest")
  library(rvest)
}

database[is.na(database)] <- 0

vc = c("China", "India", "Brazil", "Indonesia", "Mexico", 
       "South Africa", "Pakistan", "Thailand", "Vietnam", "Philippines")
va = c("Green Climate Fund", "World Bank", "Asian Development Bank")
selected = database[database$recipient_name %in% vc,]
selected = selected[selected$donor_name %in% va,]
selected[is.na(selected)] <- 0

output <- selected %>% 
  group_by(donor_name, recipient_name, lvl_1_sector_name) %>% #tally() #group_by(decade, Country, Disaster.Type) %>% tally()
summarize_if(is.numeric, sum, na.rm=TRUE)

ggplot(as.data.frame(output),
       aes(y = usd_commitment, axis1 = donor_name, axis2 = recipient_name)) +
  geom_alluvium(aes(fill = lvl_1_sector_name), width = 1/16) +
  geom_stratum(width = 1/12, fill = "grey70", color = "grey10") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Disaster", "Country"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_fill_discrete(name = "Disaster Category") +
  ggtitle("Natural Disaster occurances by Country")

ggplot(as.data.frame(output),
       aes(y = n, axis1 = Disaster.Subgroup, axis2 = Country)) +
  geom_alluvium(aes(fill = Disaster.Type), width = 1/16) +
  geom_stratum(width = 1/12, fill = "grey70", color = "grey10") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Category", "Country"), expand = c(.07, .05)) +
  scale_fill_brewer( palette = "Set1") +
  # scale_fill_discrete(name = "Disaster") + 
  ylab(NULL) +
  #a_plex_theme() + 
  # a_flat_color() +
  ggtitle("Natural Disaster occurances by Country")