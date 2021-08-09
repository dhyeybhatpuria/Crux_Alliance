


if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
master = read_excel("crux_2015-2019_master.xlsx")

# countries = unique(master$Provider)
# In case only one type of finance
# output <- master %>% group_by(Year,Provider) %>% summarise(Commitment = sum(USD.thousand)) %>% arrange(desc(Commitment))# 
# output$Commitment <- output$Commitment * 1000

output = master %>%
  select(Provider,Year,USD.thousand)
output$USD.thousand <- output$USD.thousand * 1000

output$Provider[output$Provider == "AsDB"] <- "Asian Development Bank"
output$Provider[output$Provider == "WB"] <- "World Bank"

# output$flow_type = as.factor(as.character(output$flow_type))
output$Provider = as.factor(as.character(output$Provider))
# output$DonorName = as.factor(as.character(output$DonorName))
output$USD.thousand = as.numeric(as.character(output$USD.thousand))
output$Year = as.numeric(as.character(output$Year))

output = output %>%
  group_by(Year,Provider) %>%
  summarise_all(sum)

# add zero to fill up 
output1 = merge(output, data.frame(table(output[1:2]))[-3],all.y=TRUE)
output1[is.na(output1)] <- 0
output1 = data.frame(output1)
output1$Provider = as.factor(as.character(output1$Provider))
output1$USD.thousand = as.numeric(as.character(output1$USD.thousand))
output1$Year = as.numeric(as.character(output1$Year))
# output_cumulative = output %>%
#   group_by(Provider) %>%
#   mutate(cumu_val = cumsum(USD.thousand))
# title = " "
# outputfigure = ggplot(output_cumulative, aes(fill=Provider, y=cumu_val, x=Year)) + 
#   geom_bar(position="dodge", stat="identity") + ggtitle(title) + xlab("") + ylab("in '000 USD") +
#   theme_bw()# + scale_fill_manual(values = fund.colors)
# 
# print(outputfigure)
# 
# 
# aa1 = ggplot(output_cumulative, aes(fill=Provider, y=cumu_val, x=Year))+geom_line(size = 0, alpha = 0) +
#   geom_smooth(size = 1.2, alpha = 0)+theme_minimal() + scale_color_manual(values=donor_count_multilat_colors)+
#   scale_y_continuous(labels = addUnits)+ ylab("in USD")

# ggplot(output1, aes(color=Provider, y=USD.thousand, x=Year)) +
#   geom_line(size = 0, alpha = 0) +theme_minimal() + scale_color_manual(values=donor_count_multilat_colors)+
#   scale_y_continuous(labels = addUnits)+ ylab("in USD")+geom_smooth(size = 1.2, alpha = 0)
# # ggplot(output_cumulative, aes(fill=Provider, y=cumu_val, x=Year))+geom_line(size = 1, alpha = 0.5)

aa = ggplot(output1, aes(color=Provider, y=USD.thousand, x=Year)) + 
  geom_line(size = 0, alpha = 0) +geom_smooth(size = 1.2, alpha = 0)+
  theme_minimal() + scale_color_manual(values=donor_count_multilat_colors)+
  scale_y_continuous(labels = addUnits)+ ylab("Commitments in USD")  
# scale_x_continuous(expand = c(0, 0)) #+
# # scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
# ggtitle("Orange trees getting bigger with age",
#         subtitle = "Based on the Orange data set in R")
fontsize = 14
# aa + theme(panel.grid.major = element_blank(),
#            axis.title.x = element_text(size=fontsize),
#            axis.title.y=element_text(size=fontsize),
#            panel.grid.minor = element_blank(),
#            panel.grid.minor.y= element_blank(),
#            panel.grid.major.y=  element_line(colour="#f0f0f0"),
#            text = element_text(size=fontsize),
#            legend.key = element_rect(size =fontsize, colour = NA),
#            legend.position = "bottom",
#            legend.direction = "horizontal",
#            # legend.key.size= unit(0.2, "cm"),
#            # legend.margin = unit(0, "cm"),
#            legend.title = element_text(size = fontsize, face="bold"),
#            legend.text=element_text(size=fontsize))


output_name_cumul = paste0("fig6_TS_Donors.png")
png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
  print(aa + theme(panel.grid.major = element_blank(),
                   axis.title.x = element_text(size=fontsize),
                   axis.title.y=element_text(size=fontsize),
                   panel.grid.minor = element_blank(),
                   panel.grid.minor.y= element_blank(),
                   panel.grid.major.y=  element_line(colour="#f0f0f0"),
                   text = element_text(size=fontsize),
                   panel.border = element_rect(colour = 'gray60',fill = NA),
                   legend.key = element_rect(size =fontsize, colour = NA),
                   legend.position = "bottom",
                   legend.direction = "horizontal",
                   # legend.key.size= unit(0.2, "cm"),
                   # legend.margin = unit(0, "cm"),
                   legend.title = element_text(size = fontsize, face="bold"),
                   legend.text=element_text(size=fontsize))) # scale_colour_Publication() +
Sys.sleep(3)  
dev.off()

# 
# 
# if (!require("ggalluvial")) {
#   install.packages("ggalluvial")
#   library(ggalluvial)
# }
# if (!require("ggforce")) {
#   install.packages("ggforce")
#   library(ggforce)
# }
# if (!require("ggbeeswarm")) {
#   install.packages("ggbeeswarm")
#   library(ggbeeswarm)
# }
# if (!require("awtools")) {
#   devtools::install_github('awhstin/awtools')
#   library(awtools)
# }
# gga = ggplot(output, aes(color=Provider, y=Provider, x=Year))+
#   ggbeeswarm::geom_quasirandom(
#     alpha=.8,aes(size=USD.thousand),
#     groupOnX = FALSE,
#     show.legend = TRUE) +# scale_x_continuous(labels=pretty(output$USD.thousand)) + 
#   theme_minimal() + scale_color_manual(values=donor_count_multilat_colors)+
#   a_plex_theme(plot_title_size = 16) +
#   # a_flat_color() +
#   scale_x_discrete(name ="Year",limits=c(2015,2016,2017,2018, 2019)) +
#   labs(y='')
# 
# gg <- gga + guides(color=guide_legend("Donor Countries"), 
#                    size=guide_legend("USD")) #+ scale_y_continuous(labels = scales::comma)
# gg
# 
# 
# aa = ggplot(output, aes(color=Provider, y=USD.thousand, x=Year)) + 
#   geom_line(size = 1.2, alpha = 0.6)+geom_smooth(size = 1.2, alpha = 0.6)+
#   theme_minimal() + scale_color_manual(values=donor_count_multilat_colors)+
#   scale_y_continuous(labels = addUnits)+ ylab("in USD")