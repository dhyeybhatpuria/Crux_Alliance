library(ggplot2)
if(!exists("data_filtering", mode="function")) source("base_functions.R")


df <- donor_perspective_analysis("Bilateraldonors_2015-2019_MTA.xlsx")

# datatype= "Commitments"
countryofinterest = "Vietnam"

# output <- df %>% 
#   group_by(Recipient,DonorName,flow_type,year,Value) %>%  #group_by(decade, Country, Disaster.Type) %>% tally()
#   summarize_if(is.numeric, sum, na.rm=TRUE)
# # output<-output[(output$flow_type==datatype),]
# output<-output[(output$Recipient==countryofinterest),]
# # output = output[,!(names(output) %in% "flow_type")]

output<-df[(df$Recipient==countryofinterest),]
output = output[,!(names(output) %in% "Sector")]

output$flow_type = as.factor(as.character(output$flow_type))
output$Recipient = as.factor(as.character(output$Recipient))
output$DonorName = as.factor(as.character(output$DonorName))
output$Value = as.numeric(as.character(output$Value))
output$year = as.numeric(as.character(output$year))

output = output %>%
  group_by(Recipient,DonorName,year, flow_type) %>%
  summarise_all(sum)

output_cumulative = output %>%
  group_by(Recipient,DonorName,flow_type) %>%
  mutate(cumu_val = cumsum(Value))

# 
# ggplot(output, aes(x=year, y=Value, fill=DonorName)) + #geom_line(data=dfr[!is.na(dfr$y),])
#   geom_area()+
#   # geom_line(size = 1) +
#   # geom_point(shape = 16, size = 2.5)+ ylab("number of persons") + 
#   xlab("Years")
# 
# ggplot(output, aes(x=year, y=Value, group=DonorName, color=DonorName)) + #geom_line(data=dfr[!is.na(dfr$y),])
#   geom_line(size = 1) +
#   geom_point(shape = 16, size = 2.5)+ ylab("number of persons") + 
#   xlab("Years")# +
#   # scale_y_continuous(labels = addUnits) + 
#   # scale_x_continuous(breaks = seq(1965,2020,by=10), labels = xaxislabels) +
#   # labs(title = "Homeless",
#   #      subtitle = "Number ofvent.",
#   #      caption = "Data source: EMDAT") +
#   theme_minimal() +#t heme_light() #theme_minimal()
#   theme(
#     plot.title = element_text(size = 14),
#     plot.subtitle = element_text(size = 10)
#   )
# plot(gg)
# 
# 
# ggplot(output, aes(x=year, y=Value, size = DonorName, color=DonorName)) +
#   geom_point(alpha=0.7)

titlename = paste("Trends in funds for", countryofinterest)

# library(hrbrthemes)
# library(viridis)
# plt = output %>%
#   ggplot(aes(x=year, y=Value, group=flow_type, color=flow_type)) +
#   # geom_area() +
#   geom_line(size = 1.2) +
#   scale_fill_viridis(discrete = TRUE) +
#   # theme(legend.position="bottom") +
#   labs(color = "Fund type:")+
#   ggtitle(titlename)+
#   # # theme_ipsum() +
#   # theme(
#   #   # legend.title = element_text("Funds",size=13),
#   #   legend.position="bottom",
#   #   panel.spacing = unit(1, "lines"),
#   #   strip.text.x = element_text(size = 10, ),
#   #   plot.title = element_text(size=13)
#   # ) +
#   # labs(group = "Fund")+
#   facet_wrap(~DonorName, scale="free_y")


plt = output %>%
  ggplot(aes(x=year, y=Value, group=flow_type, color=flow_type)) +
  # geom_area() +
  geom_line(size = 1.2) +
  xlab("Year") + ylab("in US$") +
  scale_fill_manual(values=fund.colors) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme(legend.position="bottom") +
  labs(color = "Fund type:") +
  ggtitle(titlename) +
  facet_wrap(~DonorName, scale="free_y")


# plt +scale_colour_Publication()+ theme_Publication(base_size=14, base_family="helvetica")
plt + scale_colour_Publication() + theme_Publication_facet()

output_cumulative = output %>%
  group_by(Recipient,DonorName,flow_type) %>%
  mutate(cumu_val = cumsum(Value))

titlename_cumulative = paste("Cumulative Trends in funds for", countryofinterest)
plt_cumulative = output_cumulative %>%
  ggplot(aes(x=year, y=cumu_val, group=flow_type, color=flow_type)) +
  # geom_area() +
  geom_line(size = 1.2) +
  xlab("Year") + ylab("Cumulative in US$") +
  scale_fill_manual(values=fund.colors) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme(legend.position="bottom") +
  labs(color = "Fund type:") +
  ggtitle(titlename_cumulative) +
  facet_wrap(~DonorName, scale="free_y")

plt_cumulative + scale_colour_Publication() + theme_Publication_facet()




