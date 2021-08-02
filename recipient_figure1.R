
if(!exists("data_filtering", mode="function")) source("base_functions.R")

df <- donor_perspective_analysis("Bilateraldonors_2015-2019_NH.xlsx",donordb = TRUE)

countries = unique(df$Recipient)

# datatype= "Commitments"
# countryofinterest = "Vietnam"

for (countryofinterest in countries) {
  # countryofinterest = countries[10]
  print(countryofinterest)
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
  
  titlename = paste("Trends in funds for", countryofinterest)
  titlename_cumulative = paste("Cumulative Trends in funds for", countryofinterest)
  
  # plt = output %>%
  #   ggplot(aes(x=year, y=Value, group=flow_type, color=flow_type)) +
  #   # geom_area() +
  #   geom_line(size = 1.2) +
  #   geom_point(size = 2) +
  #   xlab("Year") + ylab("in US$") +
  #   scale_fill_manual(values=fund.colors) +
  #   # scale_fill_viridis(discrete = TRUE) +
  #   # theme(legend.position="bottom") +
  #   labs(color = "Fund type:") +
  #   ggtitle(titlename) +
  #   facet_wrap(~DonorName, scale="free_y")
  # 
  # plt + scale_colour_Publication() + theme_Publication_facet()
  
  plt_cumulative = output_cumulative %>%
    ggplot(aes(x=year, y=cumu_val, group=flow_type, color=flow_type)) +
    geom_line(size = 1.2) +geom_point(size = 2) +
    xlab("Year") + ylab("in Million USD") +
    labs(color = "Fund type:") + theme_bw() +
    ggtitle(titlename_cumulative) +
    facet_wrap(~DonorName, scale="free_y") + scale_color_manual(values=fund.colors)
  # plt_cumulative + scale_colour_Publication() + theme_Publication_facet()
  
  output_name_cumul = paste0("fig1_recipient_cumulative_",countryofinterest,".png")
  png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
    print(plt_cumulative + theme_Publication_facet()) # scale_colour_Publication() +
  Sys.sleep(3)  
  dev.off()
  # Sys.sleep(5)
}



# 
# 
# countryofinterest = countries[10]
# 
# output<-df[(df$Recipient==countryofinterest),]
# output = output[,!(names(output) %in% "Sector")]
# 
# output$flow_type = as.factor(as.character(output$flow_type))
# output$Recipient = as.factor(as.character(output$Recipient))
# output$DonorName = as.factor(as.character(output$DonorName))
# output$Value = as.numeric(as.character(output$Value))
# output$year = as.numeric(as.character(output$year))
# 
# output = output %>%
#   group_by(Recipient,DonorName,year, flow_type) %>%
#   summarise_all(sum)
# 
# output_cumulative = output %>%
#   group_by(Recipient,DonorName,flow_type) %>%
#   mutate(cumu_val = cumsum(Value))
# 
# titlename = paste("Trends in funds for", countryofinterest)
# titlename_cumulative = paste("Cumulative Trends in funds for", countryofinterest)
# 
# # plt = output %>%
# #   ggplot(aes(x=year, y=Value, group=flow_type, color=flow_type)) +
# #   # geom_area() +
# #   geom_line(size = 1.2) +
# #   geom_point(size = 2) +
# #   xlab("Year") + ylab("in US$") +
# #   scale_fill_manual(values=fund.colors) +
# #   # scale_fill_viridis(discrete = TRUE) +
# #   # theme(legend.position="bottom") +
# #   labs(color = "Fund type:") +
# #   ggtitle(titlename) +
# #   facet_wrap(~DonorName, scale="free_y")
# # 
# # plt + scale_colour_Publication() + theme_Publication_facet()
# 
# plt_cumulative = output_cumulative %>%
#   ggplot(aes(x=year, y=cumu_val, group=flow_type, color=flow_type)) +
#   geom_line(size = 1.2) +geom_point(size = 2) +
#   xlab("Year") + ylab("Cumulative sum in Million US$") +
#   scale_fill_manual(values=fund.colors) +
#   labs(color = "Fund type:") +
#   ggtitle(titlename_cumulative) +
#   facet_wrap(~DonorName, scale="free_y")
# # plt_cumulative + scale_colour_Publication() + theme_Publication_facet()
# 
# output_name_cumul = paste0("fig1_recipient_cumulative_",countryofinterest,".png")
# png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
#   plt_cumulative + scale_colour_Publication() + theme_Publication_facet()
# Sys.sleep(3)  
# dev.off()
# # Sys.sleep(5)
