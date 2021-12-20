
library(ggforce)
library(ggrepel)
if(!exists("data_filtering", mode="function")) source("base_functions.R")

# df <- donor_perspective_analysis("Bilateraldonors_2015-2019_MTA.xlsx")
# df <- donor_perspective_analysis("Bilateraldonors_2015-2019_NH.xlsx",donordb = TRUE)
df <- data_merged
# datatype= "Commitments"
# countryofinterest = "Vietnam"

countries = unique(df$Recipient)
# 
# datatype= "Commitments"
# countryofinterest = "Vietnam"
# 
# 
# output<-df[(df$Recipient==countryofinterest),]
# # output<-output[(output$flow_type==datatype),]
# output = output[,!(names(output) %in% "Sector")]
# 
# output$flow_type = as.factor(as.character(output$flow_type))
# output$Recipient = as.factor(as.character(output$Recipient))
# output$DonorName = as.factor(as.character(output$DonorName))
# output$Value = as.numeric(as.character(output$Value))
# output$year = as.numeric(as.character(output$year))
# 
# output = output[,!(names(output) %in% "year")]
# 
# output = output %>%
#   group_by(Recipient,DonorName,flow_type) %>%
#   summarise_all(sum)
# 
# output <- output[which(output$Value>0),]
# 
# title = paste("Total commitments and disbursements to",countryofinterest, "during 2015-2019")
# outputfigure = ggplot(output, aes(fill=flow_type, y=Value, x=DonorName)) + 
#   geom_bar(position="dodge", stat="identity") + ggtitle(title) + xlab("") + ylab("in USD") +
#   scale_y_continuous(labels = addUnits) +
#   facet_zoom(ylim = c(0, 10000000), zoom.data = ifelse(Value <= 10000000, NA, FALSE),zoom.size = 1) +
#   theme_bw() + scale_fill_manual(values = fund.colors)
# 
# outputfigure  + theme(legend.position = "bottom",
#                       legend.direction = "horizontal")#+ scale_colour_Publication() + theme_Publication_stackedbars(base_size=14)
# 
# output <- output %>% 
#   mutate(anchorval = "2006")
# output$Value = output$Value/1000000
# 
# ggplot(output, aes(x = anchorval, y = Value, fill = DonorName)) +
#   geom_col() +
#   geom_text(aes(label = round(Value, 2)), # paste0(round(Value, 2), "USD in millions")
#             position = position_stack(vjust = 0.5)) +
#   scale_fill_brewer(palette = "Set2") +
#   theme_minimal(base_size = 16) +
#   ylab("USD in millions") + coord_flip() +
#   xlab(NULL)



for (countryofinterest in countries) {
  print(countryofinterest)
  output<-df[(df$Recipient==countryofinterest),]
  output = output[,!(names(output) %in% "Sector")]
  
  output$flow_type = as.factor(as.character(output$flow_type))
  output$Recipient = as.factor(as.character(output$Recipient))
  output$DonorName = as.factor(as.character(output$DonorName))
  output$Value = as.numeric(as.character(output$Value))
  output$year = as.numeric(as.character(output$year))
  
  output = output[,!(names(output) %in% "year")]
  
  output = output %>%
    group_by(Recipient,DonorName,flow_type) %>%
    summarise_all(sum)

  title = paste("Total commitments and disbursements to",countryofinterest, "during 2015-2019")
  outputfigure = ggplot(output, aes(fill=flow_type, y=Value, x=DonorName)) + 
    geom_bar(position="dodge", stat="identity") + ggtitle(title) + xlab("") + ylab("in USD") +
    scale_y_continuous(labels = addUnits) +
    # facet_zoom(ylim = c(0, 10000000), zoom.data = ifelse(Value <= 10000000, NA, FALSE)) +
    theme_bw() + scale_fill_manual(values = fund.colors)
 
  output_name = paste0("fig2_recipient_chart_",countryofinterest,".png")
  
  if (countryofinterest == "Indonesia") { 
    widthvalue = 340
  } else if (countryofinterest ==  "China") {
    widthvalue = 280
  } else if (countryofinterest ==  "Vietnam") {
    widthvalue = 320
  } else if (countryofinterest ==  "India") {
    widthvalue = 300
  } else {
    widthvalue = 250
  }
  
  
  png(output_name, width = widthvalue, height = 125, units='mm', res = 300)
  print(outputfigure + scale_colour_Publication() + theme_Publication_stackedbars(base_size=14))
  Sys.sleep(3)  
  dev.off()
}

# 
# output<-df[(df$Recipient=="Indonesia"),]
# output = output[,!(names(output) %in% "Sector")]
# 
# output$flow_type = as.factor(as.character(output$flow_type))
# output$Recipient = as.factor(as.character(output$Recipient))
# output$DonorName = as.factor(as.character(output$DonorName))
# output$Value = as.numeric(as.character(output$Value))
# output$year = as.numeric(as.character(output$year))
# 
# output = output[,!(names(output) %in% "year")]
# 
# output = output %>%
#   group_by(Recipient,DonorName,flow_type) %>%
#   summarise_all(sum)
# 
# fundolors <- c("Commitments" = "#005596", "Disbursements" = "#ca6b18")
# 
# title = paste("Total commitments and disbursements to","Indonesia", "during 2015-2019")
# outputfigure = ggplot(output, aes(fill=flow_type, y=Value, x=DonorName)) + 
#   geom_bar(position="dodge", stat="identity") + scale_y_continuous(labels = addUnits) +
#   ggtitle(title) + xlab("") + ylab("in USD") + theme_bw() + 
#   # scale_color_manual(values = fundolors)
#   scale_fill_manual(values = fundolors)
# 
# outputfigure + scale_colour_Publication() + theme_Publication_stackedbars()
# 
# output_name = paste0("fig2_recipient_","Indonesia",".png")
# png(output_name, width = 250, height = 125, units='mm', res = 300)
# print(outputfigure + scale_colour_Publication() + theme_Publication_stackedbars(base_size=14))
# Sys.sleep(3)  
# dev.off()



# 
# 
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
# output = output[,!(names(output) %in% "year")]
# 
# output = output %>%
#   group_by(Recipient,DonorName,flow_type) %>%
#   summarise_all(sum)
# 
# title = paste("Total commitments and disbursements to",countryofinterest, "during 2015-2019")
# outputfigure = ggplot(output, aes(fill=flow_type, y=Value, x=DonorName)) + 
#   geom_bar(position="dodge", stat="identity") + ggtitle(title) + xlab("") + ylab("in '000 US$") +
#   theme_bw() + scale_fill_manual(values = fund.colors)
# 
# output_name = paste0("fig2_recipient_",countryofinterest,".png")
# png(output_name, width = 250, height = 125, units='mm', res = 300)
# print(outputfigure + theme_Publication_stackedbars(base_size=14))
# Sys.sleep(3)  
# dev.off()
# 

# 
# 
# commi<-output[(output$flow_type=="Commitments"),]
# disb<-output[(output$flow_type=="Disbursements"),]
# 
# comm_title = paste("Total commitments to",countryofinterest, "during 2015-2019")
# disb_title = paste("Total disbursments to",countryofinterest, "during 2015-2019")
# 
# maxval = max(commi$Value) + (sd(commi$Value)/2)
# commit =ggplot(commi, aes(x=reorder(DonorName, Value), y=Value)) +
#   geom_segment(aes(xend=DonorName, yend=0), color="grey60") +
#   geom_point(size=4, color="orange") +
#   coord_flip() +
#   ggtitle(comm_title) +
#   theme_bw() +
#   xlab("") +
#   ylim(0, maxval)+
#   geom_text(aes(label=round(Value, digits=2)), vjust=0.4, hjust=-0.6, size = 3.5, color = "grey50") 
# 
# maxval = max(disb$Value) + (sd(disb$Value)/2)
# disburs =ggplot(disb, aes(x=reorder(DonorName, Value), y=Value)) +
#   geom_segment(aes(xend=DonorName, yend=0), color="grey60") +
#   geom_point(size=4, color="blue") +
#   coord_flip() +
#   ggtitle(disb_title) +
#   theme_bw() +
#   xlab("") +
#   ylim(0, maxval)+
#   geom_text(aes(label=round(Value, digits=2)), vjust=0.4, hjust=-0.6, size = 3.5, color = "grey50") 
# # 
# #   geom_text_repel(
# #     aes(label = Value),
# #     # family = "Poppins",
# #     size = 3.5,
# #     # min.segment.length = 0, 
# #     # seed = 42, 
# #     box.padding = 0.7,
# #     max.overlaps = Inf,
# #     # arrow = arrow(length = unit(0.030, "npc")),
# #     # nudge_x = .20,
# #     # nudge_y = .15,
# #     color = "grey50"
# #   )
# 
# commit + theme_Publication_lollipop()
# disburs + theme_Publication_lollipop()
