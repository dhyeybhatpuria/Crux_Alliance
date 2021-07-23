
library(ggrepel)
if(!exists("data_filtering", mode="function")) source("base_functions.R")

# df <- donor_perspective_analysis("Bilateraldonors_2015-2019_MTA.xlsx")
df <- donor_perspective_analysis("Bilateraldonors_2015-2019_NH.xlsx",donordb = TRUE)

# datatype= "Commitments"
countryofinterest = "Vietnam"

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

commi<-output[(output$flow_type=="Commitments"),]
disb<-output[(output$flow_type=="Disbursements"),]

comm_title = paste("Total commitments to",countryofinterest, "during 2015-2019")
disb_title = paste("Total disbursments to",countryofinterest, "during 2015-2019")

maxval = max(commi$Value) + (sd(commi$Value)/2)
commit =ggplot(commi, aes(x=reorder(DonorName, Value), y=Value)) +
  geom_segment(aes(xend=DonorName, yend=0), color="grey60") +
  geom_point(size=4, color="orange") +
  coord_flip() +
  ggtitle(comm_title) +
  theme_bw() +
  xlab("") +
  ylim(0, maxval)+
  geom_text(aes(label=round(Value, digits=2)), vjust=0.4, hjust=-0.6, size = 3.5, color = "grey50") 

maxval = max(disb$Value) + (sd(disb$Value)/2)
disburs =ggplot(disb, aes(x=reorder(DonorName, Value), y=Value)) +
  geom_segment(aes(xend=DonorName, yend=0), color="grey60") +
  geom_point(size=4, color="blue") +
  coord_flip() +
  ggtitle(disb_title) +
  theme_bw() +
  xlab("") +
  ylim(0, maxval)+
  geom_text(aes(label=round(Value, digits=2)), vjust=0.4, hjust=-0.6, size = 3.5, color = "grey50") 
# 
#   geom_text_repel(
#     aes(label = Value),
#     # family = "Poppins",
#     size = 3.5,
#     # min.segment.length = 0, 
#     # seed = 42, 
#     box.padding = 0.7,
#     max.overlaps = Inf,
#     # arrow = arrow(length = unit(0.030, "npc")),
#     # nudge_x = .20,
#     # nudge_y = .15,
#     color = "grey50"
#   )

commit + theme_Publication_lollipop()
disburs + theme_Publication_lollipop()
