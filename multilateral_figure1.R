
if(!exists("data_filtering", mode="function")) source("base_functions.R")
df <- donor_perspective_analysis("Multilateral_NH.xlsx",donordb = FALSE)

# datatype= "Commitments"
countryofinterest = "India"

output<-df[(df$Recipient==countryofinterest),]
# output <-df
output = output[,!(names(output) %in% "Sector")]

output$flow_type = as.factor(as.character(output$flow_type))
output$Recipient = as.factor(as.character(output$Recipient))
output$DonorName = as.factor(as.character(output$DonorName))
output$Value = as.numeric(as.character(output$Value))
output$year = as.numeric(as.character(output$year))

output = output %>%
  group_by(Recipient,DonorName,year, flow_type) %>%
  summarise_all(sum)
# write.csv(output, "multilateral_Donors_DB_1.csv")


output_cumulative = output %>%
  group_by(Recipient,DonorName,flow_type) %>%
  mutate(cumu_val = cumsum(Value))

titlename = paste("Trends in funds for", countryofinterest)
titlename_cumulative = paste("Cumulative Trends in funds for", countryofinterest)

plt = output %>%
  ggplot(aes(x=year, y=Value, group=flow_type, color=flow_type)) +
  # geom_area() +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  xlab("Year") + ylab("in US$") +
  scale_fill_manual(values=fund.colors) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme(legend.position="bottom") +
  labs(color = "Fund type:") +
  ggtitle(titlename) +
  facet_wrap(~DonorName, scale="free_y")

plt + scale_colour_Publication() + theme_Publication_facet()

plt_cumulative = output_cumulative %>%
  ggplot(aes(x=year, y=cumu_val, group=flow_type, color=flow_type)) +
  geom_line(size = 1.2) +geom_point(size = 2) +
  xlab("Year") + ylab("Cumulative sum in US$") +
  scale_fill_manual(values=fund.colors) +
  labs(color = "Fund type:") +
  ggtitle(titlename_cumulative) +
  facet_wrap(~DonorName, scale="free_y")

plt_cumulative + scale_colour_Publication() + theme_Publication_facet()
