
if(!exists("data_filtering", mode="function")) source("base_functions.R")

df <- donor_perspective_analysis("Bilateraldonors_2015-2019_NH.xlsx",donordb = TRUE)

datatype= "Commitments" # Disbursements Commitments
countryname = "Vietnam"

output<-df[(df$Recipient==countryname),]

output$flow_type = as.factor(as.character(output$flow_type))
output$Recipient = as.factor(as.character(output$Recipient))
output$Sector = as.factor(as.character(output$Sector))
output$DonorName = as.factor(as.character(output$DonorName))
output$Value = as.numeric(as.character(output$Value))
output$year = as.numeric(as.character(output$year))

output<-output[(output$flow_type==datatype),]

# In case only one type of finance
output <- output %>% group_by(year, DonorName) %>% summarise(Commitment = sum(Value))
output$Commitment <- output$Commitment * 1000
titletext = paste("Year-wise commitment to", countryname ,"by Donors (2015-2019)")

plt = ggplot(output, aes(fill=DonorName, y=Commitment, x=year)) + # 
  geom_bar(stat="identity") +#stat="identity" position="dodge",
  # geom_line(aes(x=Year, y=Commitment),stat="identity")+
  scale_y_continuous(labels = addUnits)+
  labs(title = titletext) +
  scale_fill_manual(values=donor_country.colors)
# +facet_wrap(~ Recipient,scales="free")

plt + theme_Publication(base_size=14, base_family="helvetica")
