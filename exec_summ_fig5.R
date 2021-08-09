


if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
master = read_excel("crux_2015-2019_master.xlsx")

# countries = unique(master$Recipient)
# In case only one type of finance
# output <- master %>% group_by(Year,Recipient) %>% summarise(Commitment = sum(USD.thousand)) %>% arrange(desc(Commitment))# 
# output$Commitment <- output$Commitment * 1000

output = master %>%
  select(Recipient,Year,USD.thousand)
output$USD.thousand <- output$USD.thousand * 1000


# output$flow_type = as.factor(as.character(output$flow_type))
output$Recipient = as.factor(as.character(output$Recipient))
# output$DonorName = as.factor(as.character(output$DonorName))
output$USD.thousand = as.numeric(as.character(output$USD.thousand))
output$Year = as.numeric(as.character(output$Year))

output = output %>%
  group_by(Year,Recipient) %>%
  summarise_all(sum)

output_cumulative = output %>%
  group_by(Recipient) %>%
  mutate(cumu_val = cumsum(USD.thousand))
# title = " "
# outputfigure = ggplot(output_cumulative, aes(fill=Recipient, y=cumu_val, x=Year)) + 
#   geom_bar(position="dodge", stat="identity") + ggtitle(title) + xlab("") + ylab("in '000 USD") +
#   theme_bw()# + scale_fill_manual(values = fund.colors)
# 
# print(outputfigure)

aa1 = ggplot(output_cumulative, aes(fill=Recipient, y=cumu_val, x=Year))+geom_line(size = 0, alpha = 0) +
  geom_smooth(size = 1.2, alpha = 0)+theme_minimal() + scale_color_manual(values=country.colors)+
  scale_y_continuous(labels = addUnits)+ ylab("in USD")

aa = ggplot(output, aes(color=Recipient, y=USD.thousand, x=Year)) + 
geom_line(size = 0, alpha = 0) +geom_smooth(size = 1.2, alpha = 0)+
  theme_minimal() + scale_color_manual(values=country.colors)+
  scale_y_continuous(labels = addUnits)+ ylab("Commitments in USD")
  # scale_x_continuous(expand = c(0, 0)) #+
  # # scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
  # ggtitle("Orange trees getting bigger with age",
  #         subtitle = "Based on the Orange data set in R")
fontsize = 14



output_name_cumul = paste0("fig5_TS_recipients.png")
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
