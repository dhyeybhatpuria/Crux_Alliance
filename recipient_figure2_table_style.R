# install.packages("formattable")

#https://www.littlemissdata.com/blog/prettytables
# https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/


library("htmltools")
library("webshot")    
library(formattable)
df <- data_merged
# datatype= "Commitments"
# countryofinterest = "Vietnam"

countries = unique(df$Recipient)


datatype= "Commitments"
countryofinterest = "Vietnam"

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
  
  output <- output[which(output$Value>0),]
  output$Value = round((output$Value/1000), 2)
  
  output = output %>% spread(flow_type, Value)
  output$Commitments[is.na(output$Commitments)] <- 0
  output$Disbursements[is.na(output$Disbursements)] <- 0
  output = output[ , !(names(output) %in% "Recipient")]
  output$Difference = round((output$Disbursements-output$Commitments),2)
  output$Difference = as.numeric(as.character(output$Difference))
  
  output = output %>% 
    rename(
      `Donor Name` = DonorName,
      `Commitments ('000 USD)` = Commitments,
      `Disbursements ('000 USD)` = Disbursements,
      `Difference ('000 USD)` = Difference
    )
  
  customGreen0 = "#DeF7E9"
  customGreen = "#99cc00"#"#71CA97"
  customRed =  "#FF3232"#"#ff7f7f"
  customBlue = "#ccccff"
  customYellow = "#ffbf00"
  
  improvement_formatter <- formatter("span", 
                                     style = x ~ style(font.weight = "bold", 
                                                       color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
                                     x ~ icontext(ifelse(x>0, "arrow-up",  ifelse(x < 0, "arrow-down", "minus")), x)
  )
  
  daa = formattable(output, 
                    align =c("l","r", "r"),  # "c",
                    list(`Indicator Name` = formatter(
                      "span", style = ~ style(color = "grey",font.weight = "bold")),
                      `Commitments ('000 USD)`= color_bar(customYellow),#color_tile(customGreen, customGreen0),
                      `Disbursements ('000 USD)`= color_bar(customYellow),#color_tile(customGreen, customGreen0)
                      `Difference ('000 USD)`= improvement_formatter
                    ))
  
  output_name = paste0("fig2_recipient_table_",countryofinterest,".png")
  export_formattable(daa, output_name)
}






# datatype= "Commitments"
# countryofinterest = "Vietnam"
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
# output$Value = round((output$Value/1000), 2)
# 
# output = output %>% spread(flow_type, Value)
# output$Commitments[is.na(output$Commitments)] <- 0
# output$Disbursements[is.na(output$Disbursements)] <- 0
# 
# output = output[ , !(names(output) %in% "Recipient")]
# output$Difference = round((output$Disbursements-output$Commitments),2)
# 
# output = output %>% 
#   rename(
#     `Donor Name` = DonorName,
#     `Commitments ('000 USD)` = Commitments,
#     `Disbursements ('000 USD)` = Disbursements,
#     `Difference ('000 USD)` = Difference
#     )
# 
# customGreen0 = "#DeF7E9"
# customGreen = "#99cc00"#"#71CA97"
# customRed =  "#FF3232"#"#ff7f7f"
# customBlue = "#ccccff"
# customYellow = "#ffbf00"
# 
# # improvement_formatter <- 
# #   formatter("span", 
# #             style = x ~ style(
# #               font.weight = "bold", 
# #               color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))
# 
# improvement_formatter <- formatter("span", 
#                                    style = x ~ style(font.weight = "bold", 
#                                                      color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
#                                    x ~ icontext(ifelse(x>0, "arrow-up",  ifelse(x < 0, "arrow-down", "minus")), x)
# )
# # improvement_formatter <- formatter("span", 
# #                                    style = x ~ style(font.weight = "bold", 
# #                                                      color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
# #                                    x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
# # )
# 
# daa = formattable(output, 
#                   align =c("l","r", "r"),  # "c",
#                   list(`Indicator Name` = formatter(
#                     "span", style = ~ style(color = "grey",font.weight = "bold")),
#                     `Commitments ('000 USD)`= color_bar(customYellow),#color_tile(customGreen, customGreen0),
#                     `Disbursements ('000 USD)`= color_bar(customYellow),#color_tile(customGreen, customGreen0)
#                     `Difference ('000 USD)`= improvement_formatter
#                   ))
# 
# output_name = paste0("fig2_recipient_table_",countryofinterest,".png")
# export_formattable(daa, output_name)
# 
