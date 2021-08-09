
if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
master = read_excel("crux_2015-2019_master.xlsx")

# countries = unique(master$Recipient)
# In case only one type of finance
output <- master %>% group_by(Recipient) %>% summarise(Commitment = sum(USD.thousand)) %>% arrange(desc(Commitment))#Year, 
output$Commitment <- output$Commitment * 1000


datavector <- setNames(as.numeric(output$Commitment), output$Recipient)

# Waffle chart
library(waffle)
waff <- waffle((datavector/10000000), rows = 20, size=0.5,
               xlab="1 square is USD 20 Million",legend_pos = "bottom", colors= country.colors)

fontsize =14
# waff + theme(text = element_text(size=fontsize),axis.title.x = element_text(size=fontsize),
#              legend.key = element_rect(size =fontsize, colour = NA),
#              legend.position = "bottom",
#              legend.direction = "horizontal",
#              legend.key.size= unit(0.6, "cm"),
#              legend.margin = unit(0, "cm"),
#              legend.title = element_text(size = fontsize, face="bold"), 
#              legend.text=element_text(size=fontsize)) 

# waffle((datavector/20000000), rows = 20, size=0.5,
       # xlab="1 square is USD 20 Million",legend_pos = "bottom")

output_name_cumul = paste0("fig4_recipient_wise.png")
png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
  print(waff + theme(text = element_text(size=fontsize),axis.title.x = element_text(size=fontsize),
                     legend.key = element_rect(size =fontsize, colour = NA),
                     legend.position = "bottom",
                     legend.direction = "horizontal",
                     legend.key.size= unit(0.6, "cm"),
                     legend.margin = unit(0, "cm"),
                     legend.title = element_text(size = fontsize, face="bold"), 
                     legend.text=element_text(size=fontsize))) # scale_colour_Publication() +
Sys.sleep(3)  
dev.off()
