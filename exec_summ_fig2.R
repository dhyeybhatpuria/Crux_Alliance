

if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
master = read_excel("crux_2015-2019_master.xlsx")
# countries = unique(master$Recipient)
output <- master %>% group_by(Finance.Instrument) %>% summarise(Commitment = sum(USD.thousand)) %>% arrange(desc(Commitment)) #Year, 
output$Commitment <- output$Commitment * 1000

datavector <- setNames(as.numeric(output$Commitment), output$Finance.Instrument)

# Waffle chart
library(waffle)
waff <- waffle((datavector/30000000), rows = 15, size=0.5,
               xlab="1 square is USD 30 Million",legend_pos = "bottom", colors= alloc.colors)

fontsize =14
waff + theme(text = element_text(size=fontsize),axis.title.x = element_text(size=fontsize),
  legend.key = element_rect(size =fontsize, colour = NA),
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.key.size= unit(0.6, "cm"),
  legend.margin = unit(0, "cm"),
  legend.title = element_text(size = fontsize, face="bold"), 
  legend.text=element_text(size=fontsize))

output_name_cumul = paste0("fig2_finance_instruments.png")
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






# 
# parts <- c(One=80, Two=30, Three=20, Four=10)
# chart <- waffle(parts, rows=8,use_glyph="shield")
# print(chart)
# 
# library(waffle)
# savings <- c(`Mortgage ($84,911)`=84911, `Auto andntuition loans ($14,414)`=14414, 
#              `Home equity loans ($10,062)`=10062, `Credit Cards ($8,565)`=8565)
# waffle(savings/392, rows=7, size=0.5, 
#        colors=c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"), 
#        title="Average Household Savings Each Year", 
#        xlab="1 square == $392")