if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
master = read_excel("crux_2015-2019_master.xlsx")

# countries = unique(master$Recipient)
# In case only one type of finance
output <- master %>% group_by(Sector) %>% summarise(Commitment = sum(USD.thousand)) %>% arrange(desc(Commitment))#Year, 
output$Commitment <- output$Commitment * 1000

# Load ggplot2
library(ggplot2)
library(forcats)
# aa = output %>%
#   mutate(name = fct_reorder(Sector, Commitment)) %>%
#   ggplot(aes(fill=Sector, x=name, y=Commitment)) + 
#   geom_bar(stat = "identity") +
#   coord_flip()+ scale_fill_manual(values=sector.colors) + scale_y_continuous(labels = addUnits)#+
#   # labs(title = titletext)
# 
# aa + theme_Publication(base_size=14, base_family="helvetica")
# 

aa = output %>%
  mutate(name = fct_reorder(Sector, Commitment)) %>%
  ggplot(aes(x=name, y=Commitment)) + 
  geom_bar(stat = "identity", col= "#f58231", fill ="#f58231") +
  coord_flip() + scale_y_continuous(labels = addUnits)#+
# labs(title = titletext)

# # aa #+ theme_Publication(base_size=14, base_family="helvetica")
# aa + theme_classic(base_size = 14)+ theme(
#   legend.position = "",panel.border = element_rect(colour = 'black',fill = NA),
#   axis.title.y = element_blank()#element_text(size=fontsize)
# )
# fontsize = 16
# aa + theme(text = element_text(size=fontsize),
#            axis.title.x = element_text(size=fontsize),
#            panel.background = element_blank(),
#            panel.border = element_rect(colour = 'gray80'),
#            plot.background = element_blank(),
#            panel.grid.major =element_blank(),
#            panel.grid.minor = element_blank(),
#            panel.grid.minor.y=element_blank(),
#            panel.grid.major.y=element_blank(),
#            legend.key = element_rect(size = fontsize, colour = NA),
#            legend.position = "",
#            legend.direction = "horizontal",
#            legend.key.size= unit(0.6, "cm"),
#            # legend.margin = unit(0, "cm"),
#            legend.title = element_text(size = fontsize, face="bold"), 
#            legend.text=element_text(size=fontsize))


output_name_cumul = paste0("fig3_sector_wise.png")
png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
print(# aa #+ theme_Publication(base_size=14, base_family="helvetica")
  aa + theme_classic(base_size = 16)+ theme(
    legend.position = "",panel.border = element_rect(colour = 'gray80',fill = NA),
    axis.title.y = element_blank()#element_text(size=fontsize)
  )) # scale_colour_Publication() +
Sys.sleep(3)  
dev.off()


