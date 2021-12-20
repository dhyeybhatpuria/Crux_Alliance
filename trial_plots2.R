if(!exists("data_filtering", mode="function")) source("base_functions.R")


# countryname = 'Switzerland'
#just first time
# master <- select_2015_2019("CRDF-RP-2000-2019.xlsx")

master <- data_filtering("crux_data_2015-2019.xlsx")

# write.xlsx(master, file = "crux_2015-2019_db.xlsx",sheetName = "newData", append = FALSE)
master = master %>% 
  rename(
    Finance.Instrument =`Financial Instrument`,
    Overlap.Commitment = `Overlap - Commitment - 2019 USD thousand`,
    Adaptation = `Adaptation-related development finance - Commitment - 2019 USD thousand`,
    Mitigation = `Mitigation-related development finance - Commitment - 2019 USD thousand`,
    Sector = `Sector (detailed)`
  )

# master_selected = subset(master, master$Provider == countryname)

output <- master %>% group_by(Provider,Finance.Instrument) %>% summarise(Commitment = sum(USD.thousand)) # Year, 
output$Commitment <- output$Commitment / 1000

library(ggplot2)
output$Commitment <- output$Commitment * 1000
ggplot(output, aes(x = Provider, y = Commitment, fill = Finance.Instrument)) + 
  geom_bar(stat = "identity")+scale_fill_brewer()
  # scale_fill_manual(values = c("#DADAEB", "#9E9AC8", "#6A51A3"))



# o1 = output %>%
#   dplyr::count(Provider,Finance.Instrument, Commitment) %>%
#   tidyr::spread(key = Finance.Instrument,value = Commitment)
# o1 = output %>%
#   dplyr::count(Provider,Finance.Instrument, Commitment) %>%
#   tidyr::spread(key = Provider,value = Commitment)
# 
# o1[is.na(o1)] <- 0
# o1 = subset(o1, select = -c(n) )
# o1$Provider = as.factor(o1$Provider)
# a1 = o1 %>%
#   ungroup() %>%
#   mutate_at(vars(-Provider), rescale)
# 
# a1 = o1 %>%
#   ungroup() %>%
#   mutate_at(vars(-Finance.Instrument), rescale)
# 
# plt <- a1 %>%
#   ggradar(
#     font.radar = "roboto",
#     grid.label.size = 5,  # Affects the grid annotations (0%, 50%, etc.)
#     axis.label.size = 5, # Afftects the names of the variables
#     group.point.size = 2   # Simply the size of the point
#   )
# plt <- plt +
#   labs(title = "Radar plot") +
#   theme(
#     # plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
#     # panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
#     plot.title.position = "plot", # slightly different from default
#     plot.title = element_text(
#       family = "lobstertwo",
#       size = 8,
#       face = "bold",
#       color = "#2a475e"
#     )
#   )
# plt


# Library
library(fmsb)
# Set graphic colors
library(RColorBrewer)
coul <- sector.colors#brewer.pal(4, "Set1")
colors_border <- sector.colors #colors_border <- coul
library(scales)
colors_in <- alpha(coul,0.3)

o1 = output %>%
  dplyr::count(Provider,Finance.Instrument, Commitment) %>%
  tidyr::spread(key = Provider,value = Commitment)
o1[is.na(o1)] <- 0
o1 = subset(o1, select = -c(n) )
att = o1
att = att %>% remove_rownames %>% column_to_rownames(var="Finance.Instrument")


axislabels = seq(rounded_integer(min(att)),rounded_integer(max(att)),length.out = 5)


radarchart( att, axistype=1 , #maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, caxislabels=axislabels, axislabcol="black", cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend
# legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)




output$Year <-as.factor(output$Year)

streamgraph(output, key="Provider", value="Commitment", date="Year", height="300px", width="500px") 
# %>%
  # sg_axis_x(5, "Year", "%Y") 

# %>%
  # sg_fill_brewer("PuOr") 


require(webr)
PieDonut(output,aes(pies=Provider,donuts=Finance.Instrument,count=Commitment))
PieDonut(output,aes(pies=Provider,donuts=Finance.Instrument))



library(CGPfunctions)

newggslopegraph(dataframe = output,
                Times = Year,
                Measurement = Commitment,
                Grouping = Provider)
