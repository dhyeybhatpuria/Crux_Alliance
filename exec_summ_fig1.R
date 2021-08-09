
require("ggrepel")
library(treemap)

# https://yjunechoe.github.io/posts/2020-06-30-treemap-with-ggplot/

if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
master = read_excel("crux_2015-2019_master.xlsx")
# countries = unique(master$Recipient)
# In case only one type of finance
output <- master %>% group_by(Provider) %>% summarise(Commitment = sum(USD.thousand)) #Year, 
output$Commitment <- output$Commitment * 1000

output$Provider[output$Provider == "AsDB"] <- "Asian Development Bank"
output$Provider[output$Provider == "WB"] <- "World Bank"

countrycolors <- data.frame(donor_count_multilat_colors)
countrycolors$Provider <- row.names(countrycolors) 
newout = merge(output, countrycolors, by="Provider")
# newout$donor_count_multilat_colors

tm_dh <- treemap(
  dtf = newout,
  index = "Provider",
  vSize = "Commitment",
  vColor = "donor_count_multilat_colors",
  type = 'color' # {treemap}'s equivalent of scale_fill_identity()
)
# head(tm_dh$tm)


tm_plot_data_dh <- tm_dh$tm %>% 
  # calculate end coordinates with height and width
  mutate(x1 = x0 + w,
         y1 = y0 + h) %>% 
  # get center coordinates for labels
  mutate(x = (x0+x1)/2,
         y = (y0+y1)/2) %>% 
  # mark primary groupings and set boundary thickness
  mutate(primary_group = ifelse(is.na(Provider), 1.2, .5)) %>% 
# remove colors from primary groupings (since secondary is already colored)
mutate(color = ifelse(is.na(Provider), NA, color))
# 
# tm_plot_data_dh %>% 
#   ggplot(aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
#   geom_rect(aes(fill = color, size = primary_group),
#             show.legend = FALSE, color = "black", alpha = 0.76) +
#   scale_fill_identity() + #scale_fill_manual(values=donor_count_multilat_colors)+
#   scale_size(range = range(tm_plot_data_dh$primary_group)) +
#   ggfittext::geom_fit_text(data = filter(tm_plot_data_dh, vSize > 21495800),
#                            aes(label = paste(Provider, round(vSize,2), sep = "\n")), min.size = 1) +
#   # pick out observations of interest and annotate with geom_text_repel
#   ggrepel::geom_text_repel(
#     data = filter(tm_plot_data_dh, vSize < 21495800), #%>%
#     # inner_join(output, by="Provider"),
#     aes(x = x, y = y, label = paste(Provider, round(vSize,2), sep = "\n")),
#     color = "black", xlim = c(1.0, NA), size = 3.5,
#     direction = "y", vjust = 0.7, force = 25
#   ) +
#   # expand x-axis limits to make room for test annotations
#   scale_x_continuous(limits = c(0, 1.2), expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0.1, 0.2)) + theme_void() + theme(plot.margin=unit(c(1,1,1.2,1.2),"cm"))
# 

savfile = tm_plot_data_dh %>% 
  ggplot(aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
  geom_rect(aes(fill = color, size = primary_group),
            show.legend = FALSE, color = "black", alpha = 0.76) +
  scale_fill_identity() + #scale_fill_manual(values=donor_count_multilat_colors)+
  scale_size(range = range(tm_plot_data_dh$primary_group)) +
  ggfittext::geom_fit_text(data = filter(tm_plot_data_dh, vSize > 21495800),
                           aes(label = paste(Provider, prettyNum(round(vSize,2),big.mark=",",scientific=FALSE), sep = "\n"),
                               min.size = 1),colour="white") +
  # pick out observations of interest and annotate with geom_text_repel
  ggrepel::geom_text_repel(
    data = filter(tm_plot_data_dh, vSize < 21495800), #%>%
    # inner_join(output, by="Provider"),
    aes(x = x, y = y, label = paste(Provider, prettyNum(round(vSize,2),big.mark=",",scientific=FALSE), sep = "\n")),
    color = "black", xlim = c(1.0, NA), size = 3.5,
    direction = "y", vjust = 0.7, force = 25
  ) +
  # expand x-axis limits to make room for test annotations
  scale_x_continuous(limits = c(0, 1.2), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.1, 0.2)) + theme_void() + theme(plot.margin=unit(c(1,1,1.2,1.2),"cm"))

output_name_cumul = paste0("fig1_donor_commitments.png")
png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
  print(savfile) # scale_colour_Publication() +
Sys.sleep(3)  
dev.off()


