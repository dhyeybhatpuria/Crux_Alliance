library(ggradar)
library(palmerpenguins)
library(tidyverse)
library(scales)
library(showtext)
font_add_google("Lobster Two", "lobstertwo")
font_add_google("Roboto", "roboto")
showtext_auto()
data("penguins", package = "palmerpenguins")
head(penguins, 3)


penguins_radar <- penguins %>%
  drop_na() %>%
  group_by(species) %>%
  summarise(
    avg_bill_length = mean(bill_length_mm),
    avg_bill_dept = mean(bill_depth_mm),
    avg_flipper_length = mean(flipper_length_mm),
    avg_body_mass = mean(body_mass_g)
  ) %>%
  ungroup() %>%
  mutate_at(vars(-species), rescale)
plt <- penguins_radar %>%
  ggradar(
    font.radar = "roboto",
    grid.label.size = 12,  # Affects the grid annotations (0%, 50%, etc.)
    axis.label.size = 8.5, # Afftects the names of the variables
    group.point.size = 3   # Simply the size of the point 
  )
plt <- plt + 
  labs(title = "Radar plot of penguins species") + 
  theme(
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.title.position = "plot", # slightly different from default
    plot.title = element_text(
      family = "lobstertwo", 
      size = 20,
      face = "bold", 
      color = "#2a475e"
    )
  )
plt




# Library
library(fmsb)

# Create data: note in High school for several students
set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

# Set graphic colors
library(RColorBrewer)
coul <- brewer.pal(4, Set1)
colors_border <- coul
library(scales)
colors_in <- alpha(coul,0.3)

# If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
radarchart( data[-c(1,2),]  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)
# Custom the radarChart !
radarchart(  data[-c(1,2),]  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

# Library
library(fmsb)
# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)

# Check your data, it has to look like this!
# head(data)

# Custom the radarChart !
radarchart( data  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)




# Add a legend
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


# Library
library(streamgraph)

# Create data:
data <- data.frame(
  year=rep(seq(1990,2016) , each=10),
  name=rep(letters[1:10] , 27),
  value=sample( seq(0,1,0.0001) , 270)
)

# Basic stream graph: just give the 3 arguments
pp <- streamgraph(data, key="name", value="value", date="year", height="300px", width="1000px")
pp 
library(dplyr)

ggplot2movies::movies %>%
  select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  tidyr::gather(genre, value, -year) %>%
  group_by(year, genre) %>%
  tally(wt=value) -> dat

streamgraph(dat, "genre", "n", "year", interactive=TRUE) %>%
  sg_axis_x(20, "year", "%Y") %>%
  sg_fill_brewer("PuOr")

library(ggradar)
df <- data.frame(matrix(runif(30), ncol = 10))
df[, 1] <- paste0("G", 1:3)
colnames(df) <- c("Group", paste("Var", 1:9))
ggradar(df)


require(ggplot2)
require(moonBook)
require(webr)
aa = acs
PieDonut(acs,aes(pies=Dx,donuts=smoking))


require(dplyr)
df=mtcars %>% group_by(gear,carb) %>% summarize(n=n())
PieDonut(df,aes(pies=gear,donuts=carb,count=n),ratioByGroup=FALSE)


hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))
hike_data$region <- as.factor(word(hike_data$location, 1, sep = " -- "))
hike_data$length_num <- as.numeric(sapply(strsplit(hike_data$length, " "), "[[", 1))
plot_df <- hike_data %>%
  group_by(region) %>%
  summarise(
    sum_length = sum(length_num),
    mean_gain = mean(as.numeric(gain)),
    n = n()
  ) %>%
  mutate(mean_gain = round(mean_gain, digits = 0))
plt <- ggplot(plot_df) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:3) * 1000),
    color = "lightgrey"
  ) + 
  # Add bars to represent the cumulative track lengths
  # str_wrap(region, 5) wraps the text so each line has at most 5 characters
  # (but it doesn't break long words!)
  geom_col(
    aes(
      x = reorder(str_wrap(region, 5), sum_length),
      y = sum_length,
      fill = n
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  
  # Add dots to represent the mean gain
  geom_point(
    aes(
      x = reorder(str_wrap(region, 5),sum_length),
      y = mean_gain
    ),
    size = 3,
    color = "gray12"
  ) +
  
  # Lollipop shaft for mean gain per region
  geom_segment(
    aes(
      x = reorder(str_wrap(region, 5), sum_length),
      y = 0,
      xend = reorder(str_wrap(region, 5), sum_length),
      yend = 3000
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  
  # Make it circular!
  coord_polar()

plt



rouded_integer <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6,round(n/1e3,2) * 1e3,  # in thousands
                          ifelse(n < 1e9, round(n/1e6,2) * 1e6,  # in millions
                                 ifelse(n < 1e12, round(n/1e9,2)* 1e9, # in billions
                                        ifelse(n < 1e15, round(n/1e12,2)* 1e12, # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}
adsw(max(att))
max(att)




library(ggplot2)

df <- structure(list(xvals = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                               1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                               2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 
                               4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 
                               5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 
                               6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 
                               7L, 7L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 9L, 
                               9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 10L, 10L, 10L, 
                               10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 11L, 11L, 11L, 
                               11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 12L, 12L, 12L, 
                               12L, 12L, 12L, 12L, 12L, 12L, 12L, 12L, 12L, 12L, 13L, 13L, 13L, 
                               13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 14L, 14L, 14L, 
                               14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 15L, 15L, 15L, 
                               15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 16L, 16L, 16L, 
                               16L, 16L, 16L, 16L, 16L, 16L, 16L, 16L, 16L, 16L, 17L, 17L, 17L, 
                               17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 18L, 18L, 18L, 
                               18L, 18L, 18L, 18L, 18L, 18L, 18L, 18L, 18L, 18L, 19L, 19L, 19L, 
                               19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 20L, 20L, 20L, 
                               20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 21L, 21L, 21L, 
                               21L, 21L, 21L, 21L, 21L, 21L, 21L, 21L, 21L, 21L, 22L, 22L, 22L, 
                               22L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 22L, 23L, 23L, 23L, 
                               23L, 23L, 23L, 23L, 23L, 23L, 23L, 23L, 23L, 23L), 
                     yvals = c(0.45, 
                               0, 0, 0.1, 0, 0.45, 0.5, 1, 0, 0, 0, 0, 0, 0.45, 0, 0.05, 0, 
                               0.2, 0.3, 0.5, 0, 1, 0, 0, 0, 0, 0.3, 0.15, 0.05, 0, 0, 0.5, 
                               0.5, 0, 1, 0, 0, 0, 0, 0.3, 0.15, 0.05, 0, 0, 0.5, 0.5, 0, 1, 
                               0, 0, 0, 0, 0.45, 0, 0.05, 0, 0.2, 0.3, 0.5, 0, 1, 0, 0, 0, 0, 
                               0.45, 0, 0.05, 0, 0.2, 0.3, 0.5, 0, 1, 0, 0, 0, 0, 0.45, 0, 0, 
                               0.1, 0, 0.45, 0.5, 0, 1, 0, 0, 0, 0, 0.45, 0, 0, 0.1, 0, 0.45, 
                               0.5, 0, 1, 0, 0, 0, 0, 0.3, 0.15, 0.05, 0, 0, 0.5, 0.5, 0, 0, 
                               1, 0, 0, 0, 0.15, 0.3, 0.05, 0, 0, 0.5, 0.5, 0, 0, 1, 0, 0, 0, 
                               0.45, 0, 0.05, 0, 0.2, 0.3, 0.5, 0, 0, 1, 0, 0, 0, 0.45, 0, 0, 
                               0.1, 0, 0.45, 0.5, 0, 0, 1, 0, 0, 0, 0.45, 0, 0.05, 0, 0.2, 0.3, 
                               0.5, 0, 0, 1, 0, 0, 0, 0.3, 0.15, 0.05, 0, 0, 0.5, 0.5, 0, 0, 
                               1, 0, 0, 0, 0.45, 0, 0, 0.1, 0, 0.45, 0.5, 0, 0, 1, 0, 0, 0, 
                               0.45, 0, 0, 0.1, 0, 0.45, 0.5, 0, 0, 0, 1, 0, 0, 0.45, 0, 0, 
                               0.1, 0, 0.45, 0.5, 0, 0, 0, 0, 1, 0, 0.45, 0, 0.05, 0, 0.2, 0.3, 
                               0.5, 0, 0, 0, 0, 0, 1, 0.3, 0.15, 0.05, 0, 0, 0.5, 0.5, 0, 0, 
                               0, 0, 0, 1, 0.45, 0, 0, 0.1, 0, 0.45, 0.5, 0, 0, 0, 0, 0, 1, 
                               0.45, 0, 0, 0.1, 0, 0.45, 0.5, 0, 0, 0, 0, 0, 1, 0.45, 0, 0, 
                               0.1, 0, 0.45, 0.5, 0, 0, 0, 0, 0, 1, 0.45, 0, 0, 0.1, 0, 0.45, 
                               0.5, 0, 0, 0, 0, 0, 1), 
                     cols = structure(c(13L, 12L, 11L, 10L, 
                                        9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 
                                        7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 
                                        5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 
                                        3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 
                                        1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 
                                        12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 
                                        11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 
                                        10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 
                                        9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 
                                        7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 
                                        5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 
                                        3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 
                                        1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 
                                        12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 
                                        11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 
                                        10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 
                                        9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 
                                        7L, 6L, 5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 
                                        5L, 4L, 3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 
                                        3L, 2L, 1L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 
                                        1L),
                                      .Label = c("Nesting Variable 6", "Nesting Variable 5", 
                                                 "Nesting Variable 4", 
                                                 "Nesting Variable 3", "Nesting Variable 2", "Nesting Variable 1", 
                                                 "blank", "mint", "green", "darkgray", "lightgray", "red", "pink"
                                      ), class = "factor")), 
                class = "data.frame", row.names = c(NA,-299L))
unique(df$cols)
ggplot(df, aes(xvals, yvals, fill = cols)) +
  geom_col(width = 1) +
  scale_y_continuous(limits = c(-2, 3)) +
  scale_fill_manual(values = rev(c("#e9cbc1", "#b54649", "gray90",
                                   "gray50", "#8ba55d", "#e2e4d6",
                                   "white", "#c3a891", "#37959d", 
                                   "#5c7890", "#dcad3c", "#55a3b9",
                                   "#f39068"))) +
  theme_void() +
  geom_vline(colour = "white", xintercept = c(0.5, 1.5, 8.5, 15.5, 16.5, 17.5),
             size = 3) +
  geom_segment(data = data.frame(x = 0.5 + 1:23, y = 0, yend = 1),
               aes(x = x, y = 0, yend = 1, xend = x), colour = "white",
               inherit.aes = FALSE) +
  scale_x_continuous(expand = c(0.2, 1)) +
  # coord_polar(start = -pi) +
  theme(legend.position = "none")
