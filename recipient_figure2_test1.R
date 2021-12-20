library(dplyr) # I use the dev version (dplyr 0.4)
library(ggplot2)
library(tidyverse)

# as_data_frame() is available in dplyr 0.4
mydf <- as_data_frame(list(Regional.District = rep("Metro", times = 22),
                           Municipality = c("Bowen Island", "Coquitlam", "Delta",
                                            "Langley City", "Maple Ridge", "New West",
                                            "North Vancouver (City)", "Port Coquitlam", "Port Moody",
                                            "Surrey", "West Vancouver", "White Rock",
                                            "Anmore", "Belcarra", "Burnaby", "Langley (Town)",
                                            "Lions Bay", "Metro Vancouver-uninc",
                                            "North Vancouver (District)", "Pitt Meadows",
                                            "Richmond", "Vancouver (City)"),
                           Population = c(3678, 126594, 100000, 25858, 76418, 66892, 50725,
                                          57431, 33933, 462345, 44058, 19278, 2203, 690,
                                          227389, 104697, 1395, 24837, 88370, 18136, 196858,
                                          642843),
                           mp = c(1839.0, 66975.0, 180272.0, 243201.0, 294339.0, 365994.0,
                                  424802.5, 478880.5, 524562.5, 772701.5, 1025903.0, 1057571.0,
                                  1068311.5, 1069758.0, 1183797.5, 1349840.5, 1402886.5, 1416002.5,
                                  1472606.0, 1525859.0, 1633356.0, 2053206.5)))


# Get label for places which has more than or less than 60,000 people

ana <- mutate(mydf, foo = ifelse(Population > 60000, Municipality, NA))
bob <- mutate(mydf, foo = ifelse(Population > 60000, NA, Municipality))


# Plot with places which have more than 60,000 people
gg1 <- ggplot(mydf, aes(x = Regional.District, y = Population, fill = Municipality)) +
  geom_bar(stat = "identity", colour = "gray32", width = 0.4, show_guide = FALSE) +
  geom_text(aes(y = ana$mp, label = ana$foo), colour = "gray32", size = 3)

# Plot with places which have less than 60,000 people
gg2 <- ggplot(mydf, aes(x = Regional.District, y = Population, fill = Municipality)) +
  geom_bar(stat = "identity", colour = "gray32", width = 0.4, show_guide = FALSE) +
  geom_text(aes(y = bob$mp, label = bob$foo), colour = "gray32")

# # Label for right
# dan <- (ggplot_build(gg2)$data[[2]]) %>%
#   filter(!label %in% c("Belcarra", "Metro Vancouver-uninc", "Anmore")) %>%
#   mutate(x = 1.35)

# Label for left
emo <- filter(ggplot_build(gg2)$data[[2]],
              label %in% c("Belcarra", "Metro Vancouver-uninc",  "Anmore")) %>%
  mutate(x = 0.65)

# Label for left
emo1 <- filter(ggplot_build(gg2)$data[[2]],
              label %in% c("Belcarra", "Metro Vancouver-uninc",  "Anmore")) %>% 
  (x = 0.65)

   # Special label for right
dan <- (ggplot_build(gg2)$data[[2]]) %>% drop_na(label) %>%
  filter(!label %in% c("Belcarra", "Metro Vancouver-uninc")) %>%
  mutate(x = 1.35)

# Add labels
gg3 <- gg1 +
  annotate("text", x = dan$x, y = dan$y, label = dan$label, colour = "gray32", size = 3) +
  annotate("text", x = emo$x, y = emo$y, label = emo$label, colour = "gray32", size = 3) +
  annotate("text", x = dan2$x, y = dan2$y, label = dan2$label, colour = "gray32", size = 3)


# Create data frames for segments
# right seg
r.seg <- data.frame(x = rep(1.2, times = 9),
                    xend = rep(1.25, times = 9),
                    y = dan$y,
                    yend = dan$y) 

# left seg
l.seg <- data.frame(x = rep(0.76, times = 3),
                    xend = rep(0.8, times = 3),
                    y = emo$y,
                    yend = emo$y)

# Anmore seg
a.seg <- data.frame(x = 1.2,
                    xend = 1.25,
                    y = 1068312,
                    yend = dan2$y)

# Draw the segments                                        
gg3 +
  annotate("segment", x = r.seg$x, xend = r.seg$xend, y = r.seg$y, yend = r.seg$yend) +
  annotate("segment", x = l.seg$x, xend = l.seg$xend, y = l.seg$y, yend = l.seg$yend) +
  annotate("segment", x = a.seg$x, xend = a.seg$xend, y = a.seg$y, yend = a.seg$yend) 
