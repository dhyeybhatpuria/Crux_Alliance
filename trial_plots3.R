
if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
master = read_excel("crux_2015-2019_master.xlsx")
# countries = unique(master$Recipient)
# In case only one type of finance
output <- master %>% group_by(Finance.Instrument) %>% summarise(Commitment = sum(USD.thousand)) %>% arrange(desc(Commitment)) #Year, 
output$Commitment <- output$Commitment * 1000
output$ex = 1

total = sum(output$Commitment)
output$pct = (output$Commitment * 100) / sum(output$Commitment)


# Waffle chart
library(waffle)
waffle(output$Commitment/30000,rows = 15)

parts <- c(One=80, Two=30, Three=20, Four=10)
chart <- waffle(parts, rows=8,use_glyph="shield")
print(chart)

library('reshape2')
melted <- melt(output, id.vars=c("Finance.Instrument"))
melted <- within(melted, {
  name <- factor(Finance.Instrument, levels = names(sort(tapply(value, Finance.Instrument, sum))))
})
ggplot(melted, aes(x= name, y = value, fill = variable, order = variable)) + 
  geom_bar(position="fill",stat = "identity") +
  xlab("") + coord_flip()



ggplot(output, aes(fill=Finance.Instrument, y=Commitment, x=ex,order = Commitment)) + 
  geom_bar(position="fill", stat="identity") +
  # theme_ipsum() +
  xlab("") + coord_flip()





library(plotly)
#creating pie chart
plot1 <- plot_ly(output) %>%
  add_pie(data, labels = ~`Finance.Instrument`, values = ~`Commitment`, 
          domain = list(
            x = c(0.15, 0.85),
            y = c(0.15, 0.85)),
          sort = F)


#printing plot
plot1.update_layout(
  font_family="Courier New",
  font_color="blue",
  title_font_family="Times New Roman",
  title_font_color="red",
  legend_title_font_color="green"
)
plot1.show()



fig = px.scatter(df, x="sepal_length", y="sepal_width", color="species",
                 title="Playing with Fonts")
fig.update_layout(
  font_family="Courier New",
  font_color="blue",
  title_font_family="Times New Roman",
  title_font_color="red",
  legend_title_font_color="green"
)
fig.update_xaxes(title_font_family="Arial")
fig.show()





# Load ggplot2
library(ggplot2)
library(dplyr)

# Create Data
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# Compute the position of labels
data <- output %>% 
  arrange(desc(Finance.Instrument)) %>%
  mutate(prop = Commitment / sum(output$Commitment) * 100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=Commitment)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = Finance.Instrument), color = "white", size=6) #+
  # scale_fill_brewer(palette="Set1")


# Compute the position of labels
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

output$Finance.Instrument

ggplot(output, aes("", Commitment, fill = Finance.Instrument)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(pct), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "market share") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#ffd700", "#bcbcbc", "#ffa500", "#254290")) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))



require(moonBook)
require(webr)
aa = PieDonut(output,aes(Finance.Instrument,count=Commitment),r0=0.6,explode=c(2,4),color = "white",
         showPieName = FALSE,showDonutName = T,explodeDonut=TRUE,start=3*pi/2,labelposition=2, 
         addPieLabel = F)
aa + scale_fill_manual(values=alloc.colors)

# load library
library(ggplot2)

# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")




