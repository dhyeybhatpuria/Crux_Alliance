# https://stackoverflow.com/questions/27831384/ggplot2-stacked-bar-chart-labels-with-leader-lines

df <- data_merged
countries = unique(df$Recipient)
datatype= "Commitments"
countryofinterest = "Vietnam"


output<-df[(df$Recipient==countryofinterest),]
output<-output[(output$flow_type==datatype),]
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

title = paste("Total commitments and disbursements to",countryofinterest, "during 2015-2019")

output$Value = output$Value/1000000

ggplot(output, aes(x = Recipient, y = Value, fill = DonorName)) +
  geom_col() +
  geom_text(aes(label = round(Value, 2)), # paste0(round(Value, 2), "USD in millions")
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16) +
  ylab("USD in millions") + coord_flip() +
  xlab(NULL)



# Get label for places which has more than or less than 15 mil 
okd <- mutate(output, foo = ifelse(Value > 15, DonorName, NA))
notokd<- mutate(output, foo = ifelse(Value > 15, NA, DonorName))

#plot with gt than 15 mil labels
gg_a = ggplot(output, aes(x = Recipient, y = Value, fill = DonorName)) +
  geom_col() + scale_fill_brewer(palette = "Set2") + xlab(NULL) +
geom_text(aes(y = round(okd$Value, 2), label = round(okd$Value, 2))) +
theme_minimal(base_size = 16) + ylab("USD in millions") #+ coord_flip() 

#plot with gt than 15 mil labels
gg_b = ggplot(output, aes(x = Recipient, y = Value, fill = DonorName)) +
  geom_col() + scale_fill_brewer(palette = "Set2") + xlab(NULL) +
  geom_text(aes(y = round(notokd$Value, 2), label = round(notokd$Value, 2))) +
  theme_minimal(base_size = 16) + ylab("USD in millions")# + coord_flip() 



# Special label for right
dan <- (ggplot_build(gg_b)$data[[2]]) %>% drop_na(label) %>%
  # filter(!label %in% c("Belcarra", "Metro Vancouver-uninc")) %>%
  mutate(x = 1.35)

# Add labels
gg3 <- gg_a +
  annotate("text", x = dan$x, y = dan$y, label = dan$label, colour = "gray32", size = 3) #+
  # annotate("text", x = emo$x, y = emo$y, label = emo$label, colour = "gray32", size = 3) +
  # annotate("text", x = dan2$x, y = dan2$y, label = dan2$label, colour = "gray32", size = 3)


# Create data frames for segments
# right seg
r.seg <- data.frame(x = rep(1, times = 9),
                    xend = rep(1, times = 9),
                    y = dan$y,
                    yend = dan$y) 

# # left seg
# l.seg <- data.frame(x = rep(0.76, times = 3),
#                     xend = rep(0.8, times = 3),
#                     y = emo$y,
#                     yend = emo$y)
# # Anmore seg
# a.seg <- data.frame(x = 1.2,
#                     xend = 1.25,
#                     y = 1068312,
#                     yend = dan2$y)

# Draw the segments                                        
gg3 +
  annotate("segment", x = r.seg$x, xend = r.seg$xend, y = r.seg$y, yend = r.seg$yend) #+
  # annotate("segment", x = l.seg$x, xend = l.seg$xend, y = l.seg$y, yend = l.seg$yend) +
  # annotate("segment", x = a.seg$x, xend = a.seg$xend, y = a.seg$y, yend = a.seg$yend)
  # 
  # 
