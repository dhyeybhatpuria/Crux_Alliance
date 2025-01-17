
if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')

#just first time
# master <- select_2015_2019("CRDF-RP-2000-2019.xlsx")
# master <- data_filtering("crux_data_2015-2019.xlsx")
# master <- data_basic_cleaning("Search_terms_MTA_Crux_CT_R_version.csv")

master = read_excel("crux_2015-2019_master.xlsx")

countries = unique(master$Provider)
for (countryofinterest in countries) {
  print(countryofinterest)
  master_selected = subset(master, master$Provider == countryofinterest)
  
  master_switz = master_selected[, c('Provider', 'Sector','Finance.Instrument','USD.thousand')]
  
  # output <- master_selected %>% group_by(Finance.Instrument, Sector) %>% summarise(Commitment = sum(USD.thousand))
  
  wrangled_t <- aggregate(USD.thousand ~ Provider + Finance.Instrument, master_switz, sum)
  wrangled_t1 <- aggregate(USD.thousand ~ Finance.Instrument + Sector, master_switz, sum)
  
  names(wrangled_t)[names(wrangled_t) == "Provider"] <- "source"
  names(wrangled_t)[names(wrangled_t) == "Finance.Instrument"] <- "target"
  names(wrangled_t)[names(wrangled_t) == "USD.thousand"] <- "value"
  names(wrangled_t1)[names(wrangled_t1) == "Finance.Instrument"] <- "source"
  names(wrangled_t1)[names(wrangled_t1) == "Sector"] <- "target"
  names(wrangled_t1)[names(wrangled_t1) == "USD.thousand"] <- "value"
  
  total <- rbind(wrangled_t, wrangled_t1)
  # total <- wrangled_t
  
  regions <- unique(as.character(total$target))
  nodes_names <-unique(as.character(total$source))
  cc <- unique(c(regions,nodes_names))
  valuelen = length(cc)-1
  nodes <- data.frame(node = c(0:valuelen), 
                      name = c(cc))
  
  restructured <- merge(total, nodes, by.x = "source", by.y = "name")
  restructured <- merge(restructured, nodes, by.x = "target", by.y = "name")
  
  links <- restructured[ , c("node.x", "node.y", "value")]
  colnames(links) <- c("target", "source", "value")
  
  rm(wrangled_t1)
  rm(wrangled_t)
  rm(total)
  rm(restructured)
  rm(cc)
  rm(nodes_names)
  rm(regions)
  rm(valuelen)
  
  # prepare colour scale
  ColourScal ='d3.scaleOrdinal() 
.range(["#383867","#33431e","#92462f", "#584c77",
"#a36629", "#b63e36","#946943", "#b74a70"])'
  
  # # 'Provider', 'Sector', 'Finance.Instrument'
  # uni1=unique(master_switz$Sector) 
  # uni2=unique(master_switz$Provider)
  # uni3=unique(master_switz$Finance.Instrument) 
  # unia <- as.factor(c(uni1, uni2, uni3))
  
  # Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
  thedf = list(nodes,links)
  names(thedf) <- c("nodes", "links")
  thedf$links$energy_type <- sub(' .*', '', thedf$nodes[thedf$links$source + 1, 'name'])
  
  # txt <- links[, .(total = sum(links$value)), by=c('source')]
  txt <- thedf$links %>% group_by(source) %>% summarise(total = sum(value))
  txt$total = txt$total * 1000
  
  data_apply <- apply(txt[ , c(2)], 2, addUnits_neo)                         # Apply function to specific columns
  data_new <- txt                                                         # Replicate original data
  data_new[ , colnames(data_new) %in% colnames(data_apply)] <- data_apply  # Replace specific columns 
  txt = data_new
  
  thedf$nodes = as.data.table(thedf$nodes)
  thedf$nodes[txt$source+1L, name := paste0(name, ' (US$ ', txt$total, ')')]
  rm(txt)
  rm(data_new)
  rm(data_apply)
  
  p_alpha <- sankeyNetwork(Links = thedf$links, Nodes = thedf$nodes, Source = "target",
                           Target = "source", Value = "value", NodeID = "name",
                           colourScale=ColourScal,
                           units = "USD", sinksRight = FALSE,
                           fontSize = 16, nodeWidth = 20, nodePadding = 40, 
                           fontFamily='Helvetica', LinkGroup = "energy_type",
                           margin = list("left"=100))
  
  output_name = paste0("fig3_sankey_",countryofinterest,".png")
  tem_output_name = paste0("fig3_sankey_",countryofinterest,".html")
  
  saveNetwork((onRender(
    p_alpha,
    '
      function(el,x){
        // select all our node text
        var node_text = d3.select(el)
          .selectAll(".node text")
          //and make them match
          //https://github.com/christophergandrud/networkD3/blob/master/inst/htmlwidgets/sankeyNetwork.js#L180-L181
          .attr("x", 10 + x.options.nodeWidth)
          .attr("text-anchor", "start");
      }
')), tem_output_name)
  Sys.sleep(3)
  webshot(tem_output_name, output_name, vwidth = 1280, vheight = 800)
}





# # ####################################################################################################
# countryofinterest = 'Switzerland'
# if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
# #just first time
# # master <- select_2015_2019("CRDF-RP-2000-2019.xlsx")
# 
# master <- data_filtering("crux_data_2015-2019.xlsx")
# # master <- data_basic_cleaning("Search_terms_MTA_Crux_CT_R_version.csv")
# 
# master = read_excel("crux_2015-2019_master.xlsx")
# # write.xlsx(master, file = "crux.xlsx",sheetName = "newData", append = FALSE)
# # master = master %>%
# #   rename(
# #     Finance.Instrument =`Financial Instrument`,
# #     Overlap.Commitment = `Overlap - Commitment - 2019 USD thousand`,
# #     Adaptation = `Adaptation-related development finance - Commitment - 2019 USD thousand`,
# #     Mitigation = `Mitigation-related development finance - Commitment - 2019 USD thousand`,
# #     Sector = `Sector (detailed)`
# #   )
# 
# 
# master_selected = subset(master, master$Provider == countryofinterest)
# 
# master_switz = master_selected[, c('Provider', 'Sector', 'USD.thousand', 'Finance.Instrument')]
# 
# # output <- master_selected %>% group_by(Finance.Instrument, Sector) %>% summarise(Commitment = sum(USD.thousand))
# 
# wrangled_t <- aggregate(USD.thousand ~ Provider + Finance.Instrument, master_switz, sum)
# wrangled_t1 <- aggregate(USD.thousand ~ Finance.Instrument + Sector, master_switz, sum)
# 
# names(wrangled_t)[names(wrangled_t) == "Provider"] <- "source"
# names(wrangled_t)[names(wrangled_t) == "Finance.Instrument"] <- "target"
# names(wrangled_t)[names(wrangled_t) == "USD.thousand"] <- "value"
# names(wrangled_t1)[names(wrangled_t1) == "Finance.Instrument"] <- "source"
# names(wrangled_t1)[names(wrangled_t1) == "Sector"] <- "target"
# names(wrangled_t1)[names(wrangled_t1) == "USD.thousand"] <- "value"
# 
# 
# total <- rbind(wrangled_t, wrangled_t1)
# 
# # total <- wrangled_t
# 
# regions <- unique(as.character(total$target))
# nodes_names <-unique(as.character(total$source))
# cc <- unique(c(regions,nodes_names))
# valuelen = length(cc)-1
# nodes <- data.frame(node = c(0:valuelen),
#                     name = c(cc))
# 
# 
# restructured <- merge(total, nodes, by.x = "source", by.y = "name")
# 
# restructured <- merge(restructured, nodes, by.x = "target", by.y = "name")
# 
# links <- restructured[ , c("node.x", "node.y", "value")]
# colnames(links) <- c("target", "source", "value")
# 
# 
# rm(wrangled_t1)
# rm(wrangled_t)
# rm(total)
# rm(restructured)
# rm(cc)
# rm(nodes_names)
# rm(regions)
# rm(valuelen)
# 
# # prepare colour scale
# # ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
# # ColourScal = JS("d3.scaleOrdinal([`#383867`, `#584c77`, `#33431e`, `#a36629`, `#92462f`, `#b63e36`, `#b74a70`, `#946943`]);")
# 
# # ColourScal ='d3.scaleOrdinal() .domain(["Switzerland", "Grant (US$ 34.69 M)","Energy (US$ 14.98 M)", "Gen. Environment Protection (US$ 14.96 M)", "Other Multisector (US$ 4.76 M)"])
# # .range(["#383867", "#b74a70", "#33431e", "#a36629", "#92462f"])' #,"#b63e36", "#b74a70", "#946943:
# 
# # ColourScal ='d3.scaleOrdinal()
# # .range(["#383867", "#584c77", "#33431e", "#a36629", "#92462f","#b63e36", "#b74a70", "#946943])' #,"#b63e36", "#b74a70", "#946943
# # blue purple green orange brickish redish pink brown
# 
# ColourScal ='d3.scaleOrdinal()
# .range(["#383867","#33431e","#92462f", "#584c77",
# "#a36629", "#b63e36","#946943", "#b74a70"])'
# 
# # # 'Provider', 'Sector', 'Finance.Instrument'
# # uni1=unique(master_switz$Sector)
# # uni2=unique(master_switz$Provider)
# # uni3=unique(master_switz$Finance.Instrument)
# # unia <- as.factor(c(uni1, uni2, uni3))
# 
# # Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
# # nodes$group <- as.factor(c("my_unique_group"))
# 
# 
# 
# thedf = list(nodes,links)
# names(thedf) <- c("nodes", "links")
# thedf$links$energy_type <- sub(' .*', '', thedf$nodes[thedf$links$source + 1, 'name'])
# 
# # txt <- links[, .(total = sum(links$value)), by=c('source')]
# txt <- thedf$links %>% group_by(source) %>% summarise(total = sum(value))
# txt$total = txt$total * 1000
# 
# 
# data_apply <- apply(txt[ , c(2)], 2, addUnits_neo)                         # Apply function to specific columns
# data_new <- txt                                                         # Replicate original data
# data_new[ , colnames(data_new) %in% colnames(data_apply)] <- data_apply  # Replace specific columns
# txt = data_new
# 
# thedf$nodes = as.data.table(thedf$nodes)
# thedf$nodes[txt$source+1L, name := paste0(name, ' (US$ ', txt$total, ')')]
# rm(txt)
# rm(data_new)
# rm(data_apply)
# 
# p_alpha <- sankeyNetwork(Links = thedf$links, Nodes = thedf$nodes, Source = "target",
#                          Target = "source", Value = "value", NodeID = "name",
#                          colourScale=ColourScal,
#                          units = "USD", sinksRight = FALSE,
#                          fontSize = 16, nodeWidth = 20, nodePadding = 40,
#                          fontFamily='Helvetica', LinkGroup = "energy_type",
#                          margin = list("left"=100))
# 
# # now let's use the new htmlwidget function
# #  onRender
# onRender(
#   p_alpha,
#   '
# function(el,x){
#   // select all our node text
#   var node_text = d3.select(el)
#     .selectAll(".node text")
#     //and make them match
#     //https://github.com/christophergandrud/networkD3/blob/master/inst/htmlwidgets/sankeyNetwork.js#L180-L181
#     .attr("x", 10 + x.options.nodeWidth)
#     .attr("text-anchor", "start");
# }
# ')
# # 
# # saveNetwork(p_alpha, "sn.html")
# # 
# # webshot("sn.html", "simpleNetwork.png")
# 
# output_name = paste0("fig4_sankey_",countryofinterest,".png")
# tem_output_name = paste0("fig4_sankey_",countryofinterest,".html")
# 
# saveNetwork((onRender(
#   p_alpha,
#   '
#       function(el,x){
#         // select all our node text
#         var node_text = d3.select(el)
#           .selectAll(".node text")
#           //and make them match
#           //https://github.com/christophergandrud/networkD3/blob/master/inst/htmlwidgets/sankeyNetwork.js#L180-L181
#           .attr("x", 10 + x.options.nodeWidth)
#           .attr("text-anchor", "start");
#       }
# ')), tem_output_name)
# Sys.sleep(3)
# webshot(tem_output_name, output_name, vwidth = 1280, vheight = 800)
