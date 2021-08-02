

countryname = 'Switzerland'
if(!exists("data_filtering", mode="function")) source("base_functions.R")
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
# master$Recipient[master$Recipient == "China (People's Republic of)"] <- "China"
# master$Finance.Instrument[master$Finance.Instrument == "Mezzanine finance instrument"] <- "Mezzanine"
# master$Finance.Instrument[master$Finance.Instrument == "Equity and shares in collective investment vehicles"] <- "Equity & shares"


# master$Mitigation

master_selected = subset(master, master$Provider == countryname)

master_switz = master[, c('Provider', 'Recipient', 'Finance.Instrument','USD.thousand')]

# Creates Sankey Diagram for the data 2
# Developer: Dhyey Bhatpuria

wrangled_t <- aggregate(USD.thousand ~ Provider + Finance.Instrument, master_switz, sum)
wrangled_t1 <- aggregate(USD.thousand ~ Finance.Instrument + Recipient, master_switz, sum)

names(wrangled_t)[names(wrangled_t) == "Provider"] <- "source"
names(wrangled_t)[names(wrangled_t) == "Finance.Instrument"] <- "target"
names(wrangled_t)[names(wrangled_t) == "USD.thousand"] <- "value"
names(wrangled_t1)[names(wrangled_t1) == "Finance.Instrument"] <- "source"
names(wrangled_t1)[names(wrangled_t1) == "Recipient"] <- "target"
names(wrangled_t1)[names(wrangled_t1) == "USD.thousand"] <- "value"


total <- rbind(wrangled_t, wrangled_t1)

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
# ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# ColourScal = JS("d3.scaleOrdinal([`#383867`, `#584c77`, `#33431e`, `#a36629`, `#92462f`, `#b63e36`, `#b74a70`, `#946943`]);")
ColourScal ='d3.scaleOrdinal() 
.range(["#383867","#33431e","#92462f", "#584c77",
"#a36629", "#b63e36","#946943", "#b74a70"])'

# # txt <- links[, .(total = sum(links$value)), by=c('source')]
# txt <- links %>% group_by(source) %>% summarise(total = sum(value))
# txt$total = round((txt$total * 1000)/1e6,2) # Converting to millions after brining values from thousands as per original data
# nodes = as.data.table(nodes)
# nodes[txt$source+1L, name := paste0(name, ' (US$ ', txt$total, ' M)')]
# rm(txt)
# p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "target",
#                    Target = "source", Value = "value", NodeID = "name",
#                    colourScale=ColourScal,
#                    # colourScale = JS("d3.scaleOrdinal([`#383867`, `#584c77`, `#33431e`, `#a36629`, `#92462f`, `#b63e36`, `#b74a70`, `#946943`]);"),
#                    units = "USD", 
#                    fontSize = 16, nodeWidth = 30, nodePadding = 40, 
#                    fontFamily='Helvetica',# LinkGroup = "source",
#                    margin = list("left"=150))
# onRender(
#   p,
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


############################### below section makes gray flows to colored

# links$energy_type <- sub(' .*', '', nodes[links$source + 1, 'name'])
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

# txt$total = round((txt$total * 1000)/1e6,2) # Converting to millions after brining values from thousands as per original data
thedf$nodes = as.data.table(thedf$nodes)
thedf$nodes[txt$source+1L, name := paste0(name, ' (US$ ', txt$total, ')')]
rm(txt)
rm(data_new)

p_alpha <- sankeyNetwork(Links = thedf$links, Nodes = thedf$nodes, Source = "target",
                   Target = "source", Value = "value", NodeID = "name",
                   colourScale=ColourScal,
                   units = "USD", 
                   fontSize = 16, nodeWidth = 30, nodePadding = 40, 
                   fontFamily='Helvetica', LinkGroup = "energy_type",
                   margin = list("left"=150))

# now let's use the new htmlwidget function
#  onRender
onRender(
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
')

# #################### move leaf node text right ################
# # for this to work
# #   install the newest htmlwidgets
# #   devtools::install_github("ramnathv/htmlwidgets")
# 
# library(htmlwidgets)
# #  add margin left since we'll need extra room
# #   if you are wondering why margin left,
# #   I think we just discovered a bug
# sn <- sankeyNetwork(
#   Links=links, Nodes=nodes, Source='src', Target='target',
#   Value='value', NodeID='name', fontSize=16,
#   width=600, height=300,
#   # give us so room for our newly aligned labels
#   margin = list("left"=100)
# )
# # see how it looks
# sn
# 
# # now let's use the new htmlwidget function
# #  onRender
# onRender(
#   sn,
#   '
# function(el,x){
#   // select all our node text
#   var node_text = d3.select(el)
#     .selectAll(".node text")
#     //and make them match
#     //https://github.com/christophergandrud/networkD3/blob/master/inst/htmlwidgets/sankeyNetwork.js#L180-L181
#     .attr("x", 6 + x.options.nodeWidth)
#     .attr("text-anchor", "start");
# }
# '
# )
