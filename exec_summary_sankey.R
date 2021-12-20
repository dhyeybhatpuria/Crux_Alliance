
if(!exists("data_filtering", mode="function")) source("base_functions.R", encoding = 'UTF-8')
#just first time
# master <- select_2015_2019("CRDF-RP-2000-2019.xlsx")

master = read_excel("crux_2015-2019_master.xlsx")
# countryofinterest = 'Switzerland'
# master <- data_filtering("crux_data_2015-2019.xlsx")
# master <- data_basic_cleaning("Search_terms_MTA_Crux_CT_R_version.csv")

master_switz = master[, c('Provider', 'Recipient', 'Finance.Instrument','USD.thousand')]
# Creates Sankey Diagram for the data 2
# Developer: Dhyey Bhatpuria

wrangled_t <- aggregate(USD.thousand ~ Provider + Recipient, master_switz, sum)
wrangled_t1 <- aggregate(USD.thousand ~ Finance.Instrument + Recipient, master_switz, sum)
names(wrangled_t)[names(wrangled_t) == "Provider"] <- "source"
names(wrangled_t)[names(wrangled_t) == "Recipient"] <- "target"
names(wrangled_t)[names(wrangled_t) == "USD.thousand"] <- "value"
names(wrangled_t1)[names(wrangled_t1) == "Finance.Instrument"] <- "source"
names(wrangled_t1)[names(wrangled_t1) == "Recipient"] <- "target"
names(wrangled_t1)[names(wrangled_t1) == "USD.thousand"] <- "value"
total <- wrangled_t# rbind(wrangled_t, wrangled_t1)
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


############################### below section makes gray flows to colored

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


txt1 <- thedf$links %>% group_by(target) %>% summarise(total = sum(value))
txt1$total = txt1$total * 1000
data_apply <- apply(txt1[ , c(2)], 2, addUnits_neo)                         # Apply function to specific columns
data_new <- txt1                                                         # Replicate original data
data_new[ , colnames(data_new) %in% colnames(data_apply)] <- data_apply  # Replace specific columns 
txt1 = data_new
# txt_aa = rbind(txt,txt1)

# txt$total = round((txt$total * 1000)/1e6,2) # Converting to millions after brining values from thousands as per original data
thedf$nodes = as.data.table(thedf$nodes)
thedf$nodes[txt$source+1L, name := paste0(name, ' (US$ ', txt$total, ')')]
thedf$nodes[txt1$target+1L, name := paste0(name, ' (US$ ', txt1$total, ')')]
thedf$nodes[txt1$target+1L, name := " "]
rm(txt)

rm(data_new)

p_alpha <- sankeyNetwork(Links = thedf$links, Nodes = thedf$nodes, Source = "target",
                         Target = "source", Value = "value", NodeID = "name",
                         colourScale=ColourScal,
                         units = "USD", 
                         fontSize = 16, nodeWidth = 20, nodePadding = 40, 
                         fontFamily='Helvetica', LinkGroup = "energy_type",
                         margin = list("left"=200))

output_name = paste0("Exc_summry_fig1_sankey_1.png")
tem_output_name = paste0("Exc_summry_fig1_sankey_1.html")

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
