## ---------------------------
##
## Script name: crux_master_code
##
## Purpose of script: Main script to generate charts
##
## Author: Dhyey Bhatpuria
##
## Date Created: 2021-10-11
##
## Email: dhyey.bhatpuria@sei.org
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

if(!exists("data_filtering", mode="function")) source("functions_base_code.R", encoding = 'UTF-8')
# plotting functions. Please make a copy in case you are editing them
donor_fin_recp_sanky = function(countryofinterest){
  master_selected = subset(master, master$Provider == countryofinterest)
  master_switz = master_selected[, c('Provider', 'Recipient', 'Finance.Instrument','USD.thousand')]
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
  
  # txt$total = round((txt$total * 1000)/1e6,2) # Converting to millions after brining values from thousands as per original data
  thedf$nodes = as.data.table(thedf$nodes)
  thedf$nodes[txt$source+1L, name := paste0(name, ' (US$ ', txt$total, ')')]
  rm(txt)
  rm(data_new)
  
  p_alpha <- sankeyNetwork(Links = thedf$links, Nodes = thedf$nodes, Source = "target",
                           Target = "source", Value = "value", NodeID = "name",
                           colourScale=ColourScal,
                           units = "USD", 
                           fontSize = 16, nodeWidth = 20, nodePadding = 40, 
                           fontFamily='Helvetica', LinkGroup = "energy_type",
                           margin = list("left"=200))
  
  output_name = paste0("fig1_sankey_",countryofinterest,".png")
  tem_output_name = paste0("fig1_sankey_",countryofinterest,".html")
  
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
donor_yr_commit_recp_stack <- function(countryofinterest){
  master_selected = subset(master, master$Provider == countryofinterest)
  output <- master_selected %>% 
    group_by(Year, Recipient) %>% 
    summarise(Commitment = sum(USD.thousand))
  output$percent <- output$Commitment/sum(output$Commitment)
  output$Commitment <- output$Commitment * 1000
  titletext = paste("Year-wise commitment by", countryofinterest,"to recipient country (2015-2019)")
  plt = ggplot(output, aes(fill=Recipient, y=Commitment, x=Year)) + # 
    geom_bar(stat="identity") +
    scale_y_continuous(labels = addUnits)+
    geom_text(aes(label=ifelse(percent >= 0.01, scales::comma(Commitment, accuracy=0.1,big.mark = ",")," ")),
              position=position_stack(vjust=0.5), colour="black",size = 5) +
    labs(title = titletext, y="Commitments (USD)") +
    scale_fill_manual(values=country.colors)
  output_name_cumul = paste0("fig2_donor_commit2country_",countryofinterest,".png")
  png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
  print(plt + theme_Publication(base_size=14, base_family="helvetica"))
  Sys.sleep(2)
  dev.off()
}
donor_instrument_sector <- function(countryofinterest){
  master_selected = subset(master, master$Provider == countryofinterest)
  master_switz = master_selected[, c('Provider', 'Sector','Finance.Instrument','USD.thousand')]

  wrangled_t <- aggregate(USD.thousand ~ Provider + Finance.Instrument, master_switz, sum)
  wrangled_t1 <- aggregate(USD.thousand ~ Finance.Instrument + Sector, master_switz, sum)
  
  names(wrangled_t)[names(wrangled_t) == "Provider"] <- "source"
  names(wrangled_t)[names(wrangled_t) == "Finance.Instrument"] <- "target"
  names(wrangled_t)[names(wrangled_t) == "USD.thousand"] <- "value"
  names(wrangled_t1)[names(wrangled_t1) == "Finance.Instrument"] <- "source"
  names(wrangled_t1)[names(wrangled_t1) == "Sector"] <- "target"
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

  # prepare colour scale
  ColourScal ='d3.scaleOrdinal() 
.range(["#383867","#33431e","#92462f", "#584c77",
"#a36629", "#b63e36","#946943", "#b74a70"])'

  thedf = list(nodes,links)
  names(thedf) <- c("nodes", "links")
  thedf$links$energy_type <- sub(' .*', '', thedf$nodes[thedf$links$source + 1, 'name'])
  
  txt <- thedf$links %>% 
    group_by(source) %>% 
    summarise(total = sum(value))
  
  txt$total = txt$total * 1000
  
  data_apply <- apply(txt[ , c(2)], 2, addUnits_neo)
  data_new <- txt
  data_new[ , colnames(data_new) %in% colnames(data_apply)] <- data_apply
  txt = data_new
  
  thedf$nodes = as.data.table(thedf$nodes)
  thedf$nodes[txt$source+1L, name := paste0(name, ' (US$ ', txt$total, ')')]

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
donor_yr_commit_sector_stack<- function(countryofinterest){
  master_selected = subset(master, master$Provider == countryofinterest)

  output <- master_selected %>% group_by(Year, Sector) %>% summarise(Commitment = sum(USD.thousand))
  output$percent = output$Commitment/sum(output$Commitment)
  output$Commitment <- output$Commitment * 1000
  
  titletext = paste("Year-wise commitment by", countryofinterest ,"for each sector (2015-2019)")
  
  plt = ggplot(output, aes(fill=Sector, y=Commitment, x=Year)) + 
    geom_bar(stat="identity") +
    scale_y_continuous(labels = addUnits)+
    geom_text(aes(label=ifelse(percent >= 0.01, scales::comma(Commitment, accuracy=0.1,big.mark = ",")," ")),
              position=position_stack(vjust=0.5), colour="black",size = 5) +
    labs(title = titletext) +
    scale_fill_manual(values=sector.colors)

  output_name_cumul = paste0("fig4_donor_commit2sector_",countryofinterest,".png")
  png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
  print(plt + theme_Publication(base_size=14, base_family="helvetica"))
  Sys.sleep(3)  
  dev.off()
}
sector_fin_recp_sanky <- function(sectorname){
  master_selected = subset(master, master$Sector == sectorname)
  master_switz = master_selected[, c('Provider', 'Recipient', 'Finance.Instrument','USD.thousand')]
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
  ColourScal ='d3.scaleOrdinal() 
.range(["#383867","#33431e","#92462f", "#584c77",
"#a36629", "#b63e36","#946943", "#b74a70"])'
  
  ############################### below section makes gray flows to colored
  thedf = list(nodes,links)
  names(thedf) <- c("nodes", "links")
  thedf$links$energy_type <- sub(' .*', '', thedf$nodes[thedf$links$source + 1, 'name'])
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
                           units = "USD", sinksRight = FALSE,
                           fontSize = 16, nodeWidth = 30, nodePadding = 40, 
                           fontFamily='Helvetica', LinkGroup = "energy_type",
                           margin = list("left"=150))
  
  output_name = paste0("fig1_sector_sankey_",sectorname,".png")
  tem_output_name = paste0("fig1_sector_sankey_",sectorname,".html")
  
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
sector_yr_commit_recp_stack <- function(sectorname){
  master_selected = subset(master, master$Sector == sectorname)
  output <- master_selected %>% group_by(Year, Recipient) %>% summarise(Commitment = sum(USD.thousand)) 
  output$percent <- output$Commitment/sum(output$Commitment)
  output$Commitment <- output$Commitment * 1000
  
  titletext = paste("Year-wise commitment to", sectorname ,"(2015-2019)")
  
  plt = ggplot(output, aes(fill=Recipient, y=Commitment, x=Year, label = Commitment)) + 
    geom_bar(stat="identity") +
    scale_y_continuous(labels = addUnits_neo)+
    geom_text(aes(label=ifelse(percent >= 0.01, scales::comma(Commitment, accuracy=0.1,big.mark = ",")," ")),
              position=position_stack(vjust=0.5), colour="black",size = 5) +
    labs(title = titletext, y="Commitments (USD)")+
    scale_fill_manual(values=country.colors)
  
  
  output_name_cumul = paste0("fig2_sector_commitment_",sectorname,".png")
  png(output_name_cumul, width = 300, height = 200, units='mm', res = 300)
  print(plt + theme_Publication(base_size=14, base_family="helvetica"))
  Sys.sleep(3)
  
  dev.off()
}

master = read_excel("crux_2015-2019_master.xlsx")



##$$$$$$$$$$$$$$$$$$$$$$$ D O N O R S $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# LIST OF DONORS
# "United States" "United Kingdom" "AsDB" "France" "Korea" "Japan"
# "Germany" "EIB" "Canada" "GCF" "WB" "Switzerland" "Denmark" 

donor_fin_recp_sanky("Denmark")
donor_yr_commit_recp_stack("Denmark")
donor_instrument_sector("Denmark")
donor_yr_commit_sector_stack("Denmark")

donor_fin_recp_sanky("United Kingdom")
donor_yr_commit_recp_stack("United Kingdom")
donor_instrument_sector("United Kingdom")
donor_yr_commit_sector_stack("United Kingdom")

##$$$$$$$$$$$$$$$$$$$$$$$ S E C T O R S $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# LIST OF SECTORS
# "Gen. Environment Protection" "Energy" "Other Multisector" 
# "Industry, Mining, Construction" "Transport and Storage" "Industry"

sector_fin_recp_sanky("Energy")
sector_yr_commit_recp_stack("Energy")

sector_fin_recp_sanky("Industry, Mining, Construction")
sector_yr_commit_recp_stack("Industry, Mining, Construction")




