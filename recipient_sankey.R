
if(!exists("data_filtering", mode="function")) source("base_functions.R")

datatype= "Disbursements" # Disbursements Commitments
countryofinterest = "Vietnam"

df <- donor_perspective_analysis("Bilateraldonors_2015-2019_NH.xlsx",donordb = TRUE)
output<-df[(df$Recipient==countryofinterest),]
# output = output[,!(names(output) %in% "Sector")]

output$flow_type = as.factor(as.character(output$flow_type))
output$Recipient = as.factor(as.character(output$Recipient))
output$Sector = as.factor(as.character(output$Sector))
output$DonorName = as.factor(as.character(output$DonorName))
output$Value = as.numeric(as.character(output$Value))
output$year = as.numeric(as.character(output$year))

output<-output[(output$flow_type==datatype),]
output = output[, c('DonorName', 'Recipient', 'Sector','Value')]

# Creates Sankey Diagram for the data 2
# Developer: Dhyey Bhatpuria

wrangled_t <- aggregate(Value ~ DonorName + Sector, output, sum)
wrangled_t1 <- aggregate(Value ~ Sector + Recipient, output, sum)

names(wrangled_t)[names(wrangled_t) == "DonorName"] <- "source"
names(wrangled_t)[names(wrangled_t) == "Sector"] <- "target"
names(wrangled_t)[names(wrangled_t) == "Value"] <- "value"
names(wrangled_t1)[names(wrangled_t1) == "Sector"] <- "source"
names(wrangled_t1)[names(wrangled_t1) == "Recipient"] <- "target"
names(wrangled_t1)[names(wrangled_t1) == "Value"] <- "value"

total <- rbind(wrangled_t, wrangled_t1)
rm(wrangled_t)
rm(wrangled_t1)

outsankey = create_sankey(total)
# now let's use the new htmlwidget function
#  onRender
onRender(
  outsankey,
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

