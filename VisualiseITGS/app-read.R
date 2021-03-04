
library(shiny)
library(data.table)
library(fst)
library(igraph)
library(countrycode)
library(networkD3)
library(feather)
library(fst)
library(shinyWidgets)

load("../../years.Rdata")
load("../../centroids.Rdata")
load("../../coords.Rdata")
load("../../unique_countries.Rdata")
load("../../unique_goods.Rdata")

#load("unique_goods.Rdata")
goods <- unique_goods
countries <- unique_countries

names(goods) <- goods

ld <- fread("~/EU_BD_hackathon_2021/hulpdata/labels_and_descriptions.csv")
ld <- ld[!is.na(ld$HS6_float),]
ld$HS6_str <- as.character(ld$HS6_float)

goods_with_names <- ld$HS6_float
names(goods_with_names) <- ld$`Self-explanatory texts in English`

#goods_with_names
goods <- append(goods, goods_with_names)

itgs_list <- vector(mode="list", length=length(years))
names(itgs_list) <- years

for (year in years) {
  load(paste("../../itgs_", year, ".Rdata", sep=""))
  itgs_list[[year]] <- itgs
}

#itgs <- ("../../itgs_2018.feather")
#load("../../itgs_2018.Rdata")

#goods <- unique_goods
#countries <- unique_countries

noneu <- c("Asia", "Americas", "Africa", "Europe", "Oceania")

# Read ITGS
# dir_itgs <- "~/data"

# itgs <- fread(file.path(dir_itgs, "itgs_reference_2019_short.csv"))
# setnames(itgs, tolower(names(itgs)))
# # Combine non-eu countries into continents
# codes <- unique(unique(itgs$destin), unique(itgs$origin) )
# codes_eu <- codes[unique(codes) %in% centroids$nuts_id]
# itgs[, origin_eu := ifelse(origin %in% codes_eu, origin,
#                            countrycode(origin, origin="iso2c", destination = 'continent'))]
# itgs[, destin_eu := ifelse(destin %in% codes_eu, destin,
#                            countrycode(destin, origin="iso2c", destination = 'continent'))]
# itgs[, consign_eu := ifelse(consign %in% codes_eu, consign,
#                             countrycode(consign, origin="iso2c", destination = 'continent'))]

#save(itgs, file="temp.Rdata")
#load("temp.Rdata")

itgs <- itgs_list$`2019`

# UI ----------------------------------------------------------------------


ui <- fluidPage(

  titlePanel("ITGS"),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Client side filtering in selectize
      #selectizeInput("product", "Good/service", choices = unique_goods, 
      #  selected = NULL, multiple = FALSE, options = NULL),
      # Server side filtering in selectize
      sliderTextInput("year", "Year:",
                  choices=years,
                  hide_min_max=TRUE,
                  grid=TRUE),
      selectizeInput("product", "Good/service", choices = NULL, 
        selected = NULL, multiple = TRUE, options = NULL),
      selectizeInput("country", "Country", choices = countries, 
        selected = NULL, multiple = TRUE, options = NULL),
      actionButton("update", "Update"),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("network", height = "800px"),
      sankeyNetworkOutput("alluvial", height = "800px")
    )
  )
)






# SERVER ------------------------------------------------------------------



server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'product', choices = goods, server = TRUE)
  
  filter_country <- reactive({
    str_year <- as.character(input$year)
    #print(str_year)
    itgs <- itgs_list[[str_year]]
    countries <- input$country
    #print(itgs$origin_eu)
    sel <- if (is.null(countries) | "<ALL>" %in% countries) TRUE else 
      itgs$origin_eu %in% countries | itgs$consign_eu %in% countries |
      itgs$destin_eu %in% countries
    #print(sel)
    flow <- itgs[sel == TRUE]
    #print(flow$origin_eu)
    # Remove flow between non eu
    noneu <- c("Asia", "Americas", "Africa", "Europe", "Oceania")
    flow <- flow[!(origin_eu %in% noneu & consign_eu %in% noneu & 
        destin_eu %in% noneu)]
    #print(flow)
    flow
  })
  
  # filter_product <- reactive({
  #   flow <- filter_country()
  #   product <- input$product
  #   if (is.null(product)) {
  #     flow[, .(value = sum(obs_value)),
  #       by = .(origin_eu, consign_eu, destin_eu)]
  #   } else {
  #     flow[hs6 == product, .(value = sum(obs_value)),
  #       by = .(origin_eu, consign_eu, destin_eu)]
  #   }
  # })
  
  filter_product <- eventReactive(input$update, {
    flow <- filter_country()
    product <- input$product
    product_list <- c()
    for (p in product){
      sub_products <- ld[startsWith(ld$HS6_str, p)]$HS6_str
      product_list <- append(product_list, sub_products)
    }
    if (is.null(product)) {
      flow[, .(value = sum(obs_value)),
           by = .(origin_eu, consign_eu, destin_eu)]
    } else {
      flow[hs6 %in% product_list, .(value = sum(obs_value)),
           by = .(origin_eu, consign_eu, destin_eu)]
    }
  })
  
  network <- reactive({
    flow <- filter_product()
    flow <- flow[, .(weight = sum(value)), by = .(origin_eu, destin_eu)]
    flow <- flow[complete.cases(flow)]
    setnames(flow, c("src", "dst", "weight"))
    flow <- flow[!(src %in% noneu | dst %in% noneu)]
    flow
  })
  
  
  
  output$network <- renderPlot({
    # Plot network
    dta <- network()
    g <- graph_from_data_frame(dta, directed = TRUE, vertices = centroids)
    par(mar = c(0,0,0,0), bg = "#555555")
    plot(centroids$x, centroids$y, asp = 1, type = 'n', xlab = "", ylab = "", 
         xaxt = 'n', yaxt = 'n', bty = 'n')
    plot(g, coords = coords, rescale = FALSE, 
         add = TRUE, edge.width = 25*(E(g)$weight/max(E(g)$weight)), 
         edge.color = "#FFFFFF50", edge.arrow.size = 0.2,
         vertex.label.color = "magenta", vertex.color = "lightblue",
         edge.curved = TRUE)
  })
  
  output$alluvial <- renderSankeyNetwork({
    flow <- filter_product()
    #print(flow)
    # Filter out small flows
    flow <- flow[order(-value)]
    flow <- flow[(cumsum(value)/sum(value)) < 0.8]
    # Build network for alluvial diagram
    nodes <- data.table(
      id = c(unique(paste0(flow$destin_eu, "_d")),
             unique(paste0(flow$consign_eu, "_c")),
             unique(flow$origin_eu)), stringsAsFactors = FALSE)
    links <- rbind(flow[, .(
      src = origin_eu,
      dst = paste0(consign_eu, "_c"),
      value = value,
      group = origin_eu)],
      flow[, .(
        src = paste0(consign_eu, "_c"),
        dst = paste0(destin_eu, "_d"),
        value = value,
        group = origin_eu)])
    links <- links[, .(value = sum(value)), by = .(src, dst, group)]
    nodes <- nodes[id %in% links$src | id %in% links$dst]
    links[, src := match(src, nodes$id)-1L]
    links[, dst := match(dst, nodes$id)-1L]
    sankeyNetwork(links, Source = "src", Target = "dst", Value = "value",
                  Nodes = nodes, NodeID = "id", LinkGroup = "group")
  })
  
}






# RUN APP -----------------------------------------------------------------


shinyApp(ui = ui, server = server)
