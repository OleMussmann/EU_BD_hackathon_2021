
library(shiny)
library(data.table)
library(fst)
library(igraph)
library(countrycode)
library(networkD3)
library(sf)
library(fst)
library(ggplot2)
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

#Read map of europe
map <- st_read("../hulpdata/geo_europa/europe_nuts0.geojson")

# Read data for export classification
export <- list()
export[["w"]] <- fread("../Jan/output/w_export_sum_2011.csv")
export[["weights"]] <- fread("../Jan/output/weights_export_2011.csv")
export[["weights"]][, sweight := weight/mean(abs(weight)), by = .(country, year)]
export[["weights"]][sweight < 0, sweight := 0]
# And for import classification
import <- list()
import[["w"]] <- fread("../Jan/output/w_import_sum_2011.csv")
import[["weights"]] <- fread("../Jan/output/weights_import_2011.csv")
import[["weights"]][, sweight := weight/mean(abs(weight)), by = .(country, year)]
import[["weights"]][sweight < 0, sweight := 0]


comext_2010 <- fread("~/data/Comext_mnd_aggregatie_2010.csv")
comext_2010
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

#itgs <- itgs_list$`2019`

# UI ----------------------------------------------------------------------


ui <- navbarPage("ITGS", 
   tabPanel("Flows", 
    fluidPage(
      
      titlePanel("ITGS"),
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          # Client side filtering in selectize
          #selectizeInput("product", "Good/service", choices = goods, 
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
          fillRow(
          plotOutput("comext_import", width="300px", height="300px"),
          plotOutput("comext_export", width="300px", height="300px")
          )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("network", width = "50vw", height = "40vh"),
          sankeyNetworkOutput("alluvial", width = "50vw", height = "40vh"),
        )
      )
    )
),
# tabPanel("Export classification",
#   fluidPage(
#     titlePanel("Export classification"),
#     selectizeInput("country_export", "Country", choices = countries, 
#       selected = NULL, multiple = TRUE, options = NULL),
#     plotOutput("export_w", height = "800px"),
#     plotOutput("export_weights", height = "800px")
#   )
# ),
tabPanel("Import and export classification",
    fluidPage(
      titlePanel("Import and export classification"),
      fillRow(
        h2("Import patterns"),
        h2("Export patterns"),
        height = "100px"
      ),
      fillRow(
        plotOutput("import_w", height = "800px"),
        plotOutput("export_w", height = "800px"),
        height = "900px"
      ),
      fillRow(
        h2("Importance of import patterns for countries"),
        h2("Importance of export patterns for countries"),
        height = "100px"
      ),
      selectizeInput("country_import", "Select countries", choices = countries, 
                     selected = NULL, multiple = TRUE, options = NULL),
      fillRow(
        plotOutput("import_weights", height = "800px"),
        plotOutput("export_weights", height = "800px"),
        height = "900px"
      )
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
  
  # # start comext
  # #PERIOD = c(201001, 201002, 201003, 201004, 201005, 201006, 201007, 201008, 201009, 201010, 201011, 201012, 201101) 
  # YEAR = c(2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2011)
  # MONTH = c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 01) 
  # DECLARANT_ISO = c("NL", "NL", "NL", "NL", "NL", "NL", "NL", "NL", "NL", "NL", "NL", "NL", "FR")
  # PRODUCT_NC = c("01234567", "01234567", "01234567", "01234568", "01234568", "01234567", "01234567", "01234567", "01234567", "01234567", "01234567", "01234567", "01234568")
  # FLOW = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2) 
  # VALUE_IN_EUROS = c(10, 35, 10, 60, 55, 10, 35, 10, 60, 55, 11, 25, 15) 
  # QUANTITY_IN_KG = c(10, 35, 10, 60, 55, 10, 35, 10, 60, 55, 11, 25, 15)  
  # 
  # df = data.frame(PERIOD, YEAR, MONTH, DECLARANT_ISO, PRODUCT_NC, FLOW, VALUE_IN_EUROS, QUANTITY_IN_KG)
  # 
  df2_import <- df[df$YEAR==2010 && df$FLOW==1]
  df2_export <- df[df$YEAR==2010 && df$FLOW==2]
  # 
  # 
  # # hard-coded year
  # #comext_import <- comext_2010[comext_2010$YEAR==2010 && comext_2010$FLOW==1 && comext_2010$country %in% countries && comext_import$PRODUCT_NC %in% product_list]
  # 
  # output$comext_import <- reactive({
  #   # countries <- input$country
  #   # product <- input$product
  #   # product_list <- c()
  #   # for (p in product){
  #   #   sub_products <- ld[startsWith(ld$HS6_str, p)]$HS6_str
  #   #   product_list <- append(product_list, sub_products)
  #   # }
  #   # comext_import <- comext_2010[comext_2010$YEAR==2010 && comext_2010$FLOW==1 && comext_2010$country %in% countries && comext_import$PRODUCT_NC %in% product_list]
  #   # renderPlot(ggplot(comext_import, aes(x=MONTH, y=VALUE_IN_EUROS, group=PRODUCT_NC, color=PRODUCT_NC)) + geom_line() + ggtitle("Import"))
  #   renderPlot(ggplot(df2_import, aes(x=MONTH, y=VALUE_IN_EUROS, group=PRODUCT_NC, color=PRODUCT_NC)) + geom_line() + ggtitle("Import"))
  #   
  # })
  output$comext_import <- renderPlot(ggplot(df2_import, aes(x=MONTH, y=VALUE_IN_EUROS, group=PRODUCT_NC, color=PRODUCT_NC)) + geom_line() + ggtitle("Import"))
  # 
  # 
  # output$comext_import <- reactive({
  #   # countries <- input$country
  #   # product <- input$product
  #   # product_list <- c()
  #   # for (p in product){
  #   #   sub_products <- ld[startsWith(ld$HS6_str, p)]$HS6_str
  #   #   product_list <- append(product_list, sub_products)
  #   # }
  #   # comext_export <- comext_2010[comext_2010$YEAR==2010 && comext_2010$FLOW==2 && comext_2010$country %in% countries && comext_import$PRODUCT_NC %in% product_list]
  #   # renderPlot(ggplot(comext_export, aes(x=MONTH, y=VALUE_IN_EUROS, group=PRODUCT_NC, color=PRODUCT_NC)) + geom_line() + ggtitle("Export"))
  #   renderPlot(ggplot(df2_export, aes(x=MONTH, y=VALUE_IN_EUROS, group=PRODUCT_NC, color=PRODUCT_NC)) + geom_line() + ggtitle("Export"))
  # })
  output$comext_export <- renderPlot(ggplot(df2_export, aes(x=MONTH, y=VALUE_IN_EUROS, group=PRODUCT_NC, color=PRODUCT_NC)) + geom_line() + ggtitle("Export"))
  # data_import <- reactive({
  #   countries <- input$country
  #   product <- input$product
  #   product_list <- c()
  #   for (p in product){
  #     sub_products <- ld[startsWith(ld$HS6_str, p)]$HS6_str
  #     product_list <- append(product_list, sub_products)
  #   }
  #   filtered <- comext_2010[comext_2010$YEAR==2010 && comext_2010$FLOW==1 && comext_2010$country %in% countries && comext_2010$PRODUCT_NC %in% product_list]
  #   print(filtered)
  #   filtered
  # })
  # 
  # output$comext_import <- renderPlot(ggplot(data_import(), aes(x=MONTH, y=VALUE_IN_EUROS, group=PRODUCT_NC, color=PRODUCT_NC)) + geom_line() + ggtitle("Import"))
  
  # end comext
  
  output$network <- renderPlot({
    # Plot network
    dta <- network()
    g <- graph_from_data_frame(dta, directed = TRUE, vertices = centroids)
    par(mar = c(0,0,0,0), bg = "#555555")
    plot(centroids$x, centroids$y, asp = 1, type = 'n', xlab = "", ylab = "", 
         xaxt = 'n', yaxt = 'n', bty = 'n')
    plot(map$geometry, add = TRUE, border = "darkgray", col = "black")
    plot(g, coords = coords, rescale = FALSE, 
         add = TRUE, edge.width = 25*(E(g)$weight/max(E(g)$weight)), 
         edge.color = "#FFFFFF50", edge.arrow.size = 0.2,
         vertex.label.color = "green", vertex.color = "lightblue",
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
  output$export_w <- renderPlot({
    ggplot(export[["w"]], aes(x = sitc2_label, y = score, fill=factor(dimension))) + 
      geom_bar(stat = "identity") +
      facet_grid(1 ~ dimension) +
      coord_flip() + xlab("SITC") + ylab("Score")
  })
  
  output$export_weights <- renderPlot({
    countries <- input$country_import
    sel <- if (is.null(countries) | "<ALL>" %in% countries) TRUE else 
      export$weights$country %in% countries
    ggplot(export[["weights"]][sel], aes(x= year, y = sweight, colour = factor(dimension))) + 
      geom_line() + facet_wrap(~ country) +
      labs(x = "Year", y = "Importance of dimensions", color = "Dimension") 
  })
  
  output$import_w <- renderPlot({
    ggplot(import[["w"]], aes(x = sitc2_label, y = score, fill=factor(dimension))) + 
      geom_bar(stat = "identity") +
      facet_grid(1 ~ dimension) +
      coord_flip() + xlab("SITC") + ylab("Score")
  })
  
  output$import_weights <- renderPlot({
    countries <- input$country_import
    sel <- if (is.null(countries) | "<ALL>" %in% countries) TRUE else 
      import$weights$country %in% countries
    ggplot(import[["weights"]][sel], aes(x= year, y = sweight,
                                         colour = factor(dimension))) + 
      geom_line() + facet_wrap(~ country) +
      labs(x = "Year", y = "Importance of dimensions", color = "Dimension") 
  })
  
}






# RUN APP -----------------------------------------------------------------


shinyApp(ui = ui, server = server)
