
library(shiny)
library(data.table)
library(fst)
library(igraph)
library(countrycode)
library(networkD3)
library(sf)
library(ggplot2)

dir_itgs <- "../../itgs"

# PREPARE DATA ------------------------------------------------------------

# Read and prep the coordinates of the countries
centroids <- fread("../hulpdata/geo_europa/europe_nuts0_centroids.csv",
  stringsAsFactors = FALSE)
# Add continents
centroids <- rbind(centroids, data.frame(
  nuts_id = c("Europe", "Americas", "Asia", "Africa", "Oceania"),
  nuts_name = c("Europe", "Americas", "Asia", "Africa", "Oceania"),
  x = c(8E6, 0E6, 1E7, 5E6, 1E7),
  y = c(3E6, 1E6, 1.5E6, 5E5, 3E5), stringsAsFactors = FALSE
))
# Coding used in this file differs from that used in ITGS
centroids$nuts_id[centroids$nuts_id == "UK"] <- "GB"
centroids$nuts_id[centroids$nuts_id == "EL"] <- "GR"

# Read ITGS
itgs <- fread(file.path(dir_itgs, "itgs_reference_2010.csv"))
setnames(itgs, tolower(names(itgs)))
# Combine non-eu countries into continents
codes <- unique(unique(itgs$destin), unique(itgs$origin) )
codes_eu <- codes[unique(codes) %in% centroids$nuts_id]
itgs[, origin_eu := ifelse(origin %in% codes_eu, origin, 
  countrycode(origin, origin="iso2c", destination = 'continent'))]
itgs[, destin_eu := ifelse(destin %in% codes_eu, destin, 
  countrycode(destin, origin="iso2c", destination = 'continent'))]
itgs[, consign_eu := ifelse(consign %in% codes_eu, consign, 
  countrycode(consign, origin="iso2c", destination = 'continent'))]


coords <- as.matrix(centroids[, c("x", "y")])
# coords[,1] <- coords[,1] - min(coords[,1])
# coords[,2] <- coords[,2] - min(coords[,2])
# m <- max(coords)
# coords[,1] <- coords[,1]/m
# coords[,2] <- coords[,2]/m

goods <- unique(itgs$hs6)
noneu <- c("Asia", "Americas", "Africa", "Europe", "Oceania")
countries <- unique(itgs$origin_eu)
countries <- c("<ALL>", countries)
countries <- setdiff(countries, noneu)

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
          selectizeInput("product", "Good/service", choices = NULL, 
            selected = NULL, multiple = FALSE, options = NULL),
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
  tabPanel("Import classification",
    fluidPage(
      titlePanel("Import classification"),
      fillCol(
      fillRow(
        plotOutput("import_w", height = "800px"),
        plotOutput("export_w", height = "800px"),
        height = "900px"
      ),
      fillRow(
      selectizeInput("country_import", "Select countries", choices = countries, 
        selected = NULL, multiple = TRUE, options = NULL),
        height = "100px"
      ),
      fillRow(
        plotOutput("import_weights", height = "800px"),
        plotOutput("export_weights", height = "800px"),
        height = "900px"
        
      ))
    )
  )
)
  





# 
# ui <- fluidPage(
# 
#   titlePanel("ITGS"),
# 
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       # Client side filtering in selectize
#       #selectizeInput("product", "Good/service", choices = goods, 
#       #  selected = NULL, multiple = FALSE, options = NULL),
#       # Server side filtering in selectize
#       selectizeInput("product", "Good/service", choices = NULL, 
#         selected = NULL, multiple = FALSE, options = NULL),
#       selectizeInput("country", "Country", choices = countries, 
#         selected = NULL, multiple = TRUE, options = NULL),
#       actionButton("update", "Update"),
#     ),
# 
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("network", height = "800px"),
#       sankeyNetworkOutput("alluvial", height = "800px")
#     )
#   )
# )






# SERVER ------------------------------------------------------------------



server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'product', choices = goods, server = TRUE)
  
  filter_country <- reactive({
    countries <- input$country
    sel <- if (is.null(countries) | "<ALL>" %in% countries) TRUE else 
      itgs$origin_eu %in% countries | itgs$consign_eu %in% countries |
      itgs$destin_eu %in% countries
    flow <- itgs[sel == TRUE]
    # Remove flow between non eu
    noneu <- c("Asia", "Americas", "Africa", "Europe", "Oceania")
    flow <- flow[!((origin_eu %in% noneu) & (consign_eu %in% noneu) & 
        (destin_eu %in% noneu))]
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
    if (is.null(product)) {
      flow[, .(value = sum(obs_value)),
        by = .(origin_eu, consign_eu, destin_eu)]
    } else {
      flow[hs6 == product, .(value = sum(obs_value)),
        by = .(origin_eu, consign_eu, destin_eu)]
    }
  })
  
  network <- reactive({
    flow <- filter_product()
    flow <- flow[, .(weight = sum(value)), by = .(origin_eu, destin_eu)]
    flow <- flow[complete.cases(flow)]
    setnames(flow, c("src", "dst", "weight"))
    flow <- flow[!(src %in% noneu) & (dst %in% noneu)]
    flow
  })
  
  
  
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
      vertex.label.color = "magenta", vertex.color = "lightblue",
      edge.curved = TRUE)
  })
  
  output$alluvial <- renderSankeyNetwork({
    flow <- filter_product()
    print(flow)
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
