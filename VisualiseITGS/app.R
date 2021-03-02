
library(shiny)
library(data.table)
library(fst)
library(igraph)
library(countrycode)


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


coords <- as.matrix(centroids[, c("x", "y")])
coords[,1] <- coords[,1] - min(coords[,1])
coords[,2] <- coords[,2] - min(coords[,2])
m <- max(coords)
coords[,1] <- coords[,1]/m
coords[,2] <- coords[,2]/m

goods <- unique(itgs$hs6)
countries <- unique(itgs$origin_eu)
countries <- c("<ALL>", countries)





# UI ----------------------------------------------------------------------


ui <- fluidPage(

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
        selected = NULL, multiple = TRUE, options = NULL)
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("network", height = "800px")
    )
  )
)






# SERVER ------------------------------------------------------------------



server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'product', choices = goods, server = TRUE)
    
  network <- reactive({
    # Filter itgs data based in product and selected countries
    product <- input$product
    tot <- itgs[hs6 == product, .(weight = sum(obs_value)), by = .(origin_eu, destin_eu)]
    if (!is.null(input$country) && !("<ALL>" %in% input$country)) {
      country <- input$country
      tot <- tot[((origin_eu %in% country) | (destin_eu %in% country))]
    }
    tot <- tot[complete.cases(tot)]
    setnames(tot, c("src", "dst", "weight"))
    tot <- tot[!(tot$src %in% c("Asia", "Americas", "Africa", "Europe", "Oceania")) |
      !(tot$dst %in% c("Asia", "Americas", "Africa", "Europe", "Oceania")), ]
    tot
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
}





# RUN APP -----------------------------------------------------------------


shinyApp(ui = ui, server = server)
