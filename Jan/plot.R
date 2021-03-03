

library(data.table)
library(fst)
library(igraph)
library(countrycode)

centroids <- fread("CBS-code/hulpdata/geo_europa/europe_nuts0_centroids.csv",
  stringsAsFactors = FALSE)

centroids

plot(centroids$x, centroids$y, pch = 20, col = "lightgray",asp = 1)
text(centroids$x, centroids$y, labels = centroids$nuts_id)

points(8E6, 3E6, col = "red")
points(0E6, 3E6, col = "red")
points(1E7, 1E6, col = "red")
points(5E6, 1E6, col = "red")


centroids <- rbind(centroids, data.frame(
  nuts_id = c("Europe", "Americas", "Asia", "Africa", "Oceania"),
  nuts_name = c("Europe", "Americas", "Asia", "Africa", "Oceania"),
  x = c(8E6, 0E6, 1E7, 5E6, 1E7),
  y = c(3E6, 1E6, 1.5E6, 5E5, 3E5), stringsAsFactors = FALSE
))


centroids$nuts_id[centroids$nuts_id == "UK"] <- "GB"
centroids$nuts_id[centroids$nuts_id == "EL"] <- "GR"

itgs <- fread("itgs/itgs_reference_2010.csv")
setnames(itgs, tolower(names(itgs)))

codes <- unique(unique(itgs$destin), unique(itgs$origin) )
codes_eu <- codes[unique(codes) %in% centroids$nuts_id]

itgs[, origin_eu := ifelse(origin %in% codes_eu, origin, 
  countrycode(origin, origin="iso2c", destination = 'continent'))]

itgs[, destin_eu := ifelse(destin %in% codes_eu, destin, 
  countrycode(destin, origin="iso2c", destination = 'continent'))]


tot <- itgs[hs6 == 10110, .(weight = sum(obs_value)), by = .(origin_eu, destin_eu)]
tot <- tot[complete.cases(tot)]
setnames(tot, c("src", "dst", "weight"))

tot <- tot[!(tot$src %in% c("Asia", "Americas", "Africa", "Europe", "Oceania")) |
  !(tot$dst %in% c("Asia", "Americas", "Africa", "Europe", "Oceania")) ,]
  

library(sf)
map <- st_read("../hulpdata/geo_europa/europe_nuts0.geojson")


g <- graph_from_data_frame(tot, directed = TRUE, vertices = centroids)


coords <- as.matrix(centroids[, c("x", "y")])
coords[,1] <- coords[,1] - min(coords[,1])
coords[,2] <- coords[,2] - min(coords[,2])
m <- max(coords)
coords[,1] <- coords[,1]/m
coords[,2] <- coords[,2]/m

par(mar = c(0,0,0,0), bg = "#555555")
plot(centroids$x, centroids$y, asp = 1, type = 'n', xlab = "", ylab = "", 
  xaxt = 'n', yaxt = 'n', bty = 'n')
plot(map$geometry, add = TRUE, border="gray", col = "black")
plot(g, coords = coords, rescale = FALSE, 
  add = TRUE, edge.width = 25*(E(g)$weight/max(E(g)$weight)), 
  edge.color = "#FFFFFF50", edge.arrow.size = 0.2,
  vertex.label.color = "magenta", vertex.color = "lightblue",
  edge.curved = TRUE)



head(tot)

library(ggplot2)

ggplot(tot, aes(x = dst, y = weight)) + geom_bar(stat = "identity") + 
  facet_wrap(~ src) + coord_flip()



# Sankey ------------------------------------------------------------------
itgs[, consign_eu := ifelse(consign %in% codes_eu, consign, 
  countrycode(consign, origin="iso2c", destination = 'continent'))]

library(networkD3)

# ?sankeyNetwork
# 
# flow <- itgs[, .(value = sum(obs_value)), by = .(origin_eu, consign_eu, destin_eu)]
# 
# flow <- itgs[hs6 == 10110, .(value = sum(obs_value)), by = .(origin_eu, destin_eu)]
# 
# nodes <- data.frame(id = c(unique(paste0(flow$destin_eu, "_d")), unique(flow$origin_eu)), stringsAsFactors = FALSE)
# links <- flow
# links[, origin_eu := match(origin_eu, nodes$id)-1L]
# links[, destin_eu := match(paste0(destin_eu, "_d"), nodes$id)-1L]
# 
# sankeyNetwork(links, Source = "origin_eu", Target = "destin_eu", Value = "value", 
#   Nodes = nodes, NodeID = "id")

countries <- c("NL", "BE")

sel <- if (is.null(countries)) TRUE else itgs$origin_eu %in% countries | 
  itgs$consign_eu %in% countries |
  itgs$destin_eu %in% countries
flow <- itgs[sel == TRUE, .(value = sum(obs_value)), by = .(origin_eu, consign_eu, destin_eu)]

# Remove flow between non eu
noneu <- c("Asia", "Americas", "Africa", "Europe", "Oceania")
flow <- flow[!(origin_eu %in% noneu & consign_eu %in% noneu & destin_eu %in% noneu)]

# Filter out small flows
flow <- flow[order(-value)]
flow <- flow[(cumsum(value)/sum(value)) < 0.7]


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

# Filter out small flows
# links <- links[order(-value)]
# links <- links[(cumsum(value)/sum(value)) < 0.8]

nodes <- nodes[id %in% links$src | id %in% links$dst]
links[, src := match(src, nodes$id)-1L]
links[, dst := match(dst, nodes$id)-1L]

sankeyNetwork(links, Source = "src", Target = "dst", Value = "value", 
  Nodes = nodes, NodeID = "id", LinkGroup = "group")














