
library(shiny)
library(data.table)
library(fst)
library(igraph)
library(countrycode)
library(networkD3)
library(feather)

#dir_itgs <- "../../itgs"
dir_itgs <- "~/data"

# PREPARE DATA ------------------------------------------------------------

# Read and prep the coordinates of the countries
centroids <- fread("~/EU_BD_hackathon_2021/hulpdata/geo_europa/europe_nuts0_centroids.csv",
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
years <- c("2017","2018", "2019")

#itgs_list <- vector(mode="list", length=length(years))
#names(itgs_list) <- years

#goods <- vector(mode="list", length=length(years))
#names(goods) <- years

#countries <- vector(mode="list", length=length(years))
#names(countries) <- years

unique_goods = c()
unique_countries = c()
#unique_codes_eu = c()

for (year in years) {
  file_name <- paste("itgs_reference_", year, "_short.csv", sep="")
  print(file_name)
  itgs <- fread(file.path(dir_itgs, file_name))
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
  
  goods <- unique(itgs$hs6)
  noneu <- c("Asia", "Americas", "Africa", "Europe", "Oceania")
  countries <- unique(itgs$origin_eu)
  countries <- c("<ALL>", countries)
  countries <- setdiff(countries, noneu)
  
  unique_goods <- unique(append(unique_goods, goods))
  unique_countries <- unique(append(unique_countries, countries))
  #unique_codes_eu <- unique(append(unique_codes_eu, codes_eu))
  
  #feather_file_name <- paste("itgs_", year, ".feather", sep="")
  itgs_file_name <- paste("itgs_", year, ".Rdata", sep="")

  #write_feather(itgs, feather_file_name)
  save(itgs, file=itgs_file_name)
}

#unique_countries

coords <- as.matrix(centroids[, c("x", "y")])
coords[,1] <- coords[,1] - min(coords[,1])
coords[,2] <- coords[,2] - min(coords[,2])
m <- max(coords)
coords[,1] <- coords[,1]/m
coords[,2] <- coords[,2]/m

save(years, file="years.Rdata")
save(centroids, file="centroids.Rdata")
save(coords, file="coords.Rdata")
save(unique_goods, file="unique_goods.Rdata")
save(unique_countries, file="unique_countries.Rdata")
#save(unique_codes_eu, file="unique_codes_eu.Rdata")


#goods <- unique(itgs$hs6)
#noneu <- c("Asia", "Americas", "Africa", "Europe", "Oceania")
#countries <- unique(itgs$origin_eu)
#countries <- c("<ALL>", countries)
#countries <- setdiff(countries, noneu)