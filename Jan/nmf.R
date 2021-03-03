# Experiment met nonnegative matrix factorisation

library(data.table)
library(fst)
library(igraph)
library(countrycode)
library(NMF)


if (!file.exists("tmp_data/itgs_2010.fst")) {

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
  
  
  
  goods <- unique(itgs$hs6)
  noneu <- c("Asia", "Americas", "Africa", "Europe", "Oceania")
  countries <- unique(itgs$origin_eu)
  countries <- setdiff(countries, noneu)
  
  
  itgs[, obs_value := obs_value*10^unit_mult]
  itgs <- itgs[, .(time_period, trade_type, hs6, destin, consign, origin, obs_value, 
    origin_eu, destin_eu, consign_eu)]
  
  dir.create("tmp_data")
  write_fst(itgs, "tmp_data/itgs_2010.fst", compress = 100)

} else {
  # goods <- unique(itgs$hs6)
  noneu <- c("Asia", "Americas", "Africa", "Europe", "Oceania")
  itgs <- read_fst("tmp_data/itgs_2010.fst", as.data.table = TRUE)
}

itgs <- itgs[complete.cases(itgs[, .(origin_eu, destin_eu, consign_eu, obs_value)])]
itgs <- itgs[, .(value = sum(obs_value)), 
  by = .(hs6, origin_eu, destin_eu, consign_eu)]

# Look at export
export <- itgs[!(origin_eu %in% noneu), .(value = sum(value)), by = .(hs6, origin_eu)]
# Normalise: sum per country == 1
export <- export[, value := value/sum(value), by = origin_eu]
export <- dcast(export, hs6 ~ origin_eu, fill = 0)
products <- export$hs6
export <- as.matrix(export[, -1])
rownames(export) <- products


# Look at import
import <- itgs[!(destin_eu %in% noneu), .(value = sum(value)), by = .(hs6, destin_eu)]
# Normalise: sum per country == 1
import <- import[, value := value/sum(value), by = destin_eu]
import <- dcast(import, hs6 ~ destin_eu, fill = 0)
products <- import$hs6
import <- as.matrix(import[, -1])
rownames(import) <- products





# m_export <- nmfEstimateRank(export, c(1:20), nrun = 5)
# plot(m_export)
# 
# m_export <- nmf(export, c(4,6,8), nrun = 10)
# consensusmap(m_export)
set.seed(1)
m_export <- nmf(export, 6, nrun = 10)
basismap(m_export)
coefmap(m_export)


# m_import <- nmfEstimateRank(import, c(1:20), nrun = 5)
# plot(m_import)
# 
# m_import <- nmf(import, c(4,6,8), nrun = 10)
# consensusmap(m_import)
set.seed(1)
m_import <- nmf(import, 6, nrun = 10)
basismap(m_import)
coefmap(m_import)




w_import <- basis(m_import)
h_import <- coef(m_import)
w_export <- basis(m_export)
h_export <- coef(m_export)
dim(w_import)
image(w_import)

labels <- fread("../hulpdata/labels_and_descriptions.csv")
labels$HS6 <- as.integer(gsub(".", "", fixed = TRUE, labels$HS6))
m <- match(as.integer(rownames(w_import)), as.integer(gsub(".", "", fixed = TRUE, labels$HS6)))

# Assign each product to a dimension
product_group_import <- apply(w_import, 1, which.max)
product_group_export <- apply(w_export, 1, which.max)
labels$Description[m[product_group_export == 5]]

table(product_group_export, product_group_import)






# Look at export
relations <- itgs[!(origin_eu %in% noneu) & !(destin_eu %in% noneu), 
  .(value = sum(value)), by = .(hs6, origin_eu, destin_eu)]
# Normalise: sum per country == 1
relations <- relations[, value := value/sum(value), by = .(origin_eu, destin_eu)]

# tmp <- CJ(origin_eu = unique(relations$origin_eu),
#   destin_eu = unique(relations$destin_eu), hs6 = unique(relations$hs6))
# tmp <- merge(tmp, relations, all.x = TRUE)
# tmp[is.na(value), value := 0]
# relations <- tmp

relations <- dcast(relations, hs6 ~ origin_eu+destin_eu, fill = 0, sep = "->")
products <- relations$hs6
relations <- as.matrix(relations[, -1])
rownames(relations) <- products


m_rel_r <- nmfEstimateRank(relations, c(1:20), nrun = 5)
plot(m_rel_r)
# 
# m_import <- nmf(import, c(4,6,8), nrun = 10)
# consensusmap(m_import)
set.seed(1)
m_rel <- nmf(relations, 15, nrun = 25)
basismap(m_rel)
coefmap(m_rel)




