# Experiment met nonnegative matrix factorisation

library(data.table)
library(fst)
library(igraph)
library(countrycode)
library(NMF)
library(ggplot2)

year <- 2011

itgs <- read_fst(sprintf("tmp_data/itgs_%04d.fst", year), as.data.table = TRUE)

goods <- unique(itgs$hs6)
noneu <- c("Asia", "Americas", "Africa", "Europe", "Oceania")
countries <- unique(itgs$origin_eu)
countries <- setdiff(countries, noneu)




# Look at export
export <- itgs[!(origin_eu %in% noneu), .(value = sum(obs_value)), 
  by = .(hs6, origin_eu)]
# Normalise: sum per country == 1
export <- export[, value := value/sum(value), by = origin_eu]
export <- dcast(export, hs6 ~ origin_eu, fill = 0)
products <- export$hs6
export <- as.matrix(export[, -1])
rownames(export) <- products


# Look at import
import <- itgs[!(destin_eu %in% noneu), .(value = sum(obs_value)), 
  by = .(hs6, destin_eu)]
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
# basismap(m_export)
# coefmap(m_export)


# m_import <- nmfEstimateRank(import, c(1:20), nrun = 5)
# plot(m_import)
# 
# m_import <- nmf(import, c(4,6,8), nrun = 10)
# consensusmap(m_import)
set.seed(1)
m_import <- nmf(import, 6, nrun = 10)
# basismap(m_import)
# coefmap(m_import)




w_import <- basis(m_import)
h_import <- coef(m_import)
w_export <- basis(m_export)
h_export <- coef(m_export)




labels <- fread("../hulpdata/HS 2012 to BEC Correlation and conversion tables.csv")
setnames(labels, c("hs6", "bec"))
m <- match(as.integer(rownames(w_import)), labels$hs6)

labels <- fread("../hulpdata/HS 2012 to SITC2 Correlation and conversion tables.csv", 
  colClasses = c("integer", "character"))
setnames(labels, c("hs6", "sitc"))
labels[, sitc2 := substr(sitc, 1, 2)]
m <- match(as.integer(rownames(w_import)), labels$hs6)

library(jsonlite)
sitc <- jsonlite::fromJSON("https://comtrade.un.org/data/cache/classificationS2.json")
ms <- match(labels$sitc2, sitc$results$id)
labels$sitc2_label <- sitc$results$text[ms]
# Shorten long label of 77
labels[sitc2 == "77", sitc2_label := gsub(" [(]{1}.*$", "", sitc2_label)]



dir.create("output")


wimp <- as.data.table(w_import)
setnames(wimp, paste0("dim", 1:6))
wimp$hs6 <- rownames(w_import)
wimp <- melt(wimp, id.vars = "hs6", variable.name = "dimension", 
  value.name = "score")
wimp[, dimension := as.integer(substr(dimension, 4,4))]
m <- match(wimp$hs6, labels$hs6)
wimp[, sitc2 := labels$sitc2[m]]
wimp[, sitc2_label := labels$sitc2_label[m]]



tabi <- wimp[, .(score = sum(score), sitc2_label = first(sitc2_label)),
  by = .(dimension, sitc2)]
tabi <- tabi[complete.cases(tabi), ]

ggplot(tabi, aes(x = sitc2_label, y = score)) + geom_bar(stat = "identity") +
  facet_grid(1 ~ dimension) +
  coord_flip() + xlab("SITC") + ylab("Score")

fwrite(wimp, "output/w_import_2011.csv")
fwrite(tabi, "output/w_import_sum_2011.csv")


wexp <- as.data.table(w_export)
setnames(wexp, paste0("dim", 1:6))
wexp$hs6 <- rownames(w_export)
wexp <- melt(wexp, id.vars = "hs6", variable.name = "dimension", 
  value.name = "score")
wexp[, dimension := as.integer(substr(dimension, 4,4))]
m <- match(wexp$hs6, labels$hs6)
wexp[, sitc2 := labels$sitc2[m]]
wexp[, sitc2_label := labels$sitc2_label[m]]


tabe <- wexp[, .(score = sum(score), sitc2_label = first(sitc2_label)),
  by = .(dimension, sitc2)]
tabe <- tabe[complete.cases(tabe), ]

ggplot(tabe, aes(x = sitc2_label, y = score)) + geom_bar(stat = "identity") +
  facet_grid(1 ~ dimension) +
  coord_flip() + xlab("SITC") + ylab("Score")

fwrite(wexp, "output/w_export_2011.csv")
fwrite(tabe, "output/w_export_sum_2011.csv")





# Scores per country per year ---------------------------------------------


wexpm <- dcast(wexp[, .(hs6, dimension, score)], hs6 ~ dimension)
wexpm[, hs6 := as.integer(hs6)]

wimpm <- dcast(wimp[, .(hs6, dimension, score)], hs6 ~ dimension)
wimpm[, hs6 := as.integer(hs6)]



years <- 2010:2019
all_weights_import <- vector('list', length(years))
all_weights_export <- vector('list', length(years))

for (i in seq_along(years)) {
  year <- years[i]
  message("Starting year ", year)
  itgs <- read_fst(sprintf("tmp_data/itgs_%04d.fst", year), as.data.table = TRUE)
  itgs <- itgs[hs6 %in% goods]
  
  # Look at export
  export <- itgs[!(origin_eu %in% noneu), .(value = sum(obs_value)), 
    by = .(hs6, origin_eu)]
  export <- export[origin_eu %in% countries]
  export <- export[, value := value/sum(value), by = origin_eu]
  export <- merge(export, wexpm, by = c("hs6"))
  
  country <- "AT"
  weights_export <- lapply(unique(export$origin_eu), function(country, export, year) {
    d <- export[origin_eu == country]
    m <- lm(value ~ `1` + `2` + `3` + `4` + `5` + `6`, data = d)
    data.table(
      year = year,
      country = country,
      dimension = seq_len(length(coef(m))-1),
      weight = as.numeric(coef(m)[-1]))
  }, export = export, year = year)
  all_weights_export[[i]] <- rbindlist(weights_export)
  
  
  # Look at import
  import <- itgs[!(destin_eu %in% noneu), .(value = sum(obs_value)), 
    by = .(hs6, destin_eu)]
  import <- import[destin_eu %in% countries]
  import <- import[, value := value/sum(value), by = destin_eu]
  import <- merge(import, wimpm, by = c("hs6"))
  weights_import <- lapply(unique(import$destin_eu), function(country, import, year) {
    d <- import[destin_eu == country]
    m <- lm(value ~ `1` + `2` + `3` + `4` + `5` + `6`, data = d)
    data.table(
      year = year,
      country = country,
      dimension = seq_len(length(coef(m))-1),
      weight = as.numeric(coef(m)[-1]))
  }, import = import, year = year)
  all_weights_import[[i]] <- rbindlist(weights_import)
  
  message("Finished year ", year)
}


weights_export <- rbindlist(all_weights_export)
weights_import <- rbindlist(all_weights_import)

fwrite(weights_export, "output/weights_export_2011.csv")
fwrite(weights_import, "output/weights_import_2011.csv")


weights_export[, sweight := weight/mean(abs(weight)), by = .(country, year)]
weights_export[sweight < 0, sweight := 0]
ggplot(weights_export, aes(x= year, y = sweight, colour = factor(dimension))) + 
  geom_line() + facet_wrap(~ country) +
  labs(x = "Year", y = "Importance of dimensions", color = "Dimension") 

weights_import[, sweight := weight/mean(abs(weight)), by = .(country, year)]
weights_import[sweight < 0, sweight := 0]
ggplot(weights_import, aes(x= year, y = sweight, colour = factor(dimension))) + 
  geom_line() + facet_wrap(~ country) + 
  labs(x = "Year", y = "Importance of dimensions", color = "Dimension")


# 
# 
# export <- dcast(export, hs6 ~ origin_eu, fill = 0)
# products <- export$hs6
# export <- as.matrix(export[, -1])
# rownames(export) <- products
# 
# 
# # Look at import
# import <- itgs[!(destin_eu %in% noneu), .(value = sum(obs_value)), 
#   by = .(hs6, destin_eu)]
# # Normalise: sum per country == 1
# import <- import[, value := value/sum(value), by = destin_eu]
# import <- dcast(import, hs6 ~ destin_eu, fill = 0)
# products <- import$hs6
# import <- as.matrix(import[, -1])
# rownames(import) <- products
# 
# 
# 









# 
# 
# dcast(tab, sitc2 + sitc2_label ~ dimension, value.var = "score" )
# 
# 
# # Assign each product to a dimension
# product_group_import <- apply(w_import, 1, which.max)
# product_group_export <- apply(w_export, 1, which.max)
# 
# table(labels$sitc2[m[product_group_export == 1]])
# table(labels$sitc2[m[product_group_export == 2]])
# table(labels$sitc2[m[product_group_export == 3]])
# table(labels$sitc2[m[product_group_export == 4]])
# table(labels$sitc2[m[product_group_export == 5]])
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# labels$Parent[m[product_group_export == 1]]
# 
# table(product_group_export, product_group_import)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# w1 <- w_import[,1]
# w2 <- w_import[,2]
# w3 <- w_import[,3]
# w4 <- w_import[,4]
# w5 <- w_import[,5]
# w6 <- w_import[,6]
# 
# 
# m <- lm(import[,1] ~ w1 + w2 + w3 + w4 + w5 + w6)
# summary(m)
# 
# h_import[,1]
# plot(coef(m)[-1], h_import[,1])
# 
# # 
# # # Look at export
# # relations <- itgs[!(origin_eu %in% noneu) & !(destin_eu %in% noneu), 
# #   .(value = sum(value)), by = .(hs6, origin_eu, destin_eu)]
# # # Normalise: sum per country == 1
# # relations <- relations[, value := value/sum(value), by = .(origin_eu, destin_eu)]
# # 
# # # tmp <- CJ(origin_eu = unique(relations$origin_eu),
# # #   destin_eu = unique(relations$destin_eu), hs6 = unique(relations$hs6))
# # # tmp <- merge(tmp, relations, all.x = TRUE)
# # # tmp[is.na(value), value := 0]
# # # relations <- tmp
# # 
# # relations <- dcast(relations, hs6 ~ origin_eu+destin_eu, fill = 0, sep = "->")
# # products <- relations$hs6
# # relations <- as.matrix(relations[, -1])
# # rownames(relations) <- products
# # 
# # 
# # m_rel_r <- nmfEstimateRank(relations, c(1:20), nrun = 5)
# # plot(m_rel_r)
# # # 
# # # m_import <- nmf(import, c(4,6,8), nrun = 10)
# # # consensusmap(m_import)
# # set.seed(1)
# # m_rel <- nmf(relations, 15, nrun = 25)
# # basismap(m_rel)
# # coefmap(m_rel)
# 
# 
# 

