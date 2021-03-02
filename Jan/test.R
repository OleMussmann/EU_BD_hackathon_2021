

library(data.table)
library(fst)
library(igraph)

dta <- fread("~/studies/estat-hckt-nl-study/itgs_reference_2010.csv")

setnames(dta, tolower(names(dta)))

write_fst(dta, "data/proces/itgs_reference_2010.fst")

head(dta)
sapply(dta, class)

dta[, .N, by = time_period]
dta[, .N, by = trade_type]
dta[, .N, by = hs6]
dta[, .N, by = unit_mult]
dta[, .N, by = decimals]
dta[, .N, by = obs_status]
dta[, .N, by = conf_status]
dta[, .N, by = prod_stage]


nrow(dta)
length(unique(dta$origin))

library(igraph)

sel <- dta[hs6 == 200551 & (origin == "NL" | destin == " NL" )]

g <- dta[hs6 == 200551, .(src = origin, dst = destin, weight = obs_value)]


g <- graph_from_data_frame(g[src == "NL" | dst == " NL"], directed = TRUE)
g <- graph_from_data_frame(g, directed = TRUE)
g
plot(g, edge.size = E(g)$weight, vertex.size = 1, edge.arrow.size = 0.4)

?plot.igraph
