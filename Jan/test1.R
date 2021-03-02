

library(data.table)
library(fst)


itgs <- read_fst("~/Jan/data/proces/itgs_reference_2010.fst")


fn <- sprintf("~/studies/CBS-code/comext_data/TRANSPORT_HS/trhs2010%02d.dat", 1:12)





dta52 <- fread("~/studies/CBS-code/comext_data/TRANSPORT_HS/trhs201052.dat", colClasses = "character")

dta01 <- fread("~/studies/CBS-code/comext_data/TRANSPORT_HS/trhs201001.dat", colClasses = "character")

# FLOW 1 = import; 2 = export
# TRANSPORT_MODE
# 0 = onbek
# 1..9


setnames(dta52, tolower(names(dta52)))

dta52[, value_in_euros := as.numeric(value_in_euros)]

dta52[, .N, by = declarant_iso]
dta52[, .N, by = partner_iso]

export <- dta52[flow == 2, .(export = sum(value_in_euros)), 
  by = .(declarant_iso, partner_iso, product_hs)]
setnames(export, c("src", "dst", "product_hs", "export"))


export_itgs <- itgs[, .(export = sum(obs_value*10^(unit_mult))), 
  by = .(consign, destin, hs6)]
setnames(export_itgs, c("src", "dst", "product_hs", "export_itgs"))
nrow(export)
nrow(export_itgs)

sapply(export, class)
export[, product_hs := as.integer(product_hs)]


export_merge <- merge(export_itgs, export, all = TRUE)






# -------------------------------------------------------------------------

dta52 <- fread("~/studies/CBS-code/comext_data/PRODUCTS/full201052.dat", colClasses = "character")

setnames(dta52, tolower(names(dta52)))
dta52
sel52 <- dta52[substr(product_nc, 1, 6) == "271121"]



fn <- sprintf("~/studies/CBS-code/comext_data/PRODUCTS/full2010%02d.dat", 1:12)

comext_maand <- lapply(fn, fread, colClasses = "character")
comext_maand <- rbindlist(comext_maand)
write_fst(comext_maand, "~/Jan/comext_maand_2010.fst")

setnames(comext_maand, tolower(names(comext_maand)))

tmp <- comext_maand[substr(product_nc, 1, 6) == "271121"]
View(tmp)
tmp[declarant_iso == "BE" & partner_iso == "NL", sum(as.numeric(value_in_euros)), by = flow]

sel52 <- dta52[substr(product_nc, 1, 6) == "271121" & declarant_iso == "BE" & partner_iso == "NL"]
sel52
