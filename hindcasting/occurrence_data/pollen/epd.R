library(EPDr)

# Connecting to the EPD database
epd.connection <- connect_to_epd(database = "EPD",
               user = "postgres",
               password = "epdpass",
               driver = RPostgres::Postgres())


# Searching for entities
e_ids <- list_e(epd.connection, country = c("France", "Spain", "Italy"))
e_ids <- e_ids$e_


# Retrieving data for multiple entities
epd_all <- lapply(e_ids, get_entity, epd.connection)
epd_all <- lapply(epd_all, entity_to_matrices)


# Standardizing data across multiple entities

## remove entities with some restrictions on their use
vapply(epd_all, check_restriction, FUN.VALUE=logical(1))
epd_all <- remove_restricted(epd_all)

## remove all the entities that have no ages for the biological counts, or that have no counts
vapply(epd_all, check_defaultchron, FUN.VALUE=logical(1))
epd_all <- remove_wo_ages(epd_all)
epd_all <- remove_wo_counts(epd_all)

## we are interested in tree pollen data
epd_all <- lapply(epd_all, filter_taxagroups,
                  c("TRSH"))

## change each taxa name in the objects for the accepted name according to the EPD
epd.taxonomy <- get_taxonomy_epd(epd.connection)
epd_all <- lapply(epd_all, taxa_to_acceptedtaxa, epd.taxonomy)
epd_all <- unify_taxonomy(epd_all, epd.taxonomy)

## make chronologies from Giesecke the default to be used
epd_all <- lapply(epd_all, giesecke_default_chron)

## Standardizing time of counts data (interpolation or averaging)
epd_all <- lapply(epd_all, interpolate_counts, seq(0, 22000, by = 1000))
epd_all[[2]]@commdf@counts[, 1:7]


# Calculate data quality
epd_all[[1]]@agesdf@dataquality


# Create a table, map 
epd_tables <- lapply(epd_all,
                     table_by_taxa_age,
                     c("Pinus"), c("12000"))
epd_table <- do.call(rbind, epd_tables)
map_taxa_age(epd_all, "Fagus", "12000", pres_abse = T, pollen_thres = 2) #2% as in Saltre et al. 2013
map_taxa_age(epd_all, "Pinus", "12000", pres_abse = T, pollen_thres = 5) #2% as in Cheddadi et al. 2003
map_taxa_age(epd_all, c("Abies"), "12000", pres_abse = T, pollen_thres = 2) #2% as in Liepelt et al. 2009
