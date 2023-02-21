# Extract info about a specific taxa

pollen <- filter_taxa(pollen_prop_fltd, taxa) 
macrofossils <- filter_taxa(macrofossil_samples_fltd, taxa) 
others <- filter_taxa(other_samples_fltd, taxa)


# Temporal interpolation, in 500 year intervals, and filtering prop>=2% for pollen
pollen_int <- pollen %>%
  mutate(time = (round(age / 500, 0) * 500)) %>%
  group_by(time, lat, long, sitename, datasetid) %>%
  summarize(prop = mean(prop)) %>%
  dplyr::filter(prop >= 0.02)

macrofossils_int <- macrofossils %>%
  mutate(time = (round(age / 500, 0) * 500)) %>%
  group_by(time, lat, long, sitename, datasetid, element) %>%
  summarize(count = sum(value))

others_int <- others %>%
  mutate(time = (round(age / 500, 0) * 500)) %>%
  group_by(time, lat, long, sitename, datasetid, element) %>%
  summarize(count = sum(value))