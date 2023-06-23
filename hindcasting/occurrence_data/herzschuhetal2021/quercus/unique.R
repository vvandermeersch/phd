dup <- dataset_count[duplicated(dataset_count[, c("ID (Site)", "Depth sed [m]", "pollen_count", "Abi [#]", "Fag [#]", "Pin [#]", "Bet [#]", "Aln [#]")]),
                      ]

ref <- dataset_count[,c("ID (Site)", "Depth sed [m]", "Que [#]")]
ref$row<- 1:nrow(ref)
ref <- data.frame(ref)
names(ref) <- c("siteid", "depth", "Quercus.all", "row")


que <- pollen_counts[,c("siteid", "depth",
                  "Quercus", "Quercus.evg.type")]
que$Quercus.all <- que$Quercus + que$Quercus.evg.type
que$depth <- round(que$depth/100,2)

test <- right_join(ref, que)
dataset_count$