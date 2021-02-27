#FIG.1 - Number of unique publications over time

duplicates <- duplicated(allData$id)

uniquePublications <- allData[!duplicates, ]

uniquePublications <- uniquePublications[uniquePublications$publication_year < 1801, ]

amountByYear <- uniquePublications %>% count(publication_year)

fig <- ggplot(data=amountByYear, aes(publication_year, y = n)) +
  geom_smooth(fill = "white") +
  geom_point() +
  ggtitle("Count of unique works in ESTC")

png(file="../../../output/figures/fig1_estc_count.png",
    width=600, height=350)

print(fig)

dev.off()



