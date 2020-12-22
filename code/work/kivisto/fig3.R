#FIG.3 - Cumulative number of unique publications where Steele or Addison is author over time
sortedByYear <- arrange(adSt, publication_year)
duplicates2 <- duplicated(sortedByYear$finalWorkField)
uniqueAdSt <- sortedByYear[!duplicates2, ]
uniqueCount <- uniqueAdSt %>% group_by(actor_id, publication_year) %>% dplyr::summarise(n = n())
uniqueCount <- uniqueCount %>% group_by(actor_id) %>% mutate(cs = cumsum(n))

fig3 <- ggplot(data = uniqueCount, aes(publication_year, y = cs, color = actor_id)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1719, color = "blue") + 
  geom_vline(xintercept = 1729, color = "red") +
  scale_color_hue(labels = c("Steele", "Addison")) +
  ggtitle("Addison & Steele unique works over time\n-\ncumulative")

png(file="../../../output/figures/fig3_addison_steele_cumu.png",
    width=1200, height=700)

print(fig3)

dev.off()
