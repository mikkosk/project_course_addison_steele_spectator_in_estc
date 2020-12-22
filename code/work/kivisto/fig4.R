#FIG.4 - Cumulative number of all publications where Steele or Addison is actor over time

sortedByYear <- arrange(adSt, publication_year)
allCount <- sortedByYear %>% group_by(actor_id, publication_year) %>% dplyr::summarise(n = n())
allCount <- allCount %>% group_by(actor_id) %>% mutate(cs = cumsum(n))

fig4 <- ggplot(data = allCount, aes(publication_year, y = cs, color = actor_id)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1719, color = "blue") + 
  geom_vline(xintercept = 1729, color = "red") +
  scale_color_hue(labels = c("Steele", "Addison")) + 
  ggtitle("Addison & Steele all works over time\n-\ncumulative")

png(file="../../../output/figures/fig4_addison_steele_cumu.png",
    width=1200, height=700)

print(fig4)

dev.off()
