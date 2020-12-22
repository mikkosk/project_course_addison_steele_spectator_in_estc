#FIG.2 - Number of publications where Steele or Addison is an actor over time

adStCount <- adSt %>% group_by(actor_id, publication_year) %>% dplyr::summarise(n = n())

fig2 <- ggplot(data = adStCount, aes(publication_year, y = n, color = actor_id)) +
  geom_smooth(fill = "white") +
  geom_point() +
  geom_vline(xintercept = 1719, color = "blue") + 
  geom_vline(xintercept = 1729, color = "red") +
  scale_color_hue(labels = c("Steele", "Addison")) +
  ggtitle("Number of works where Adison or Steele is an actor")

png(file="../../../output/figures/fig2_addison_steele_count.png",
    width=1200, height=700)

print(fig2)

dev.off()
