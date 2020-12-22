#FIG. 15 - Count of texts from the Spectator by decade
spectatorUniqueDecade <- spectator %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(publication_decade) %>%
  dplyr::summarise(n = n())

fig15 <- ggplot(spectatorUniqueDecade, aes(publication_decade, y = n)) +
  geom_smooth(fill = "white") +
  geom_bar(stat="identity")+
  ggtitle("Count of texts from the Spectator by decade")

png(file="../../../output/figures/fig15_spectator_count_decade.png",
    width=1200, height=700)

print(fig15)

dev.off()
