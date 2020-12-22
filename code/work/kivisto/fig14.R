
#FIG. 14 - Number of texts from the Spectator (republications of Spectator or its usage in other publications)

spectatorUnique <- spectator %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(publication_year) %>%
  dplyr::summarise(n = n())

fig14 <- ggplot(data=spectatorUnique, aes(publication_year, y = n)) +
  geom_smooth(fill = "white") +
  geom_point()+
  ggtitle("The Spectator count")

png(file="../../../output/figures/fig14_spectator_count.png",
    width=1200, height=700)

print(fig14)

dev.off()