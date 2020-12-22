#FIG. 17 - Spectator publishing locations - precentage

spectatorLocation <- spectator %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(publication_place_752, publication_year) %>%
  dplyr::summarise(n = n())

locationCount <- arrange(spectatorLocation, n)

fig17 <- ggplot(data = locationCount, aes(fill=publication_place_752, y = n, x = publication_year)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = palette)+
  ggtitle("Spectator publishing locations - precentage")

png(file="../../../output/figures/fig17_spec_pub_loc_pre.png",
    width=1200, height=700)

print(fig17)

dev.off()