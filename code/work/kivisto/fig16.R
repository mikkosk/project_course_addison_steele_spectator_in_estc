#FIG. 16 - Spectator publishing locations - number
spectatorLocation <- spectator %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(publication_place_752, publication_year) %>%
  dplyr::summarise(n = n())

locationCount <- arrange(spectatorLocation, n)

fig16 <- ggplot(data = locationCount, aes(fill=publication_place_752, y = n, x = publication_year)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = palette)+
  ggtitle("Spectator publishing locations - number")

png(file="../../../output/figures/fig16_spec_pub_loc_num.png",
    width=1200, height=700)

print(fig16)

dev.off()