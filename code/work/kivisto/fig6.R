#FIG. 6 - Steele & Addison publishing locations by year (percentage)
groupLocation <- adSt %>% group_by(actor_id, publication_place_752, publication_year) %>% dplyr::summarise(n = n())

groupLocation <- arrange(groupLocation, n)

fig6S <- ggplot(data = groupLocation[which(groupLocation$actor_id == '22167754'), ], aes(fill=publication_place_752, y = n, x = publication_year)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = palette) +
  ggtitle("Publishing locations Steele (%)")

png(file="../Omat jutut/figures/fig6_St_pub_loc.png",
    width=1200, height=700)

print(fig6S)

dev.off()

fig6A <- ggplot(data = groupLocation[which(groupLocation$actor_id == '7413288'), ], aes(fill=publication_place_752, y = n, x = publication_year)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = palette) +
  ggtitle("Publishing locations Addison (%)")

png(file="../../../output/figures/fig6_addison_pub_loc.png",
    width=1200, height=700)

print(fig6A)

dev.off()
