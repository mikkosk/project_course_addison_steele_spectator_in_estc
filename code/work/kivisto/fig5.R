#FIG. 5 - Steele & Addison publishing locations by year (number)
groupLocation <- adSt %>% group_by(actor_id, publication_place_752, publication_year) %>% dplyr::summarise(n = n())

groupLocation <- arrange(groupLocation, n)

fig5S <- ggplot(data = groupLocation[which(groupLocation$actor_id == '22167754'), ], aes(fill=publication_place_752, y = n, x = publication_year)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = palette) +
  ggtitle("Publishing locations Steele")

png(file="../Omat jutut/figures/fig5_St_pub_loc.png",
    width=1200, height=700)

print(fig5S)

dev.off()

fig5A <- ggplot(data = groupLocation[which(groupLocation$actor_id == '7413288'), ], aes(fill=publication_place_752, y = n, x = publication_year)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = palette)+
  ggtitle("Publishing locations Addison")

png(file="../../../output/figures/fig5_addison_pub_loc.png",
    width=1200, height=700)

print(fig5A)

dev.off()
