#FIG. 19 - Spectator recurring publishers

spectatorPublishers <- spectator %>%
  tidyr::separate_rows(actor_roles_all) %>%
  filter(actor_roles_all == "publisher") %>%
  group_by(publication_year, publication_decade, actor_id) %>%
  dplyr::summarise(n=n(), dup=duplicated(actor_id)) %>%
  group_by(publication_decade, dup) %>%
  dplyr::summarise(sum = sum(n))

fig19Abs <- ggplot(data = spectatorPublishers, aes(fill=dup, y = sum, x = publication_decade)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = palette) +
  ggtitle("Spectator recurring publishers - number")

png(file="../../../output/figures/fig19_spec_pub_rec_num.png",
    width=1200, height=700)

print(fig19Abs)

dev.off()


fig19Pre <- ggplot(data = spectatorPublishers, aes(fill=dup, y = sum, x = publication_decade)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = palette) +
  ggtitle("Spectator recurring publishers - precentage")

png(file="../../../output/figures/fig19_spec_pub_rec_pre.png",
    width=1200, height=700)

print(fig19Pre)

dev.off()