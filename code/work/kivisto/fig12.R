#FIG. 12 - Recurring publishers in Steele publications

publishersSteele <- filter(allData, id %in% steele$id) %>% 
  filter(actor_id != 22167754) %>%
  tidyr::separate_rows(actor_roles_all) %>%
  filter(actor_roles_all == "publisher") %>%
  group_by(publication_year, publication_decade, actor_id) %>%
  dplyr::summarise(n=n(), dup=duplicated(actor_id)) %>%
  group_by(publication_decade, dup) %>%
  dplyr::summarise(sum = sum(n))

fig12Abs <- ggplot(data = publishersSteele, aes(fill=dup, y = sum, x = publication_decade)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = palette)+
  ggtitle("Recurring publishers in Steele publications - number")

png(file="../../../output/figures/fig12_steele_pub_abs.png",
    width=1200, height=700)

print(fig12Abs)

dev.off()

fig12Pre <- ggplot(data = publishersSteele, aes(fill=dup, y = sum, x = publication_decade)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = palette)+
  ggtitle("Recurring publishers in Steele publications - precentage")

png(file="../../../output/figures/fig12_steele_pub_pre.png",
    width=1200, height=700)

print(fig12Pre)

dev.off()
