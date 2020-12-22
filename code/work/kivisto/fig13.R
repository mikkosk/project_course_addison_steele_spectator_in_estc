#FIG. 13 - Recurring publishers in Addison publications
publishersAddison <- filter(allData, id %in% addison$id) %>% 
  filter(actor_id != 7413288) %>%
  tidyr::separate_rows(actor_roles_all) %>%
  filter(actor_roles_all == "publisher") %>%
  group_by(publication_year, publication_decade, actor_id) %>%
  dplyr::summarise(n=n(), dup=duplicated(actor_id)) %>%
  group_by(publication_decade, dup) %>%
  dplyr::summarise(sum = sum(n))

fig13Abs <- ggplot(data = publishersAddison, aes(fill=dup, y = sum, x = publication_decade)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = palette)+
  ggtitle("Recurring publishers in Addison publications")

png(file="../../../output/figures/fig13_addison_pub_abs.png",
    width=1200, height=700)

print(fig13Abs)

dev.off()

fig13Pre <- ggplot(data = publishersAddison, aes(fill=dup, y = sum, x = publication_decade)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = palette)+
  ggtitle("Recurring publishers in Addison publications")

png(file="../../../output/figures/fig13_addison_pub_pre.png",
    width=1200, height=700)

print(fig13Pre)

dev.off()