#FIG. 20 - Common publishers among the Spectator and other Addison & Steele works
adStNoSpecPub <- filter(allData, id %in% adSt$id) %>%
  filter(!id %in% spectator$id) %>%
  tidyr::separate_rows(actor_roles_all) %>%
  filter(actor_roles_all == "publisher") %>%
  distinct(actor_id, .keep_all = TRUE)

spectatorPub <- spectator %>%
  tidyr::separate_rows(actor_roles_all) %>%
  filter(actor_roles_all == "publisher") %>%
  distinct(actor_id, .keep_all = TRUE)

allPublishers <- adStNoSpecPub %>%
  rbind(spectatorPub) %>%
  group_by(actor_id) %>%
  dplyr::summarise(n = n()) %>%
  group_by(n) %>%
  dplyr::summarise(shared = n())

tf <- allPublishers$n == 2

allPublishers$sharedPublisher = tf

fig20 <- ggplot(data = allPublishers, aes(x="", y=shared, fill=sharedPublisher)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle("Common publishers among the Spectator and other Addison & Steele works")

png(file="../../../output/figures/fig20_common_pub.png",
    width=1200, height=700)

print(fig20)

dev.off()