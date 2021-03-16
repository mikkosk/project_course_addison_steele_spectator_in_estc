draw13 <- function(data, position, title) { 
  data <- data %>%
    tidyr::separate_rows(actor_roles_all) %>%
    filter(actor_roles_all == "publisher") %>%
    filter(!is.na(actor_id)) %>%
    filter(actor_id != "") %>%
    group_by(publication_decade) %>%
    distinct(actor_id, .keep_all = TRUE) %>%
    group_by(publication_decade, is_organization) %>%
    dplyr::summarise(n = n())
  
  figure <- ggplot(data = data, aes(fill=is_organization, y = n, x = publication_decade)) +
    geom_bar(position=position, stat="identity") +
    scale_fill_manual(values = palette)+
    ggtitle(title)
  
  return(figure)
}

fig13.1 <- draw13(spectator, "fill", "Plain data")

fig13.2 <- draw13(estc_only, "fill", "ESTC only")

fig13.3 <- draw13(estc_and_bernard, "fill", "ESTC and Bernard")

fig13.4 <- draw13(pure_only, "fill", "Pure Spectator")

final13 <- fig13.1 + fig13.2 + fig13.3 + fig13.4

png(file="../../../output/figures/spectator/fig13_organization.png",
    width=1200, height=700)

print(final13)

dev.off()


