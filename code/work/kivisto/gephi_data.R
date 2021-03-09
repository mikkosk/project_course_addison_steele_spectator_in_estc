addison_gephi <- allData %>%
  filter(id %in% addison$id) %>%
  mutate(set_type = "Addison")

steele_gephi <- allData %>%
  filter(id %in% steele$id) %>%
  mutate(set_type = "Steele")

swift_gephi <- allData %>%
  filter(id %in% swift$id) %>%
  mutate(set_type = "Swift")

tonson_gephi <- allData %>%
  filter(id %in% tonson$id) %>%
  mutate(set_type = "Tonson")

spectator_gephi <- spectator %>%
  mutate(set_type = "Spectator")


whigs = c("Addison", "Steele", "Tonson", "Spectator")

tories = c("Swift")

all_gephi <- spectator[1:26] %>%
  mutate(set_type ="Specatator") %>%
  rbind(addison_gephi) %>%
  rbind(steele_gephi) %>%
  rbind(tonson_gephi) %>%
  rbind(swift_gephi) %>%
  group_by(actor_id) %>%
  mutate(final_type = paste0(sort(unique(set_type)), collapse=",")) %>%
  broader_location() %>%
  mutate(countries = paste0(sort(unique(broader_location)), collapse = ",")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(whig_tory = ifelse(any(sapply(tories, function(x) grepl(x, final_type, fixed = TRUE))), ifelse(any(sapply(whigs, function(x) grepl(x, final_type, fixed = TRUE))), "Both", "Tory"), "Whig")) %>%
  ungroup() %>%
  distinct(id, actor_id, final_type, country_752, whig_tory, .keep_all = TRUE)

nodes_publisher <- all_gephi %>%
  filter(!is.na(actor_id)) %>%
  filter(actor_id != "") %>%
  tidyr::separate_rows(actor_roles_all) %>%
  filter(actor_roles_all == "publisher") %>%
  group_by(actor_id) %>%
  mutate(name = first(actor_name_primary)) %>%
  group_by(actor_id, name, final_type, countries, whig_tory) %>%
  dplyr::summarise(publications = n()) %>% 
  rename(Id = actor_id, Label = name) %>%
  ungroup()

edges_publisher <- all_gephi %>%
  mutate(Source = actor_id) %>%
  mutate(Target = actor_id) %>%
  group_by(id) %>%
  tidyr::complete(Source, Target) %>%
  filter(Source != Target) %>%
  rowwise() %>%
  mutate(key = paste0(sort(c(Source,Target)), collapse = "")) %>%
  distinct(key, .keep_all = TRUE) %>%
  group_by(Source, Target) %>%
  dplyr::summarise(Weight = n()) %>% 
  mutate(edge = "publisher") %>%
  filter(Source %in% nodes_publisher$Id) %>%
  filter(Target %in% nodes_publisher$Id)

actors <- all_gephi %>%
  distinct(actor_id, .keep_all = TRUE) %>%
  tidyr::separate_rows(actor_roles_all) %>%
  filter(actor_roles_all == "publisher") %>%
  group_by(actor_id) %>%
  mutate(name = first(actor_name_primary)) %>%
  group_by(actor_id, name, final_type, countries, whig_tory) %>%
  dplyr::summarise(publications = n()) %>% 
  rename(Id = actor_id, Label = name)

work <- all_gephi %>%
  filter(!is.na(finalWorkField)) %>%
  filter(finalWorkField != "") %>%
  group_by(finalWorkField) %>%
  mutate(title = first(title)) %>%
  ungroup() %>%
  rename(Id = finalWorkField, Label = title) %>%
  mutate(final_type = "PUBLICATION") %>%
  group_by(Id, Label, final_type) %>%
  dplyr::summarise(publications = n())

nodes_works <- work %>% rbind(actors) %>% ungroup() %>% filter(!Id %in% nodes_publisher$Id)

edges_works <- all_gephi %>%
  filter(!is.na(finalWorkField)) %>%
  filter(finalWorkField != "") %>%
  filter(actor_id != "") %>%
  filter(!is.na(actor_id)) %>%
  tidyr::separate_rows(actor_roles_all) %>%
  filter(actor_roles_all == "publisher") %>%
  group_by(finalWorkField, actor_id) %>%
  dplyr::summarise(Weight = n()) %>%
  rename(Target = actor_id , Source = finalWorkField) %>%
  mutate(edge = "works")

final_edges <- edges_publisher %>% rbind(edges_works)
final_nodes <- nodes_publisher %>% rbind(nodes_works)

write.csv(final_edges,"C:/Users/mikko/OneDrive/Työpöytä/Gradu/edges.csv", row.names = FALSE)
write.csv(final_nodes,"C:/Users/mikko/OneDrive/Työpöytä/Gradu/nodes.csv", row.names = FALSE)
##write.csv(edges_works,"C:/Users/mikko/OneDrive/Työpöytä/Gradu/edges_works.csv", row.names = FALSE)
##write.csv(nodes_works,"C:/Users/mikko/OneDrive/Työpöytä/Gradu/nodes_works.csv", row.names = FALSE)

