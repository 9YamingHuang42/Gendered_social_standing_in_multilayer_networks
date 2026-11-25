library(dagitty)
library(dplyr)
library(tidyr)
library(tibble)
library(statnet)
library(igraph)
library(rethinking)
library(gt)
library(gto)
library(sf)
library(elevatr)
library(raster)
library(ggrepel)
library(ggdist)
library(ggh4x)

#1 Dag ----
Dag <-  
  dagitty('dag {
bb="-0.5,-0.5,0.5,0.5"
"Kinship system" [pos="-0.349,0.076"]
"Market participation" [pos="0.300,0.000"]
"Network centrality" [outcome,pos="0.000,0.320"]
Age [pos="0.000,-0.380"]
Gender [pos="0.200,-0.300"]
Inheritance [pos="-0.200,-0.300"]
Residence [pos="-0.359,-0.100"]
U_Individual [latent,pos="0.205,0.229"]
U_Village [latent,pos="-0.192,0.232"]
"Kinship system" -> "Market participation"
"Kinship system" -> "Network centrality"
"Market participation" -> "Network centrality"
Age -> "Market participation"
Age -> "Network centrality"
Age -> Inheritance
Gender -> "Market participation"
Gender -> "Network centrality"
Gender -> Inheritance
Inheritance -> "Kinship system"
Inheritance -> "Market participation"
Inheritance -> "Network centrality"
Inheritance -> Residence
Residence -> "Market participation"
Residence -> "Network centrality"
U_Individual -> "Network centrality"
U_Village -> "Network centrality"
}
')

#2 Data preparation ----
load(file = "Gendered_social_standing_in_multilayer_networks.RData")

##2.1 Receive recognition and structural position ----
###2.1.1 Social networks ----
Social.igraph <- igraph::graph_from_data_frame(
  d = Social.ties[, c("Ego.ID", "Alter.ID", "weight", "Ego.VID", "Alter.VID")],
  vertices = Indiv.data,
  directed = T) 

Social.igraph.consan <- igraph::graph_from_data_frame(
  d = Social.ties[, c("Ego.ID", "Alter.ID", "Alter.consanguineal", "Ego.VID", "Alter.VID")] %>% 
    filter(Alter.consanguineal==1),
  vertices = Indiv.data,
  directed = T) 

Social.igraph.affine <- igraph::graph_from_data_frame(
  d = Social.ties[, c("Ego.ID", "Alter.ID", "Alter.affinal", "Ego.VID", "Alter.VID")] %>% 
    filter(Alter.affinal==1),
  vertices = Indiv.data,
  directed = T) 

Social.igraph.fri <- igraph::graph_from_data_frame(
  d = Social.ties[, c("Ego.ID", "Alter.ID", "Alter.friend", "Ego.VID", "Alter.VID")] %>% 
    filter(Alter.friend==1),
  vertices = Indiv.data,
  directed = T) 

###2.1.2 Reputation networks ----
Reputation.igraph <- igraph::graph_from_data_frame(
  d = Reputation.ties[, c("Ego.ID", "Alter.ID", "weight", "Ego.VID", "Alter.VID")] %>% 
    filter(weight!=0),
  vertices = Indiv.data,
  directed = T) 

Reputation.igraph.generosity <- igraph::graph_from_data_frame(
  d = Reputation.ties[, c("Ego.ID", "Alter.ID", "Generous", "Ego.VID", "Alter.VID")] %>% 
    filter(Generous==1),
  vertices = Indiv.data,
  directed = T) 

Reputation.igraph.religiosity <- igraph::graph_from_data_frame(
  d = Reputation.ties[, c("Ego.ID", "Alter.ID", "Religious", "Ego.VID", "Alter.VID")] %>% 
    filter(Religious==1),
  vertices = Indiv.data,
  directed = T) 

Reputation.igraph.influence <- igraph::graph_from_data_frame(
  d = Reputation.ties[, c("Ego.ID", "Alter.ID", "Influential", "Ego.VID", "Alter.VID")] %>% 
    filter(Influential==1),
  vertices = Indiv.data,
  directed = T) 

Reputation.igraph.knowledge <- igraph::graph_from_data_frame(
  d = Reputation.ties[, c("Ego.ID", "Alter.ID", "Knowledgeable", "Ego.VID", "Alter.VID")] %>% 
    filter(Knowledgeable==1),
  vertices = Indiv.data,
  directed = T) 

###2.1.3 Calculation ----
####2.1.3.1 Node- and structure-level ----
make_indicator <- function(g,
                           ids,
                           network.type,
                           network.domain,
                           seed = 123,
                           damping = 0.85) {
  # node detection
  degree.in <- igraph::degree(g, mode = "in") %>% as.integer()
  pagerank <- page_rank(g, directed = TRUE, damping = damping)$vector %>% as.numeric()
  
  between <- igraph::betweenness(g) %>% as.numeric()
  harmonic.in <- harmonic_centrality(g, mode = "in") %>% as.numeric()
  
  g_ud <- as_undirected(g, mode = "collapse")
  triangle <- count_triangles(g_ud) %>% as.integer()
  
  # community detection
  set.seed(seed)
  comm <- cluster_louvain(g_ud)
  memb <- membership(comm)
  comm.sizes <- sizes(comm)
  
  community.id <- memb %>% as.integer()
  community.size <- unname(comm.sizes[as.character(memb)]) %>% as.integer()
  
  tibble(Ego.ID = ids) %>%
    mutate(Degree.in = degree.in,
           Page.rank = pagerank,
           Between = between,
           Harmonic.in = harmonic.in,
           Triangle = triangle,
           Community.ID = community.id,
           Community.size = community.size,
           Network.type = network.type,
           Network.domain = network.domain)
}

Social.indiv.indicator.consan <- make_indicator(
  g = Social.igraph.consan,
  ids = Indiv.data$ID,
  network.type = "Consanguineal",
  network.domain = "Social",
  seed = 123)
Social.indiv.indicator.affine <- make_indicator(
  g = Social.igraph.affine,
  ids = Indiv.data$ID,
  network.type = "Affinal",
  network.domain = "Social",
  seed = 123)
Social.indiv.indicator.fri <- make_indicator(
  g = Social.igraph.fri,
  ids = Indiv.data$ID,
  network.type = "Friend",
  network.domain = "Social",
  seed = 123)

Reputation.indiv.indicator.generosity <- make_indicator(
  g = Reputation.igraph.generosity,
  ids = Indiv.data$ID,
  network.type = "Generosity",
  network.domain = "Reputation",
  seed = 123)
Reputation.indiv.indicator.religiosity <- make_indicator(
  g = Reputation.igraph.religiosity,
  ids = Indiv.data$ID,
  network.type = "Religiosity",
  network.domain = "Reputation",
  seed = 123)
Reputation.indiv.indicator.influence <- make_indicator(
  g = Reputation.igraph.influence,
  ids = Indiv.data$ID,
  network.type = "Influence",
  network.domain = "Reputation",
  seed = 123)
Reputation.indiv.indicator.knowledge <- make_indicator(
  g = Reputation.igraph.knowledge,
  ids = Indiv.data$ID,
  network.type = "Knowledge",
  network.domain = "Reputation",
  seed = 123)

Indiv.indicator.all <- dplyr::bind_rows(
  Social.indiv.indicator.consan,
  Social.indiv.indicator.affine,
  Social.indiv.indicator.fri,
  Reputation.indiv.indicator.generosity,
  Reputation.indiv.indicator.religiosity,
  Reputation.indiv.indicator.influence,
  Reputation.indiv.indicator.knowledge) %>% 
  dplyr::rename(ID = Ego.ID) %>% 
  left_join(Indiv.data, by = c("ID")) %>% 
  mutate(Network.type = factor(Network.type,
                               levels = c("Consanguineal", "Affinal", "Friend",
                                          "Generosity", "Religiosity",
                                          "Influence", "Knowledge"),
                               labels = c("Consanguineal", "Affinal", "Friend",
                                          "Generosity", "Religiosity",
                                          "Influence", "Knowledge")),
         Network.domain = factor(Network.domain,
                                 levels = c("Social", "Reputation"),
                                 labels = c("Social", "Reputation")),
         Between_0 = ifelse(Between > 0, 1, 0),
         Harmonic_in_0 = ifelse(Harmonic.in > 0, 1, 0),
         Page.rank.e2 = Page.rank*100,
         Community.size.log = log(Community.size))

Between_pos_idx <- which(Indiv.indicator.all$Between > 0)
Harmonic_in_pos_idx <- which(Indiv.indicator.all$Harmonic.in > 0)

##2.2 Multiplex position ----
###2.2.1 Cross-layer recognition----
Multiplex.networks <- tibble(Network.type = c("Consanguineal", "Affinal", "Friend",
                                              "Generosity", "Religiosity",
                                              "Influence", "Knowledge"),
                             Network.domain = c("Social", "Social", "Social",
                                                "Reputation", "Reputation", 
                                                "Reputation", "Reputation"),
                             graph = list(Social.igraph.consan,
                                          Social.igraph.affine,
                                          Social.igraph.fri,
                                          Reputation.igraph.generosity,
                                          Reputation.igraph.religiosity,
                                          Reputation.igraph.influence,
                                          Reputation.igraph.knowledge))

get_networks_participation_by_type <- function(graph_list, ids) {
  in_participation <- purrr::map(graph_list$graph, ~ igraph::degree(.x, mode = "in") > 0) %>%
    set_names(graph_list$Network.type) %>%
    as_tibble()
  
  tibble(ID = ids) %>%
    bind_cols(in_participation %>% rename_with(~ paste0(.x, ".in.active"))) %>% 
    mutate(across(ends_with(".in.active"), ~ as.integer(.x)))
}

Multiplex.networks.participation.type <- get_networks_participation_by_type(graph_list = Multiplex.networks,
                                                                            ids = Indiv.data$ID) 

get_networks_participation_by_domain <- function(graph_list, ids) {
  domain_levels <- unique(graph_list$Network.domain)
  in_list <- list()
  
  for (dom in domain_levels) {
    g_sub <- graph_list %>% filter(Network.domain == dom)
    
    in_list[[dom]] <- reduce(purrr::map(g_sub$graph, ~ igraph::degree(.x, mode = "in") > 0),  `|`)
  }
  
  in_participation  <- as_tibble(in_list) %>% rename_with(~ paste0(.x, ".in.active"))
  
  tibble(ID = ids) %>%
    bind_cols(in_participation) %>%
    mutate(across(ends_with(".in.active"), ~ as.integer(.x))) %>% 
    mutate(If.networks.participation.domain.in = case_when(Social.in.active==1&Reputation.in.active==1 ~ 1,
                                                           TRUE ~ 0 ))
}

Multiplex.networks.participation.domain <- get_networks_participation_by_domain(graph_list = Multiplex.networks,
                                                                                ids = Indiv.data$ID) %>% 
  left_join(Indiv.data,
            by = c("ID")) %>% 
  left_join(Multiplex.networks.participation.type,
            by = c("ID")) %>% 
  mutate(Con.gene.cross = Consanguineal.in.active * Generosity.in.active,
         Con.reli.cross = Consanguineal.in.active * Religiosity.in.active,
         Con.influ.cross = Consanguineal.in.active * Influence.in.active,
         Con.know.cross = Consanguineal.in.active * Knowledge.in.active,
         
         Aff.gene.cross = Affinal.in.active * Generosity.in.active,
         Aff.reli.cross = Affinal.in.active * Religiosity.in.active,
         Aff.influ.cross = Affinal.in.active * Influence.in.active,
         Aff.know.cross = Affinal.in.active * Knowledge.in.active,
         
         Fri.gene.cross = Friend.in.active * Generosity.in.active,
         Fri.reli.cross = Friend.in.active * Religiosity.in.active,
         Fri.influ.cross = Friend.in.active * Influence.in.active,
         Fri.know.cross = Friend.in.active * Knowledge.in.active)

Multiplex_cross_long <- Multiplex.networks.participation.domain %>%
  dplyr::select(ID, 
                Con.gene.cross, Con.reli.cross, Con.influ.cross, Con.know.cross,
                Aff.gene.cross, Aff.reli.cross, Aff.influ.cross, Aff.know.cross,
                Fri.gene.cross, Fri.reli.cross, Fri.influ.cross, Fri.know.cross) %>%
  pivot_longer(cols = ends_with(".cross"),
               names_to = "Cross.type",
               values_to = "Cross.active") %>%
  mutate(Cross.type = factor(Cross.type,
                             levels = c("Con.gene.cross", "Con.reli.cross", "Con.influ.cross", "Con.know.cross",
                                        "Aff.gene.cross", "Aff.reli.cross", "Aff.influ.cross", "Aff.know.cross",
                                        "Fri.gene.cross", "Fri.reli.cross", "Fri.influ.cross", "Fri.know.cross")) %>% as.integer(),
         ID = as.integer(factor(ID))) %>% 
  left_join(Indiv.data,
            by = c("ID"))

###2.2.2 Cross-layer ties ----
get_cross_networks_ties <- function(graph_list, ids) {
  n <- vcount(graph_list$graph[[1]])
  
  social_graphs <- graph_list %>% filter(Network.domain == "Social") %>% pull(graph)
  rep_graphs <- graph_list %>% filter(Network.domain == "Reputation") %>% pull(graph)
  
  cross_in <- integer(n)
  
  for (i in seq_len(n)) {
    social_in <- unique(unlist(purrr::map(social_graphs, ~ as.integer(neighbors(.x, i, mode = "in")))))
    rep_in <- unique(unlist(purrr::map(rep_graphs,    ~ as.integer(neighbors(.x, i, mode = "in")))))
    cross_in[i] <- length(intersect(social_in, rep_in))
  }
  
  tibble(ID = ids,
         Cross.network.domain.tie.count.in  = cross_in,
         If.cross.network.domain.in  = ifelse(cross_in > 0, 1, 0) %>% as.integer())
}

Cross.network.ties <- get_cross_networks_ties(graph_list = Multiplex.networks,
                                              ids = Indiv.data$ID) %>% 
  left_join(Indiv.data,
            by = c("ID"))

get_cross_network_ties_by_pair <- function(graph_list, ids) {
  
  social_layers <- graph_list %>%
    filter(Network.domain == "Social")
  
  reputation_layers <- graph_list %>%
    filter(Network.domain == "Reputation")
  
  n <- igraph::vcount(graph_list$graph[[1]])
  
  reference_names <- igraph::V(graph_list$graph[[1]])$name
  
  same_vertices <- purrr::map_lgl(graph_list$graph,
                                  ~ identical(igraph::V(.x)$name, reference_names))
  
  if (!all(same_vertices)) {
    stop("Vertex names or vertex order differ across network layers.")
  }
  
  result <- tibble(ID = ids)
  
  for (s in seq_len(nrow(social_layers))) {
    for (r in seq_len(nrow(reputation_layers))) {
      
      social_name <- social_layers$Network.type[s]
      reputation_name <- reputation_layers$Network.type[r]
      
      g_social <- social_layers$graph[[s]]
      g_reputation <- reputation_layers$graph[[r]]
      
      overlap_count <- integer(n)
      
      for (i in seq_len(n)) {
        
        social_in <- as.integer(igraph::neighbors(g_social, i, mode = "in"))
        
        reputation_in <- as.integer(igraph::neighbors(g_reputation, i, mode = "in"))
        
        overlap_count[i] <- length(intersect(social_in, reputation_in))
      }
      
      pair_name <- paste(social_name, reputation_name, sep = ".")
      
      result[[paste0(pair_name, ".count")]] <- overlap_count
      
      result[[paste0(pair_name, ".active")]] <- as.integer(overlap_count > 0)
    }
  }
  result
}

Cross.network.ties.pair <- get_cross_network_ties_by_pair(graph_list = Multiplex.networks,
                                                          ids = Indiv.data$ID) %>%
  left_join(Indiv.data, by = "ID")

Cross.network.ties.pair.long <- Cross.network.ties.pair %>%
  dplyr::select(ID, ends_with(".active")) %>%
  pivot_longer(cols = ends_with(".active"),
               names_to = "Cross.pair",
               values_to = "Cross.active") %>%
  mutate(Cross.pair = sub("\\.active$", "", Cross.pair),
         ID = as.integer(factor(ID))) %>% 
  mutate(Cross.pair = factor(Cross.pair,
                             levels = c("Consanguineal.Generosity", "Consanguineal.Religiosity",
                                        "Consanguineal.Influence", "Consanguineal.Knowledge",
                                        
                                        "Affinal.Generosity", "Affinal.Religiosity",
                                        "Affinal.Influence", "Affinal.Knowledge",
                                        
                                        "Friend.Generosity", "Friend.Religiosity",
                                        "Friend.Influence", "Friend.Knowledge")),
         Cross.type = as.integer(Cross.pair)) %>% 
  left_join(Indiv.data,
            by = c("ID"))

##2.3 Data for models ----
Model_list <- with(Indiv.indicator.all, list(
  N = 8484,
  
  Gender = as.integer(Gender), 
  Net_type = as.integer(Network.type),
  Net_domain = as.integer(Network.domain),
  AgeC = as.integer(Age),
  Inheritance = as.integer(Inheritance),
  Residence = as.integer(Residence),
  Kinship = as.integer(Kinship),
  Market = Market ,
  ID = as.integer(ID),
  VID = as.integer(VID),
  
  Degree_in = Degree.in, 
  Page_rank = Page.rank.e2,
  
  Between = Between,
  N_between_pos = length(Between_pos_idx),
  Between_0 = Between_0,
  Between_pos_idx = Between_pos_idx,
  Between_pos_log = log(Between[Between_pos_idx]),
  
  Harmonic_in = Harmonic.in,
  N_harmonic_in_pos = length(Harmonic_in_pos_idx),
  Harmonic_in_0 = Harmonic_in_0,
  Harmonic_in_pos_idx = Harmonic_in_pos_idx,
  Harmonic_in_pos = Harmonic.in[Harmonic_in_pos_idx],
  Net_domain_in_pos = as.integer(Network.domain)[Harmonic_in_pos_idx],
  Gender_in_pos = as.integer(Gender)[Harmonic_in_pos_idx],
  Market_in_pos = Market[Harmonic_in_pos_idx],
  AgeC_in_pos = as.integer(Age)[Harmonic_in_pos_idx],
  Inheritance_in_pos = as.integer(Inheritance)[Harmonic_in_pos_idx],
  Residence_in_pos = as.integer(Residence)[Harmonic_in_pos_idx],
  Kinship_in_pos = as.integer(Kinship)[Harmonic_in_pos_idx],
  VID_in_pos = as.integer(VID)[Harmonic_in_pos_idx],
  ID_in_pos = as.integer(ID)[Harmonic_in_pos_idx],
  
  Triangle = Triangle,
  Community_size_log = Community.size.log))

Model_multiplex_networks_participation_list <- with(Multiplex.networks.participation.domain, list(
  Gender = as.integer(Gender), 
  AgeC = as.integer(Age),
  Inheritance = as.integer(Inheritance),
  Residence = as.integer(Residence),
  Kinship = as.integer(Kinship),
  Market = Market ,
  ID = as.integer(ID),
  VID = as.integer(VID),
  If_multinetworks_participation = If.networks.participation.domain.in))

Model_multiplex_networks_participation_long_list <- with(Multiplex_cross_long, list(
  Cross_active = Cross.active,
  Gender = as.integer(Gender), 
  AgeC = as.integer(Age),
  Inheritance = as.integer(Inheritance),
  Residence = as.integer(Residence),
  Kinship = as.integer(Kinship),
  Market = Market ,
  ID = as.integer(ID),
  VID = as.integer(VID),
  Cross_type = Cross.type))

Model_cross_network_ties_list <- with(Cross.network.ties, list(
  If_cross_network = If.cross.network.domain.in,
  Gender = as.integer(Gender), 
  AgeC = as.integer(Age),
  Inheritance = as.integer(Inheritance),
  Residence = as.integer(Residence),
  Kinship = as.integer(Kinship),
  Market = Market ,
  ID = as.integer(ID),
  VID = as.integer(VID)))

Model_cross_network_ties_long_list <- with(Cross.network.ties.pair.long, list(
  Cross_active = Cross.active,
  Cross_type = Cross.type,
  Gender = as.integer(Gender), 
  AgeC = as.integer(Age),
  Inheritance = as.integer(Inheritance),
  Residence = as.integer(Residence),
  Kinship = as.integer(Kinship),
  Market = Market ,
  ID = as.integer(ID),
  VID = as.integer(VID)))