Igraph.colors <- c(V1 = "#A6CEE3", V2 = "#3385BB", V3 = "#84BF96", V4 = "#6DBD57", V5 = "#7F9D55",
                   V6 = "#F57C7C", V7 = "#E42622", V8 = "#FBB268", V9 = "#FE8D19", V10 = "#DE9E83",
                   V11 = "#C6C3E1", V12 = "#977899", V13 = "#F3E587", V14 = "#B15928")

#1 Social network (all three types) ----
Social.igraph.node.colors <- adjustcolor(Igraph.colors[
  as.character(igraph::V(Social.igraph)$VID)], alpha.f = 0.85)
Social.igraph.edge.colors <- Igraph.colors[as.character(igraph::E(Social.igraph)$Alter.VID)]
Social.igraph.shape <- ifelse(igraph::V(Social.igraph)$Gender == "Female", "square", "circle")
Social.igraph.deg <- igraph::degree(Social.igraph, mode = "in")
Social.igraph.deg.rescale <- datawizard::rescale(sqrt(Social.igraph.deg + 1),
                                                 to = c(0.8, 6))
Social.igraph.layout <- layout_with_fr(Social.igraph, 
                                       niter = 500000, 
                                       start.temp = sqrt(vcount(Social.igraph))*30,
                                       grid = "nogrid") %>% 
  igraph::norm_coords(xmin = -2, xmax = 2,
                      ymin = -2, ymax = 2)

jpeg(width = 150, height = 140, units = "mm", res = 300)

par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
set.seed(123)
plot.igraph(Social.igraph,
            vertex.color = Social.igraph.node.colors,
            vertex.frame.color = adjustcolor("black", alpha = 0.6),
            vertex.frame.width = 0.05 ,
            vertex.size = Social.igraph.deg.rescale,
            vertex.shape = Social.igraph.shape, 
            vertex.label = NA,
            edge.arrow.size = 0.15 ,
            edge.width = 0.6 ,
            edge.color = Social.igraph.edge.colors,
            edge.curved = T, 
            layout = Social.igraph.layout)
dev.off()

#2 Consanguineal network ----
Consanguineal.igraph.node.colors <- adjustcolor(Igraph.colors[
  as.character(igraph::V(Social.igraph.consan)$VID)], alpha.f = 0.85)
Consanguineal.igraph.edge.colors <- Igraph.colors[as.character(igraph::E(Social.igraph.consan)$Alter.VID)]
Consanguineal.igraph.shape <- ifelse(igraph::V(Social.igraph.consan)$Gender == "Female", "square", "circle")
Consanguineal.igraph.deg <- igraph::degree(Social.igraph.consan, mode = "in")
Consanguineal.igraph.deg.rescale <- datawizard::rescale(Consanguineal.igraph.deg, to = c(1.5, 8))
Consanguineal.igraph.layout <- layout_with_fr(Social.igraph.consan, 
                                              niter = 300000, 
                                              start.temp = sqrt(vcount(Social.igraph.consan))*30,
                                              grid = "nogrid")

jpeg(width = 150, height = 140, units = "mm", res = 300)

par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
set.seed(123)
plot.igraph(Social.igraph.consan,
            vertex.color = Consanguineal.igraph.node.colors,
            vertex.frame.color = adjustcolor("black", alpha = 0.6),
            vertex.frame.width = 0.05,
            vertex.size = Consanguineal.igraph.deg.rescale,
            vertex.shape = Consanguineal.igraph.shape, 
            edge.arrow.size = 0.15,
            edge.width = 0.6,
            edge.color = Consanguineal.igraph.edge.colors,
            vertex.label = NA,
            layout = Consanguineal.igraph.layout,
            edge.curved = T, 
            vertex.frame.color = NA)
dev.off()

#3 Affinal network ----
Affinal.igraph.node.colors <- adjustcolor(Igraph.colors[
  as.character(igraph::V(Social.igraph.affine)$VID)], alpha.f = 0.85)
Affinal.igraph.edge.colors <- Igraph.colors[as.character(igraph::E(Social.igraph.affine)$Alter.VID)]
Affinal.igraph.shape <- ifelse(igraph::V(Social.igraph.affine)$Gender == "Female", "square", "circle")
Affinal.igraph.deg <- igraph::degree(Social.igraph.affine, mode = "in")
Affinal.igraph.deg.rescale <- datawizard::rescale(Affinal.igraph.deg, to = c(1.5, 8))
Affinal.igraph.layout <- layout_with_fr(Social.igraph.affine, 
                                        niter = 300000, 
                                        start.temp = sqrt(vcount(Social.igraph.affine))*30,
                                        grid = "nogrid") %>% 
  igraph::norm_coords(xmin = -2, xmax = 2,
                      ymin = -2, ymax = 2)

jpeg(width = 150, height = 140, units = "mm", res = 300)

par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
set.seed(123)
plot.igraph(Social.igraph.affine,
            vertex.color = Affinal.igraph.node.colors,
            vertex.frame.color = adjustcolor("black", alpha = 0.6),
            vertex.frame.width = 0.05,
            vertex.size = Affinal.igraph.deg.rescale,
            vertex.shape = Affinal.igraph.shape, 
            edge.arrow.size = 0.15,
            edge.width = 0.6,
            edge.color = Affinal.igraph.edge.colors,
            vertex.label = NA,
            layout = Affinal.igraph.layout,
            edge.curved = T, 
            vertex.frame.color = NA)
dev.off()

#4 Friendship network ----
Friend.igraph.node.colors <- adjustcolor(Igraph.colors[
  as.character(igraph::V(Social.igraph.fri)$VID)], alpha.f = 0.85)
Friend.igraph.edge.colors <- Igraph.colors[as.character(igraph::E(Social.igraph.fri)$Alter.VID)]
Friend.igraph.shape <- ifelse(igraph::V(Social.igraph.fri)$Gender == "Female", "square", "circle")
Friend.igraph.deg <- igraph::degree(Social.igraph.fri, mode = "in")
Friend.igraph.deg.rescale <- datawizard::rescale(Friend.igraph.deg, to = c(1.5, 8))
Friend.igraph.layout <- layout_with_fr(Social.igraph.fri, 
                                       niter = 300000, 
                                       start.temp = sqrt(vcount(Social.igraph.fri))*30,
                                       grid = "nogrid") %>% 
  igraph::norm_coords(xmin = -2, xmax = 2,
                      ymin = -2, ymax = 2)

jpeg(width = 150, height = 140, units = "mm", res = 300)

par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
set.seed(123)
plot.igraph(Social.igraph.fri,
            vertex.color = Friend.igraph.node.colors,
            vertex.frame.color = adjustcolor("black", alpha = 0.6),
            vertex.frame.width = 0.05,
            vertex.size = Friend.igraph.deg.rescale,
            vertex.shape = Friend.igraph.shape, 
            edge.arrow.size = 0.15,
            edge.width = 0.6,
            edge.color = Friend.igraph.edge.colors,
            vertex.label = NA,
            layout = Friend.igraph.layout,
            edge.curved = T, 
            vertex.frame.color = NA)
dev.off()

#5 Reputational network (all four types) ----
Reputation.igraph.node.colors <- adjustcolor(Igraph.colors[
  as.character(igraph::V(Reputation.igraph)$VID)], alpha.f = 0.7)
Reputation.igraph.edge.colors <- adjustcolor(Igraph.colors[
  as.character(igraph::E(Reputation.igraph)$Alter.VID)], alpha.f = 0.7)
Reputation.igraph.shape <- ifelse(igraph::V(Reputation.igraph)$Gender == "Female", "square", "circle")
Reputation.igraph.deg <- igraph::degree(Reputation.igraph, mode = "in")
Reputation.igraph.deg.rescale <- datawizard::rescale(Reputation.igraph.deg, to = c(1.5, 8))
Reputation.igraph.layout <- layout_with_fr(Reputation.igraph, 
                                           niter = 500000, 
                                           start.temp = sqrt(vcount(Reputation.igraph))*30,
                                           grid = "nogrid") %>% 
  igraph::norm_coords(xmin = -2, xmax = 2,
                      ymin = -2, ymax = 2)

jpeg(width = 150, height = 140, units = "mm", res = 300)

par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
set.seed(123)
plot.igraph(Reputation.igraph,
            vertex.color = Reputation.igraph.node.colors,
            vertex.frame.color = adjustcolor("black", alpha = 0.6),
            vertex.frame.width = 0.05 ,
            vertex.size = Reputation.igraph.deg.rescale,
            vertex.shape = Reputation.igraph.shape, 
            vertex.label = NA,
            edge.arrow.size = 0.15 ,
            edge.width = 0.6 ,
            edge.color = Reputation.igraph.edge.colors,
            edge.curved = T, 
            layout = Reputation.igraph.layout)
dev.off()

#6 Generosity network ----
Generosity.igraph.node.colors <- adjustcolor(Igraph.colors[
  as.character(igraph::V(Reputation.igraph.generosity)$VID)], alpha.f = 0.85)
Generosity.igraph.edge.colors <- Igraph.colors[as.character(igraph::E(Reputation.igraph.generosity)$Alter.VID)]
Generosity.igraph.shape <- ifelse(igraph::V(Reputation.igraph.generosity)$Gender == "Female", "square", "circle")
Generosity.igraph.deg <- igraph::degree(Reputation.igraph.generosity, mode = "in")
Generosity.igraph.deg.rescale <- datawizard::rescale(Generosity.igraph.deg, to = c(1.5, 8))
Generosity.igraph.layout <- layout_with_fr(Reputation.igraph.generosity, 
                                           niter = 300000, 
                                           start.temp = sqrt(vcount(Reputation.igraph.generosity))*30,
                                           grid = "nogrid") %>% 
  igraph::norm_coords(xmin = -2, xmax = 2,
                      ymin = -2, ymax = 2)

jpeg(width = 150, height = 140, units = "mm", res = 300)

par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
set.seed(123)
plot.igraph(Reputation.igraph.generosity,
            vertex.color = Generosity.igraph.node.colors,
            vertex.frame.color = adjustcolor("black", alpha = 0.6),
            vertex.frame.width = 0.05,
            vertex.size = Generosity.igraph.deg.rescale,
            vertex.shape = Generosity.igraph.shape, 
            edge.arrow.size = 0.15,
            edge.width = 0.6,
            edge.color = Generosity.igraph.edge.colors,
            vertex.label = NA,
            layout = Generosity.igraph.layout,
            edge.curved = T, 
            vertex.frame.color = NA)
dev.off()

#7 Religiosity network ----
Religiosity.igraph.node.colors <- adjustcolor(Igraph.colors[
  as.character(igraph::V(Reputation.igraph.religiosity)$VID)], alpha.f = 0.85)
Religiosity.igraph.edge.colors <- Igraph.colors[as.character(igraph::E(Reputation.igraph.religiosity)$Alter.VID)]
Religiosity.igraph.shape <- ifelse(igraph::V(Reputation.igraph.religiosity)$Gender == "Female", "square", "circle")
Religiosity.igraph.deg <- igraph::degree(Reputation.igraph.religiosity, mode = "in")
Religiosity.igraph.deg.rescale <- datawizard::rescale(Religiosity.igraph.deg, to = c(1.5, 8))
Religiosity.igraph.layout <- layout_with_fr(Reputation.igraph.religiosity, 
                                            niter = 300000, 
                                            start.temp = sqrt(vcount(Reputation.igraph.religiosity))*30,
                                            grid = "nogrid") %>% 
  igraph::norm_coords(xmin = -2, xmax = 2,
                      ymin = -2, ymax = 2)

jpeg(width = 150, height = 140, units = "mm", res = 300)

par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
set.seed(123)
plot.igraph(Reputation.igraph.religiosity,
            vertex.color = Religiosity.igraph.node.colors,
            vertex.frame.color = adjustcolor("black", alpha = 0.6),
            vertex.frame.width = 0.05,
            vertex.size = Religiosity.igraph.deg.rescale,
            vertex.shape = Religiosity.igraph.shape, 
            edge.arrow.size = 0.15,
            edge.width = 0.6,
            edge.color = Religiosity.igraph.edge.colors,
            vertex.label = NA,
            layout = Religiosity.igraph.layout,
            edge.curved = T, 
            vertex.frame.color = NA)
dev.off()

#8 Informal leadership network ----
Influence.igraph.node.colors <- adjustcolor(Igraph.colors[
  as.character(igraph::V(Reputation.igraph.influence)$VID)], alpha.f = 0.85)
Influence.igraph.edge.colors <- Igraph.colors[as.character(igraph::E(Reputation.igraph.influence)$Alter.VID)]
Influence.igraph.shape <- ifelse(igraph::V(Reputation.igraph.influence)$Gender == "Female", "square", "circle")
Influence.igraph.deg <- igraph::degree(Reputation.igraph.influence, mode = "in")
Influence.igraph.deg.rescale <- datawizard::rescale(Influence.igraph.deg, to = c(1.5, 8))
Influence.igraph.layout <- layout_with_fr(Reputation.igraph.influence, 
                                          niter = 300000, 
                                          start.temp = sqrt(vcount(Reputation.igraph.influence))*30,
                                          grid = "nogrid") %>% 
  igraph::norm_coords(xmin = -2, xmax = 2,
                      ymin = -2, ymax = 2)

jpeg(width = 150, height = 140, units = "mm", res = 300)

par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
set.seed(123)
plot.igraph(Reputation.igraph.influence,
            vertex.color = Influence.igraph.node.colors,
            vertex.frame.color = adjustcolor("black", alpha = 0.6),
            vertex.frame.width = 0.05,
            vertex.size = Influence.igraph.deg.rescale,
            vertex.shape = Influence.igraph.shape, 
            edge.arrow.size = 0.15,
            edge.width = 0.6,
            edge.color = Influence.igraph.edge.colors,
            vertex.label = NA,
            layout = Influence.igraph.layout,
            edge.curved = T, 
            vertex.frame.color = NA)
dev.off()

#9 Knowledge network ----
Knowledge.igraph.node.colors <- adjustcolor(Igraph.colors[
  as.character(igraph::V(Reputation.igraph.knowledge)$VID)], alpha.f = 0.85)
Knowledge.igraph.edge.colors <- Igraph.colors[as.character(igraph::E(Reputation.igraph.knowledge)$Alter.VID)]
Knowledge.igraph.shape <- ifelse(igraph::V(Reputation.igraph.knowledge)$Gender == "Female", "square", "circle")
Knowledge.igraph.deg <- igraph::degree(Reputation.igraph.knowledge, mode = "in")
Knowledge.igraph.deg.rescale <- datawizard::rescale(Knowledge.igraph.deg, to = c(1.5, 8))
Knowledge.igraph.layout <- layout_with_fr(Reputation.igraph.knowledge, 
                                          niter = 300000, 
                                          start.temp = sqrt(vcount(Reputation.igraph.knowledge))*30,
                                          grid = "nogrid") %>% 
  igraph::norm_coords(xmin = -2, xmax = 2,
                      ymin = -2, ymax = 2)

jpeg(width = 150, height = 140, units = "mm", res = 300)

par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
set.seed(123)
plot.igraph(Reputation.igraph.knowledge,
            vertex.color = Knowledge.igraph.node.colors,
            vertex.frame.color = adjustcolor("black", alpha = 0.6),
            vertex.frame.width = 0.05,
            vertex.size = Knowledge.igraph.deg.rescale,
            vertex.shape = Knowledge.igraph.shape, 
            edge.arrow.size = 0.15,
            edge.width = 0.6,
            edge.color = Knowledge.igraph.edge.colors,
            vertex.label = NA,
            layout = Knowledge.igraph.layout,
            edge.curved = T, 
            vertex.frame.color = NA)
dev.off()
