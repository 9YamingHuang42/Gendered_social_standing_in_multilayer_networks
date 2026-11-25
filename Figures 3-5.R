#1 Figure 3 ----
tibble(Value = c(Post_degree_GNet2_1$bGNet[,1],
                 Post_degree_GNet1_1$bGNet[,1],
                 Post_degree_GNet1_1$bGNet[,2],
                 Post_degree_GNet1_1$bGNet[,3],
                 Post_degree_GNet2_2$bGANet[,1,1], 
                 Post_degree_GNet2_2$bGANet[,2,1], 
                 Post_degree_GNet2_2$bGANet[,3,1], 
                 Post_degree_GNet2_2$bGANet[,4,1], 
                 Post_degree_GNet2_3$bGRNet[,1,1],
                 Post_degree_GNet2_3$bGRNet[,2,1],
                 Post_degree_GNet2_3$bGRNet[,3,1],
                 Post_degree_GNet2_4$bGKNet[,1,1],
                 Post_degree_GNet2_4$bGKNet[,2,1],
                 Post_degree_GNet2_4$bGKNet[,3,1],
                 Post_degree_GNet2_5$bGMNet[,1],
                 
                 Post_pagerank_GNet2_1$bGNet[,1],
                 Post_pagerank_GNet1_1$bGNet[,1],
                 Post_pagerank_GNet1_1$bGNet[,2],
                 Post_pagerank_GNet1_1$bGNet[,3],
                 Post_pagerank_GNet2_2$bGANet[,1,1], 
                 Post_pagerank_GNet2_2$bGANet[,2,1], 
                 Post_pagerank_GNet2_2$bGANet[,3,1], 
                 Post_pagerank_GNet2_2$bGANet[,4,1], 
                 Post_pagerank_GNet2_3$bGRNet[,1,1],
                 Post_pagerank_GNet2_3$bGRNet[,2,1],
                 Post_pagerank_GNet2_3$bGRNet[,3,1],
                 Post_pagerank_GNet2_4$bGKNet[,1,1],
                 Post_pagerank_GNet2_4$bGKNet[,2,1],
                 Post_pagerank_GNet2_4$bGKNet[,3,1],
                 Post_pagerank_GNet2_5$bGMNet[,1]),
       Variable = rep(c(rep("Overall domain", 14000), 
                        rep("Consanguineal", 14000), 
                        rep("Affinal", 14000), 
                        rep("Friendship", 14000), 
                        rep("<35", 14000), 
                        rep("35-49", 14000), 
                        rep("50-64", 14000), 
                        rep(">=65", 14000),
                        rep("Patrilocal", 14000), 
                        rep("Bilocal", 14000), 
                        rep("Matrilocal", 14000), 
                        rep("Matrilineal", 14000), 
                        rep("Non-unilineal", 14000), 
                        rep("Patrilineal", 14000), 
                        rep("Market", 14000)), 2),
       Model = rep(c(rep("Model 1", 14000), 
                     rep("Model 2", 42000), 
                     rep("Model 3", 56000), 
                     rep("Model 4", 42000), 
                     rep("Model 5", 42000),
                     rep("Model 6", 14000)),2),
       Type = c(rep("In-degree", 210000),
                rep("PageRank", 210000)),
       Domain = rep("Social", 420000)) %>% 
  mutate(Variable = factor(Variable,
                           levels = c("Market", 
                                      "Patrilineal", "Non-unilineal", "Matrilineal", 
                                      "Patrilocal", "Bilocal", "Matrilocal", 
                                      ">=65", "50-64", "35-49", "<35", 
                                      "Friendship", "Affinal", "Consanguineal", 
                                      "Overall domain"),
                           labels = c("Market", 
                                      "Patrilineal", "Non-unilineal", "Matrilineal", 
                                      "Patrilocal", "Bilocal", "Matrilocal", 
                                      ">=65", "50-64", "35-49", "<35", 
                                      "Friendship", "Affinal", "Consanguineal", 
                                      "Overall domain")),
         Model = factor(Model, 
                        levels = c("Model 1", "Model 2",
                                   "Model 3", "Model 4",
                                   "Model 5", "Model 6"), 
                        labels = c("Model 1", "Model 2",
                                   "Model 3", "Model 4",
                                   "Model 5", "Model 6")),
         Type = factor(Type,
                       levels = c("In-degree", "PageRank"),
                       labels = c("In-degree", "PageRank"))) %>% 
  ggplot(aes(x = Value, y = Variable, 
             fill = Model, color = Model)) +
  facet_grid(Domain ~ Type, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.8,
             color = "grey30") +
  stat_pointinterval(.width = .90,
                     point_interval = mean_qi,
                     point_size = 1.8,
                     interval_size = 1)+
  scale_x_continuous(limits = c(-1.0, 1.5), breaks = c(-1.0, -0.5, 0, 0.5, 1)) +
  scale_fill_manual(values = alpha(c("#E15759", "#F28E2B", "#B07AA1", "#4E79A7", "#76B7B2", "#59A14F"), 0.3)) +
  scale_color_manual(values = c("#E15759", "#F28E2B", "#B07AA1", "#4E79A7", "#76B7B2", "#59A14F")) +
  labs(x = "Posterior distribution of estimates",
       y = NULL,
       fill = NULL,
       colour = NULL) +
  theme_classic() +
  theme(plot.title = element_text(size=16, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
        strip.text = element_text(size = 14, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
        panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        axis.text.x = element_text(size = 12, colour = "black", face = "bold"),
        axis.text.y = element_text(size = 12, colour = "black", face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 8)),
        legend.position = c(0.995,0.995),
        legend.justification = c(0.995,0.995),
        legend.text = element_text(size = 8, face = "bold"))

tibble(Value = c(Post_degree_GNet2_1$bGNet[,2],
                 Post_degree_GNet1_1$bGNet[,4],
                 Post_degree_GNet1_1$bGNet[,5],
                 Post_degree_GNet1_1$bGNet[,6],
                 Post_degree_GNet1_1$bGNet[,7],
                 Post_degree_GNet2_2$bGANet[,1,2], 
                 Post_degree_GNet2_2$bGANet[,2,2], 
                 Post_degree_GNet2_2$bGANet[,3,2], 
                 Post_degree_GNet2_2$bGANet[,4,2], 
                 Post_degree_GNet2_3$bGRNet[,1,2],
                 Post_degree_GNet2_3$bGRNet[,2,2],
                 Post_degree_GNet2_3$bGRNet[,3,2],
                 Post_degree_GNet2_4$bGKNet[,1,2],
                 Post_degree_GNet2_4$bGKNet[,2,2],
                 Post_degree_GNet2_4$bGKNet[,3,2],
                 Post_degree_GNet2_5$bGMNet[,2],
                 
                 Post_pagerank_GNet2_1$bGNet[,2],
                 Post_pagerank_GNet1_1$bGNet[,4],
                 Post_pagerank_GNet1_1$bGNet[,5],
                 Post_pagerank_GNet1_1$bGNet[,6],
                 Post_pagerank_GNet1_1$bGNet[,7],
                 Post_pagerank_GNet2_2$bGANet[,1,2], 
                 Post_pagerank_GNet2_2$bGANet[,2,2], 
                 Post_pagerank_GNet2_2$bGANet[,3,2], 
                 Post_pagerank_GNet2_2$bGANet[,4,2], 
                 Post_pagerank_GNet2_3$bGRNet[,1,2],
                 Post_pagerank_GNet2_3$bGRNet[,2,2],
                 Post_pagerank_GNet2_3$bGRNet[,3,2],
                 Post_pagerank_GNet2_4$bGKNet[,1,2],
                 Post_pagerank_GNet2_4$bGKNet[,2,2],
                 Post_pagerank_GNet2_4$bGKNet[,3,2],
                 Post_pagerank_GNet2_5$bGMNet[,2]),
       Variable = rep(c(rep("Overall domain", 14000), 
                        rep("Generosity", 14000), 
                        rep("Religiosity", 14000), 
                        rep("Informal leadership", 14000), 
                        rep("Knowledge", 14000), 
                        rep("<35", 14000), 
                        rep("35-49", 14000), 
                        rep("50-64", 14000), 
                        rep(">=65", 14000),
                        rep("Patrilocal", 14000), 
                        rep("Bilocal", 14000), 
                        rep("Matrilocal", 14000), 
                        rep("Matrilineal", 14000), 
                        rep("Non-unilineal", 14000), 
                        rep("Patrilineal", 14000), 
                        rep("Market", 14000)), 2),
       Model = rep(c(rep("Model 1", 14000), 
                     rep("Model 2", 56000), 
                     rep("Model 3", 56000), 
                     rep("Model 4", 42000), 
                     rep("Model 5", 42000),
                     rep("Model 6", 14000)),2),
       Type = c(rep("In-degree", 224000),
                rep("PageRank", 224000)),
       Domain = rep("Reputational", 448000)) %>% 
  mutate(Variable = factor(Variable,
                           levels = c("Market", 
                                      "Patrilineal", "Non-unilineal", "Matrilineal", 
                                      "Patrilocal", "Bilocal", "Matrilocal", 
                                      ">=65", "50-64", "35-49", "<35", 
                                      "Knowledge", "Informal leadership", "Religiosity", "Generosity", 
                                      "Overall domain"),
                           labels = c("Market", 
                                      "Patrilineal", "Non-unilineal", "Matrilineal", 
                                      "Patrilocal", "Bilocal", "Matrilocal", 
                                      ">=65", "50-64", "35-49", "<35", 
                                      "Knowledge", "Informal leadership", "Religiosity", "Generosity", 
                                      "Overall domain")),
         Model = factor(Model, 
                        levels = c("Model 1", "Model 2",
                                   "Model 3", "Model 4",
                                   "Model 5", "Model 6"), 
                        labels = c("Model 1", "Model 2",
                                   "Model 3", "Model 4",
                                   "Model 5", "Model 6")),
         Type = factor(Type,
                       levels = c("In-degree", "PageRank"),
                       labels = c("In-degree", "PageRank"))) %>% 
  ggplot(aes(x = Value, y = Variable, 
             fill = Model, color = Model)) +
  facet_grid(Domain ~ Type, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.8,
             color = "grey30") +
  stat_pointinterval(.width = .90,
                     point_interval = mean_qi,
                     point_size = 1.8,
                     interval_size = 1)+
  scale_fill_manual(values = alpha(c("#E15759", "#F28E2B", "#B07AA1", "#4E79A7", "#76B7B2", "#59A14F"), 0.3)) +
  scale_color_manual(values = c("#E15759", "#F28E2B", "#B07AA1", "#4E79A7", "#76B7B2", "#59A14F")) +
  scale_x_continuous(limits = c(-1.0, 1.5), breaks = c(-1.0, -0.5, 0, 0.5, 1)) +
  labs(x = "Posterior distribution of estimates",
       y = NULL,
       fill = NULL,
       colour = NULL) +
  theme_classic() +
  theme(plot.title = element_text(size=16, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
        strip.text = element_text(size = 14, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
        panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        axis.text.x = element_text(size = 12, colour = "black", face = "bold"),
        axis.text.y = element_text(size = 12, colour = "black", face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 8)),
        legend.position = "none")

#2 Figure 4 ----
tibble(Value = c(Post_between_GNet2_1$bGNet_pos[,1],
                 Post_between_GNet1_1$bGNet_pos[,1],
                 Post_between_GNet1_1$bGNet_pos[,2],
                 Post_between_GNet1_1$bGNet_pos[,3],
                 Post_between_GNet2_2$bGANet_pos[,1,1], 
                 Post_between_GNet2_2$bGANet_pos[,2,1], 
                 Post_between_GNet2_2$bGANet_pos[,3,1], 
                 Post_between_GNet2_2$bGANet_pos[,4,1], 
                 Post_between_GNet2_3$bGRNet_pos[,1,1],
                 Post_between_GNet2_3$bGRNet_pos[,2,1],
                 Post_between_GNet2_3$bGRNet_pos[,3,1],
                 Post_between_GNet2_4$bGKNet_pos[,1,1],
                 Post_between_GNet2_4$bGKNet_pos[,2,1],
                 Post_between_GNet2_4$bGKNet_pos[,3,1],
                 Post_between_GNet2_5$bGMNet_pos[,1],
                 
                 Post_harmonic_GNet2_1$bGNet_pos[,1],
                 Post_harmonic_GNet1_1$bGNet_pos[,1],
                 Post_harmonic_GNet1_1$bGNet_pos[,2],
                 Post_harmonic_GNet1_1$bGNet_pos[,3],
                 Post_harmonic_GNet2_2$bGANet_pos[,1,1], 
                 Post_harmonic_GNet2_2$bGANet_pos[,2,1], 
                 Post_harmonic_GNet2_2$bGANet_pos[,3,1], 
                 Post_harmonic_GNet2_2$bGANet_pos[,4,1], 
                 Post_harmonic_GNet2_3$bGRNet_pos[,1,1],
                 Post_harmonic_GNet2_3$bGRNet_pos[,2,1],
                 Post_harmonic_GNet2_3$bGRNet_pos[,3,1],
                 Post_harmonic_GNet2_4$bGKNet_pos[,1,1],
                 Post_harmonic_GNet2_4$bGKNet_pos[,2,1],
                 Post_harmonic_GNet2_4$bGKNet_pos[,3,1],
                 Post_harmonic_GNet2_5$bGMNet_pos[,1],
                 
                 Post_triangle_GNet2_1$bGNet[,1],
                 Post_triangle_GNet1_1$bGNet[,1],
                 Post_triangle_GNet1_1$bGNet[,2],
                 Post_triangle_GNet1_1$bGNet[,3],
                 Post_triangle_GNet2_2$bGANet[,1,1], 
                 Post_triangle_GNet2_2$bGANet[,2,1], 
                 Post_triangle_GNet2_2$bGANet[,3,1], 
                 Post_triangle_GNet2_2$bGANet[,4,1], 
                 Post_triangle_GNet2_3$bGRNet[,1,1],
                 Post_triangle_GNet2_3$bGRNet[,2,1],
                 Post_triangle_GNet2_3$bGRNet[,3,1],
                 Post_triangle_GNet2_4$bGKNet[,1,1],
                 Post_triangle_GNet2_4$bGKNet[,2,1],
                 Post_triangle_GNet2_4$bGKNet[,3,1],
                 Post_triangle_GNet2_5$bGMNet[,1], 
                 
                 Post_commsize_GNet2_1$bGNet[,1],
                 Post_commsize_GNet1_1$bGNet[,1],
                 Post_commsize_GNet1_1$bGNet[,2],
                 Post_commsize_GNet1_1$bGNet[,3],
                 Post_commsize_GNet2_2$bGANet[,1,1], 
                 Post_commsize_GNet2_2$bGANet[,2,1], 
                 Post_commsize_GNet2_2$bGANet[,3,1], 
                 Post_commsize_GNet2_2$bGANet[,4,1], 
                 Post_commsize_GNet2_3$bGRNet[,1,1],
                 Post_commsize_GNet2_3$bGRNet[,2,1],
                 Post_commsize_GNet2_3$bGRNet[,3,1],
                 Post_commsize_GNet2_4$bGKNet[,1,1],
                 Post_commsize_GNet2_4$bGKNet[,2,1],
                 Post_commsize_GNet2_4$bGKNet[,3,1],
                 Post_commsize_GNet2_5$bGMNet[,1]),
       Variable = rep(c(rep("Overall domain", 14000), 
                        rep("Consanguineal", 14000), 
                        rep("Affinal", 14000), 
                        rep("Friendship", 14000), 
                        rep("<35", 14000), 
                        rep("35-49", 14000), 
                        rep("50-64", 14000), 
                        rep(">=65", 14000),
                        rep("Patrilocal", 14000), 
                        rep("Bilocal", 14000), 
                        rep("Matrilocal", 14000), 
                        rep("Matrilineal", 14000), 
                        rep("Non-unilineal", 14000), 
                        rep("Patrilineal", 14000), 
                        rep("Market", 14000)), 4),
       Model = rep(c(rep("Model 1", 14000), 
                     rep("Model 2", 42000), 
                     rep("Model 3", 56000), 
                     rep("Model 4", 42000), 
                     rep("Model 5", 42000),
                     rep("Model 6", 14000)),4),
       Type = c(rep("Betweenness centrality", 210000),
                rep("Inward harmonic centrality", 210000),
                rep("Triangle count", 210000),
                rep("Community size", 210000)),
       Domain = rep("Social", 840000)) %>% 
  mutate(Variable = factor(Variable,
                           levels = c("Market", 
                                      "Patrilineal", "Non-unilineal", "Matrilineal", 
                                      "Patrilocal", "Bilocal", "Matrilocal", 
                                      ">=65", "50-64", "35-49", "<35", 
                                      "Friendship", "Affinal", "Consanguineal", 
                                      "Overall domain"),
                           labels = c("Market", 
                                      "Patrilineal", "Non-unilineal", "Matrilineal", 
                                      "Patrilocal", "Bilocal", "Matrilocal", 
                                      ">=65", "50-64", "35-49", "<35", 
                                      "Friendship", "Affinal", "Consanguineal", 
                                      "Overall domain")),
         Model = factor(Model, 
                        levels = c("Model 1", "Model 2",
                                   "Model 3", "Model 4",
                                   "Model 5", "Model 6"), 
                        labels = c("Model 1", "Model 2",
                                   "Model 3", "Model 4",
                                   "Model 5", "Model 6")),
         Type = factor(Type,
                       levels = c("Betweenness centrality", "Inward harmonic centrality", 
                                  "Triangle count", "Community size"),
                       labels = c("Betweenness centrality", "Inward harmonic centrality", 
                                  "Triangle count", "Community size"))) %>% 
  ggplot(aes(x = Value, y = Variable, 
             fill = Model, color = Model)) +
  facet_grid(Domain ~ Type, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.8,
             color = "grey30") +
  stat_pointinterval(.width = .90,
                     point_interval = mean_qi,
                     point_size = 1.8,
                     interval_size = 1)+
  scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1, 0, 1)) +
  scale_fill_manual(values = alpha(c("#E15759", "#F28E2B", "#B07AA1", "#4E79A7", "#76B7B2", "#59A14F"), 0.3)) +
  scale_color_manual(values = c("#E15759", "#F28E2B", "#B07AA1", "#4E79A7", "#76B7B2", "#59A14F")) +
  labs(x = "Posterior distribution of estimates",
       y = NULL,
       fill = NULL,
       colour = NULL) +
  theme_classic() +
  theme(plot.title = element_text(size=16, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
        strip.text = element_text(size = 14, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
        panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        axis.text.x = element_text(size = 12, colour = "black", face = "bold"),
        axis.text.y = element_text(size = 12, colour = "black", face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 8)),
        legend.position = c(0.995,0.995),
        legend.justification = c(0.995,0.995),
        legend.text = element_text(size = 8, face = "bold"))

tibble(Value = c(Post_between_GNet2_1$bGNet_pos[,2],
                 Post_between_GNet1_1$bGNet_pos[,4],
                 Post_between_GNet1_1$bGNet_pos[,5],
                 Post_between_GNet1_1$bGNet_pos[,6],
                 Post_between_GNet1_1$bGNet_pos[,7],
                 Post_between_GNet2_2$bGANet_pos[,1,2], 
                 Post_between_GNet2_2$bGANet_pos[,2,2], 
                 Post_between_GNet2_2$bGANet_pos[,3,2], 
                 Post_between_GNet2_2$bGANet_pos[,4,2], 
                 Post_between_GNet2_3$bGRNet_pos[,1,2],
                 Post_between_GNet2_3$bGRNet_pos[,2,2],
                 Post_between_GNet2_3$bGRNet_pos[,3,2],
                 Post_between_GNet2_4$bGKNet_pos[,1,2],
                 Post_between_GNet2_4$bGKNet_pos[,2,2],
                 Post_between_GNet2_4$bGKNet_pos[,3,2],
                 Post_between_GNet2_5$bGMNet_pos[,2],
                 
                 Post_harmonic_GNet2_1$bGNet_pos[,2],
                 Post_harmonic_GNet1_1$bGNet_pos[,4],
                 Post_harmonic_GNet1_1$bGNet_pos[,5],
                 Post_harmonic_GNet1_1$bGNet_pos[,6],
                 Post_harmonic_GNet1_1$bGNet_pos[,7],
                 Post_harmonic_GNet2_2$bGANet_pos[,1,2], 
                 Post_harmonic_GNet2_2$bGANet_pos[,2,2], 
                 Post_harmonic_GNet2_2$bGANet_pos[,3,2], 
                 Post_harmonic_GNet2_2$bGANet_pos[,4,2], 
                 Post_harmonic_GNet2_3$bGRNet_pos[,1,2],
                 Post_harmonic_GNet2_3$bGRNet_pos[,2,2],
                 Post_harmonic_GNet2_3$bGRNet_pos[,3,2],
                 Post_harmonic_GNet2_4$bGKNet_pos[,1,2],
                 Post_harmonic_GNet2_4$bGKNet_pos[,2,2],
                 Post_harmonic_GNet2_4$bGKNet_pos[,3,2],
                 Post_harmonic_GNet2_5$bGMNet_pos[,2],
                 
                 Post_triangle_GNet2_1$bGNet[,2],
                 Post_triangle_GNet1_1$bGNet[,4],
                 Post_triangle_GNet1_1$bGNet[,5],
                 Post_triangle_GNet1_1$bGNet[,6],
                 Post_triangle_GNet1_1$bGNet[,7],
                 Post_triangle_GNet2_2$bGANet[,1,2], 
                 Post_triangle_GNet2_2$bGANet[,2,2], 
                 Post_triangle_GNet2_2$bGANet[,3,2], 
                 Post_triangle_GNet2_2$bGANet[,4,2], 
                 Post_triangle_GNet2_3$bGRNet[,1,2],
                 Post_triangle_GNet2_3$bGRNet[,2,2],
                 Post_triangle_GNet2_3$bGRNet[,3,2],
                 Post_triangle_GNet2_4$bGKNet[,1,2],
                 Post_triangle_GNet2_4$bGKNet[,2,2],
                 Post_triangle_GNet2_4$bGKNet[,3,2],
                 Post_triangle_GNet2_5$bGMNet[,2], 
                 
                 Post_commsize_GNet2_1$bGNet[,2],
                 Post_commsize_GNet1_1$bGNet[,4],
                 Post_commsize_GNet1_1$bGNet[,5],
                 Post_commsize_GNet1_1$bGNet[,6],
                 Post_commsize_GNet1_1$bGNet[,7],
                 Post_commsize_GNet2_2$bGANet[,1,2], 
                 Post_commsize_GNet2_2$bGANet[,2,2], 
                 Post_commsize_GNet2_2$bGANet[,3,2], 
                 Post_commsize_GNet2_2$bGANet[,4,2], 
                 Post_commsize_GNet2_3$bGRNet[,1,2],
                 Post_commsize_GNet2_3$bGRNet[,2,2],
                 Post_commsize_GNet2_3$bGRNet[,3,2],
                 Post_commsize_GNet2_4$bGKNet[,1,2],
                 Post_commsize_GNet2_4$bGKNet[,2,2],
                 Post_commsize_GNet2_4$bGKNet[,3,2],
                 Post_commsize_GNet2_5$bGMNet[,2]),
       Variable = rep(c(rep("Overall domain", 14000), 
                        rep("Generosity", 14000), 
                        rep("Religiosity", 14000), 
                        rep("Informal leadership", 14000), 
                        rep("Knowledge", 14000), 
                        rep("<35", 14000), 
                        rep("35-49", 14000), 
                        rep("50-64", 14000), 
                        rep(">=65", 14000),
                        rep("Patrilocal", 14000), 
                        rep("Bilocal", 14000), 
                        rep("Matrilocal", 14000), 
                        rep("Matrilineal", 14000), 
                        rep("Non-unilineal", 14000), 
                        rep("Patrilineal", 14000), 
                        rep("Market", 14000)), 4),
       Model = rep(c(rep("Model 1", 14000), 
                     rep("Model 2", 56000), 
                     rep("Model 3", 56000), 
                     rep("Model 4", 42000), 
                     rep("Model 5", 42000),
                     rep("Model 6", 14000)),4),
       Type = c(rep("Betweenness centrality", 224000),
                rep("Inward harmonic centrality", 224000),
                rep("Triangle count", 224000),
                rep("Community size", 224000)),
       Domain = rep("Reputational", 896000)) %>% 
  mutate(Variable = factor(Variable,
                           levels = c("Market", 
                                      "Patrilineal", "Non-unilineal", "Matrilineal", 
                                      "Patrilocal", "Bilocal", "Matrilocal", 
                                      ">=65", "50-64", "35-49", "<35", 
                                      "Knowledge", "Informal leadership", "Religiosity", "Generosity", 
                                      "Overall domain"),
                           labels = c("Market", 
                                      "Patrilineal", "Non-unilineal", "Matrilineal", 
                                      "Patrilocal", "Bilocal", "Matrilocal", 
                                      ">=65", "50-64", "35-49", "<35", 
                                      "Knowledge", "Informal leadership", "Religiosity", "Generosity", 
                                      "Overall domain")),
         Model = factor(Model, 
                        levels = c("Model 1", "Model 2",
                                   "Model 3", "Model 4",
                                   "Model 5", "Model 6"), 
                        labels = c("Model 1", "Model 2",
                                   "Model 3", "Model 4",
                                   "Model 5", "Model 6")),
         Type = factor(Type,
                       levels = c("Betweenness centrality", "Inward harmonic centrality", 
                                  "Triangle count", "Community size"),
                       labels = c("Betweenness centrality", "Inward harmonic centrality", 
                                  "Triangle count", "Community size"))) %>% 
  ggplot(aes(x = Value, y = Variable, 
             fill = Model, color = Model)) +
  facet_grid(Domain ~ Type, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.8,
             color = "grey30") +
  stat_pointinterval(.width = .90,
                     point_interval = mean_qi,
                     point_size = 1.8,
                     interval_size = 1)+
  scale_fill_manual(values = alpha(c("#E15759", "#F28E2B", "#B07AA1", "#4E79A7", "#76B7B2", "#59A14F"), 0.3)) +
  scale_color_manual(values = c("#E15759", "#F28E2B", "#B07AA1", "#4E79A7", "#76B7B2", "#59A14F")) +
  scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1, 0, 1)) +
  labs(x = "Posterior distribution of estimates",
       y = NULL,
       fill = NULL,
       colour = NULL) +
  theme_classic() +
  theme(plot.title = element_text(size=16, face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
        strip.text = element_text(size = 14, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
        panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        axis.text.x = element_text(size = 12, colour = "black", face = "bold"),
        axis.text.y = element_text(size = 12, colour = "black", face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 8)),
        legend.position = "none")

#3 Figure 5 ----
##3.1 Prediction of cross-layer recognition ----
Layer_labels <- c("Consanguineal × Generosity",
                  "Consanguineal × Religiosity",
                  "Consanguineal × Leadership",
                  "Consanguineal × Knowledge",
                  "Affinal × Generosity",
                  "Affinal × Religiosity",
                  "Affinal × Leadership",
                  "Affinal × Knowledge",
                  "Friendship × Generosity",
                  "Friendship × Religiosity",
                  "Friendship × Leadership",
                  "Friendship × Knowledge")

Layer_order <- c(1, 5, 9,
                 2, 6, 10,
                 3, 7, 11,
                 4, 8, 12)

Layer_labels_plot <- Layer_labels[Layer_order]

generate_layer_parti_prediction <- function(post_samples,
                                            n_samples = 14000,
                                            n_context = 1000) {
  
  set.seed(123)
  
  n_available <- nrow(post_samples$bType)
  if (is.null(n_samples)) n_samples <- n_available
  n_samples <- min(n_samples, n_available)
  
  n_type <- ncol(post_samples$bType)
  
  draw_ids <- sample(seq_len(n_available), size = n_samples, replace = FALSE)
  
  pred_array <- array(NA_real_, 
                      dim = c(n_samples, n_type, 2),
                      dimnames = list(NULL, paste0("Type_", seq_len(n_type)), c("Female", "Male")))
  
  n_village <- ncol(post_samples$bV)
  n_indiv   <- ncol(post_samples$bI)
  
  for (s in seq_len(n_samples)) {
    
    d <- draw_ids[s]
    
    v_id <- sample(seq_len(n_village), size = n_context, replace = TRUE)
    i_id <- sample(seq_len(n_indiv),   size = n_context, replace = TRUE)
    
    random_context <- post_samples$bV[d, v_id] + post_samples$bI[d, i_id]
    
    for (k in seq_len(n_type)) {
      for (g in 1:2) {
        
        eta <- post_samples$bType[d, k] +
          post_samples$bGType[d, k] * (g - 1) +
          random_context
        
        pred_array[s, k, g] <- mean(plogis(eta))
      }
    }
  }
  
  return(pred_array)
}

Predicted_layer_parti <- generate_layer_parti_prediction(Post_multi_network_partici_GNet_1_1)

Predicted_layer_long <- expand.grid(Draw = seq_len(dim(Predicted_layer_parti)[1]),
                                    Layer = seq_len(dim(Predicted_layer_parti)[2]),
                                    Gender = c("Female", "Male"),
                                    KEEP.OUT.ATTRS = FALSE) %>%
  mutate(Pred = as.vector(Predicted_layer_parti),
         Layer = factor(Layer, levels = 1:12, labels = Layer_labels),
         Gender = factor(Gender, levels = c("Female", "Male")))

Mean_layer <- Predicted_layer_long %>%
  group_by(Layer, Gender) %>%
  summarise(Pred  = mean(Pred),
            .groups = "drop") %>% 
  mutate(Layer = factor(Layer, levels = rev(Layer_labels_plot))) 

Mean_layer_seg <- Mean_layer %>%
  dplyr::select(Layer, Gender, Pred) %>%
  tidyr::pivot_wider(names_from = Gender,
                     values_from = Pred)

Layer_draw_ids <- sample(seq_len(dim(Predicted_layer_parti)[1]),
                         size = 200)

Draw_layer_long <- Predicted_layer_long %>%
  dplyr::filter(Draw %in% Layer_draw_ids) %>%
  dplyr::mutate(Layer = factor(Layer, levels = rev(Layer_labels_plot)))

Draw_layer_seg <- Draw_layer_long %>%
  dplyr::select(Draw, Layer, Gender, Pred) %>%
  tidyr::pivot_wider(names_from = Gender, values_from = Pred)

##3.2 Prediction of cross-layer ties ----
generate_cross_tie_prediction <- generate_layer_parti_prediction
Predicted_cross_ties <- generate_cross_tie_prediction(Post_cross_network_GNet_1_1)

Predicted_cross_ties_long <- expand.grid(Draw = seq_len(dim(Predicted_cross_ties)[1]),
                                         Layer = seq_len(dim(Predicted_cross_ties)[2]),
                                         Gender = c("Female", "Male"),
                                         KEEP.OUT.ATTRS = FALSE) %>%
  mutate(Pred = as.vector(Predicted_cross_ties),
         Layer = factor(Layer, levels = 1:12, labels = Layer_labels),
         Gender = factor(Gender, levels = c("Female", "Male")))

Mean_cross_ties <- Predicted_cross_ties_long %>%
  group_by(Layer, Gender) %>%
  summarise(Pred  = mean(Pred),
            .groups = "drop") %>% 
  mutate(Layer = factor(Layer, levels = rev(Layer_labels_plot))) 

Mean_cross_ties_seg <- Mean_cross_ties %>%
  dplyr::select(Layer, Gender, Pred) %>%
  tidyr::pivot_wider(names_from = Gender,
                     values_from = Pred)

Cross_ties_draw_ids <- sample(seq_len(dim(Predicted_cross_ties)[1]),
                              size = 200)

Draw_cross_ties_long <- Predicted_cross_ties_long %>%
  dplyr::filter(Draw %in% Cross_ties_draw_ids) %>%
  dplyr::mutate(Layer = factor(Layer, levels = rev(Layer_labels_plot)))

Draw_cross_ties_seg <- Draw_cross_ties_long %>%
  dplyr::select(Draw, Layer, Gender, Pred) %>%
  tidyr::pivot_wider(names_from = Gender, values_from = Pred)

##3.3 Predicted figures ----
Measure_levels <- c("Cross-layer recognition",
                    "Cross-layer ties")

Multiplex_plot_add_facet_variables <- function(df) {
  df %>%
    mutate(Social = sub(" × .*", "", as.character(Layer)),
           Reputation = sub(".* × ", "", as.character(Layer)),
           Social = factor(Social,
                           levels = c("Consanguineal", "Affinal", "Friendship")),
           Reputation = factor(Reputation,
                               levels = c("Generosity", "Religiosity", "Leadership", "Knowledge")))
}

Draw_combined <- bind_rows(Draw_layer_long %>%
                             mutate(Measure = "Cross-layer recognition"),
                           Draw_cross_ties_long %>%
                             mutate(Measure = "Cross-layer ties")) %>% 
  mutate(Measure = factor(Measure, levels = Measure_levels),
         Layer = factor(Layer, levels = Layer_labels_plot)) %>% 
  Multiplex_plot_add_facet_variables()

Draw_combined_seg <- Draw_combined %>%
  dplyr::select(Measure, Reputation, Social, Draw, Gender, Pred) %>%
  tidyr::pivot_wider(names_from = Gender, values_from = Pred)

Mean_combined <- bind_rows(Mean_layer %>%
                             mutate(Measure = "Cross-layer recognition"),
                           Mean_cross_ties %>%
                             mutate(Measure = "Cross-layer ties")) %>% 
  mutate(Measure = factor(Measure, levels = Measure_levels),
         Layer = factor(Layer, levels = Layer_labels_plot)) %>% 
  Multiplex_plot_add_facet_variables()

Mean_seg_combined <- bind_rows(Mean_layer_seg %>%
                                 mutate(Measure = "Cross-layer recognition"),
                               Mean_cross_ties_seg %>%
                                 mutate(Measure = "Cross-layer ties")) %>% 
  mutate(Measure = factor(Measure, levels = Measure_levels),
         Layer = factor(Layer, levels = Layer_labels_plot)) %>% 
  Multiplex_plot_add_facet_variables()

Multiplex.plot <- 
  ggplot() +
  geom_segment(data = Draw_combined_seg,
               aes(x = as.numeric(Social) - 0.2,
                   xend = as.numeric(Social) + 0.2,
                   y = Female,
                   yend = Male),
               colour = "#4D4D4D",
               linewidth = 0.20,
               alpha = 0.08) +
  geom_point(data = Draw_combined,
             aes(x = as.numeric(Social),
                 y = Pred,
                 colour = Gender),
             position = position_dodge(width = 0.8),
             size = 0.8,
             alpha = 0.08) +
  geom_segment(data = Mean_seg_combined,
               aes(x = as.numeric(Social) - 0.2,
                   xend = as.numeric(Social) + 0.2,
                   y = Female,
                   yend = Male),
               colour = "#4D4D4D",
               linewidth = 0.8) +
  geom_point(data = Mean_combined,
             aes(x = as.numeric(Social),
                 y = Pred,
                 color = Gender, 
                 fill = Gender),
             position = position_dodge(width = 0.8),
             shape = 21,
             stroke = 0.7,
             size = 2.5,
             alpha = 0.90) +
  facet_grid(Measure ~ Reputation, scales = "free_y") +
  scale_x_continuous(breaks = 1:3,
                     labels = c("Consanguineal", "Affinal", "Friendship"),
                     expand = expansion(add = 0.45)) +
  ggh4x::facetted_pos_scales( 
    y = list(scale_y_continuous(limits = c(0.1, 0.35),
                                breaks = c(0.1, 0.2, 0.3),
                                labels = scales::percent_format(accuracy = 1)),
             scale_y_continuous(limits = c(0, 0.15),
                                breaks = seq(0, 0.15, by = 0.05),
                                labels = scales::percent_format(accuracy = 1)))) +
  scale_colour_manual(values = c("Female" = "#E15759", "Male" = "#4E79A7")) +
  scale_fill_manual(values = c("Female" = "#E15759", "Male" = "#4E79A7"))+
  labs(x = NULL,
       y = "Predicted probability",
       colour = NULL,
       fill = NULL) +
  theme_classic() +
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(size = 10, face = "bold"),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_blank(),
        panel.spacing.x = unit(0.15, "cm"),
        legend.position = c(0.01, 0.43),
        legend.justification = c(0.01, 0.43),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", size = 9, face = "bold"),
        axis.text.y = element_text(colour = "black", size = 9, face = "bold"))

Multiplex.plot
