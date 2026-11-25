#Statistical model results

#1 Estimates ----
##1.1 Received recognition ----
Post_degree_GNet1_1 <- extract.samples(Model_degree_in_GNet1_1)
Post_degree_GNet2_1 <- extract.samples(Model_degree_in_GNet2_1)
Post_degree_GNet2_2 <- extract.samples(Model_degree_in_GNet2_2)
Post_degree_GNet2_3 <- extract.samples(Model_degree_in_GNet2_3)
Post_degree_GNet2_4 <- extract.samples(Model_degree_in_GNet2_4)
Post_degree_GNet2_5 <- extract.samples(Model_degree_in_GNet2_5)

Post_pagerank_GNet1_1 <- extract.samples(Model_pagerank_GNet1_1)
Post_pagerank_GNet2_1 <- extract.samples(Model_pagerank_GNet2_1)
Post_pagerank_GNet2_2 <- extract.samples(Model_pagerank_GNet2_2)
Post_pagerank_GNet2_3 <- extract.samples(Model_pagerank_GNet2_3)
Post_pagerank_GNet2_4 <- extract.samples(Model_pagerank_GNet2_4)
Post_pagerank_GNet2_5 <- extract.samples(Model_pagerank_GNet2_5)

###1.1.1 In-degree ----
####1.1.1.1 Total ----
Output_degree_in_GNet1_1 <- data.frame(Mean = c(Pre_degree_in_GNet1_1$mean),
                                       CI5 = c(Pre_degree_in_GNet1_1$`5%`) ,
                                       CI95 = c(Pre_degree_in_GNet1_1$`95%`) ,
                                       Variable = c("Consanguineal[women]",
                                                    "Affinal[women]",
                                                    "Friendship[women]",
                                                    "Generosity[women]",
                                                    "Religiosity[women]",
                                                    "Influence[women]",
                                                    "Knowledge[women]",
                                                    "Consanguineal[men−women]",
                                                    "Affinal[men−women]",
                                                    "Friendship[men−women]", 
                                                    "Generosity[men−women]", 
                                                    "Religiosity[men−women]",
                                                    "Influence[men−women]",
                                                    "Knowledge[men−women]", 
                                                    "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet1_1

Output_degree_in_GNet2_1 <- data.frame(Mean = c(Pre_degree_in_GNet2_1$mean),
                                       CI5 = c(Pre_degree_in_GNet2_1$`5%`) ,
                                       CI95 = c(Pre_degree_in_GNet2_1$`95%`) ,
                                       Variable = c("Social[women]",
                                                    "Reputation[women]",
                                                    "Social[men−women]",
                                                    "Reputation[men−women]", 
                                                    "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet2_1

Output_degree_in_GNet2_2 <- data.frame(Mean = c(Pre_degree_in_GNet2_2$mean),
                                       CI5 = c(Pre_degree_in_GNet2_2$`5%`) ,
                                       CI95 = c(Pre_degree_in_GNet2_2$`95%`) ,
                                       Variable = c("Social: <35[women]",
                                                    "Social: 35-49[women]",
                                                    "Social: 50-64[women]",
                                                    "Social: >=65[women]",
                                                    "Reputation: <35[women]",
                                                    "Reputation: 35-49[women]",
                                                    "Reputation: 50-64[women]",
                                                    "Reputation: >=65[women]",
                                                    "Social: <35[men−women]",
                                                    "Social: 35-49[men−women]",
                                                    "Social: 50-64[men−women]",
                                                    "Social: >=65[men−women]",
                                                    "Reputation: <35[men−women]", 
                                                    "Reputation: 35-49[men−women]", 
                                                    "Reputation: 50-64[men−women]", 
                                                    "Reputation: >=65[men−women]", 
                                                    "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet2_2

Output_degree_in_GNet2_3 <- data.frame(Mean = c(Pre_degree_in_GNet2_3$mean),
                                       CI5 = c(Pre_degree_in_GNet2_3$`5%`) ,
                                       CI95 = c(Pre_degree_in_GNet2_3$`95%`) ,
                                       Variable = c("Social: patrilocal[women]",
                                                    "Social: bilocal[women]",
                                                    "Social: matrilocal[women]",
                                                    "Reputation: patrilocal[women]",
                                                    "Reputation: bilocal[women]",
                                                    "Reputation: matrilocal[women]",
                                                    "Social: patrilocal[men−women]",
                                                    "Social: bilocal[men−women]",
                                                    "Social: matrilocal[men−women]",
                                                    "Reputation: patrilocal[men−women]", 
                                                    "Reputation: bilocal[men−women]", 
                                                    "Reputation: matrilocal[men−women]", 
                                                    "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet2_3

Output_degree_in_GNet2_4 <- data.frame(Mean = c(Pre_degree_in_GNet2_4$mean),
                                       CI5 = c(Pre_degree_in_GNet2_4$`5%`) ,
                                       CI95 = c(Pre_degree_in_GNet2_4$`95%`) ,
                                       Variable = c("Social: matrilineal[women]",
                                                    "Social: non-unilineal[women]",
                                                    "Social: patrilineal[women]",
                                                    "Reputation: matrilineal[women]",
                                                    "Reputation: non-unilineal[women]",
                                                    "Reputation: patrilineal[women]",
                                                    "Social: matrilineal[men−women]",
                                                    "Social: non-unilineal[men−women]",
                                                    "Social: patrilineal[men−women]",
                                                    "Reputation: matrilineal[men−women]", 
                                                    "Reputation: non-unilineal[men−women]", 
                                                    "Reputation: patrilineal[men−women]", 
                                                    "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet2_4

Output_degree_in_GNet2_5 <- data.frame(Mean = c(Pre_degree_in_GNet2_5$mean),
                                       CI5 = c(Pre_degree_in_GNet2_5$`5%`) ,
                                       CI95 = c(Pre_degree_in_GNet2_5$`95%`) ,
                                       Variable = c("Social[women]",
                                                    "Reputation[women]",
                                                    "Social[men−women]",
                                                    "Reputation[men−women]", 
                                                    "Market: social[women] ",
                                                    "Market: reputation[women] ",
                                                    "Market: social[men−women] ",
                                                    "Market: reputation[men−women] ",
                                                    "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet2_5

####1.1.1.2 Direct ----
Output_degree_in_GNet1_1_d <- data.frame(Mean = c(Pre_degree_in_GNet1_1_d$mean),
                                         CI5 = c(Pre_degree_in_GNet1_1_d$`5%`) ,
                                         CI95 = c(Pre_degree_in_GNet1_1_d$`95%`) ,
                                         Variable = c("Consanguineal[women]",
                                                      "Affinal[women]",
                                                      "Friendship[women]",
                                                      "Generosity[women]",
                                                      "Religiosity[women]",
                                                      "Influence[women]",
                                                      "Knowledge[women]",
                                                      "Consanguineal[men−women]",
                                                      "Affinal[men−women]",
                                                      "Friendship[men−women]", 
                                                      "Generosity[men−women]", 
                                                      "Religiosity[men−women]",
                                                      "Influence[men−women]",
                                                      "Knowledge[men−women]",
                                                      "<35", "35-49", "50-64", ">=65",
                                                      "Non-inheritor", "Inheritor", 
                                                      "Patrilocal", "Bilocal", "Matrilocal",
                                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                                      "Market",
                                                      "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet1_1_d

Output_degree_in_GNet2_1_d <- data.frame(Mean = c(Pre_degree_in_GNet2_1_d$mean),
                                         CI5 = c(Pre_degree_in_GNet2_1_d$`5%`) ,
                                         CI95 = c(Pre_degree_in_GNet2_1_d$`95%`) ,
                                         Variable = c("Social[women]",
                                                      "Reputation[women]",
                                                      "Social[men−women]",
                                                      "Reputation[men−women]", 
                                                      "<35", "35-49", "50-64", ">=65",
                                                      "Non-inheritor", "Inheritor", 
                                                      "Patrilocal", "Bilocal", "Matrilocal",
                                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                                      "Market",
                                                      "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet2_1_d

Output_degree_in_GNet2_2_d <- data.frame(Mean = c(Pre_degree_in_GNet2_2_d$mean),
                                         CI5 = c(Pre_degree_in_GNet2_2_d$`5%`) ,
                                         CI95 = c(Pre_degree_in_GNet2_2_d$`95%`) ,
                                         Variable = c("Social: <35[women]",
                                                      "Social: 35-49[women]",
                                                      "Social: 50-64[women]",
                                                      "Social: >=65[women]",
                                                      "Reputation: <35[women]",
                                                      "Reputation: 35-49[women]",
                                                      "Reputation: 50-64[women]",
                                                      "Reputation: >=65[women]",
                                                      "Social: <35[men−women]",
                                                      "Social: 35-49[men−women]",
                                                      "Social: 50-64[men−women]",
                                                      "Social: >=65[men−women]",
                                                      "Reputation: <35[men−women]", 
                                                      "Reputation: 35-49[men−women]", 
                                                      "Reputation: 50-64[men−women]", 
                                                      "Reputation: >=65[men−women]", 
                                                      "Non-inheritor", "Inheritor", 
                                                      "Patrilocal", "Bilocal", "Matrilocal",
                                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                                      "Market",
                                                      "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet2_2_d

Output_degree_in_GNet2_3_d <- data.frame(Mean = c(Pre_degree_in_GNet2_3_d$mean),
                                         CI5 = c(Pre_degree_in_GNet2_3_d$`5%`) ,
                                         CI95 = c(Pre_degree_in_GNet2_3_d$`95%`) ,
                                         Variable = c("Social: patrilocal[women]",
                                                      "Social: bilocal[women]",
                                                      "Social: matrilocal[women]",
                                                      "Reputation: patrilocal[women]",
                                                      "Reputation: bilocal[women]",
                                                      "Reputation: matrilocal[women]",
                                                      "Social: patrilocal[men−women]",
                                                      "Social: bilocal[men−women]",
                                                      "Social: matrilocal[men−women]",
                                                      "Reputation: patrilocal[men−women]", 
                                                      "Reputation: bilocal[men−women]", 
                                                      "Reputation: matrilocal[men−women]", 
                                                      "<35", "35-49", "50-64", ">=65",
                                                      "Non-inheritor", "Inheritor", 
                                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                                      "Market",
                                                      "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet2_3_d

Output_degree_in_GNet2_4_d <- data.frame(Mean = c(Pre_degree_in_GNet2_4_d$mean),
                                         CI5 = c(Pre_degree_in_GNet2_4_d$`5%`) ,
                                         CI95 = c(Pre_degree_in_GNet2_4_d$`95%`) ,
                                         Variable = c("Social: matrilineal[women]",
                                                      "Social: non-unilineal[women]",
                                                      "Social: patrilineal[women]",
                                                      "Reputation: matrilineal[women]",
                                                      "Reputation: non-unilineal[women]",
                                                      "Reputation: patrilineal[women]",
                                                      "Social: matrilineal[men−women]",
                                                      "Social: non-unilineal[men−women]",
                                                      "Social: patrilineal[men−women]",
                                                      "Reputation: matrilineal[men−women]", 
                                                      "Reputation: non-unilineal[men−women]", 
                                                      "Reputation: patrilineal[men−women]", 
                                                      "<35", "35-49", "50-64", ">=65",
                                                      "Non-inheritor", "Inheritor", 
                                                      "Patrilocal", "Bilocal", "Matrilocal",
                                                      "Market",
                                                      "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: matrilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Social: patrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: matrilineal[men−women]",
                                      "Reputation: patrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: matrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: patrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: matrilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Social: patrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: matrilineal[men−women]",
                                      "Reputation: patrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: matrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet2_4_d

Output_degree_in_GNet2_5_d <- data.frame(Mean = c(Pre_degree_in_GNet2_5_d$mean),
                                         CI5 = c(Pre_degree_in_GNet2_5_d$`5%`) ,
                                         CI95 = c(Pre_degree_in_GNet2_5_d$`95%`) ,
                                         Variable = c("Social[women]",
                                                      "Reputation[women]",
                                                      "Social[men−women]",
                                                      "Reputation[men−women]", 
                                                      "Market: social[women] ",
                                                      "Market: reputation[women] ",
                                                      "Market: social[men−women] ",
                                                      "Market: reputation[men−women] ",
                                                      "<35", "35-49", "50-64", ">=65",
                                                      "Non-inheritor", "Inheritor", 
                                                      "Patrilocal", "Bilocal", "Matrilocal",
                                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                                      "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_degree_in_GNet2_5_d

###1.1.2 PageRank ----
####1.1.2.1 Total ----
Output_pagerank_GNet1_1 <- data.frame(Mean = c(Pre_pagerank_GNet1_1$mean),
                                      CI5 = c(Pre_pagerank_GNet1_1$`5%`) ,
                                      CI95 = c(Pre_pagerank_GNet1_1$`95%`) ,
                                      Variable = c("Consanguineal[women]",
                                                   "Affinal[women]",
                                                   "Friendship[women]",
                                                   "Generosity[women]",
                                                   "Religiosity[women]",
                                                   "Influence[women]",
                                                   "Knowledge[women]",
                                                   "Consanguineal[men−women]",
                                                   "Affinal[men−women]",
                                                   "Friendship[men−women]", 
                                                   "Generosity[men−women]", 
                                                   "Religiosity[men−women]",
                                                   "Influence[men−women]",
                                                   "Knowledge[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet1_1

Output_pagerank_GNet2_1 <- data.frame(Mean = c(Pre_pagerank_GNet2_1$mean),
                                      CI5 = c(Pre_pagerank_GNet2_1$`5%`) ,
                                      CI95 = c(Pre_pagerank_GNet2_1$`95%`) ,
                                      Variable = c("Social[women]",
                                                   "Reputation[women]",
                                                   "Social[men−women]",
                                                   "Reputation[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet2_1

Output_pagerank_GNet2_2 <- data.frame(Mean = c(Pre_pagerank_GNet2_2$mean),
                                      CI5 = c(Pre_pagerank_GNet2_2$`5%`) ,
                                      CI95 = c(Pre_pagerank_GNet2_2$`95%`) ,
                                      Variable = c("Social: <35[women]",
                                                   "Social: 35-49[women]",
                                                   "Social: 50-64[women]",
                                                   "Social: >=65[women]",
                                                   "Reputation: <35[women]",
                                                   "Reputation: 35-49[women]",
                                                   "Reputation: 50-64[women]",
                                                   "Reputation: >=65[women]",
                                                   "Social: <35[men−women]",
                                                   "Social: 35-49[men−women]",
                                                   "Social: 50-64[men−women]",
                                                   "Social: >=65[men−women]",
                                                   "Reputation: <35[men−women]", 
                                                   "Reputation: 35-49[men−women]", 
                                                   "Reputation: 50-64[men−women]", 
                                                   "Reputation: >=65[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet2_2

Output_pagerank_GNet2_3 <- data.frame(Mean = c(Pre_pagerank_GNet2_3$mean),
                                      CI5 = c(Pre_pagerank_GNet2_3$`5%`) ,
                                      CI95 = c(Pre_pagerank_GNet2_3$`95%`) ,
                                      Variable = c("Social: patrilocal[women]",
                                                   "Social: bilocal[women]",
                                                   "Social: matrilocal[women]",
                                                   "Reputation: patrilocal[women]",
                                                   "Reputation: bilocal[women]",
                                                   "Reputation: matrilocal[women]",
                                                   "Social: patrilocal[men−women]",
                                                   "Social: bilocal[men−women]",
                                                   "Social: matrilocal[men−women]",
                                                   "Reputation: patrilocal[men−women]", 
                                                   "Reputation: bilocal[men−women]", 
                                                   "Reputation: matrilocal[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet2_3

Output_pagerank_GNet2_4 <- data.frame(Mean = c(Pre_pagerank_GNet2_4$mean),
                                      CI5 = c(Pre_pagerank_GNet2_4$`5%`) ,
                                      CI95 = c(Pre_pagerank_GNet2_4$`95%`) ,
                                      Variable = c("Social: matrilineal[women]",
                                                   "Social: non-unilineal[women]",
                                                   "Social: patrilineal[women]",
                                                   "Reputation: matrilineal[women]",
                                                   "Reputation: non-unilineal[women]",
                                                   "Reputation: patrilineal[women]",
                                                   "Social: matrilineal[men−women]",
                                                   "Social: non-unilineal[men−women]",
                                                   "Social: patrilineal[men−women]",
                                                   "Reputation: matrilineal[men−women]", 
                                                   "Reputation: non-unilineal[men−women]", 
                                                   "Reputation: patrilineal[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet2_4

Output_pagerank_GNet2_5 <- data.frame(Mean = c(Pre_pagerank_GNet2_5$mean),
                                      CI5 = c(Pre_pagerank_GNet2_5$`5%`) ,
                                      CI95 = c(Pre_pagerank_GNet2_5$`95%`) ,
                                      Variable = c("Social[women]",
                                                   "Reputation[women]",
                                                   "Social[men−women]",
                                                   "Reputation[men−women]", 
                                                   "Market: social[women] ",
                                                   "Market: reputation[women] ",
                                                   "Market: social[men−women] ",
                                                   "Market: reputation[men−women] ",
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet2_5

####1.1.2.2 Direct ----
Output_pagerank_GNet1_1_d <- data.frame(Mean = c(Pre_pagerank_GNet1_1_d$mean),
                                        CI5 = c(Pre_pagerank_GNet1_1_d$`5%`) ,
                                        CI95 = c(Pre_pagerank_GNet1_1_d$`95%`) ,
                                        Variable = c("Consanguineal[women]",
                                                     "Affinal[women]",
                                                     "Friendship[women]",
                                                     "Generosity[women]",
                                                     "Religiosity[women]",
                                                     "Influence[women]",
                                                     "Knowledge[women]",
                                                     "Consanguineal[men−women]",
                                                     "Affinal[men−women]",
                                                     "Friendship[men−women]", 
                                                     "Generosity[men−women]", 
                                                     "Religiosity[men−women]",
                                                     "Influence[men−women]",
                                                     "Knowledge[men−women]",
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet1_1_d

Output_pagerank_GNet2_1_d <- data.frame(Mean = c(Pre_pagerank_GNet2_1_d$mean),
                                        CI5 = c(Pre_pagerank_GNet2_1_d$`5%`) ,
                                        CI95 = c(Pre_pagerank_GNet2_1_d$`95%`) ,
                                        Variable = c("Social[women]",
                                                     "Reputation[women]",
                                                     "Social[men−women]",
                                                     "Reputation[men−women]", 
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet2_1_d

Output_pagerank_GNet2_2_d <- data.frame(Mean = c(Pre_pagerank_GNet2_2_d$mean),
                                        CI5 = c(Pre_pagerank_GNet2_2_d$`5%`) ,
                                        CI95 = c(Pre_pagerank_GNet2_2_d$`95%`) ,
                                        Variable = c("Social: <35[women]",
                                                     "Social: 35-49[women]",
                                                     "Social: 50-64[women]",
                                                     "Social: >=65[women]",
                                                     "Reputation: <35[women]",
                                                     "Reputation: 35-49[women]",
                                                     "Reputation: 50-64[women]",
                                                     "Reputation: >=65[women]",
                                                     "Social: <35[men−women]",
                                                     "Social: 35-49[men−women]",
                                                     "Social: 50-64[men−women]",
                                                     "Social: >=65[men−women]",
                                                     "Reputation: <35[men−women]", 
                                                     "Reputation: 35-49[men−women]", 
                                                     "Reputation: 50-64[men−women]", 
                                                     "Reputation: >=65[men−women]", 
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet2_2_d

Output_pagerank_GNet2_3_d <- data.frame(Mean = c(Pre_pagerank_GNet2_3_d$mean),
                                        CI5 = c(Pre_pagerank_GNet2_3_d$`5%`) ,
                                        CI95 = c(Pre_pagerank_GNet2_3_d$`95%`) ,
                                        Variable = c("Social: patrilocal[women]",
                                                     "Social: bilocal[women]",
                                                     "Social: matrilocal[women]",
                                                     "Reputation: patrilocal[women]",
                                                     "Reputation: bilocal[women]",
                                                     "Reputation: matrilocal[women]",
                                                     "Social: patrilocal[men−women]",
                                                     "Social: bilocal[men−women]",
                                                     "Social: matrilocal[men−women]",
                                                     "Reputation: patrilocal[men−women]", 
                                                     "Reputation: bilocal[men−women]", 
                                                     "Reputation: matrilocal[men−women]", 
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet2_3_d

Output_pagerank_GNet2_4_d <- data.frame(Mean = c(Pre_pagerank_GNet2_4_d$mean),
                                        CI5 = c(Pre_pagerank_GNet2_4_d$`5%`) ,
                                        CI95 = c(Pre_pagerank_GNet2_4_d$`95%`) ,
                                        Variable = c("Social: matrilineal[women]",
                                                     "Social: non-unilineal[women]",
                                                     "Social: patrilineal[women]",
                                                     "Reputation: matrilineal[women]",
                                                     "Reputation: non-unilineal[women]",
                                                     "Reputation: patrilineal[women]",
                                                     "Social: matrilineal[men−women]",
                                                     "Social: non-unilineal[men−women]",
                                                     "Social: patrilineal[men−women]",
                                                     "Reputation: matrilineal[men−women]", 
                                                     "Reputation: non-unilineal[men−women]", 
                                                     "Reputation: patrilineal[men−women]", 
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: matrilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Social: patrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: matrilineal[men−women]",
                                      "Reputation: patrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: matrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: patrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: matrilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Social: patrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: matrilineal[men−women]",
                                      "Reputation: patrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: matrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet2_4_d

Output_pagerank_GNet2_5_d <- data.frame(Mean = c(Pre_pagerank_GNet2_5_d$mean),
                                        CI5 = c(Pre_pagerank_GNet2_5_d$`5%`) ,
                                        CI95 = c(Pre_pagerank_GNet2_5_d$`95%`) ,
                                        Variable = c("Social[women]",
                                                     "Reputation[women]",
                                                     "Social[men−women]",
                                                     "Reputation[men−women]", 
                                                     "Market: social[women] ",
                                                     "Market: reputation[women] ",
                                                     "Market: social[men−women] ",
                                                     "Market: reputation[men−women] ",
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_pagerank_GNet2_5_d

##1.2 Structural position ----
Post_between_GNet1_1 <- extract.samples(Model_between_GNet1_1)
Post_between_GNet2_1 <- extract.samples(Model_between_GNet2_1)
Post_between_GNet2_2 <- extract.samples(Model_between_GNet2_2)
Post_between_GNet2_3 <- extract.samples(Model_between_GNet2_3)
Post_between_GNet2_4 <- extract.samples(Model_between_GNet2_4)
Post_between_GNet2_5 <- extract.samples(Model_between_GNet2_5)

Post_harmonic_GNet1_1 <- extract.samples(Model_harmonic_in_GNet1_1)
Post_harmonic_GNet2_1 <- extract.samples(Model_harmonic_in_GNet2_1)
Post_harmonic_GNet2_2 <- extract.samples(Model_harmonic_in_GNet2_2)
Post_harmonic_GNet2_3 <- extract.samples(Model_harmonic_in_GNet2_3)
Post_harmonic_GNet2_4 <- extract.samples(Model_harmonic_in_GNet2_4)
Post_harmonic_GNet2_5 <- extract.samples(Model_harmonic_in_GNet2_5)

Post_triangle_GNet1_1 <- extract.samples(Model_triangle_GNet1_1)
Post_triangle_GNet2_1 <- extract.samples(Model_triangle_GNet2_1)
Post_triangle_GNet2_2 <- extract.samples(Model_triangle_GNet2_2)
Post_triangle_GNet2_3 <- extract.samples(Model_triangle_GNet2_3)
Post_triangle_GNet2_4 <- extract.samples(Model_triangle_GNet2_4)
Post_triangle_GNet2_5 <- extract.samples(Model_triangle_GNet2_5)

Post_commsize_GNet1_1 <- extract.samples(Model_commsize_GNet1_1)
Post_commsize_GNet2_1 <- extract.samples(Model_commsize_GNet2_1)
Post_commsize_GNet2_2 <- extract.samples(Model_commsize_GNet2_2)
Post_commsize_GNet2_3 <- extract.samples(Model_commsize_GNet2_3)
Post_commsize_GNet2_4 <- extract.samples(Model_commsize_GNet2_4)
Post_commsize_GNet2_5 <- extract.samples(Model_commsize_GNet2_5)

###1.2.1 Betweenness ----
####1.2.1.1 Total ----
Output_between_GNet1_1 <- data.frame(Mean = c(Pre_between_GNet1_1$mean),
                                     CI5 = c(Pre_between_GNet1_1$`5%`) ,
                                     CI95 = c(Pre_between_GNet1_1$`95%`) ,
                                     Variable = c("Consanguineal[women]",
                                                  "Affinal[women]",
                                                  "Friendship[women]",
                                                  "Generosity[women]",
                                                  "Religiosity[women]",
                                                  "Influence[women]",
                                                  "Knowledge[women]",
                                                  "Consanguineal[men−women]",
                                                  "Affinal[men−women]",
                                                  "Friendship[men−women]", 
                                                  "Generosity[men−women]", 
                                                  "Religiosity[men−women]",
                                                  "Influence[men−women]",
                                                  "Knowledge[men−women]",
                                                  
                                                  "Consanguineal[women]",
                                                  "Affinal[women]",
                                                  "Friendship[women]",
                                                  "Generosity[women]",
                                                  "Religiosity[women]",
                                                  "Influence[women]",
                                                  "Knowledge[women]",
                                                  "Consanguineal[men−women]",
                                                  "Affinal[men−women]",
                                                  "Friendship[men−women]", 
                                                  "Generosity[men−women]", 
                                                  "Religiosity[men−women]",
                                                  "Influence[men−women]",
                                                  "Knowledge[men−women]", 
                                                  "V_bar", "V_bar"), 
                                     Component = c(rep("Zero component", 14), rep("Positive component", 14), 
                                                   "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet1_1

Output_between_GNet2_1 <- data.frame(Mean = c(Pre_between_GNet2_1$mean),
                                     CI5 = c(Pre_between_GNet2_1$`5%`) ,
                                     CI95 = c(Pre_between_GNet2_1$`95%`) ,
                                     Variable = c("Social[women]",
                                                  "Reputation[women]",
                                                  "Social[men−women]",
                                                  "Reputation[men−women]", 
                                                  "Social[women]",
                                                  "Reputation[women]",
                                                  "Social[men−women]",
                                                  "Reputation[men−women]", 
                                                  "V_bar", "V_bar"), 
                                     Component = c(rep("Zero component", 4), rep("Positive component", 4), 
                                                   "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet2_1

Output_between_GNet2_2 <- data.frame(Mean = c(Pre_between_GNet2_2$mean),
                                     CI5 = c(Pre_between_GNet2_2$`5%`) ,
                                     CI95 = c(Pre_between_GNet2_2$`95%`) ,
                                     Variable = c("Social: <35[women]",
                                                  "Social: 35-49[women]",
                                                  "Social: 50-64[women]",
                                                  "Social: >=65[women]",
                                                  "Reputation: <35[women]",
                                                  "Reputation: 35-49[women]",
                                                  "Reputation: 50-64[women]",
                                                  "Reputation: >=65[women]",
                                                  "Social: <35[men−women]",
                                                  "Social: 35-49[men−women]",
                                                  "Social: 50-64[men−women]",
                                                  "Social: >=65[men−women]",
                                                  "Reputation: <35[men−women]", 
                                                  "Reputation: 35-49[men−women]", 
                                                  "Reputation: 50-64[men−women]", 
                                                  "Reputation: >=65[men−women]", 
                                                  "Social: <35[women]",
                                                  "Social: 35-49[women]",
                                                  "Social: 50-64[women]",
                                                  "Social: >=65[women]",
                                                  "Reputation: <35[women]",
                                                  "Reputation: 35-49[women]",
                                                  "Reputation: 50-64[women]",
                                                  "Reputation: >=65[women]",
                                                  "Social: <35[men−women]",
                                                  "Social: 35-49[men−women]",
                                                  "Social: 50-64[men−women]",
                                                  "Social: >=65[men−women]",
                                                  "Reputation: <35[men−women]", 
                                                  "Reputation: 35-49[men−women]", 
                                                  "Reputation: 50-64[men−women]", 
                                                  "Reputation: >=65[men−women]",
                                                  "V_bar", "V_bar"),
                                     Component = c(rep("Zero component", 16), rep("Positive component", 16), 
                                                   "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet2_2

Output_between_GNet2_3 <- data.frame(Mean = c(Pre_between_GNet2_3$mean),
                                     CI5 = c(Pre_between_GNet2_3$`5%`) ,
                                     CI95 = c(Pre_between_GNet2_3$`95%`) ,
                                     Variable = c("Social: patrilocal[women]",
                                                  "Social: bilocal[women]",
                                                  "Social: matrilocal[women]",
                                                  "Reputation: patrilocal[women]",
                                                  "Reputation: bilocal[women]",
                                                  "Reputation: matrilocal[women]",
                                                  "Social: patrilocal[men−women]",
                                                  "Social: bilocal[men−women]",
                                                  "Social: matrilocal[men−women]",
                                                  "Reputation: patrilocal[men−women]", 
                                                  "Reputation: bilocal[men−women]", 
                                                  "Reputation: matrilocal[men−women]", 
                                                  "Social: patrilocal[women]",
                                                  "Social: bilocal[women]",
                                                  "Social: matrilocal[women]",
                                                  "Reputation: patrilocal[women]",
                                                  "Reputation: bilocal[women]",
                                                  "Reputation: matrilocal[women]",
                                                  "Social: patrilocal[men−women]",
                                                  "Social: bilocal[men−women]",
                                                  "Social: matrilocal[men−women]",
                                                  "Reputation: patrilocal[men−women]", 
                                                  "Reputation: bilocal[men−women]", 
                                                  "Reputation: matrilocal[men−women]", 
                                                  "V_bar", "V_bar"),
                                     Component = c(rep("Zero component", 12), rep("Positive component", 12), 
                                                   "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet2_3

Output_between_GNet2_4 <- data.frame(Mean = c(Pre_between_GNet2_4$mean),
                                     CI5 = c(Pre_between_GNet2_4$`5%`) ,
                                     CI95 = c(Pre_between_GNet2_4$`95%`) ,
                                     Variable = c("Social: matrilineal[women]",
                                                  "Social: non-unilineal[women]",
                                                  "Social: patrilineal[women]",
                                                  "Reputation: matrilineal[women]",
                                                  "Reputation: non-unilineal[women]",
                                                  "Reputation: patrilineal[women]",
                                                  "Social: matrilineal[men−women]",
                                                  "Social: non-unilineal[men−women]",
                                                  "Social: patrilineal[men−women]",
                                                  "Reputation: matrilineal[men−women]", 
                                                  "Reputation: non-unilineal[men−women]", 
                                                  "Reputation: patrilineal[men−women]", 
                                                  "Social: matrilineal[women]",
                                                  "Social: non-unilineal[women]",
                                                  "Social: patrilineal[women]",
                                                  "Reputation: matrilineal[women]",
                                                  "Reputation: non-unilineal[women]",
                                                  "Reputation: patrilineal[women]",
                                                  "Social: matrilineal[men−women]",
                                                  "Social: non-unilineal[men−women]",
                                                  "Social: patrilineal[men−women]",
                                                  "Reputation: matrilineal[men−women]", 
                                                  "Reputation: non-unilineal[men−women]", 
                                                  "Reputation: patrilineal[men−women]", 
                                                  "V_bar", "V_bar"),
                                     Component = c(rep("Zero component", 12), rep("Positive component", 12), 
                                                   "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet2_4

Output_between_GNet2_5 <- data.frame(Mean = c(Pre_between_GNet2_5$mean),
                                     CI5 = c(Pre_between_GNet2_5$`5%`) ,
                                     CI95 = c(Pre_between_GNet2_5$`95%`) ,
                                     Variable = c("Social[women]",
                                                  "Reputation[women]",
                                                  "Social[men−women]",
                                                  "Reputation[men−women]", 
                                                  "Market: social[women] ",
                                                  "Market: reputation[women] ",
                                                  "Market: social[men−women] ",
                                                  "Market: reputation[men−women] ",
                                                  "Social[women]",
                                                  "Reputation[women]",
                                                  "Social[men−women]",
                                                  "Reputation[men−women]", 
                                                  "Market: social[women] ",
                                                  "Market: reputation[women] ",
                                                  "Market: social[men−women] ",
                                                  "Market: reputation[men−women] ",
                                                  "V_bar", "V_bar"),
                                     Component = c(rep("Zero component", 8), rep("Positive component", 8), 
                                                   "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet2_5

####1.2.1.2 Direct ----
Output_between_GNet1_1_d <- data.frame(Mean = c(Pre_between_GNet1_1_d$mean),
                                       CI5 = c(Pre_between_GNet1_1_d$`5%`) ,
                                       CI95 = c(Pre_between_GNet1_1_d$`95%`) ,
                                       Variable = c("Consanguineal[women]",
                                                    "Affinal[women]",
                                                    "Friendship[women]",
                                                    "Generosity[women]",
                                                    "Religiosity[women]",
                                                    "Influence[women]",
                                                    "Knowledge[women]",
                                                    
                                                    "Consanguineal[men−women]",
                                                    "Affinal[men−women]",
                                                    "Friendship[men−women]", 
                                                    "Generosity[men−women]", 
                                                    "Religiosity[men−women]",
                                                    "Influence[men−women]",
                                                    "Knowledge[men−women]",
                                                    
                                                    "Consanguineal[women]",
                                                    "Affinal[women]",
                                                    "Friendship[women]",
                                                    "Generosity[women]",
                                                    "Religiosity[women]",
                                                    "Influence[women]",
                                                    "Knowledge[women]",
                                                    
                                                    "Consanguineal[men−women]",
                                                    "Affinal[men−women]",
                                                    "Friendship[men−women]", 
                                                    "Generosity[men−women]", 
                                                    "Religiosity[men−women]",
                                                    "Influence[men−women]",
                                                    "Knowledge[men−women]",
                                                    
                                                    "<35", "35-49", "50-64", ">=65",
                                                    "<35", "35-49", "50-64", ">=65",
                                                    "Non-inheritor", "Inheritor", 
                                                    "Non-inheritor", "Inheritor", 
                                                    "Patrilocal", "Bilocal", "Matrilocal",
                                                    "Patrilocal", "Bilocal", "Matrilocal",
                                                    "Matrilineal", "Non-unilineal", "Patrilineal",
                                                    "Matrilineal", "Non-unilineal", "Patrilineal",
                                                    "Market", "Market",
                                                    "V_bar", "V_bar"), 
                                       Component = c(rep("Zero component", 14), rep("Positive component", 14), 
                                                     rep("Zero component", 4), rep("Positive component", 4),
                                                     rep("Zero component", 2), rep("Positive component", 2),
                                                     rep("Zero component", 3), rep("Positive component", 3),
                                                     rep("Zero component", 3), rep("Positive component", 3),
                                                     "Zero component", "Positive component",
                                                     "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet1_1_d

Output_between_GNet2_1_d <- data.frame(Mean = c(Pre_between_GNet2_1_d$mean),
                                       CI5 = c(Pre_between_GNet2_1_d$`5%`) ,
                                       CI95 = c(Pre_between_GNet2_1_d$`95%`) ,
                                       Variable = c("Social[women]",
                                                    "Reputation[women]",
                                                    "Social[men−women]",
                                                    "Reputation[men−women]", 
                                                    
                                                    "Social[women]",
                                                    "Reputation[women]",
                                                    "Social[men−women]",
                                                    "Reputation[men−women]", 
                                                    
                                                    "<35", "35-49", "50-64", ">=65",
                                                    "<35", "35-49", "50-64", ">=65",
                                                    "Non-inheritor", "Inheritor", 
                                                    "Non-inheritor", "Inheritor", 
                                                    "Patrilocal", "Bilocal", "Matrilocal",
                                                    "Patrilocal", "Bilocal", "Matrilocal",
                                                    "Matrilineal", "Non-unilineal", "Patrilineal",
                                                    "Matrilineal", "Non-unilineal", "Patrilineal",
                                                    "Market", "Market",
                                                    "V_bar", "V_bar"),
                                       Component = c(rep("Zero component", 4), rep("Positive component", 4), 
                                                     rep("Zero component", 4), rep("Positive component", 4),
                                                     rep("Zero component", 2), rep("Positive component", 2),
                                                     rep("Zero component", 3), rep("Positive component", 3),
                                                     rep("Zero component", 3), rep("Positive component", 3),
                                                     "Zero component", "Positive component",
                                                     "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet2_1_d

Output_between_GNet2_2_d <- data.frame(Mean = c(Pre_between_GNet2_2_d$mean),
                                       CI5 = c(Pre_between_GNet2_2_d$`5%`) ,
                                       CI95 = c(Pre_between_GNet2_2_d$`95%`) ,
                                       Variable = c("Social: <35[women]",
                                                    "Social: 35-49[women]",
                                                    "Social: 50-64[women]",
                                                    "Social: >=65[women]",
                                                    "Reputation: <35[women]",
                                                    "Reputation: 35-49[women]",
                                                    "Reputation: 50-64[women]",
                                                    "Reputation: >=65[women]",
                                                    "Social: <35[men−women]",
                                                    "Social: 35-49[men−women]",
                                                    "Social: 50-64[men−women]",
                                                    "Social: >=65[men−women]",
                                                    "Reputation: <35[men−women]", 
                                                    "Reputation: 35-49[men−women]", 
                                                    "Reputation: 50-64[men−women]", 
                                                    "Reputation: >=65[men−women]", 
                                                    
                                                    "Social: <35[women]",
                                                    "Social: 35-49[women]",
                                                    "Social: 50-64[women]",
                                                    "Social: >=65[women]",
                                                    "Reputation: <35[women]",
                                                    "Reputation: 35-49[women]",
                                                    "Reputation: 50-64[women]",
                                                    "Reputation: >=65[women]",
                                                    "Social: <35[men−women]",
                                                    "Social: 35-49[men−women]",
                                                    "Social: 50-64[men−women]",
                                                    "Social: >=65[men−women]",
                                                    "Reputation: <35[men−women]", 
                                                    "Reputation: 35-49[men−women]", 
                                                    "Reputation: 50-64[men−women]", 
                                                    "Reputation: >=65[men−women]", 
                                                    
                                                    "Non-inheritor", "Inheritor", 
                                                    "Non-inheritor", "Inheritor", 
                                                    "Patrilocal", "Bilocal", "Matrilocal",
                                                    "Patrilocal", "Bilocal", "Matrilocal",
                                                    "Matrilineal", "Non-unilineal", "Patrilineal",
                                                    "Matrilineal", "Non-unilineal", "Patrilineal",
                                                    "Market", "Market",
                                                    "V_bar", "V_bar"),
                                       Component = c(rep("Zero component", 16), rep("Positive component", 16), 
                                                     rep("Zero component", 2), rep("Positive component", 2),
                                                     rep("Zero component", 3), rep("Positive component", 3),
                                                     rep("Zero component", 3), rep("Positive component", 3),
                                                     "Zero component", "Positive component",
                                                     "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet2_2_d

Output_between_GNet2_3_d <- data.frame(Mean = c(Pre_between_GNet2_3_d$mean),
                                       CI5 = c(Pre_between_GNet2_3_d$`5%`) ,
                                       CI95 = c(Pre_between_GNet2_3_d$`95%`) ,
                                       Variable = c("Social: patrilocal[women]",
                                                    "Social: bilocal[women]",
                                                    "Social: matrilocal[women]",
                                                    "Reputation: patrilocal[women]",
                                                    "Reputation: bilocal[women]",
                                                    "Reputation: matrilocal[women]",
                                                    "Social: patrilocal[men−women]",
                                                    "Social: bilocal[men−women]",
                                                    "Social: matrilocal[men−women]",
                                                    "Reputation: patrilocal[men−women]", 
                                                    "Reputation: bilocal[men−women]", 
                                                    "Reputation: matrilocal[men−women]", 
                                                    
                                                    "Social: patrilocal[women]",
                                                    "Social: bilocal[women]",
                                                    "Social: matrilocal[women]",
                                                    "Reputation: patrilocal[women]",
                                                    "Reputation: bilocal[women]",
                                                    "Reputation: matrilocal[women]",
                                                    "Social: patrilocal[men−women]",
                                                    "Social: bilocal[men−women]",
                                                    "Social: matrilocal[men−women]",
                                                    "Reputation: patrilocal[men−women]", 
                                                    "Reputation: bilocal[men−women]", 
                                                    "Reputation: matrilocal[men−women]", 
                                                    
                                                    "<35", "35-49", "50-64", ">=65",
                                                    "<35", "35-49", "50-64", ">=65",
                                                    "Non-inheritor", "Inheritor", 
                                                    "Non-inheritor", "Inheritor", 
                                                    "Matrilineal", "Non-unilineal", "Patrilineal",
                                                    "Matrilineal", "Non-unilineal", "Patrilineal",
                                                    "Market", "Market",
                                                    "V_bar", "V_bar"),
                                       Component = c(rep("Zero component", 12), rep("Positive component", 12), 
                                                     rep("Zero component", 4), rep("Positive component", 4),
                                                     rep("Zero component", 2), rep("Positive component", 2),
                                                     rep("Zero component", 3), rep("Positive component", 3),
                                                     "Zero component", "Positive component",
                                                     "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet2_3_d

Output_between_GNet2_4_d <- data.frame(Mean = c(Pre_between_GNet2_4_d$mean),
                                       CI5 = c(Pre_between_GNet2_4_d$`5%`) ,
                                       CI95 = c(Pre_between_GNet2_4_d$`95%`) ,
                                       Variable = c("Social: matrilineal[women]",
                                                    "Social: non-unilineal[women]",
                                                    "Social: patrilineal[women]",
                                                    "Reputation: matrilineal[women]",
                                                    "Reputation: non-unilineal[women]",
                                                    "Reputation: patrilineal[women]",
                                                    "Social: matrilineal[men−women]",
                                                    "Social: non-unilineal[men−women]",
                                                    "Social: patrilineal[men−women]",
                                                    "Reputation: matrilineal[men−women]", 
                                                    "Reputation: non-unilineal[men−women]", 
                                                    "Reputation: patrilineal[men−women]", 
                                                    
                                                    "Social: matrilineal[women]",
                                                    "Social: non-unilineal[women]",
                                                    "Social: patrilineal[women]",
                                                    "Reputation: matrilineal[women]",
                                                    "Reputation: non-unilineal[women]",
                                                    "Reputation: patrilineal[women]",
                                                    "Social: matrilineal[men−women]",
                                                    "Social: non-unilineal[men−women]",
                                                    "Social: patrilineal[men−women]",
                                                    "Reputation: matrilineal[men−women]", 
                                                    "Reputation: non-unilineal[men−women]", 
                                                    "Reputation: patrilineal[men−women]", 
                                                    
                                                    "<35", "35-49", "50-64", ">=65",
                                                    "<35", "35-49", "50-64", ">=65",
                                                    "Non-inheritor", "Inheritor", 
                                                    "Non-inheritor", "Inheritor", 
                                                    "Patrilocal", "Bilocal", "Matrilocal",
                                                    "Patrilocal", "Bilocal", "Matrilocal",
                                                    "Market", "Market",
                                                    "V_bar", "V_bar"),
                                       Component = c(rep("Zero component", 12), rep("Positive component", 12), 
                                                     rep("Zero component", 4), rep("Positive component", 4),
                                                     rep("Zero component", 2), rep("Positive component", 2),
                                                     rep("Zero component", 3), rep("Positive component", 3),
                                                     "Zero component", "Positive component",
                                                     "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet2_4_d

Output_between_GNet2_5_d <- data.frame(Mean = c(Pre_between_GNet2_5_d$mean),
                                       CI5 = c(Pre_between_GNet2_5_d$`5%`) ,
                                       CI95 = c(Pre_between_GNet2_5_d$`95%`) ,
                                       Variable = c("Social[women]",
                                                    "Reputation[women]",
                                                    "Social[men−women]",
                                                    "Reputation[men−women]", 
                                                    "Market: social[women] ",
                                                    "Market: reputation[women] ",
                                                    "Market: social[men−women] ",
                                                    "Market: reputation[men−women] ",
                                                    
                                                    "Social[women]",
                                                    "Reputation[women]",
                                                    "Social[men−women]",
                                                    "Reputation[men−women]", 
                                                    "Market: social[women] ",
                                                    "Market: reputation[women] ",
                                                    "Market: social[men−women] ",
                                                    "Market: reputation[men−women] ",
                                                    
                                                    "<35", "35-49", "50-64", ">=65",
                                                    "<35", "35-49", "50-64", ">=65",
                                                    "Non-inheritor", "Inheritor", 
                                                    "Non-inheritor", "Inheritor", 
                                                    "Patrilocal", "Bilocal", "Matrilocal",
                                                    "Patrilocal", "Bilocal", "Matrilocal",
                                                    "Matrilineal", "Non-unilineal", "Patrilineal",
                                                    "Matrilineal", "Non-unilineal", "Patrilineal",
                                                    "V_bar", "V_bar"),
                                       Component = c(rep("Zero component", 8), rep("Positive component", 8), 
                                                     rep("Zero component", 4), rep("Positive component", 4),
                                                     rep("Zero component", 2), rep("Positive component", 2),
                                                     rep("Zero component", 3), rep("Positive component", 3),
                                                     rep("Zero component", 3), rep("Positive component", 3),
                                                     "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_between_GNet2_5_d

###1.2.2 Inward harmonic centrality ----
####1.2.2.1 Total ----
Output_harmonic_in_GNet1_1 <- data.frame(Mean = c(Pre_harmonic_in_GNet1_1$mean),
                                         CI5 = c(Pre_harmonic_in_GNet1_1$`5%`) ,
                                         CI95 = c(Pre_harmonic_in_GNet1_1$`95%`) ,
                                         Variable = c("Consanguineal[women]",
                                                      "Affinal[women]",
                                                      "Friendship[women]",
                                                      "Generosity[women]",
                                                      "Religiosity[women]",
                                                      "Influence[women]",
                                                      "Knowledge[women]",
                                                      "Consanguineal[men−women]",
                                                      "Affinal[men−women]",
                                                      "Friendship[men−women]", 
                                                      "Generosity[men−women]", 
                                                      "Religiosity[men−women]",
                                                      "Influence[men−women]",
                                                      "Knowledge[men−women]", 
                                                      "Consanguineal[women]",
                                                      "Affinal[women]",
                                                      "Friendship[women]",
                                                      "Generosity[women]",
                                                      "Religiosity[women]",
                                                      "Influence[women]",
                                                      "Knowledge[women]",
                                                      "Consanguineal[men−women]",
                                                      "Affinal[men−women]",
                                                      "Friendship[men−women]", 
                                                      "Generosity[men−women]", 
                                                      "Religiosity[men−women]",
                                                      "Influence[men−women]",
                                                      "Knowledge[men−women]", 
                                                      "V_bar", "V_bar"), 
                                         Component = c(rep("Zero component", 14), rep("Positive component", 14), 
                                                       "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet1_1

Output_harmonic_in_GNet2_1 <- data.frame(Mean = c(Pre_harmonic_in_GNet2_1$mean),
                                         CI5 = c(Pre_harmonic_in_GNet2_1$`5%`) ,
                                         CI95 = c(Pre_harmonic_in_GNet2_1$`95%`) ,
                                         Variable = c("Social[women]",
                                                      "Reputation[women]",
                                                      "Social[men−women]",
                                                      "Reputation[men−women]", 
                                                      "Social[women]",
                                                      "Reputation[women]",
                                                      "Social[men−women]",
                                                      "Reputation[men−women]", 
                                                      "V_bar", "V_bar"), 
                                         Component = c(rep("Zero component", 4), rep("Positive component", 4), 
                                                       "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet2_1

Output_harmonic_in_GNet2_2 <- data.frame(Mean = c(Pre_harmonic_in_GNet2_2$mean),
                                         CI5 = c(Pre_harmonic_in_GNet2_2$`5%`) ,
                                         CI95 = c(Pre_harmonic_in_GNet2_2$`95%`) ,
                                         Variable = c("Social: <35[women]",
                                                      "Social: 35-49[women]",
                                                      "Social: 50-64[women]",
                                                      "Social: >=65[women]",
                                                      "Reputation: <35[women]",
                                                      "Reputation: 35-49[women]",
                                                      "Reputation: 50-64[women]",
                                                      "Reputation: >=65[women]",
                                                      "Social: <35[men−women]",
                                                      "Social: 35-49[men−women]",
                                                      "Social: 50-64[men−women]",
                                                      "Social: >=65[men−women]",
                                                      "Reputation: <35[men−women]", 
                                                      "Reputation: 35-49[men−women]", 
                                                      "Reputation: 50-64[men−women]", 
                                                      "Reputation: >=65[men−women]", 
                                                      "Social: <35[women]",
                                                      "Social: 35-49[women]",
                                                      "Social: 50-64[women]",
                                                      "Social: >=65[women]",
                                                      "Reputation: <35[women]",
                                                      "Reputation: 35-49[women]",
                                                      "Reputation: 50-64[women]",
                                                      "Reputation: >=65[women]",
                                                      "Social: <35[men−women]",
                                                      "Social: 35-49[men−women]",
                                                      "Social: 50-64[men−women]",
                                                      "Social: >=65[men−women]",
                                                      "Reputation: <35[men−women]", 
                                                      "Reputation: 35-49[men−women]", 
                                                      "Reputation: 50-64[men−women]", 
                                                      "Reputation: >=65[men−women]",
                                                      "V_bar", "V_bar"),
                                         Component = c(rep("Zero component", 16), rep("Positive component", 16), 
                                                       "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet2_2

Output_harmonic_in_GNet2_3 <- data.frame(Mean = c(Pre_harmonic_in_GNet2_3$mean),
                                         CI5 = c(Pre_harmonic_in_GNet2_3$`5%`) ,
                                         CI95 = c(Pre_harmonic_in_GNet2_3$`95%`) ,
                                         Variable = c("Social: patrilocal[women]",
                                                      "Social: bilocal[women]",
                                                      "Social: matrilocal[women]",
                                                      "Reputation: patrilocal[women]",
                                                      "Reputation: bilocal[women]",
                                                      "Reputation: matrilocal[women]",
                                                      "Social: patrilocal[men−women]",
                                                      "Social: bilocal[men−women]",
                                                      "Social: matrilocal[men−women]",
                                                      "Reputation: patrilocal[men−women]", 
                                                      "Reputation: bilocal[men−women]", 
                                                      "Reputation: matrilocal[men−women]", 
                                                      "Social: patrilocal[women]",
                                                      "Social: bilocal[women]",
                                                      "Social: matrilocal[women]",
                                                      "Reputation: patrilocal[women]",
                                                      "Reputation: bilocal[women]",
                                                      "Reputation: matrilocal[women]",
                                                      "Social: patrilocal[men−women]",
                                                      "Social: bilocal[men−women]",
                                                      "Social: matrilocal[men−women]",
                                                      "Reputation: patrilocal[men−women]", 
                                                      "Reputation: bilocal[men−women]", 
                                                      "Reputation: matrilocal[men−women]", 
                                                      "V_bar", "V_bar"),
                                         Component = c(rep("Zero component", 12), rep("Positive component", 12), 
                                                       "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet2_3

Output_harmonic_in_GNet2_4 <- data.frame(Mean = c(Pre_harmonic_in_GNet2_4$mean),
                                         CI5 = c(Pre_harmonic_in_GNet2_4$`5%`) ,
                                         CI95 = c(Pre_harmonic_in_GNet2_4$`95%`) ,
                                         Variable = c("Social: matrilineal[women]",
                                                      "Social: non-unilineal[women]",
                                                      "Social: patrilineal[women]",
                                                      "Reputation: matrilineal[women]",
                                                      "Reputation: non-unilineal[women]",
                                                      "Reputation: patrilineal[women]",
                                                      "Social: matrilineal[men−women]",
                                                      "Social: non-unilineal[men−women]",
                                                      "Social: patrilineal[men−women]",
                                                      "Reputation: matrilineal[men−women]", 
                                                      "Reputation: non-unilineal[men−women]", 
                                                      "Reputation: patrilineal[men−women]", 
                                                      "Social: matrilineal[women]",
                                                      "Social: non-unilineal[women]",
                                                      "Social: patrilineal[women]",
                                                      "Reputation: matrilineal[women]",
                                                      "Reputation: non-unilineal[women]",
                                                      "Reputation: patrilineal[women]",
                                                      "Social: matrilineal[men−women]",
                                                      "Social: non-unilineal[men−women]",
                                                      "Social: patrilineal[men−women]",
                                                      "Reputation: matrilineal[men−women]", 
                                                      "Reputation: non-unilineal[men−women]", 
                                                      "Reputation: patrilineal[men−women]", 
                                                      "V_bar", "V_bar"),
                                         Component = c(rep("Zero component", 12), rep("Positive component", 12), 
                                                       "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet2_4

Output_harmonic_in_GNet2_5 <- data.frame(Mean = c(Pre_harmonic_in_GNet2_5$mean),
                                         CI5 = c(Pre_harmonic_in_GNet2_5$`5%`) ,
                                         CI95 = c(Pre_harmonic_in_GNet2_5$`95%`) ,
                                         Variable = c("Social[women]",
                                                      "Reputation[women]",
                                                      "Social[men−women]",
                                                      "Reputation[men−women]", 
                                                      "Market: social[women] ",
                                                      "Market: reputation[women] ",
                                                      "Market: social[men−women] ",
                                                      "Market: reputation[men−women] ",
                                                      "Social[women]",
                                                      "Reputation[women]",
                                                      "Social[men−women]",
                                                      "Reputation[men−women]", 
                                                      "Market: social[women] ",
                                                      "Market: reputation[women] ",
                                                      "Market: social[men−women] ",
                                                      "Market: reputation[men−women] ",
                                                      "V_bar", "V_bar"),
                                         Component = c(rep("Zero component", 8), rep("Positive component", 8), 
                                                       "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet2_5

####1.2.2.2 Direct ----
Output_harmonic_in_GNet1_1_d <- data.frame(Mean = c(Pre_harmonic_in_GNet1_1_d$mean),
                                           CI5 = c(Pre_harmonic_in_GNet1_1_d$`5%`) ,
                                           CI95 = c(Pre_harmonic_in_GNet1_1_d$`95%`) ,
                                           Variable = c("Consanguineal[women]",
                                                        "Affinal[women]",
                                                        "Friendship[women]",
                                                        "Generosity[women]",
                                                        "Religiosity[women]",
                                                        "Influence[women]",
                                                        "Knowledge[women]",
                                                        
                                                        "Consanguineal[men−women]",
                                                        "Affinal[men−women]",
                                                        "Friendship[men−women]", 
                                                        "Generosity[men−women]", 
                                                        "Religiosity[men−women]",
                                                        "Influence[men−women]",
                                                        "Knowledge[men−women]",
                                                        
                                                        "Consanguineal[women]",
                                                        "Affinal[women]",
                                                        "Friendship[women]",
                                                        "Generosity[women]",
                                                        "Religiosity[women]",
                                                        "Influence[women]",
                                                        "Knowledge[women]",
                                                        
                                                        "Consanguineal[men−women]",
                                                        "Affinal[men−women]",
                                                        "Friendship[men−women]", 
                                                        "Generosity[men−women]", 
                                                        "Religiosity[men−women]",
                                                        "Influence[men−women]",
                                                        "Knowledge[men−women]",
                                                        
                                                        "<35", "35-49", "50-64", ">=65",
                                                        "<35", "35-49", "50-64", ">=65",
                                                        "Non-inheritor", "Inheritor", 
                                                        "Non-inheritor", "Inheritor", 
                                                        "Patrilocal", "Bilocal", "Matrilocal",
                                                        "Patrilocal", "Bilocal", "Matrilocal",
                                                        "Matrilineal", "Non-unilineal", "Patrilineal",
                                                        "Matrilineal", "Non-unilineal", "Patrilineal",
                                                        "Market", "Market",
                                                        "V_bar", "V_bar"), 
                                           Component = c(rep("Zero component", 14), rep("Positive component", 14), 
                                                         rep("Zero component", 4), rep("Positive component", 4),
                                                         rep("Zero component", 2), rep("Positive component", 2),
                                                         rep("Zero component", 3), rep("Positive component", 3),
                                                         rep("Zero component", 3), rep("Positive component", 3),
                                                         "Zero component", "Positive component",
                                                         "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet1_1_d

Output_harmonic_in_GNet2_1_d <- data.frame(Mean = c(Pre_harmonic_in_GNet2_1_d$mean),
                                           CI5 = c(Pre_harmonic_in_GNet2_1_d$`5%`) ,
                                           CI95 = c(Pre_harmonic_in_GNet2_1_d$`95%`) ,
                                           Variable = c("Social[women]",
                                                        "Reputation[women]",
                                                        "Social[men−women]",
                                                        "Reputation[men−women]", 
                                                        
                                                        "Social[women]",
                                                        "Reputation[women]",
                                                        "Social[men−women]",
                                                        "Reputation[men−women]", 
                                                        
                                                        "<35", "35-49", "50-64", ">=65",
                                                        "<35", "35-49", "50-64", ">=65",
                                                        "Non-inheritor", "Inheritor", 
                                                        "Non-inheritor", "Inheritor", 
                                                        "Patrilocal", "Bilocal", "Matrilocal",
                                                        "Patrilocal", "Bilocal", "Matrilocal",
                                                        "Matrilineal", "Non-unilineal", "Patrilineal",
                                                        "Matrilineal", "Non-unilineal", "Patrilineal",
                                                        "Market", "Market",
                                                        "V_bar", "V_bar"),
                                           Component = c(rep("Zero component", 4), rep("Positive component", 4), 
                                                         rep("Zero component", 4), rep("Positive component", 4),
                                                         rep("Zero component", 2), rep("Positive component", 2),
                                                         rep("Zero component", 3), rep("Positive component", 3),
                                                         rep("Zero component", 3), rep("Positive component", 3),
                                                         "Zero component", "Positive component",
                                                         "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet2_1_d

Output_harmonic_in_GNet2_2_d <- data.frame(Mean = c(Pre_harmonic_in_GNet2_2_d$mean),
                                           CI5 = c(Pre_harmonic_in_GNet2_2_d$`5%`) ,
                                           CI95 = c(Pre_harmonic_in_GNet2_2_d$`95%`) ,
                                           Variable = c("Social: <35[women]",
                                                        "Social: 35-49[women]",
                                                        "Social: 50-64[women]",
                                                        "Social: >=65[women]",
                                                        "Reputation: <35[women]",
                                                        "Reputation: 35-49[women]",
                                                        "Reputation: 50-64[women]",
                                                        "Reputation: >=65[women]",
                                                        "Social: <35[men−women]",
                                                        "Social: 35-49[men−women]",
                                                        "Social: 50-64[men−women]",
                                                        "Social: >=65[men−women]",
                                                        "Reputation: <35[men−women]", 
                                                        "Reputation: 35-49[men−women]", 
                                                        "Reputation: 50-64[men−women]", 
                                                        "Reputation: >=65[men−women]", 
                                                        
                                                        "Social: <35[women]",
                                                        "Social: 35-49[women]",
                                                        "Social: 50-64[women]",
                                                        "Social: >=65[women]",
                                                        "Reputation: <35[women]",
                                                        "Reputation: 35-49[women]",
                                                        "Reputation: 50-64[women]",
                                                        "Reputation: >=65[women]",
                                                        "Social: <35[men−women]",
                                                        "Social: 35-49[men−women]",
                                                        "Social: 50-64[men−women]",
                                                        "Social: >=65[men−women]",
                                                        "Reputation: <35[men−women]", 
                                                        "Reputation: 35-49[men−women]", 
                                                        "Reputation: 50-64[men−women]", 
                                                        "Reputation: >=65[men−women]", 
                                                        
                                                        "Non-inheritor", "Inheritor", 
                                                        "Non-inheritor", "Inheritor", 
                                                        "Patrilocal", "Bilocal", "Matrilocal",
                                                        "Patrilocal", "Bilocal", "Matrilocal",
                                                        "Matrilineal", "Non-unilineal", "Patrilineal",
                                                        "Matrilineal", "Non-unilineal", "Patrilineal",
                                                        "Market", "Market",
                                                        "V_bar", "V_bar"),
                                           Component = c(rep("Zero component", 16), rep("Positive component", 16), 
                                                         rep("Zero component", 2), rep("Positive component", 2),
                                                         rep("Zero component", 3), rep("Positive component", 3),
                                                         rep("Zero component", 3), rep("Positive component", 3),
                                                         "Zero component", "Positive component",
                                                         "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet2_2_d

Output_harmonic_in_GNet2_3_d <- data.frame(Mean = c(Pre_harmonic_in_GNet2_3_d$mean),
                                           CI5 = c(Pre_harmonic_in_GNet2_3_d$`5%`) ,
                                           CI95 = c(Pre_harmonic_in_GNet2_3_d$`95%`) ,
                                           Variable = c("Social: patrilocal[women]",
                                                        "Social: bilocal[women]",
                                                        "Social: matrilocal[women]",
                                                        "Reputation: patrilocal[women]",
                                                        "Reputation: bilocal[women]",
                                                        "Reputation: matrilocal[women]",
                                                        "Social: patrilocal[men−women]",
                                                        "Social: bilocal[men−women]",
                                                        "Social: matrilocal[men−women]",
                                                        "Reputation: patrilocal[men−women]", 
                                                        "Reputation: bilocal[men−women]", 
                                                        "Reputation: matrilocal[men−women]", 
                                                        
                                                        "Social: patrilocal[women]",
                                                        "Social: bilocal[women]",
                                                        "Social: matrilocal[women]",
                                                        "Reputation: patrilocal[women]",
                                                        "Reputation: bilocal[women]",
                                                        "Reputation: matrilocal[women]",
                                                        "Social: patrilocal[men−women]",
                                                        "Social: bilocal[men−women]",
                                                        "Social: matrilocal[men−women]",
                                                        "Reputation: patrilocal[men−women]", 
                                                        "Reputation: bilocal[men−women]", 
                                                        "Reputation: matrilocal[men−women]", 
                                                        
                                                        "<35", "35-49", "50-64", ">=65",
                                                        "<35", "35-49", "50-64", ">=65",
                                                        "Non-inheritor", "Inheritor", 
                                                        "Non-inheritor", "Inheritor", 
                                                        "Matrilineal", "Non-unilineal", "Patrilineal",
                                                        "Matrilineal", "Non-unilineal", "Patrilineal",
                                                        "Market", "Market",
                                                        "V_bar", "V_bar"),
                                           Component = c(rep("Zero component", 12), rep("Positive component", 12), 
                                                         rep("Zero component", 4), rep("Positive component", 4),
                                                         rep("Zero component", 2), rep("Positive component", 2),
                                                         rep("Zero component", 3), rep("Positive component", 3),
                                                         "Zero component", "Positive component",
                                                         "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet2_3_d

Output_harmonic_in_GNet2_4_d <- data.frame(Mean = c(Pre_harmonic_in_GNet2_4_d$mean),
                                           CI5 = c(Pre_harmonic_in_GNet2_4_d$`5%`) ,
                                           CI95 = c(Pre_harmonic_in_GNet2_4_d$`95%`) ,
                                           Variable = c("Social: matrilineal[women]",
                                                        "Social: non-unilineal[women]",
                                                        "Social: patrilineal[women]",
                                                        "Reputation: matrilineal[women]",
                                                        "Reputation: non-unilineal[women]",
                                                        "Reputation: patrilineal[women]",
                                                        "Social: matrilineal[men−women]",
                                                        "Social: non-unilineal[men−women]",
                                                        "Social: patrilineal[men−women]",
                                                        "Reputation: matrilineal[men−women]", 
                                                        "Reputation: non-unilineal[men−women]", 
                                                        "Reputation: patrilineal[men−women]", 
                                                        
                                                        "Social: matrilineal[women]",
                                                        "Social: non-unilineal[women]",
                                                        "Social: patrilineal[women]",
                                                        "Reputation: matrilineal[women]",
                                                        "Reputation: non-unilineal[women]",
                                                        "Reputation: patrilineal[women]",
                                                        "Social: matrilineal[men−women]",
                                                        "Social: non-unilineal[men−women]",
                                                        "Social: patrilineal[men−women]",
                                                        "Reputation: matrilineal[men−women]", 
                                                        "Reputation: non-unilineal[men−women]", 
                                                        "Reputation: patrilineal[men−women]", 
                                                        
                                                        "<35", "35-49", "50-64", ">=65",
                                                        "<35", "35-49", "50-64", ">=65",
                                                        "Non-inheritor", "Inheritor", 
                                                        "Non-inheritor", "Inheritor", 
                                                        "Patrilocal", "Bilocal", "Matrilocal",
                                                        "Patrilocal", "Bilocal", "Matrilocal",
                                                        "Market", "Market",
                                                        "V_bar", "V_bar"),
                                           Component = c(rep("Zero component", 12), rep("Positive component", 12), 
                                                         rep("Zero component", 4), rep("Positive component", 4),
                                                         rep("Zero component", 2), rep("Positive component", 2),
                                                         rep("Zero component", 3), rep("Positive component", 3),
                                                         "Zero component", "Positive component",
                                                         "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet2_4_d

Output_harmonic_in_GNet2_5_d <- data.frame(Mean = c(Pre_harmonic_in_GNet2_5_d$mean),
                                           CI5 = c(Pre_harmonic_in_GNet2_5_d$`5%`) ,
                                           CI95 = c(Pre_harmonic_in_GNet2_5_d$`95%`) ,
                                           Variable = c("Social[women]",
                                                        "Reputation[women]",
                                                        "Social[men−women]",
                                                        "Reputation[men−women]", 
                                                        "Market: social[women] ",
                                                        "Market: reputation[women] ",
                                                        "Market: social[men−women] ",
                                                        "Market: reputation[men−women] ",
                                                        
                                                        "Social[women]",
                                                        "Reputation[women]",
                                                        "Social[men−women]",
                                                        "Reputation[men−women]", 
                                                        "Market: social[women] ",
                                                        "Market: reputation[women] ",
                                                        "Market: social[men−women] ",
                                                        "Market: reputation[men−women] ",
                                                        
                                                        "<35", "35-49", "50-64", ">=65",
                                                        "<35", "35-49", "50-64", ">=65",
                                                        "Non-inheritor", "Inheritor", 
                                                        "Non-inheritor", "Inheritor", 
                                                        "Patrilocal", "Bilocal", "Matrilocal",
                                                        "Patrilocal", "Bilocal", "Matrilocal",
                                                        "Matrilineal", "Non-unilineal", "Patrilineal",
                                                        "Matrilineal", "Non-unilineal", "Patrilineal",
                                                        "V_bar", "V_bar"),
                                           Component = c(rep("Zero component", 8), rep("Positive component", 8), 
                                                         rep("Zero component", 4), rep("Positive component", 4),
                                                         rep("Zero component", 2), rep("Positive component", 2),
                                                         rep("Zero component", 3), rep("Positive component", 3),
                                                         rep("Zero component", 3), rep("Positive component", 3),
                                                         "Zero component", "Positive component")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar")),
         Component = factor(Component,
                            levels = c("Positive component", "Zero component")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  arrange(Component, Variable) %>%
  dplyr::select(Component, Variable, "Mean [CI]") %>%
  gt(groupname_col = "Component") %>%
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  tab_style( style = cell_text( weight = "bold" ),
             locations = cells_row_groups() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_harmonic_in_GNet2_5_d

###1.2.3 Triangle count ----
####1.2.3.1 Total ----
Output_triangle_GNet1_1 <- data.frame(Mean = c(Pre_triangle_GNet1_1$mean),
                                      CI5 = c(Pre_triangle_GNet1_1$`5%`) ,
                                      CI95 = c(Pre_triangle_GNet1_1$`95%`) ,
                                      Variable = c("Consanguineal[women]",
                                                   "Affinal[women]",
                                                   "Friendship[women]",
                                                   "Generosity[women]",
                                                   "Religiosity[women]",
                                                   "Influence[women]",
                                                   "Knowledge[women]",
                                                   "Consanguineal[men−women]",
                                                   "Affinal[men−women]",
                                                   "Friendship[men−women]", 
                                                   "Generosity[men−women]", 
                                                   "Religiosity[men−women]",
                                                   "Influence[men−women]",
                                                   "Knowledge[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet1_1

Output_triangle_GNet2_1 <- data.frame(Mean = c(Pre_triangle_GNet2_1$mean),
                                      CI5 = c(Pre_triangle_GNet2_1$`5%`) ,
                                      CI95 = c(Pre_triangle_GNet2_1$`95%`) ,
                                      Variable = c("Social[women]",
                                                   "Reputation[women]",
                                                   "Social[men−women]",
                                                   "Reputation[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet2_1

Output_triangle_GNet2_2 <- data.frame(Mean = c(Pre_triangle_GNet2_2$mean),
                                      CI5 = c(Pre_triangle_GNet2_2$`5%`) ,
                                      CI95 = c(Pre_triangle_GNet2_2$`95%`) ,
                                      Variable = c("Social: <35[women]",
                                                   "Social: 35-49[women]",
                                                   "Social: 50-64[women]",
                                                   "Social: >=65[women]",
                                                   "Reputation: <35[women]",
                                                   "Reputation: 35-49[women]",
                                                   "Reputation: 50-64[women]",
                                                   "Reputation: >=65[women]",
                                                   "Social: <35[men−women]",
                                                   "Social: 35-49[men−women]",
                                                   "Social: 50-64[men−women]",
                                                   "Social: >=65[men−women]",
                                                   "Reputation: <35[men−women]", 
                                                   "Reputation: 35-49[men−women]", 
                                                   "Reputation: 50-64[men−women]", 
                                                   "Reputation: >=65[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet2_2

Output_triangle_GNet2_3 <- data.frame(Mean = c(Pre_triangle_GNet2_3$mean),
                                      CI5 = c(Pre_triangle_GNet2_3$`5%`) ,
                                      CI95 = c(Pre_triangle_GNet2_3$`95%`) ,
                                      Variable = c("Social: patrilocal[women]",
                                                   "Social: bilocal[women]",
                                                   "Social: matrilocal[women]",
                                                   "Reputation: patrilocal[women]",
                                                   "Reputation: bilocal[women]",
                                                   "Reputation: matrilocal[women]",
                                                   "Social: patrilocal[men−women]",
                                                   "Social: bilocal[men−women]",
                                                   "Social: matrilocal[men−women]",
                                                   "Reputation: patrilocal[men−women]", 
                                                   "Reputation: bilocal[men−women]", 
                                                   "Reputation: matrilocal[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet2_3

Output_triangle_GNet2_4 <- data.frame(Mean = c(Pre_triangle_GNet2_4$mean),
                                      CI5 = c(Pre_triangle_GNet2_4$`5%`) ,
                                      CI95 = c(Pre_triangle_GNet2_4$`95%`) ,
                                      Variable = c("Social: matrilineal[women]",
                                                   "Social: non-unilineal[women]",
                                                   "Social: patrilineal[women]",
                                                   "Reputation: matrilineal[women]",
                                                   "Reputation: non-unilineal[women]",
                                                   "Reputation: patrilineal[women]",
                                                   "Social: matrilineal[men−women]",
                                                   "Social: non-unilineal[men−women]",
                                                   "Social: patrilineal[men−women]",
                                                   "Reputation: matrilineal[men−women]", 
                                                   "Reputation: non-unilineal[men−women]", 
                                                   "Reputation: patrilineal[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet2_4

Output_triangle_GNet2_5 <- data.frame(Mean = c(Pre_triangle_GNet2_5$mean),
                                      CI5 = c(Pre_triangle_GNet2_5$`5%`) ,
                                      CI95 = c(Pre_triangle_GNet2_5$`95%`) ,
                                      Variable = c("Social[women]",
                                                   "Reputation[women]",
                                                   "Social[men−women]",
                                                   "Reputation[men−women]", 
                                                   "Market: social[women] ",
                                                   "Market: reputation[women] ",
                                                   "Market: social[men−women] ",
                                                   "Market: reputation[men−women] ",
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet2_5

####1.2.3.2 Direct ----
Output_triangle_GNet1_1_d <- data.frame(Mean = c(Pre_triangle_GNet1_1_d$mean),
                                        CI5 = c(Pre_triangle_GNet1_1_d$`5%`) ,
                                        CI95 = c(Pre_triangle_GNet1_1_d$`95%`) ,
                                        Variable = c("Consanguineal[women]",
                                                     "Affinal[women]",
                                                     "Friendship[women]",
                                                     "Generosity[women]",
                                                     "Religiosity[women]",
                                                     "Influence[women]",
                                                     "Knowledge[women]",
                                                     "Consanguineal[men−women]",
                                                     "Affinal[men−women]",
                                                     "Friendship[men−women]", 
                                                     "Generosity[men−women]", 
                                                     "Religiosity[men−women]",
                                                     "Influence[men−women]",
                                                     "Knowledge[men−women]",
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet1_1_d

Output_triangle_GNet2_1_d <- data.frame(Mean = c(Pre_triangle_GNet2_1_d$mean),
                                        CI5 = c(Pre_triangle_GNet2_1_d$`5%`) ,
                                        CI95 = c(Pre_triangle_GNet2_1_d$`95%`) ,
                                        Variable = c("Social[women]",
                                                     "Reputation[women]",
                                                     "Social[men−women]",
                                                     "Reputation[men−women]", 
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet2_1_d

Output_triangle_GNet2_2_d <- data.frame(Mean = c(Pre_triangle_GNet2_2_d$mean),
                                        CI5 = c(Pre_triangle_GNet2_2_d$`5%`) ,
                                        CI95 = c(Pre_triangle_GNet2_2_d$`95%`) ,
                                        Variable = c("Social: <35[women]",
                                                     "Social: 35-49[women]",
                                                     "Social: 50-64[women]",
                                                     "Social: >=65[women]",
                                                     "Reputation: <35[women]",
                                                     "Reputation: 35-49[women]",
                                                     "Reputation: 50-64[women]",
                                                     "Reputation: >=65[women]",
                                                     "Social: <35[men−women]",
                                                     "Social: 35-49[men−women]",
                                                     "Social: 50-64[men−women]",
                                                     "Social: >=65[men−women]",
                                                     "Reputation: <35[men−women]", 
                                                     "Reputation: 35-49[men−women]", 
                                                     "Reputation: 50-64[men−women]", 
                                                     "Reputation: >=65[men−women]", 
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet2_2_d

Output_triangle_GNet2_3_d <- data.frame(Mean = c(Pre_triangle_GNet2_3_d$mean),
                                        CI5 = c(Pre_triangle_GNet2_3_d$`5%`) ,
                                        CI95 = c(Pre_triangle_GNet2_3_d$`95%`) ,
                                        Variable = c("Social: patrilocal[women]",
                                                     "Social: bilocal[women]",
                                                     "Social: matrilocal[women]",
                                                     "Reputation: patrilocal[women]",
                                                     "Reputation: bilocal[women]",
                                                     "Reputation: matrilocal[women]",
                                                     "Social: patrilocal[men−women]",
                                                     "Social: bilocal[men−women]",
                                                     "Social: matrilocal[men−women]",
                                                     "Reputation: patrilocal[men−women]", 
                                                     "Reputation: bilocal[men−women]", 
                                                     "Reputation: matrilocal[men−women]", 
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet2_3_d

Output_triangle_GNet2_4_d <- data.frame(Mean = c(Pre_triangle_GNet2_4_d$mean),
                                        CI5 = c(Pre_triangle_GNet2_4_d$`5%`) ,
                                        CI95 = c(Pre_triangle_GNet2_4_d$`95%`) ,
                                        Variable = c("Social: matrilineal[women]",
                                                     "Social: non-unilineal[women]",
                                                     "Social: patrilineal[women]",
                                                     "Reputation: matrilineal[women]",
                                                     "Reputation: non-unilineal[women]",
                                                     "Reputation: patrilineal[women]",
                                                     "Social: matrilineal[men−women]",
                                                     "Social: non-unilineal[men−women]",
                                                     "Social: patrilineal[men−women]",
                                                     "Reputation: matrilineal[men−women]", 
                                                     "Reputation: non-unilineal[men−women]", 
                                                     "Reputation: patrilineal[men−women]", 
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: matrilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Social: patrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: matrilineal[men−women]",
                                      "Reputation: patrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: matrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: patrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: matrilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Social: patrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: matrilineal[men−women]",
                                      "Reputation: patrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: matrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet2_4_d

Output_triangle_GNet2_5_d <- data.frame(Mean = c(Pre_triangle_GNet2_5_d$mean),
                                        CI5 = c(Pre_triangle_GNet2_5_d$`5%`) ,
                                        CI95 = c(Pre_triangle_GNet2_5_d$`95%`) ,
                                        Variable = c("Social[women]",
                                                     "Reputation[women]",
                                                     "Social[men−women]",
                                                     "Reputation[men−women]", 
                                                     "Market: social[women] ",
                                                     "Market: reputation[women] ",
                                                     "Market: social[men−women] ",
                                                     "Market: reputation[men−women] ",
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_triangle_GNet2_5_d

###1.2.4 Community size ----
####1.2.4.1 Total ----
Output_commsize_GNet1_1 <- data.frame(Mean = c(Pre_commsize_GNet1_1$mean),
                                      CI5 = c(Pre_commsize_GNet1_1$`5%`) ,
                                      CI95 = c(Pre_commsize_GNet1_1$`95%`) ,
                                      Variable = c("Consanguineal[women]",
                                                   "Affinal[women]",
                                                   "Friendship[women]",
                                                   "Generosity[women]",
                                                   "Religiosity[women]",
                                                   "Influence[women]",
                                                   "Knowledge[women]",
                                                   "Consanguineal[men−women]",
                                                   "Affinal[men−women]",
                                                   "Friendship[men−women]", 
                                                   "Generosity[men−women]", 
                                                   "Religiosity[men−women]",
                                                   "Influence[men−women]",
                                                   "Knowledge[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet1_1

Output_commsize_GNet2_1 <- data.frame(Mean = c(Pre_commsize_GNet2_1$mean),
                                      CI5 = c(Pre_commsize_GNet2_1$`5%`) ,
                                      CI95 = c(Pre_commsize_GNet2_1$`95%`) ,
                                      Variable = c("Social[women]",
                                                   "Reputation[women]",
                                                   "Social[men−women]",
                                                   "Reputation[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet2_1

Output_commsize_GNet2_2 <- data.frame(Mean = c(Pre_commsize_GNet2_2$mean),
                                      CI5 = c(Pre_commsize_GNet2_2$`5%`) ,
                                      CI95 = c(Pre_commsize_GNet2_2$`95%`) ,
                                      Variable = c("Social: <35[women]",
                                                   "Social: 35-49[women]",
                                                   "Social: 50-64[women]",
                                                   "Social: >=65[women]",
                                                   "Reputation: <35[women]",
                                                   "Reputation: 35-49[women]",
                                                   "Reputation: 50-64[women]",
                                                   "Reputation: >=65[women]",
                                                   "Social: <35[men−women]",
                                                   "Social: 35-49[men−women]",
                                                   "Social: 50-64[men−women]",
                                                   "Social: >=65[men−women]",
                                                   "Reputation: <35[men−women]", 
                                                   "Reputation: 35-49[men−women]", 
                                                   "Reputation: 50-64[men−women]", 
                                                   "Reputation: >=65[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet2_2

Output_commsize_GNet2_3 <- data.frame(Mean = c(Pre_commsize_GNet2_3$mean),
                                      CI5 = c(Pre_commsize_GNet2_3$`5%`) ,
                                      CI95 = c(Pre_commsize_GNet2_3$`95%`) ,
                                      Variable = c("Social: patrilocal[women]",
                                                   "Social: bilocal[women]",
                                                   "Social: matrilocal[women]",
                                                   "Reputation: patrilocal[women]",
                                                   "Reputation: bilocal[women]",
                                                   "Reputation: matrilocal[women]",
                                                   "Social: patrilocal[men−women]",
                                                   "Social: bilocal[men−women]",
                                                   "Social: matrilocal[men−women]",
                                                   "Reputation: patrilocal[men−women]", 
                                                   "Reputation: bilocal[men−women]", 
                                                   "Reputation: matrilocal[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet2_3

Output_commsize_GNet2_4 <- data.frame(Mean = c(Pre_commsize_GNet2_4$mean),
                                      CI5 = c(Pre_commsize_GNet2_4$`5%`) ,
                                      CI95 = c(Pre_commsize_GNet2_4$`95%`) ,
                                      Variable = c("Social: matrilineal[women]",
                                                   "Social: non-unilineal[women]",
                                                   "Social: patrilineal[women]",
                                                   "Reputation: matrilineal[women]",
                                                   "Reputation: non-unilineal[women]",
                                                   "Reputation: patrilineal[women]",
                                                   "Social: matrilineal[men−women]",
                                                   "Social: non-unilineal[men−women]",
                                                   "Social: patrilineal[men−women]",
                                                   "Reputation: matrilineal[men−women]", 
                                                   "Reputation: non-unilineal[men−women]", 
                                                   "Reputation: patrilineal[men−women]", 
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar"),
                           labels = c("Social: matrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: patrilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Social: matrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: patrilineal[men−women]",
                                      "Reputation: matrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: patrilineal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet2_4

Output_commsize_GNet2_5 <- data.frame(Mean = c(Pre_commsize_GNet2_5$mean),
                                      CI5 = c(Pre_commsize_GNet2_5$`5%`) ,
                                      CI95 = c(Pre_commsize_GNet2_5$`95%`) ,
                                      Variable = c("Social[women]",
                                                   "Reputation[women]",
                                                   "Social[men−women]",
                                                   "Reputation[men−women]", 
                                                   "Market: social[women] ",
                                                   "Market: reputation[women] ",
                                                   "Market: social[men−women] ",
                                                   "Market: reputation[men−women] ",
                                                   "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet2_5

####1.2.4.2 Direct ----
Output_commsize_GNet1_1_d <- data.frame(Mean = c(Pre_commsize_GNet1_1_d$mean),
                                        CI5 = c(Pre_commsize_GNet1_1_d$`5%`) ,
                                        CI95 = c(Pre_commsize_GNet1_1_d$`95%`) ,
                                        Variable = c("Consanguineal[women]",
                                                     "Affinal[women]",
                                                     "Friendship[women]",
                                                     "Generosity[women]",
                                                     "Religiosity[women]",
                                                     "Influence[women]",
                                                     "Knowledge[women]",
                                                     "Consanguineal[men−women]",
                                                     "Affinal[men−women]",
                                                     "Friendship[men−women]", 
                                                     "Generosity[men−women]", 
                                                     "Religiosity[men−women]",
                                                     "Influence[men−women]",
                                                     "Knowledge[men−women]",
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Consanguineal[women]",
                                      "Affinal[women]",
                                      "Friendship[women]",
                                      "Generosity[women]",
                                      "Religiosity[women]",
                                      "Influence[women]",
                                      "Knowledge[women]",
                                      "Consanguineal[men−women]",
                                      "Affinal[men−women]",
                                      "Friendship[men−women]", 
                                      "Generosity[men−women]", 
                                      "Religiosity[men−women]",
                                      "Influence[men−women]",
                                      "Knowledge[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet1_1_d

Output_commsize_GNet2_1_d <- data.frame(Mean = c(Pre_commsize_GNet2_1_d$mean),
                                        CI5 = c(Pre_commsize_GNet2_1_d$`5%`) ,
                                        CI95 = c(Pre_commsize_GNet2_1_d$`95%`) ,
                                        Variable = c("Social[women]",
                                                     "Reputation[women]",
                                                     "Social[men−women]",
                                                     "Reputation[men−women]", 
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet2_1_d

Output_commsize_GNet2_2_d <- data.frame(Mean = c(Pre_commsize_GNet2_2_d$mean),
                                        CI5 = c(Pre_commsize_GNet2_2_d$`5%`) ,
                                        CI95 = c(Pre_commsize_GNet2_2_d$`95%`) ,
                                        Variable = c("Social: <35[women]",
                                                     "Social: 35-49[women]",
                                                     "Social: 50-64[women]",
                                                     "Social: >=65[women]",
                                                     "Reputation: <35[women]",
                                                     "Reputation: 35-49[women]",
                                                     "Reputation: 50-64[women]",
                                                     "Reputation: >=65[women]",
                                                     "Social: <35[men−women]",
                                                     "Social: 35-49[men−women]",
                                                     "Social: 50-64[men−women]",
                                                     "Social: >=65[men−women]",
                                                     "Reputation: <35[men−women]", 
                                                     "Reputation: 35-49[men−women]", 
                                                     "Reputation: 50-64[men−women]", 
                                                     "Reputation: >=65[men−women]", 
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: <35[women]",
                                      "Social: 35-49[women]",
                                      "Social: 50-64[women]",
                                      "Social: >=65[women]",
                                      "Reputation: <35[women]",
                                      "Reputation: 35-49[women]",
                                      "Reputation: 50-64[women]",
                                      "Reputation: >=65[women]",
                                      "Social: <35[men−women]",
                                      "Social: 35-49[men−women]",
                                      "Social: 50-64[men−women]",
                                      "Social: >=65[men−women]",
                                      "Reputation: <35[men−women]", 
                                      "Reputation: 35-49[men−women]", 
                                      "Reputation: 50-64[men−women]", 
                                      "Reputation: >=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet2_2_d

Output_commsize_GNet2_3_d <- data.frame(Mean = c(Pre_commsize_GNet2_3_d$mean),
                                        CI5 = c(Pre_commsize_GNet2_3_d$`5%`) ,
                                        CI95 = c(Pre_commsize_GNet2_3_d$`95%`) ,
                                        Variable = c("Social: patrilocal[women]",
                                                     "Social: bilocal[women]",
                                                     "Social: matrilocal[women]",
                                                     "Reputation: patrilocal[women]",
                                                     "Reputation: bilocal[women]",
                                                     "Reputation: matrilocal[women]",
                                                     "Social: patrilocal[men−women]",
                                                     "Social: bilocal[men−women]",
                                                     "Social: matrilocal[men−women]",
                                                     "Reputation: patrilocal[men−women]", 
                                                     "Reputation: bilocal[men−women]", 
                                                     "Reputation: matrilocal[men−women]", 
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: patrilocal[women]",
                                      "Social: bilocal[women]",
                                      "Social: matrilocal[women]",
                                      "Reputation: patrilocal[women]",
                                      "Reputation: bilocal[women]",
                                      "Reputation: matrilocal[women]",
                                      "Social: patrilocal[men−women]",
                                      "Social: bilocal[men−women]",
                                      "Social: matrilocal[men−women]",
                                      "Reputation: patrilocal[men−women]", 
                                      "Reputation: bilocal[men−women]", 
                                      "Reputation: matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet2_3_d

Output_commsize_GNet2_4_d <- data.frame(Mean = c(Pre_commsize_GNet2_4_d$mean),
                                        CI5 = c(Pre_commsize_GNet2_4_d$`5%`) ,
                                        CI95 = c(Pre_commsize_GNet2_4_d$`95%`) ,
                                        Variable = c("Social: matrilineal[women]",
                                                     "Social: non-unilineal[women]",
                                                     "Social: patrilineal[women]",
                                                     "Reputation: matrilineal[women]",
                                                     "Reputation: non-unilineal[women]",
                                                     "Reputation: patrilineal[women]",
                                                     "Social: matrilineal[men−women]",
                                                     "Social: non-unilineal[men−women]",
                                                     "Social: patrilineal[men−women]",
                                                     "Reputation: matrilineal[men−women]", 
                                                     "Reputation: non-unilineal[men−women]", 
                                                     "Reputation: patrilineal[men−women]", 
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Market",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social: patrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: matrilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Social: patrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: matrilineal[men−women]",
                                      "Reputation: patrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: matrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Social: patrilineal[women]",
                                      "Social: non-unilineal[women]",
                                      "Social: matrilineal[women]",
                                      "Reputation: patrilineal[women]",
                                      "Reputation: non-unilineal[women]",
                                      "Reputation: matrilineal[women]",
                                      "Social: patrilineal[men−women]",
                                      "Social: non-unilineal[men−women]",
                                      "Social: matrilineal[men−women]",
                                      "Reputation: patrilineal[men−women]", 
                                      "Reputation: non-unilineal[men−women]", 
                                      "Reputation: matrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet2_4_d

Output_commsize_GNet2_5_d <- data.frame(Mean = c(Pre_commsize_GNet2_5_d$mean),
                                        CI5 = c(Pre_commsize_GNet2_5_d$`5%`) ,
                                        CI95 = c(Pre_commsize_GNet2_5_d$`95%`) ,
                                        Variable = c("Social[women]",
                                                     "Reputation[women]",
                                                     "Social[men−women]",
                                                     "Reputation[men−women]", 
                                                     "Market: social[women] ",
                                                     "Market: reputation[women] ",
                                                     "Market: social[men−women] ",
                                                     "Market: reputation[men−women] ",
                                                     "<35", "35-49", "50-64", ">=65",
                                                     "Non-inheritor", "Inheritor", 
                                                     "Patrilocal", "Bilocal", "Matrilocal",
                                                     "Matrilineal", "Non-unilineal", "Patrilineal",
                                                     "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar"),
                           labels = c("Social[women]",
                                      "Reputation[women]",
                                      "Social[men−women]",
                                      "Reputation[men−women]", 
                                      "Market: social[women] ",
                                      "Market: reputation[women] ",
                                      "Market: social[men−women] ",
                                      "Market: reputation[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_commsize_GNet2_5_d

##1.3 Multiplex position ----
Post_multi_network_partici_GNet_1 <- extract.samples(Model_multi_network_partici_GNet_1)
Post_multi_network_partici_GNet_1_1 <- extract.samples(Model_multi_network_partici_GNet_1_1)
Post_multi_network_partici_GNet_2 <- extract.samples(Model_multi_network_partici_GNet_2)
Post_multi_network_partici_GNet_3 <- extract.samples(Model_multi_network_partici_GNet_3)
Post_multi_network_partici_GNet_4 <- extract.samples(Model_multi_network_partici_GNet_4)
Post_multi_network_partici_GNet_5 <- extract.samples(Model_multi_network_partici_GNet_5)

Post_cross_network_GNet_1 <- extract.samples(Model_cross_network_GNet_1)
Post_cross_network_GNet_1_1 <- extract.samples(Model_cross_network_GNet_1_1)
Post_cross_network_GNet_2 <- extract.samples(Model_cross_network_GNet_2)
Post_cross_network_GNet_3 <- extract.samples(Model_cross_network_GNet_3)
Post_cross_network_GNet_4 <- extract.samples(Model_cross_network_GNet_4)
Post_cross_network_GNet_5 <- extract.samples(Model_cross_network_GNet_5)

###1.3.1 Cross-layer recognition ----
####1.3.1.1 Total ----
Output_multi_network_partici_GNet_1 <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_1$mean),
             CI5 = c(Pre_multi_network_partici_GNet_1$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_1$`95%`) ,
             Variable = c("Women",
                          "Men−women",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Women",
                                      "Men−women",
                                      "V_bar"),
                           labels = c("Women",
                                      "Men−women",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_1

Output_multi_network_partici_GNet1_1 <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_1_1$mean),
             CI5 = c(Pre_multi_network_partici_GNet_1_1$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_1_1$`95%`) ,
             Variable = c("Consanguineal × generosity[women]",
                          "Consanguineal × religiosity[women]",
                          "Consanguineal × influence[women]",
                          "Consanguineal × knowledge[women]",
                          "Affinal × generosity[women]",
                          "Affinal × religiosity[women]",
                          "Affinal × influence[women]",
                          "Affinal × knowledge[women]",
                          "Friendship × generosity[women]",
                          "Friendship × religiosity[women]",
                          "Friendship × influence[women]",
                          "Friendship × knowledge[women]",
                          
                          "Consanguineal × generosity[men–women]",
                          "Consanguineal × religiosity[men–women]",
                          "Consanguineal × influence[men–women]",
                          "Consanguineal × knowledge[men–women]",
                          "Affinal × generosity[men–women]",
                          "Affinal × religiosity[men–women]",
                          "Affinal × influence[men–women]",
                          "Affinal × knowledge[men–women]",
                          "Friendship × generosity[men–women]",
                          "Friendship × religiosity[men–women]",
                          "Friendship × influence[men–women]",
                          "Friendship × knowledge[men–women]",
                          
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal × generosity[women]",
                                      "Consanguineal × religiosity[women]",
                                      "Consanguineal × influence[women]",
                                      "Consanguineal × knowledge[women]",
                                      "Affinal × generosity[women]",
                                      "Affinal × religiosity[women]",
                                      "Affinal × influence[women]",
                                      "Affinal × knowledge[women]",
                                      "Friendship × generosity[women]",
                                      "Friendship × religiosity[women]",
                                      "Friendship × influence[women]",
                                      "Friendship × knowledge[women]",
                                      
                                      "Consanguineal × generosity[men–women]",
                                      "Consanguineal × religiosity[men–women]",
                                      "Consanguineal × influence[men–women]",
                                      "Consanguineal × knowledge[men–women]",
                                      "Affinal × generosity[men–women]",
                                      "Affinal × religiosity[men–women]",
                                      "Affinal × influence[men–women]",
                                      "Affinal × knowledge[men–women]",
                                      "Friendship × generosity[men–women]",
                                      "Friendship × religiosity[men–women]",
                                      "Friendship × influence[men–women]",
                                      "Friendship × knowledge[men–women]",
                                      
                                      "V_bar"),
                           labels = c("Consanguineal × generosity[women]",
                                      "Consanguineal × religiosity[women]",
                                      "Consanguineal × influence[women]",
                                      "Consanguineal × knowledge[women]",
                                      "Affinal × generosity[women]",
                                      "Affinal × religiosity[women]",
                                      "Affinal × influence[women]",
                                      "Affinal × knowledge[women]",
                                      "Friendship × generosity[women]",
                                      "Friendship × religiosity[women]",
                                      "Friendship × influence[women]",
                                      "Friendship × knowledge[women]",
                                      
                                      "Consanguineal × generosity[men–women]",
                                      "Consanguineal × religiosity[men–women]",
                                      "Consanguineal × influence[men–women]",
                                      "Consanguineal × knowledge[men–women]",
                                      "Affinal × generosity[men–women]",
                                      "Affinal × religiosity[men–women]",
                                      "Affinal × influence[men–women]",
                                      "Affinal × knowledge[men–women]",
                                      "Friendship × generosity[men–women]",
                                      "Friendship × religiosity[men–women]",
                                      "Friendship × influence[men–women]",
                                      "Friendship × knowledge[men–women]",
                                      
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet1_1

Output_multi_network_partici_GNet_2 <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_2$mean),
             CI5 = c(Pre_multi_network_partici_GNet_2$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_2$`95%`) ,
             Variable = c("<35[women]",
                          "35–49[women]",
                          "50–64[women]",
                          "≥65[women]",
                          "<35[men−women]",
                          "35–49[men−women]",
                          "50–64[men−women]",
                          "≥65[men−women]",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("<35[women]",
                                      "35–49[women]",
                                      "50–64[women]",
                                      "≥65[women]",
                                      "<35[men−women]",
                                      "35–49[men−women]",
                                      "50–64[men−women]",
                                      "≥65[men−women]",
                                      "V_bar"),
                           labels = c("<35[women]",
                                      "35–49[women]",
                                      "50–64[women]",
                                      "≥65[women]",
                                      "<35[men−women]",
                                      "35–49[men−women]",
                                      "50–64[men−women]",
                                      "≥65[men−women]",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_2

Output_multi_network_partici_GNet_3 <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_3$mean),
             CI5 = c(Pre_multi_network_partici_GNet_3$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_3$`95%`) ,
             Variable = c("Patrilocal[women]",
                          "Bilocal[women]",
                          "Matrilocal[women]",
                          "Patrilocal[men−women]", 
                          "Bilocal[men−women]", 
                          "Matrilocal[men−women]", 
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Patrilocal[women]",
                                      "Bilocal[women]",
                                      "Matrilocal[women]",
                                      "Patrilocal[men−women]", 
                                      "Bilocal[men−women]", 
                                      "Matrilocal[men−women]", 
                                      "V_bar"),
                           labels = c("Patrilocal[women]",
                                      "Bilocal[women]",
                                      "Matrilocal[women]",
                                      "Patrilocal[men−women]", 
                                      "Bilocal[men−women]", 
                                      "Matrilocal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_3

Output_multi_network_partici_GNet_4 <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_4$mean),
             CI5 = c(Pre_multi_network_partici_GNet_4$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_4$`95%`) ,
             Variable = c("Matrilineal[women]",
                          "Non-unilineal[women]",
                          "Patrilineal[women]",
                          "Matrilineal[men−women]", 
                          "Non-unilineal[men−women]", 
                          "Patrilineal[men−women]", 
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Matrilineal[women]",
                                      "Non-unilineal[women]",
                                      "Patrilineal[women]",
                                      "Matrilineal[men−women]", 
                                      "Non-unilineal[men−women]", 
                                      "Patrilineal[men−women]", 
                                      "V_bar"),
                           labels = c("Matrilineal[women]",
                                      "Non-unilineal[women]",
                                      "Patrilineal[women]",
                                      "Matrilineal[men−women]", 
                                      "Non-unilineal[men−women]", 
                                      "Patrilineal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_4

Output_multi_network_partici_GNet_5 <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_5$mean),
             CI5 = c(Pre_multi_network_partici_GNet_5$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_5$`95%`) ,
             Variable = c("Market[women]",
                          "Market[men−women] ",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Market[women]",
                                      "Market[men−women] ",
                                      "V_bar"),
                           labels = c("Market[women]",
                                      "Market[men−women] ",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_5

####1.3.1.2 Direct ----
Output_multi_network_partici_GNet_1_d <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_1_d$mean),
             CI5 = c(Pre_multi_network_partici_GNet_1_d$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_1_d$`95%`) ,
             Variable = c("Women",
                          "Men−women",
                          "<35", "35-49", "50-64", ">=65",
                          "Non-inheritor", "Inheritor", 
                          "Patrilocal", "Bilocal", "Matrilocal",
                          "Matrilineal", "Non-unilineal", "Patrilineal",
                          "Market",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Women",
                                      "Men−women",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Women",
                                      "Men−women",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_1_d

Output_multi_network_partici_GNet_1_1_d <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_1_1_d$mean),
             CI5 = c(Pre_multi_network_partici_GNet_1_1_d$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_1_1_d$`95%`) ,
             Variable = c("Consanguineal × generosity[women]",
                          "Consanguineal × religiosity[women]",
                          "Consanguineal × influence[women]",
                          "Consanguineal × knowledge[women]",
                          "Affinal × generosity[women]",
                          "Affinal × religiosity[women]",
                          "Affinal × influence[women]",
                          "Affinal × knowledge[women]",
                          "Friendship × generosity[women]",
                          "Friendship × religiosity[women]",
                          "Friendship × influence[women]",
                          "Friendship × knowledge[women]",
                          
                          "Consanguineal × generosity[men–women]",
                          "Consanguineal × religiosity[men–women]",
                          "Consanguineal × influence[men–women]",
                          "Consanguineal × knowledge[men–women]",
                          "Affinal × generosity[men–women]",
                          "Affinal × religiosity[men–women]",
                          "Affinal × influence[men–women]",
                          "Affinal × knowledge[men–women]",
                          "Friendship × generosity[men–women]",
                          "Friendship × religiosity[men–women]",
                          "Friendship × influence[men–women]",
                          "Friendship × knowledge[men–women]",
                          
                          "<35", "35-49", "50-64", ">=65",
                          "Non-inheritor", "Inheritor", 
                          "Patrilocal", "Bilocal", "Matrilocal",
                          "Matrilineal", "Non-unilineal", "Patrilineal",
                          "Market",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal × generosity[women]",
                                      "Consanguineal × religiosity[women]",
                                      "Consanguineal × influence[women]",
                                      "Consanguineal × knowledge[women]",
                                      "Affinal × generosity[women]",
                                      "Affinal × religiosity[women]",
                                      "Affinal × influence[women]",
                                      "Affinal × knowledge[women]",
                                      "Friendship × generosity[women]",
                                      "Friendship × religiosity[women]",
                                      "Friendship × influence[women]",
                                      "Friendship × knowledge[women]",
                                      
                                      "Consanguineal × generosity[men–women]",
                                      "Consanguineal × religiosity[men–women]",
                                      "Consanguineal × influence[men–women]",
                                      "Consanguineal × knowledge[men–women]",
                                      "Affinal × generosity[men–women]",
                                      "Affinal × religiosity[men–women]",
                                      "Affinal × influence[men–women]",
                                      "Affinal × knowledge[men–women]",
                                      "Friendship × generosity[men–women]",
                                      "Friendship × religiosity[men–women]",
                                      "Friendship × influence[men–women]",
                                      "Friendship × knowledge[men–women]",
                                      
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Consanguineal × generosity[women]",
                                      "Consanguineal × religiosity[women]",
                                      "Consanguineal × influence[women]",
                                      "Consanguineal × knowledge[women]",
                                      "Affinal × generosity[women]",
                                      "Affinal × religiosity[women]",
                                      "Affinal × influence[women]",
                                      "Affinal × knowledge[women]",
                                      "Friendship × generosity[women]",
                                      "Friendship × religiosity[women]",
                                      "Friendship × influence[women]",
                                      "Friendship × knowledge[women]",
                                      
                                      "Consanguineal × generosity[men–women]",
                                      "Consanguineal × religiosity[men–women]",
                                      "Consanguineal × influence[men–women]",
                                      "Consanguineal × knowledge[men–women]",
                                      "Affinal × generosity[men–women]",
                                      "Affinal × religiosity[men–women]",
                                      "Affinal × influence[men–women]",
                                      "Affinal × knowledge[men–women]",
                                      "Friendship × generosity[men–women]",
                                      "Friendship × religiosity[men–women]",
                                      "Friendship × influence[men–women]",
                                      "Friendship × knowledge[men–women]",
                                      
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_1_1_d

Output_multi_network_partici_GNet_2_d <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_2_d$mean),
             CI5 = c(Pre_multi_network_partici_GNet_2_d$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_2_d$`95%`) ,
             Variable = c("<35[women]",
                          "35-49[women]",
                          "50-64[women]",
                          ">=65[women]",
                          "<35[men−women]", 
                          "35-49[men−women]", 
                          "50-64[men−women]", 
                          ">=65[men−women]", 
                          "Non-inheritor", "Inheritor", 
                          "Patrilocal", "Bilocal", "Matrilocal",
                          "Matrilineal", "Non-unilineal", "Patrilineal",
                          "Market",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("<35[women]",
                                      "35-49[women]",
                                      "50-64[women]",
                                      ">=65[women]",
                                      "<35[men−women]", 
                                      "35-49[men−women]", 
                                      "50-64[men−women]", 
                                      ">=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("<35[women]",
                                      "35-49[women]",
                                      "50-64[women]",
                                      ">=65[women]",
                                      "<35[men−women]", 
                                      "35-49[men−women]", 
                                      "50-64[men−women]", 
                                      ">=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_2_d

Output_multi_network_partici_GNet_3_d <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_3_d$mean),
             CI5 = c(Pre_multi_network_partici_GNet_3_d$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_3_d$`95%`) ,
             Variable = c("Patrilocal[women]",
                          "Bilocal[women]",
                          "Matrilocal[women]",
                          "Patrilocal[men−women]", 
                          "Bilocal[men−women]", 
                          "Matrilocal[men−women]", 
                          "<35", "35-49", "50-64", ">=65",
                          "Non-inheritor", "Inheritor", 
                          "Matrilineal", "Non-unilineal", "Patrilineal",
                          "Market",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Patrilocal[women]",
                                      "Bilocal[women]",
                                      "Matrilocal[women]",
                                      "Patrilocal[men−women]", 
                                      "Bilocal[men−women]", 
                                      "Matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Patrilocal[women]",
                                      "Bilocal[women]",
                                      "Matrilocal[women]",
                                      "Patrilocal[men−women]", 
                                      "Bilocal[men−women]", 
                                      "Matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_3_d

Output_multi_network_partici_GNet_4_d <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_4_d$mean),
             CI5 = c(Pre_multi_network_partici_GNet_4_d$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_4_d$`95%`) ,
             Variable = c("Matrilineal[women]",
                          "Non-unilineal[women]",
                          "Patrilineal[women]",
                          "Matrilineal[men−women]", 
                          "Non-unilineal[men−women]", 
                          "Patrilineal[men−women]", 
                          "<35", "35-49", "50-64", ">=65",
                          "Non-inheritor", "Inheritor", 
                          "Patrilocal", "Bilocal", "Matrilocal",
                          "Market",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Matrilineal[women]",
                                      "Non-unilineal[women]",
                                      "Patrilineal[women]",
                                      "Matrilineal[men−women]", 
                                      "Non-unilineal[men−women]", 
                                      "Patrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Matrilineal[women]",
                                      "Non-unilineal[women]",
                                      "Patrilineal[women]",
                                      "Matrilineal[men−women]", 
                                      "Non-unilineal[men−women]", 
                                      "Patrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_4_d

Output_multi_network_partici_GNet_5_d <- 
  data.frame(Mean = c(Pre_multi_network_partici_GNet_5_d$mean),
             CI5 = c(Pre_multi_network_partici_GNet_5_d$`5%`) ,
             CI95 = c(Pre_multi_network_partici_GNet_5_d$`95%`) ,
             Variable = c("Market[women]",
                          "Market[men−women] ",
                          "<35", "35-49", "50-64", ">=65",
                          "Non-inheritor", "Inheritor", 
                          "Patrilocal", "Bilocal", "Matrilocal",
                          "Matrilineal", "Non-unilineal", "Patrilineal",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Market[women]",
                                      "Market[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar"),
                           labels = c("Market[women]",
                                      "Market[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_multi_network_partici_GNet_5_d

###1.3.2 Cross-layer ties ----
####1.3.2.1 Total ----
Output_cross_network_GNet_1 <- 
  data.frame(Mean = c(Pre_cross_network_GNet_1$mean),
             CI5 = c(Pre_cross_network_GNet_1$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_1$`95%`) ,
             Variable = c("Women",
                          "Men−women",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Women",
                                      "Men−women",
                                      "V_bar"),
                           labels = c("Women",
                                      "Men−women",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_1

Output_cross_network_GNet1_1 <- 
  data.frame(Mean = c(Pre_cross_network_GNet_1_1$mean),
             CI5 = c(Pre_cross_network_GNet_1_1$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_1_1$`95%`) ,
             Variable = c("Consanguineal × generosity[women]",
                          "Consanguineal × religiosity[women]",
                          "Consanguineal × influence[women]",
                          "Consanguineal × knowledge[women]",
                          "Affinal × generosity[women]",
                          "Affinal × religiosity[women]",
                          "Affinal × influence[women]",
                          "Affinal × knowledge[women]",
                          "Friendship × generosity[women]",
                          "Friendship × religiosity[women]",
                          "Friendship × influence[women]",
                          "Friendship × knowledge[women]",
                          
                          "Consanguineal × generosity[men–women]",
                          "Consanguineal × religiosity[men–women]",
                          "Consanguineal × influence[men–women]",
                          "Consanguineal × knowledge[men–women]",
                          "Affinal × generosity[men–women]",
                          "Affinal × religiosity[men–women]",
                          "Affinal × influence[men–women]",
                          "Affinal × knowledge[men–women]",
                          "Friendship × generosity[men–women]",
                          "Friendship × religiosity[men–women]",
                          "Friendship × influence[men–women]",
                          "Friendship × knowledge[men–women]",
                          
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal × generosity[women]",
                                      "Consanguineal × religiosity[women]",
                                      "Consanguineal × influence[women]",
                                      "Consanguineal × knowledge[women]",
                                      "Affinal × generosity[women]",
                                      "Affinal × religiosity[women]",
                                      "Affinal × influence[women]",
                                      "Affinal × knowledge[women]",
                                      "Friendship × generosity[women]",
                                      "Friendship × religiosity[women]",
                                      "Friendship × influence[women]",
                                      "Friendship × knowledge[women]",
                                      
                                      "Consanguineal × generosity[men–women]",
                                      "Consanguineal × religiosity[men–women]",
                                      "Consanguineal × influence[men–women]",
                                      "Consanguineal × knowledge[men–women]",
                                      "Affinal × generosity[men–women]",
                                      "Affinal × religiosity[men–women]",
                                      "Affinal × influence[men–women]",
                                      "Affinal × knowledge[men–women]",
                                      "Friendship × generosity[men–women]",
                                      "Friendship × religiosity[men–women]",
                                      "Friendship × influence[men–women]",
                                      "Friendship × knowledge[men–women]",
                                      
                                      "V_bar"),
                           labels = c("Consanguineal × generosity[women]",
                                      "Consanguineal × religiosity[women]",
                                      "Consanguineal × influence[women]",
                                      "Consanguineal × knowledge[women]",
                                      "Affinal × generosity[women]",
                                      "Affinal × religiosity[women]",
                                      "Affinal × influence[women]",
                                      "Affinal × knowledge[women]",
                                      "Friendship × generosity[women]",
                                      "Friendship × religiosity[women]",
                                      "Friendship × influence[women]",
                                      "Friendship × knowledge[women]",
                                      
                                      "Consanguineal × generosity[men–women]",
                                      "Consanguineal × religiosity[men–women]",
                                      "Consanguineal × influence[men–women]",
                                      "Consanguineal × knowledge[men–women]",
                                      "Affinal × generosity[men–women]",
                                      "Affinal × religiosity[men–women]",
                                      "Affinal × influence[men–women]",
                                      "Affinal × knowledge[men–women]",
                                      "Friendship × generosity[men–women]",
                                      "Friendship × religiosity[men–women]",
                                      "Friendship × influence[men–women]",
                                      "Friendship × knowledge[men–women]",
                                      
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet1_1

Output_cross_network_GNet_2 <- 
  data.frame(Mean = c(Pre_cross_network_GNet_2$mean),
             CI5 = c(Pre_cross_network_GNet_2$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_2$`95%`) ,
             Variable = c("<35[women]",
                          "35–49[women]",
                          "50–64[women]",
                          "≥65[women]",
                          "<35[men−women]",
                          "35–49[men−women]",
                          "50–64[men−women]",
                          "≥65[men−women]",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("<35[women]",
                                      "35–49[women]",
                                      "50–64[women]",
                                      "≥65[women]",
                                      "<35[men−women]",
                                      "35–49[men−women]",
                                      "50–64[men−women]",
                                      "≥65[men−women]",
                                      "V_bar"),
                           labels = c("<35[women]",
                                      "35–49[women]",
                                      "50–64[women]",
                                      "≥65[women]",
                                      "<35[men−women]",
                                      "35–49[men−women]",
                                      "50–64[men−women]",
                                      "≥65[men−women]",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_2

Output_cross_network_GNet_3 <- 
  data.frame(Mean = c(Pre_cross_network_GNet_3$mean),
             CI5 = c(Pre_cross_network_GNet_3$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_3$`95%`) ,
             Variable = c("Patrilocal[women]",
                          "Bilocal[women]",
                          "Matrilocal[women]",
                          "Patrilocal[men−women]", 
                          "Bilocal[men−women]", 
                          "Matrilocal[men−women]", 
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Patrilocal[women]",
                                      "Bilocal[women]",
                                      "Matrilocal[women]",
                                      "Patrilocal[men−women]", 
                                      "Bilocal[men−women]", 
                                      "Matrilocal[men−women]", 
                                      "V_bar"),
                           labels = c("Patrilocal[women]",
                                      "Bilocal[women]",
                                      "Matrilocal[women]",
                                      "Patrilocal[men−women]", 
                                      "Bilocal[men−women]", 
                                      "Matrilocal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_3

Output_cross_network_GNet_4 <- 
  data.frame(Mean = c(Pre_cross_network_GNet_4$mean),
             CI5 = c(Pre_cross_network_GNet_4$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_4$`95%`) ,
             Variable = c("Matrilineal[women]",
                          "Non-unilineal[women]",
                          "Patrilineal[women]",
                          "Matrilineal[men−women]", 
                          "Non-unilineal[men−women]", 
                          "Patrilineal[men−women]", 
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Matrilineal[women]",
                                      "Non-unilineal[women]",
                                      "Patrilineal[women]",
                                      "Matrilineal[men−women]", 
                                      "Non-unilineal[men−women]", 
                                      "Patrilineal[men−women]", 
                                      "V_bar"),
                           labels = c("Matrilineal[women]",
                                      "Non-unilineal[women]",
                                      "Patrilineal[women]",
                                      "Matrilineal[men−women]", 
                                      "Non-unilineal[men−women]", 
                                      "Patrilineal[men−women]", 
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_4

Output_cross_network_GNet_5 <- 
  data.frame(Mean = c(Pre_cross_network_GNet_5$mean),
             CI5 = c(Pre_cross_network_GNet_5$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_5$`95%`) ,
             Variable = c("Market[women]",
                          "Market[men−women] ",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Market[women]",
                                      "Market[men−women] ",
                                      "V_bar"),
                           labels = c("Market[women]",
                                      "Market[men−women] ",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_5

####1.3.2.2 Direct ----
Output_cross_network_GNet_1_d <- 
  data.frame(Mean = c(Pre_cross_network_GNet_1_d$mean),
             CI5 = c(Pre_cross_network_GNet_1_d$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_1_d$`95%`) ,
             Variable = c("Women",
                          "Men−women",
                          "<35", "35-49", "50-64", ">=65",
                          "Non-inheritor", "Inheritor", 
                          "Patrilocal", "Bilocal", "Matrilocal",
                          "Matrilineal", "Non-unilineal", "Patrilineal",
                          "Market",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Women",
                                      "Men−women",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Women",
                                      "Men−women",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_1_d

Output_cross_network_GNet_1_1_d <- 
  data.frame(Mean = c(Pre_cross_network_GNet_1_1_d$mean),
             CI5 = c(Pre_cross_network_GNet_1_1_d$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_1_1_d$`95%`) ,
             Variable = c("Consanguineal × generosity[women]",
                          "Consanguineal × religiosity[women]",
                          "Consanguineal × influence[women]",
                          "Consanguineal × knowledge[women]",
                          "Affinal × generosity[women]",
                          "Affinal × religiosity[women]",
                          "Affinal × influence[women]",
                          "Affinal × knowledge[women]",
                          "Friendship × generosity[women]",
                          "Friendship × religiosity[women]",
                          "Friendship × influence[women]",
                          "Friendship × knowledge[women]",
                          
                          "Consanguineal × generosity[men–women]",
                          "Consanguineal × religiosity[men–women]",
                          "Consanguineal × influence[men–women]",
                          "Consanguineal × knowledge[men–women]",
                          "Affinal × generosity[men–women]",
                          "Affinal × religiosity[men–women]",
                          "Affinal × influence[men–women]",
                          "Affinal × knowledge[men–women]",
                          "Friendship × generosity[men–women]",
                          "Friendship × religiosity[men–women]",
                          "Friendship × influence[men–women]",
                          "Friendship × knowledge[men–women]",
                          
                          "<35", "35-49", "50-64", ">=65",
                          "Non-inheritor", "Inheritor", 
                          "Patrilocal", "Bilocal", "Matrilocal",
                          "Matrilineal", "Non-unilineal", "Patrilineal",
                          "Market",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Consanguineal × generosity[women]",
                                      "Consanguineal × religiosity[women]",
                                      "Consanguineal × influence[women]",
                                      "Consanguineal × knowledge[women]",
                                      "Affinal × generosity[women]",
                                      "Affinal × religiosity[women]",
                                      "Affinal × influence[women]",
                                      "Affinal × knowledge[women]",
                                      "Friendship × generosity[women]",
                                      "Friendship × religiosity[women]",
                                      "Friendship × influence[women]",
                                      "Friendship × knowledge[women]",
                                      
                                      "Consanguineal × generosity[men–women]",
                                      "Consanguineal × religiosity[men–women]",
                                      "Consanguineal × influence[men–women]",
                                      "Consanguineal × knowledge[men–women]",
                                      "Affinal × generosity[men–women]",
                                      "Affinal × religiosity[men–women]",
                                      "Affinal × influence[men–women]",
                                      "Affinal × knowledge[men–women]",
                                      "Friendship × generosity[men–women]",
                                      "Friendship × religiosity[men–women]",
                                      "Friendship × influence[men–women]",
                                      "Friendship × knowledge[men–women]",
                                      
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Consanguineal × generosity[women]",
                                      "Consanguineal × religiosity[women]",
                                      "Consanguineal × influence[women]",
                                      "Consanguineal × knowledge[women]",
                                      "Affinal × generosity[women]",
                                      "Affinal × religiosity[women]",
                                      "Affinal × influence[women]",
                                      "Affinal × knowledge[women]",
                                      "Friendship × generosity[women]",
                                      "Friendship × religiosity[women]",
                                      "Friendship × influence[women]",
                                      "Friendship × knowledge[women]",
                                      
                                      "Consanguineal × generosity[men–women]",
                                      "Consanguineal × religiosity[men–women]",
                                      "Consanguineal × influence[men–women]",
                                      "Consanguineal × knowledge[men–women]",
                                      "Affinal × generosity[men–women]",
                                      "Affinal × religiosity[men–women]",
                                      "Affinal × influence[men–women]",
                                      "Affinal × knowledge[men–women]",
                                      "Friendship × generosity[men–women]",
                                      "Friendship × religiosity[men–women]",
                                      "Friendship × influence[men–women]",
                                      "Friendship × knowledge[men–women]",
                                      
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_1_1_d

Output_cross_network_GNet_2_d <- 
  data.frame(Mean = c(Pre_cross_network_GNet_2_d$mean),
             CI5 = c(Pre_cross_network_GNet_2_d$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_2_d$`95%`) ,
             Variable = c("<35[women]",
                          "35-49[women]",
                          "50-64[women]",
                          ">=65[women]",
                          "<35[men−women]", 
                          "35-49[men−women]", 
                          "50-64[men−women]", 
                          ">=65[men−women]", 
                          "Non-inheritor", "Inheritor", 
                          "Patrilocal", "Bilocal", "Matrilocal",
                          "Matrilineal", "Non-unilineal", "Patrilineal",
                          "Market",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("<35[women]",
                                      "35-49[women]",
                                      "50-64[women]",
                                      ">=65[women]",
                                      "<35[men−women]", 
                                      "35-49[men−women]", 
                                      "50-64[men−women]", 
                                      ">=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("<35[women]",
                                      "35-49[women]",
                                      "50-64[women]",
                                      ">=65[women]",
                                      "<35[men−women]", 
                                      "35-49[men−women]", 
                                      "50-64[men−women]", 
                                      ">=65[men−women]", 
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_2_d

Output_cross_network_GNet_3_d <- 
  data.frame(Mean = c(Pre_cross_network_GNet_3_d$mean),
             CI5 = c(Pre_cross_network_GNet_3_d$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_3_d$`95%`) ,
             Variable = c("Patrilocal[women]",
                          "Bilocal[women]",
                          "Matrilocal[women]",
                          "Patrilocal[men−women]", 
                          "Bilocal[men−women]", 
                          "Matrilocal[men−women]", 
                          "<35", "35-49", "50-64", ">=65",
                          "Non-inheritor", "Inheritor", 
                          "Matrilineal", "Non-unilineal", "Patrilineal",
                          "Market",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Patrilocal[women]",
                                      "Bilocal[women]",
                                      "Matrilocal[women]",
                                      "Patrilocal[men−women]", 
                                      "Bilocal[men−women]", 
                                      "Matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Patrilocal[women]",
                                      "Bilocal[women]",
                                      "Matrilocal[women]",
                                      "Patrilocal[men−women]", 
                                      "Bilocal[men−women]", 
                                      "Matrilocal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_3_d

Output_cross_network_GNet_4_d <- 
  data.frame(Mean = c(Pre_cross_network_GNet_4_d$mean),
             CI5 = c(Pre_cross_network_GNet_4_d$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_4_d$`95%`) ,
             Variable = c("Matrilineal[women]",
                          "Non-unilineal[women]",
                          "Patrilineal[women]",
                          "Matrilineal[men−women]", 
                          "Non-unilineal[men−women]", 
                          "Patrilineal[men−women]", 
                          "<35", "35-49", "50-64", ">=65",
                          "Non-inheritor", "Inheritor", 
                          "Patrilocal", "Bilocal", "Matrilocal",
                          "Market",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Matrilineal[women]",
                                      "Non-unilineal[women]",
                                      "Patrilineal[women]",
                                      "Matrilineal[men−women]", 
                                      "Non-unilineal[men−women]", 
                                      "Patrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar"),
                           labels = c("Matrilineal[women]",
                                      "Non-unilineal[women]",
                                      "Patrilineal[women]",
                                      "Matrilineal[men−women]", 
                                      "Non-unilineal[men−women]", 
                                      "Patrilineal[men−women]", 
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Market",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_4_d

Output_cross_network_GNet_5_d <- 
  data.frame(Mean = c(Pre_cross_network_GNet_5_d$mean),
             CI5 = c(Pre_cross_network_GNet_5_d$`5%`) ,
             CI95 = c(Pre_cross_network_GNet_5_d$`95%`) ,
             Variable = c("Market[women]",
                          "Market[men−women] ",
                          "<35", "35-49", "50-64", ">=65",
                          "Non-inheritor", "Inheritor", 
                          "Patrilocal", "Bilocal", "Matrilocal",
                          "Matrilineal", "Non-unilineal", "Patrilineal",
                          "V_bar")) %>% 
  mutate(Variable = factor(Variable, 
                           levels = c("Market[women]",
                                      "Market[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar"),
                           labels = c("Market[women]",
                                      "Market[men−women] ",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Non-inheritor", "Inheritor", 
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "V_bar")),
         "Mean [CI]" = paste0(format(round(Mean, 2), nsmall = 2), 
                              "[" , 
                              format(round(CI5, 2), nsmall = 2), 
                              ", ", 
                              format(round(CI95, 2), nsmall = 2), 
                              "]", 
                              sep = "")) %>% 
  dplyr::select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 2 ) ,
               column_labels.border.bottom.color = "black" ,
               column_labels.border.bottom.width = px( 2 ) ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 2 ) ) %>% 
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

Output_cross_network_GNet_5_d

#2 Proportion of gender differences ----
##2.1 Received recognition and structural position ----
Pro_receive_structural <- 
  tibble(Variable = c("Social", "Reputational",
                      "Consanguineal", "Affinal", "Friend",
                      "Generosity", "Religiosity",
                      "Influence", "Knowledge",
                      "<35", "35-49", "50-64", ">=65",
                      "<35", "35-49", "50-64", ">=65",
                      "Patrilocal", "Bilocal", "Matrilocal",
                      "Patrilocal", "Bilocal", "Matrilocal",
                      "Matrilineal", "Non-unilineal", "Patrilineal",
                      "Matrilineal", "Non-unilineal", "Patrilineal",
                      "Market", "Market"),
         Domain = c("Social", "Reputational",
                    "Social", "Social", "Social",
                    "Reputational", "Reputational",
                    "Reputational", "Reputational",
                    "Social", "Social", "Social", "Social",
                    "Reputational", "Reputational",
                    "Reputational", "Reputational",
                    "Social", "Social", "Social",
                    "Reputational", "Reputational", "Reputational",
                    "Social", "Social", "Social",
                    "Reputational", "Reputational", "Reputational",
                    "Social", "Reputational"),
         Indegree = c(sprintf("%0.4f", length(which((Post_degree_GNet2_1$bGNet[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_1$bGNet[,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet1_1$bGNet[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet1_1$bGNet[,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet1_1$bGNet[,3])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet1_1$bGNet[,4])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet1_1$bGNet[,5])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet1_1$bGNet[,6])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet1_1$bGNet[,7])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_2$bGANet[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_2$bGANet[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_2$bGANet[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_2$bGANet[,4,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_2$bGANet[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_2$bGANet[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_2$bGANet[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_2$bGANet[,4,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_3$bGRNet[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_3$bGRNet[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_3$bGRNet[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_3$bGRNet[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_3$bGRNet[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_3$bGRNet[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_4$bGKNet[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_4$bGKNet[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_4$bGKNet[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_4$bGKNet[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_4$bGKNet[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_4$bGKNet[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_5$bGMNet[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_degree_GNet2_5$bGMNet[,2])> 0))/14000)),
         Harmonic = c(sprintf("%0.4f", length(which((Post_harmonic_GNet2_1$bGNet_pos[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_1$bGNet_pos[,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet1_1$bGNet_pos[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet1_1$bGNet_pos[,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet1_1$bGNet_pos[,3])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet1_1$bGNet_pos[,4])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet1_1$bGNet_pos[,5])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet1_1$bGNet_pos[,6])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet1_1$bGNet_pos[,7])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_2$bGANet_pos[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_2$bGANet_pos[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_2$bGANet_pos[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_2$bGANet_pos[,4,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_2$bGANet_pos[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_2$bGANet_pos[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_2$bGANet_pos[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_2$bGANet_pos[,4,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_3$bGRNet_pos[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_3$bGRNet_pos[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_3$bGRNet_pos[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_3$bGRNet_pos[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_3$bGRNet_pos[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_3$bGRNet_pos[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_4$bGKNet_pos[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_4$bGKNet_pos[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_4$bGKNet_pos[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_4$bGKNet_pos[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_4$bGKNet_pos[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_4$bGKNet_pos[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_5$bGMNet_pos[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_harmonic_GNet2_5$bGMNet_pos[,2])> 0))/14000)),
         PageRank = c(sprintf("%0.4f", length(which((Post_pagerank_GNet2_1$bGNet[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_1$bGNet[,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet1_1$bGNet[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet1_1$bGNet[,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet1_1$bGNet[,3])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet1_1$bGNet[,4])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet1_1$bGNet[,5])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet1_1$bGNet[,6])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet1_1$bGNet[,7])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_2$bGANet[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_2$bGANet[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_2$bGANet[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_2$bGANet[,4,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_2$bGANet[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_2$bGANet[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_2$bGANet[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_2$bGANet[,4,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_3$bGRNet[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_3$bGRNet[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_3$bGRNet[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_3$bGRNet[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_3$bGRNet[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_3$bGRNet[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_4$bGKNet[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_4$bGKNet[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_4$bGKNet[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_4$bGKNet[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_4$bGKNet[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_4$bGKNet[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_5$bGMNet[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_pagerank_GNet2_5$bGMNet[,2])> 0))/14000)),
         Between = c(sprintf("%0.4f", length(which((Post_between_GNet2_1$bGNet_pos[,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_1$bGNet_pos[,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet1_1$bGNet_pos[,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet1_1$bGNet_pos[,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet1_1$bGNet_pos[,3])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet1_1$bGNet_pos[,4])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet1_1$bGNet_pos[,5])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet1_1$bGNet_pos[,6])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet1_1$bGNet_pos[,7])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_2$bGANet_pos[,1,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_2$bGANet_pos[,2,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_2$bGANet_pos[,3,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_2$bGANet_pos[,4,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_2$bGANet_pos[,1,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_2$bGANet_pos[,2,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_2$bGANet_pos[,3,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_2$bGANet_pos[,4,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_3$bGRNet_pos[,1,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_3$bGRNet_pos[,2,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_3$bGRNet_pos[,3,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_3$bGRNet_pos[,1,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_3$bGRNet_pos[,2,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_3$bGRNet_pos[,3,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_4$bGKNet_pos[,1,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_4$bGKNet_pos[,2,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_4$bGKNet_pos[,3,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_4$bGKNet_pos[,1,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_4$bGKNet_pos[,2,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_4$bGKNet_pos[,3,2])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_5$bGMNet_pos[,1])> 0))/14000),
                     sprintf("%0.4f", length(which((Post_between_GNet2_5$bGMNet_pos[,2])> 0))/14000)),
         Triangle = c(sprintf("%0.4f", length(which((Post_triangle_GNet2_1$bGNet[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_1$bGNet[,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet1_1$bGNet[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet1_1$bGNet[,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet1_1$bGNet[,3])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet1_1$bGNet[,4])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet1_1$bGNet[,5])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet1_1$bGNet[,6])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet1_1$bGNet[,7])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_2$bGANet[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_2$bGANet[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_2$bGANet[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_2$bGANet[,4,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_2$bGANet[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_2$bGANet[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_2$bGANet[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_2$bGANet[,4,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_3$bGRNet[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_3$bGRNet[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_3$bGRNet[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_3$bGRNet[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_3$bGRNet[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_3$bGRNet[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_4$bGKNet[,1,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_4$bGKNet[,2,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_4$bGKNet[,3,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_4$bGKNet[,1,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_4$bGKNet[,2,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_4$bGKNet[,3,2])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_5$bGMNet[,1])> 0))/14000),
                      sprintf("%0.4f", length(which((Post_triangle_GNet2_5$bGMNet[,2])> 0))/14000)),
         Community = c(sprintf("%0.4f", length(which((Post_commsize_GNet2_1$bGNet[,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_1$bGNet[,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet1_1$bGNet[,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet1_1$bGNet[,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet1_1$bGNet[,3])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet1_1$bGNet[,4])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet1_1$bGNet[,5])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet1_1$bGNet[,6])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet1_1$bGNet[,7])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_2$bGANet[,1,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_2$bGANet[,2,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_2$bGANet[,3,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_2$bGANet[,4,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_2$bGANet[,1,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_2$bGANet[,2,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_2$bGANet[,3,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_2$bGANet[,4,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_3$bGRNet[,1,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_3$bGRNet[,2,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_3$bGRNet[,3,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_3$bGRNet[,1,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_3$bGRNet[,2,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_3$bGRNet[,3,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_4$bGKNet[,1,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_4$bGKNet[,2,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_4$bGKNet[,3,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_4$bGKNet[,1,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_4$bGKNet[,2,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_4$bGKNet[,3,2])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_5$bGMNet[,1])> 0))/14000),
                       sprintf("%0.4f", length(which((Post_commsize_GNet2_5$bGMNet[,2])> 0))/14000))) %>% 
  mutate(Indegree = paste0(format(round(as.numeric(Indegree)*100, 2), nsmall = 2), "%"),
         Harmonic = paste0(format(round(as.numeric(Harmonic)*100, 2), nsmall = 2), "%"),
         PageRank = paste0(format(round(as.numeric(PageRank)*100, 2), nsmall = 2), "%"),
         Between = paste0(format(round(as.numeric(Between)*100, 2), nsmall = 2), "%"),
         Triangle = paste0(format(round(as.numeric(Triangle)*100, 2), nsmall = 2), "%"),
         Community = paste0(format(round(as.numeric(Community)*100, 2), nsmall = 2), "%"),
         Variable = factor(Variable,
                           levels = c("Social", "Reputational",
                                      "Consanguineal", "Affinal", "Friend",
                                      "Generosity", "Religiosity",
                                      "Influence", "Knowledge",
                                      "<35", "35-49", "50-64", ">=65",
                                      "Patrilocal", "Bilocal", "Matrilocal",
                                      "Matrilineal", "Non-unilineal", "Patrilineal",
                                      "Market")),
         Domain = factor(Domain,
                         levels = c("Social", "Reputational"))) %>% 
  arrange(Domain, Variable) %>%
  gt(groupname_col = "Domain") %>% 
  fmt_number( ) %>% 
  sub_missing(columns = everything() , 
              missing_text = " ") %>% 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels( everything())) %>% 
  tab_style(style = list(cell_text(font = "Times New Roman")),
            locations = cells_body()) %>% 
  cols_align(align = "left",
             columns = "Variable") %>%
  cols_align(align = "center",
             columns = c("Indegree", "Harmonic", "PageRank", 
                         "Between", "Triangle", "Community")) %>% 
  opt_table_lines(extent = "default") %>%
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.top.width = px(2),
              column_labels.border.bottom.color = "black",
              column_labels.border.bottom.width = px(2),
              table_body.hlines.color = "lightgrey",
              table.border.bottom.color = "black",
              table.border.bottom.width = px(2)) %>% 
  cols_width("Variable" ~ px(300), 
             everything() ~ px(150))

Pro_receive_structural

##2.2 Multiplex position ----
###2.2.1 Cross-layer recognition ----
Pro_multi_partici <- 
  tibble(Variable = c("Social × reputational", 
                      "Consanguineal × Generosity",
                      "Consanguineal × Religiosity",
                      "Consanguineal × leadership",
                      "Consanguineal × Knowledge",
                      "Affinal × Generosity",
                      "Affinal × Religiosity",
                      "Affinal × leadership",
                      "Affinal × Knowledge",
                      "Friendship × Generosity",
                      "Friendship × Religiosity",
                      "Friendship × leadership",
                      "Friendship × Knowledge",
                      
                      "<35", "35-49", "50-64", ">=65",
                      "Patrilocal", "Bilocal", "Matrilocal",
                      "Matrilineal", "Non-unilineal", "Patrilineal",
                      "Market"),
         Pro = c(sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1$bG[,1])> 0))/14000),
                 
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,1])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,2])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,3])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,4])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,5])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,6])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,7])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,8])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,9])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,10])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,11])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_1_1$bGType[,12])> 0))/14000),
                 
                 
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_2$bGA[,1])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_2$bGA[,2])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_2$bGA[,3])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_2$bGA[,4])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_3$bGR[,1])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_3$bGR[,2])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_3$bGR[,3])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_4$bGK[,1])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_4$bGK[,2])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_4$bGK[,3])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_multi_network_partici_GNet_5$bGM[,1])> 0))/14000))) %>% 
  mutate(Pro = paste0(format(round(as.numeric(Pro)*100, 2), nsmall = 2), "%")) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing(columns = everything() , 
              missing_text = " ") %>% 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels( everything())) %>% 
  tab_style(style = list(cell_text(font = "Times New Roman")),
            locations = cells_body()) %>% 
  cols_align(align = "left",
             columns = "Variable") %>%
  cols_align(align = "center",
             columns = c("Pro")) %>% 
  opt_table_lines(extent = "default") %>%
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.top.width = px(2),
              column_labels.border.bottom.color = "black",
              column_labels.border.bottom.width = px(2),
              table_body.hlines.color = "lightgrey",
              table.border.bottom.color = "black",
              table.border.bottom.width = px(2)) %>% 
  cols_width("Variable" ~ px(300), 
             everything() ~ px(150))

Pro_multi_partici

###2.2.2 Cross-layer ties ----
Pro_cross_network <- 
  tibble(Variable = c("Social × reputational", 
                      "Consanguineal × Generosity",
                      "Consanguineal × Religiosity",
                      "Consanguineal × leadership",
                      "Consanguineal × Knowledge",
                      "Affinal × Generosity",
                      "Affinal × Religiosity",
                      "Affinal × leadership",
                      "Affinal × Knowledge",
                      "Friendship × Generosity",
                      "Friendship × Religiosity",
                      "Friendship × leadership",
                      "Friendship × Knowledge",
                      "<35", "35-49", "50-64", ">=65",
                      "Patrilocal", "Bilocal", "Matrilocal",
                      "Matrilineal", "Non-unilineal", "Patrilineal",
                      "Market"),
         Pro = c(sprintf("%0.4f", length(which((Post_cross_network_GNet_1$bG[,1])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,1])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,2])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,3])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,4])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,5])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,6])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,7])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,8])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,9])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,10])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,11])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_1_1$bGType[,12])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_2$bGA[,1])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_2$bGA[,2])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_2$bGA[,3])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_2$bGA[,4])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_3$bGR[,1])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_3$bGR[,2])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_3$bGR[,3])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_4$bGK[,1])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_4$bGK[,2])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_4$bGK[,3])> 0))/14000),
                 sprintf("%0.4f", length(which((Post_cross_network_GNet_5$bGM[,1])> 0))/14000))) %>% 
  mutate(Pro = paste0(format(round(as.numeric(Pro)*100, 2), nsmall = 2), "%")) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing(columns = everything() , 
              missing_text = " ") %>% 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels( everything())) %>% 
  tab_style(style = list(cell_text(font = "Times New Roman")),
            locations = cells_body()) %>% 
  cols_align(align = "left",
             columns = "Variable") %>%
  cols_align(align = "center",
             columns = c("Pro")) %>% 
  opt_table_lines(extent = "default") %>%
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.top.width = px(2),
              column_labels.border.bottom.color = "black",
              column_labels.border.bottom.width = px(2),
              table_body.hlines.color = "lightgrey",
              table.border.bottom.color = "black",
              table.border.bottom.width = px(2)) %>% 
  cols_width("Variable" ~ px(300), 
             everything() ~ px(150))

Pro_cross_network
