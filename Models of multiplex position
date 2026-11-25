# Analyses of cross-layer recognition and ties

#1 Gender differences ----
##1.1 Cross-layer recognition ----
###1.1.1 Total ----
# Across broad social and reputational domains
{set.seed(123)
  Model_multi_network_partici_GNet_1 <- ulam(
    alist(
      If_multinetworks_participation ~ bernoulli(p),
      logit(p) <- a + bG*(Gender-1) + 
        bV[VID], 
      
      a ~ normal(0, 0.3),
      bG ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-0.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_1 <- precis(Model_multi_network_partici_GNet_1, depth = 3, prob = 0.90, 
                                           pars = c("a", "bG", "V_bar"))
Pre_multi_network_partici_GNet_1

# Across specific social and reputational network layers
{set.seed(123)
  Model_multi_network_partici_GNet_1_1 <- ulam(
    alist(
      Cross_active ~ bernoulli(p),
      logit(p) <- bType[Cross_type] +
        bGType[Cross_type] * (Gender - 1) +
        bV[VID] + 
        bI[ID], 
      
      bType[Cross_type] ~ normal(0, 0.3),
      bGType[Cross_type] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(-2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_long_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_1_1 <- precis(Model_multi_network_partici_GNet_1_1, depth = 3, prob = 0.90, 
                                             pars = c("bType", "bGType", "V_bar"))
Pre_multi_network_partici_GNet_1_1

###1.1.2 Direct ----
# Across broad social and reputational domains
{set.seed(123)
  Model_multi_network_partici_GNet_1_d <- ulam(
    alist(
      If_multinetworks_participation ~ bernoulli(p),
      logit(p) <- a + bG*(Gender-1) + 
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + bM * Market +
        bV[VID], 
      
      a ~ normal(0, 0.3),
      bG ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-0.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_1_d <- precis(Model_multi_network_partici_GNet_1_d, depth = 3, prob = 0.90, 
                                             pars = c("a", "bG", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_multi_network_partici_GNet_1_d

# Across specific social and reputational network layers
{set.seed(123)
  Model_multi_network_partici_GNet_1_1_d <- ulam(
    alist(
      Cross_active ~ bernoulli(p),
      logit(p) <- bType[Cross_type] +
        bGType[Cross_type] * (Gender - 1) +
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID], 
      
      bType[Cross_type] ~ normal(0, 0.3),
      bGType[Cross_type] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(-2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_long_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_1_1_d <- precis(Model_multi_network_partici_GNet_1_1_d, depth = 3, prob = 0.90, 
                                               pars = c("bType", "bGType", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_multi_network_partici_GNet_1_1_d

##1.2 Cross-layer ties ----
###1.2.1 Total ----
# Across broad social and reputational domains
{set.seed(123)
  Model_cross_network_GNet_1 <- ulam(
    alist(
      If_cross_network ~ bernoulli(p),
      logit(p) <- a + bG*(Gender-1) + 
        bV[VID], 
      
      a ~ normal(0, 0.3),
      bG ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_cross_network_ties_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_1 <- precis(Model_cross_network_GNet_1, depth = 3, prob = 0.90, 
                                   pars = c("a", "bG", "V_bar"))
Pre_cross_network_GNet_1

# Across specific social and reputational network layers
{set.seed(123)
  Model_cross_network_GNet_1_1 <- ulam(
    alist(
      Cross_active ~ bernoulli(p),
      logit(p) <- bType[Cross_type] +
        bGType[Cross_type] * (Gender - 1) +
        bV[VID] + 
        bI[ID],
      
      bType[Cross_type] ~ normal(0, 0.3),
      bGType[Cross_type] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(-5.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_cross_network_ties_long_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_1_1 <- precis(Model_cross_network_GNet_1_1, depth = 3, prob = 0.90, 
                                     pars = c("bType", "bGType", "V_bar"))
Pre_cross_network_GNet_1_1

###1.2.2 Direct ----
# Across broad social and reputational domains
{set.seed(123)
  Model_cross_network_GNet_1_d <- ulam(
    alist(
      If_cross_network ~ bernoulli(p),
      logit(p) <- a + bG*(Gender-1) + 
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + bM * Market +
        bV[VID], 
      
      a ~ normal(0, 0.3),
      bG ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_cross_network_ties_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_1_d <- precis(Model_cross_network_GNet_1_d, depth = 3, prob = 0.90, 
                                     pars = c("a", "bG", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_cross_network_GNet_1_d

# Across specific social and reputational network layers
{set.seed(123)
  Model_cross_network_GNet_1_1_d <- ulam(
    alist(
      Cross_active ~ bernoulli(p),
      logit(p) <- bType[Cross_type] +
        bGType[Cross_type] * (Gender - 1) +
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID] ,
      
      bType[Cross_type] ~ normal(0, 0.3),
      bGType[Cross_type] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(-5.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_cross_network_ties_long_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_1_1_d <- precis(Model_cross_network_GNet_1_1_d, depth = 3, prob = 0.90, 
                                       pars = c("bType", "bGType", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_cross_network_GNet_1_1_d

#2 Gender differences stratified by age cohorts ----
##2.1 Cross-layer recognition ----
###2.1.1 Total ----
{set.seed(123)
  Model_multi_network_partici_GNet_2 <- ulam(
    alist(
      If_multinetworks_participation ~ bernoulli(p),
      logit(p) <- bA[AgeC] + bGA[AgeC]*(Gender-1) + 
        bV[VID],
      
      bA[AgeC] ~ normal(0, 0.3),
      bGA[AgeC] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-0.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_2 <- precis(Model_multi_network_partici_GNet_2, depth = 3, prob = 0.90, 
                                           pars = c("bA", "bGA", "V_bar"))
Pre_multi_network_partici_GNet_2

###2.1.2 Direct ----
{set.seed(123)
  Model_multi_network_partici_GNet_2_d <- ulam(
    alist(
      If_multinetworks_participation ~ bernoulli(p),
      logit(p) <- bA[AgeC] + bGA[AgeC]*(Gender-1) + 
        bIn[Inheritance] + bR[Residence] + 
        bK[Kinship] + bM * Market +
        bV[VID], 
      
      bA[AgeC] ~ normal(0, 0.3),
      bGA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-0.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_2_d <- precis(Model_multi_network_partici_GNet_2_d, depth = 3, prob = 0.90, 
                                             pars = c("bA", "bGA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_multi_network_partici_GNet_2_d

##2.2 Cross-network ties ----
###2.2.1 Total ----
{set.seed(123)
  Model_cross_network_GNet_2 <- ulam(
    alist(
      If_cross_network ~ bernoulli(p),
      logit(p) <- bA[AgeC] + bGA[AgeC]*(Gender-1) + 
        bV[VID],
      
      bA[AgeC] ~ normal(0, 0.3),
      bGA[AgeC] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_cross_network_ties_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_2 <- precis(Model_cross_network_GNet_2, depth = 3, prob = 0.90, 
                                   pars = c("bA", "bGA", "V_bar"))
Pre_cross_network_GNet_2

###2.2.2 Direct ----
{set.seed(123)
  Model_cross_network_GNet_2_d <- ulam(
    alist(
      If_cross_network ~ bernoulli(p),
      logit(p) <- bA[AgeC] + bGA[AgeC]*(Gender-1) + 
        bIn[Inheritance] + bR[Residence] + 
        bK[Kinship] + bM * Market +
        bV[VID], 
      
      bA[AgeC] ~ normal(0, 0.3),
      bGA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_cross_network_ties_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_2_d <- precis(Model_cross_network_GNet_2_d, depth = 3, prob = 0.90, 
                                     pars = c("bA", "bGA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_cross_network_GNet_2_d

#3 Gender differences stratified by residence patterns ----
##3.1 Cross-layer recognition ----
###3.1.1 Total ----
{set.seed(123)
  Model_multi_network_partici_GNet_3 <- ulam(
    alist(
      If_multinetworks_participation ~ bernoulli(p),
      logit(p) <- bR[Residence] + bGR[Residence]*(Gender-1) + 
        bV[VID], 
      
      bR[Residence] ~ normal(0, 0.3),
      bGR[Residence] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-0.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_3 <- precis(Model_multi_network_partici_GNet_3, depth = 3, prob = 0.90, 
                                           pars = c("bR", "bGR", "V_bar"))
Pre_multi_network_partici_GNet_3

###3.1.2 Direct ----
{set.seed(123)
  Model_multi_network_partici_GNet_3_d <- ulam(
    alist(
      If_multinetworks_participation ~ bernoulli(p),
      logit(p) <- bR[Residence] + bGR[Residence]*(Gender-1) +
        bA[AgeC] + bIn[Inheritance] +
        bK[Kinship] + bM * Market +
        bV[VID], 
      
      bR[Residence] ~ normal(0, 0.3),
      bGR[Residence] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-0.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_3_d <- precis(Model_multi_network_partici_GNet_3_d, depth = 3, prob = 0.90, 
                                             pars = c("bR", "bGR", "bA", "bIn", "bK", "bM", "V_bar"))
Pre_multi_network_partici_GNet_3_d

##3.2 Cross-layer ties ----
###3.2.1 Total ----
{set.seed(123)
  Model_cross_network_GNet_3 <- ulam(
    alist(
      If_cross_network ~ bernoulli(p),
      logit(p) <- bR[Residence] + bGR[Residence]*(Gender-1) +
        bV[VID], 
      
      bR[Residence] ~ normal(0, 0.3),
      bGR[Residence] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_cross_network_ties_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_3 <- precis(Model_cross_network_GNet_3, depth = 3, prob = 0.90, 
                                   pars = c("bR", "bGR", "V_bar"))
Pre_cross_network_GNet_3

###3.2.2 Direct ----
{set.seed(123)
  Model_cross_network_GNet_3_d <- ulam(
    alist(
      If_cross_network ~ bernoulli(p),
      logit(p) <- bR[Residence] + bGR[Residence]*(Gender-1) +
        bA[AgeC] + bIn[Inheritance] +
        bK[Kinship] + bM * Market +
        bV[VID], 
      
      bR[Residence] ~ normal(0, 0.3),
      bGR[Residence] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_cross_network_ties_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_3_d <- precis(Model_cross_network_GNet_3_d, depth = 3, prob = 0.90, 
                                     pars = c("bR", "bGR", "bA", "bIn", "bK", "bM", "V_bar"))
Pre_cross_network_GNet_3_d

#4 Gender differences stratified by kinship systems ----
##4.1 Cross-layer recognition ----
###4.1.1 Total ----
{set.seed(123)
  Model_multi_network_partici_GNet_4 <- ulam(
    alist(
      If_multinetworks_participation ~ bernoulli(p),
      logit(p) <- bK[Kinship] + bGK[Kinship]*(Gender-1) + 
        bV[VID], 
      
      bK[Kinship] ~ normal(0, 0.3),
      bGK[Kinship] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-0.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_4 <- precis(Model_multi_network_partici_GNet_4, depth = 3, prob = 0.90, 
                                           pars = c("bK", "bGK", "V_bar"))
Pre_multi_network_partici_GNet_4

###4.1.2 Direct ----
{set.seed(123)
  Model_multi_network_partici_GNet_4_d <- ulam(
    alist(
      If_multinetworks_participation ~ bernoulli(p),
      logit(p) <- bK[Kinship] + bGK[Kinship]*(Gender-1) +
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bM * Market +
        bV[VID], 
      
      bK[Kinship] ~ normal(0, 0.3),
      bGK[Kinship] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-0.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_4_d <- precis(Model_multi_network_partici_GNet_4_d, depth = 3, prob = 0.90, 
                                             pars = c("bK", "bGK", "bA", "bIn", "bR", "bM", "V_bar"))
Pre_multi_network_partici_GNet_4_d

##4.2 Cross-layer ties ----
###4.2.1 Total ----
{set.seed(123)
  Model_cross_network_GNet_4 <- ulam(
    alist(
      If_cross_network ~ bernoulli(p),
      logit(p) <- bK[Kinship] + bGK[Kinship]*(Gender-1) +
        bV[VID], 
      
      bK[Kinship] ~ normal(0, 0.3),
      bGK[Kinship] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_cross_network_ties_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_4 <- precis(Model_cross_network_GNet_4, depth = 3, prob = 0.90, 
                                   pars = c("bK", "bGK", "V_bar"))
Pre_cross_network_GNet_4

###4.2.2 Direct ----
{set.seed(123)
  Model_cross_network_GNet_4_d <- ulam(
    alist(
      If_cross_network ~ bernoulli(p),
      logit(p) <- bK[Kinship] + bGK[Kinship]*(Gender-1) +
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bM * Market +
        bV[VID],
      
      bK[Kinship] ~ normal(0, 0.3),
      bGK[Kinship] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_cross_network_ties_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_4_d <- precis(Model_cross_network_GNet_4_d, depth = 3, prob = 0.90, 
                                     pars = c("bK", "bGK", "bA", "bIn", "bR", "bM", "V_bar"))
Pre_cross_network_GNet_4_d

#5 How market participation shape gender differences ----
##5.1 Cross-layer recognition ----
###5.1.1 Total ----
{set.seed(123)
  Model_multi_network_partici_GNet_5 <- ulam(
    alist(
      If_multinetworks_participation ~ bernoulli(p),
      logit(p) <- bM * Market + bGM * (Gender - 1) * Market + 
        bV[VID], 
      
      bM ~ normal(0, 0.3),
      bGM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-0.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_5 <- precis(Model_multi_network_partici_GNet_5, depth = 3, prob = 0.90, 
                                           pars = c("bM", "bGM", "V_bar"))
Pre_multi_network_partici_GNet_5

###5.1.2 Direct ----
{set.seed(123)
  Model_multi_network_partici_GNet_5_d <- ulam(
    alist(
      If_multinetworks_participation ~ bernoulli(p),
      logit(p) <- bM * Market + bGM * (Gender - 1) * Market + 
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + 
        bV[VID],
      
      bM ~ normal(0, 0.3),
      bGM ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-0.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_multiplex_networks_participation_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_multi_network_partici_GNet_5_d <- precis(Model_multi_network_partici_GNet_5_d, depth = 3, prob = 0.90, 
                                             pars = c("bM", "bGM", "bA", "bIn", "bR", "bK", "V_bar"))
Pre_multi_network_partici_GNet_5_d

##5.2 Cross-layer ties ----
###5.2.1 Total ----
{set.seed(123)
  Model_cross_network_GNet_5 <- ulam(
    alist(
      If_cross_network ~ bernoulli(p),
      logit(p) <- bM * Market + bGM * (Gender - 1) * Market + 
        bV[VID], 
      
      bM ~ normal(0, 0.3),
      bGM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_cross_network_ties_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_5 <- precis(Model_cross_network_GNet_5, depth = 3, prob = 0.90, 
                                   pars = c("bM", "bGM", "V_bar"))
Pre_cross_network_GNet_5

###5.2.2 Direct ----
{set.seed(123)
  Model_cross_network_GNet_5_d <- ulam(
    alist(
      If_cross_network ~ bernoulli(p),
      logit(p) <- bM * Market + bGM * (Gender - 1) * Market + 
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + 
        bV[VID], 
      
      bM ~ normal(0, 0.3),
      bGM ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1)
    ) , data = Model_cross_network_ties_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.99) 
  ) }
Pre_cross_network_GNet_5_d <- precis(Model_cross_network_GNet_5_d, depth = 3, prob = 0.90, 
                                     pars = c("bM", "bGM", "bA", "bIn", "bR", "bK", "V_bar"))
Pre_cross_network_GNet_5_d
