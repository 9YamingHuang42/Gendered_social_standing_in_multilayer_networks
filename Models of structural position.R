# Analyses of betweenness centrality, inward harmonic centrality, triangle count, and community size

#1 Gender differences across network domains/types ----
##1.1 Betweenness ----
###1.1.1 Total ----
mean(Indiv.indicator.all$Between == 0) # 74.78% zero value
hist(log(Indiv.indicator.all$Between[Indiv.indicator.all$Between > 0])) # fit normal distribution

# Stratify by network types
{set.seed(123)
  Model_between_GNet1_1 <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_type] + 
        bGNet_p[Net_type]*(Gender-1) +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bNet_pos[Net_type[Between_pos_idx]] + 
        bGNet_pos[Net_type[Between_pos_idx]]*(Gender[Between_pos_idx]-1) +
        bV_pos[VID[Between_pos_idx]] + 
        bI_pos[ID[Between_pos_idx]], 
      
      # Priors: hurdle part
      bNet_p[Net_type] ~ normal(0, 0.3),
      bGNet_p[Net_type] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      bNet_pos[Net_type] ~ normal(0, 0.3),
      bGNet_pos[Net_type] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet1_1 <- precis(Model_between_GNet1_1, depth = 3, prob = 0.90, 
                              pars = c("bNet_p", "bGNet_p", "bNet_pos", "bGNet_pos", 
                                       "V_bar_p", "V_bar_pos"))
Pre_between_GNet1_1

# Stratify by network domains
{set.seed(123)
  Model_between_GNet2_1 <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_domain] + 
        bGNet_p[Net_domain]*(Gender-1) +
        bV_p[VID] + # village intercepts
        bI_p[ID], # individual intercepts
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bNet_pos[Net_domain[Between_pos_idx]] + 
        bGNet_pos[Net_domain[Between_pos_idx]]*(Gender[Between_pos_idx]-1) +
        bV_pos[VID[Between_pos_idx]] + # village intercepts
        bI_pos[ID[Between_pos_idx]], # individual intercepts
      
      # Priors: hurdle part
      bNet_p[Net_domain] ~ normal(0, 0.3),
      bGNet_p[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      bNet_pos[Net_domain] ~ normal(0, 0.3),
      bGNet_pos[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet2_1 <- precis(Model_between_GNet2_1, depth = 3, prob = 0.90, 
                              pars = c("bNet_p", "bGNet_p", "bNet_pos", "bGNet_pos", 
                                       "V_bar_p", "V_bar_pos"))
Pre_between_GNet2_1

###1.1.2 Direct ----
# Stratify by network types
{set.seed(123)
  Model_between_GNet1_1_d <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_type] + 
        bGNet_p[Net_type]*(Gender-1) +
        bA_p[AgeC] + bIn_p[Inheritance] +
        bR_p[Residence] + bK_p[Kinship] + bM_p * Market +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bNet_pos[Net_type[Between_pos_idx]] + 
        bGNet_pos[Net_type[Between_pos_idx]]*(Gender[Between_pos_idx]-1) +
        bA_pos[AgeC[Between_pos_idx]] + bIn_pos[Inheritance[Between_pos_idx]] +
        bR_pos[Residence[Between_pos_idx]] + bK_pos[Kinship[Between_pos_idx]] + 
        bM_pos * Market[Between_pos_idx] +
        bV_pos[VID[Between_pos_idx]] + 
        bI_pos[ID[Between_pos_idx]], 
      
      # Priors: hurdle part
      bNet_p[Net_type] ~ normal(0, 0.3),
      bGNet_p[Net_type] ~ normal(0, 0.3),
      bA_p[AgeC] ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bR_p[Residence] ~ normal(0, 0.3),
      bK_p[Kinship] ~ normal(0, 0.3),
      bM_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      bNet_pos[Net_type] ~ normal(0, 0.3),
      bGNet_pos[Net_type] ~ normal(0, 0.3),
      bA_pos[AgeC] ~ normal(0, 0.3),
      bIn_pos[Inheritance] ~ normal(0, 0.3),
      bR_pos[Residence] ~ normal(0, 0.3),
      bK_pos[Kinship] ~ normal(0, 0.3),
      bM_pos ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet1_1_d <- precis(Model_between_GNet1_1_d, depth = 3, prob = 0.90, 
                                pars = c("bNet_p", "bGNet_p", "bNet_pos", "bGNet_pos", 
                                         "bA_p", "bA_pos", "bIn_p", "bIn_pos", "bR_p", "bR_pos", 
                                         "bK_p", "bK_pos", "bM_p", "bM_pos", "V_bar_p", "V_bar_pos"))
Pre_between_GNet1_1_d

# Stratify by network domains
{set.seed(123)
  Model_between_GNet2_1_d <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_domain] + 
        bGNet_p[Net_domain]*(Gender-1) +
        bA_p[AgeC] + bIn_p[Inheritance] +
        bR_p[Residence] + bK_p[Kinship] + bM_p * Market +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bNet_pos[Net_domain[Between_pos_idx]] + 
        bGNet_pos[Net_domain[Between_pos_idx]]*(Gender[Between_pos_idx]-1) +
        bA_pos[AgeC[Between_pos_idx]] + bIn_pos[Inheritance[Between_pos_idx]] +
        bR_pos[Residence[Between_pos_idx]] + bK_pos[Kinship[Between_pos_idx]] + 
        bM_pos * Market[Between_pos_idx] +
        bV_pos[VID[Between_pos_idx]] + 
        bI_pos[ID[Between_pos_idx]], 
      
      # Priors: hurdle part
      bNet_p[Net_domain] ~ normal(0, 0.3),
      bGNet_p[Net_domain] ~ normal(0, 0.3),
      bA_p[AgeC] ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bR_p[Residence] ~ normal(0, 0.3),
      bK_p[Kinship] ~ normal(0, 0.3),
      bM_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      bNet_pos[Net_domain] ~ normal(0, 0.3),
      bGNet_pos[Net_domain] ~ normal(0, 0.3),
      bA_pos[AgeC] ~ normal(0, 0.3),
      bIn_pos[Inheritance] ~ normal(0, 0.3),
      bR_pos[Residence] ~ normal(0, 0.3),
      bK_pos[Kinship] ~ normal(0, 0.3),
      bM_pos ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet2_1_d <- precis(Model_between_GNet2_1_d, depth = 3, prob = 0.90, 
                                pars = c("bNet_p", "bGNet_p", "bNet_pos", "bGNet_pos", 
                                         "bA_p", "bA_pos", "bIn_p", "bIn_pos", "bR_p", "bR_pos", 
                                         "bK_p", "bK_pos", "bM_p", "bM_pos", "V_bar_p", "V_bar_pos"))
Pre_between_GNet2_1_d

##1.2 Inward harmonic centrality ----
###1.2.1 Total ----
# Stratify by network types
{set.seed(123)
  Model_harmonic_in_GNet1_1 <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_type] +
        bGNet_p[Net_type]*(Gender-1) +
        bV_p[VID] +
        bI_p[ID],
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bNet_pos[Net_type[Harmonic_in_pos_idx]] +
        bGNet_pos[Net_type[Harmonic_in_pos_idx]]*(Gender[Harmonic_in_pos_idx]-1) +
        bV_pos[VID[Harmonic_in_pos_idx]] + 
        bI_pos[ID[Harmonic_in_pos_idx]], 
      
      # Priors: hurdle part
      bNet_p[Net_type] ~ normal(0, 0.3),
      bGNet_p[Net_type] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      bNet_pos[Net_type] ~ normal(0, 0.2),
      bGNet_pos[Net_type] ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet1_1 <- precis(Model_harmonic_in_GNet1_1, depth = 3, prob = 0.90, 
                                  pars = c("bNet_p", "bGNet_p", "bNet_pos", "bGNet_pos", 
                                           "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet1_1

# Stratify by network domains
{set.seed(123)
  Model_harmonic_in_GNet2_1 <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_domain] +
        bGNet_p[Net_domain]*(Gender-1) +
        bV_p[VID] +
        bI_p[ID],
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale ),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bNet_pos[Net_domain[Harmonic_in_pos_idx]] +
        bGNet_pos[Net_domain[Harmonic_in_pos_idx]]*(Gender[Harmonic_in_pos_idx]-1) +
        bV_pos[VID[Harmonic_in_pos_idx]] +
        bI_pos[ID[Harmonic_in_pos_idx]], 
      
      # Priors: hurdle part
      bNet_p[Net_domain] ~ normal(0, 0.3),
      bGNet_p[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      bNet_pos[Net_domain] ~ normal(0, 0.2),
      bGNet_pos[Net_domain] ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet2_1 <- precis(Model_harmonic_in_GNet2_1, depth = 3, prob = 0.90, 
                                  pars = c("bNet_p", "bGNet_p", "bNet_pos", "bGNet_pos", 
                                           "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet2_1

###1.2.2 Direct ----
# Stratify by network types
{set.seed(123)
  Model_harmonic_in_GNet1_1_d <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_type] +
        bGNet_p[Net_type]*(Gender-1) +
        bA_p[AgeC] + bIn_p[Inheritance] +
        bR_p[Residence] + bK_p[Kinship] + bM_p * Market +
        bV_p[VID] + 
        bI_p[ID],
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bNet_pos[Net_type[Harmonic_in_pos_idx]] +
        bGNet_pos[Net_type[Harmonic_in_pos_idx]]*(Gender[Harmonic_in_pos_idx]-1) +
        bA_pos[AgeC[Harmonic_in_pos_idx]] + bIn_pos[Inheritance[Harmonic_in_pos_idx]] +
        bR_pos[Residence[Harmonic_in_pos_idx]] + bK_pos[Kinship[Harmonic_in_pos_idx]] + 
        bM_pos * Market[Harmonic_in_pos_idx] +
        bV_pos[VID[Harmonic_in_pos_idx]] + 
        bI_pos[ID[Harmonic_in_pos_idx]], 
      
      # Priors: hurdle part
      bNet_p[Net_type] ~ normal(0, 0.3),
      bGNet_p[Net_type] ~ normal(0, 0.3),
      bA_p[AgeC] ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bR_p[Residence] ~ normal(0, 0.3),
      bK_p[Kinship] ~ normal(0, 0.3),
      bM_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      bNet_pos[Net_type] ~ normal(0, 0.2),
      bGNet_pos[Net_type] ~ normal(0, 0.2),
      bA_pos[AgeC] ~ normal(0, 0.2),
      bIn_pos[Inheritance] ~ normal(0, 0.2),
      bR_pos[Residence] ~ normal(0, 0.2),
      bK_pos[Kinship] ~ normal(0, 0.2),
      bM_pos ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet1_1_d <- precis(Model_harmonic_in_GNet1_1_d, depth = 3, prob = 0.90, 
                                    pars = c("bNet_p", "bGNet_p", "bNet_pos", "bGNet_pos", 
                                             "bA_p", "bA_pos", "bIn_p", "bIn_pos", "bR_p", "bR_pos", 
                                             "bK_p", "bK_pos", "bM_p", "bM_pos", "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet1_1_d

# Stratify by network domains
{set.seed(123)
  Model_harmonic_in_GNet2_1_d <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_domain] +
        bGNet_p[Net_domain]*(Gender-1) +
        bA_p[AgeC] + bIn_p[Inheritance] +
        bR_p[Residence] + bK_p[Kinship] + bM_p * Market +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale ),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bNet_pos[Net_domain[Harmonic_in_pos_idx]] +
        bGNet_pos[Net_domain[Harmonic_in_pos_idx]]*(Gender[Harmonic_in_pos_idx]-1) +
        bA_pos[AgeC[Harmonic_in_pos_idx]] + bIn_pos[Inheritance[Harmonic_in_pos_idx]] +
        bR_pos[Residence[Harmonic_in_pos_idx]] + bK_pos[Kinship[Harmonic_in_pos_idx]] + 
        bM_pos * Market[Harmonic_in_pos_idx] +
        bV_pos[VID[Harmonic_in_pos_idx]] + 
        bI_pos[ID[Harmonic_in_pos_idx]], 
      
      # Priors: hurdle part
      bNet_p[Net_domain] ~ normal(0, 0.3),
      bGNet_p[Net_domain] ~ normal(0, 0.3),
      bA_p[AgeC] ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bR_p[Residence] ~ normal(0, 0.3),
      bK_p[Kinship] ~ normal(0, 0.3),
      bM_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      bNet_pos[Net_domain] ~ normal(0, 0.2),
      bGNet_pos[Net_domain] ~ normal(0, 0.2),
      bA_pos[AgeC] ~ normal(0, 0.2),
      bIn_pos[Inheritance] ~ normal(0, 0.2),
      bR_pos[Residence] ~ normal(0, 0.2),
      bK_pos[Kinship] ~ normal(0, 0.2),
      bM_pos ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet2_1_d <- precis(Model_harmonic_in_GNet2_1_d, depth = 3, prob = 0.90, 
                                    pars = c("bNet_p", "bGNet_p", "bNet_pos", "bGNet_pos", 
                                             "bA_p", "bA_pos", "bIn_p", "bIn_pos", "bR_p", "bR_pos", 
                                             "bK_p", "bK_pos", "bM_p", "bM_pos", "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet2_1_d

##1.3 Triangle count ----
c( mean = mean(Indiv.indicator.all$Triangle) ,
   var = var(Indiv.indicator.all$Triangle) , 
   ratio = var(Indiv.indicator.all$Triangle)/mean(Indiv.indicator.all$Triangle)) 
# var>>mean and ratio more than 2 (overdispersion)

###1.3.1 Total ----
# Stratify by network types
{set.seed(123)
  Model_triangle_GNet1_1 <- ulam(
    alist(
      Triangle ~ dgampois(lambda, phi),
      log(lambda) <- bNet[Net_type] + 
        bGNet[Net_type]*(Gender-1) + 
        bV[VID] + 
        bI[ID], 
      
      phi ~ exponential(1),
      
      bNet[Net_type] ~ normal(0, 0.3),
      bGNet[Net_type] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet1_1 <- precis(Model_triangle_GNet1_1, depth = 3, prob = 0.90, 
                               pars = c("bNet", "bGNet", "V_bar"))
Pre_triangle_GNet1_1

# Stratify by network domains
{set.seed(123)
  Model_triangle_GNet2_1 <- ulam(
    alist(
      Triangle ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bNet[Net_domain] + 
        bGNet[Net_domain]*(Gender-1) + 
        bV[VID] + 
        bI[ID], 
      
      phi ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.3),
      bGNet[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet2_1 <- precis(Model_triangle_GNet2_1, depth = 3, prob = 0.90, 
                               pars = c("bNet", "bGNet", "V_bar"))
Pre_triangle_GNet2_1

###1.3.2 Direct ----
# Stratify by network types
{set.seed(123)
  Model_triangle_GNet1_1_d <- ulam(
    alist(
      Triangle ~ dgampois(lambda, phi) ,
      log(lambda) <- bNet[Net_type] + 
        bGNet[Net_type]*(Gender-1) + 
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + bM * Market +
        bV[VID] +
        bI[ID],
      
      phi ~ exponential(1),
      
      bNet[Net_type] ~ normal(0, 0.3),
      bGNet[Net_type] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet1_1_d <- precis(Model_triangle_GNet1_1_d, depth = 3, prob = 0.90, 
                                 pars = c("bNet", "bGNet", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_triangle_GNet1_1_d

# Stratify by network domains
{set.seed(123)
  Model_triangle_GNet2_1_d <- ulam(
    alist(
      Triangle ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bNet[Net_domain] + 
        bGNet[Net_domain]*(Gender-1) + 
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID], 
      
      phi ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.3),
      bGNet[Net_domain] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet2_1_d <- precis(Model_triangle_GNet2_1_d, depth = 3, prob = 0.90, 
                                 pars = c("bNet", "bGNet", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_triangle_GNet2_1_d

##1.4 Community size ----
c( mean = mean(log(Indiv.indicator.all$Community.size)) ,
   var = var(Indiv.indicator.all$Community.size) , 
   ratio = var(Indiv.indicator.all$Community.size)/mean(Indiv.indicator.all$Community.size)) 
# var>>mean and ratio more than 2 (overdispersion)

###1.4.1 Total ----
# Stratify by network types
{set.seed(123)
  Model_commsize_GNet1_1 <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bNet[Net_type] + 
        bGNet[Net_type]*(Gender-1) + 
        bV[VID] + 
        bI[ID] , 
      
      sigma ~ exponential(1),
      
      bNet[Net_type] ~ normal(0, 0.3),
      bGNet[Net_type] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet1_1 <- precis(Model_commsize_GNet1_1, depth = 3, prob = 0.90, 
                               pars = c("bNet", "bGNet", "V_bar"))
Pre_commsize_GNet1_1

# Stratify by network domains
{set.seed(123)
  Model_commsize_GNet2_1 <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bNet[Net_domain] + 
        bGNet[Net_domain]*(Gender-1) + 
        bV[VID] + 
        bI[ID], 
      
      sigma ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.3),
      bGNet[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet2_1 <- precis(Model_commsize_GNet2_1, depth = 3, prob = 0.90, 
                               pars = c("bNet", "bGNet", "V_bar"))
Pre_commsize_GNet2_1

###1.4.2 Direct ----
# Stratify by network types
{set.seed(123)
  Model_commsize_GNet1_1_d <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bNet[Net_type] + 
        bGNet[Net_type]*(Gender-1) + 
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID], 
      
      sigma ~ exponential(1),
      
      bNet[Net_type] ~ normal(0, 0.3),
      bGNet[Net_type] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet1_1_d <- precis(Model_commsize_GNet1_1_d, depth = 3, prob = 0.90, 
                                 pars = c("bNet", "bGNet", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_commsize_GNet1_1_d

# Stratify by network domains
{set.seed(123)
  Model_commsize_GNet2_1_d <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bNet[Net_domain] + 
        bGNet[Net_domain]*(Gender-1) + 
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID], 
      
      sigma ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.3),
      bGNet[Net_domain] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet2_1_d <- precis(Model_commsize_GNet2_1_d, depth = 3, prob = 0.90, 
                                 pars = c("bNet", "bGNet", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_commsize_GNet2_1_d

#2 Gender differences stratified by network domains and age cohorts ----
##2.1 Betweenness ----
###2.1.1 Total ----
{set.seed(123)
  Model_between_GNet2_2 <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bANet_p[AgeC, Net_domain] +
        bGANet_p[AgeC, Net_domain]*(Gender-1) +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bANet_pos[AgeC[Between_pos_idx], Net_domain[Between_pos_idx]] +
        bGANet_pos[AgeC[Between_pos_idx], Net_domain[Between_pos_idx]]*(Gender[Between_pos_idx]-1) +
        bV_pos[VID[Between_pos_idx]] + 
        bI_pos[ID[Between_pos_idx]], 
      
      # Priors: hurdle part
      matrix[AgeC, Net_domain]: bANet_p ~ normal(0, 0.3),
      matrix[AgeC, Net_domain]: bGANet_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      matrix[AgeC, Net_domain]: bANet_pos ~ normal(0, 0.3),
      matrix[AgeC, Net_domain]: bGANet_pos ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet2_2 <- precis(Model_between_GNet2_2, depth = 3, prob = 0.90, 
                              pars = c("bANet_p", "bGANet_p", "bANet_pos", "bGANet_pos", 
                                       "V_bar_p", "V_bar_pos"))
Pre_between_GNet2_2

###2.1.2 Direct ----
{set.seed(123)
  Model_between_GNet2_2_d <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bANet_p[AgeC, Net_domain] +
        bGANet_p[AgeC, Net_domain]*(Gender-1) +
        bIn_p[Inheritance] + bR_p[Residence] + 
        bK_p[Kinship] + bM_p * Market +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bANet_pos[AgeC[Between_pos_idx], Net_domain[Between_pos_idx]] +
        bGANet_pos[AgeC[Between_pos_idx], Net_domain[Between_pos_idx]]*(Gender[Between_pos_idx]-1) +
        bIn_pos[Inheritance[Between_pos_idx]] + bR_pos[Residence[Between_pos_idx]] + 
        bK_pos[Kinship[Between_pos_idx]] + bM_pos * Market[Between_pos_idx] +
        bV_pos[VID[Between_pos_idx]] + 
        bI_pos[ID[Between_pos_idx]], 
      
      # Priors: hurdle part
      matrix[AgeC, Net_domain]: bANet_p ~ normal(0, 0.3),
      matrix[AgeC, Net_domain]: bGANet_p ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bR_p[Residence] ~ normal(0, 0.3),
      bK_p[Kinship] ~ normal(0, 0.3),
      bM_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      matrix[AgeC, Net_domain]: bANet_pos ~ normal(0, 0.3),
      matrix[AgeC, Net_domain]: bGANet_pos ~ normal(0, 0.3),
      bIn_pos[Inheritance] ~ normal(0, 0.3),
      bR_pos[Residence] ~ normal(0, 0.3),
      bK_pos[Kinship] ~ normal(0, 0.3),
      bM_pos ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet2_2_d <- precis(Model_between_GNet2_2_d, depth = 3, prob = 0.90, 
                                pars = c("bANet_p", "bGANet_p", "bANet_pos", "bGANet_pos", 
                                         "bIn_p", "bIn_pos", "bR_p", "bR_pos", 
                                         "bK_p", "bK_pos", "bM_p", "bM_pos",
                                         "V_bar_p", "V_bar_pos"))
Pre_between_GNet2_2_d

##2.2 Inward harmonic centrality ----
###2.2.1 Total ----
{set.seed(123)
  Model_harmonic_in_GNet2_2 <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bANet_p[AgeC, Net_domain] +
        bGANet_p[AgeC, Net_domain]*(Gender-1) +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale ),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bANet_pos[AgeC[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]] +
        bGANet_pos[AgeC[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]]*(Gender[Harmonic_in_pos_idx]-1) +
        bV_pos[VID[Harmonic_in_pos_idx]] + 
        bI_pos[ID[Harmonic_in_pos_idx]], 
      
      # Priors: hurdle part
      matrix[AgeC, Net_domain]: bANet_p ~ normal(0, 0.3),
      matrix[AgeC, Net_domain]: bGANet_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      matrix[AgeC, Net_domain]: bANet_pos ~ normal(0, 0.2),
      matrix[AgeC, Net_domain]: bGANet_pos ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet2_2 <- precis(Model_harmonic_in_GNet2_2, depth = 3, prob = 0.90, 
                                  pars = c("bANet_p", "bGANet_p", "bANet_pos", "bGANet_pos", 
                                           "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet2_2

###2.2.2 Direct ----
{set.seed(123)
  Model_harmonic_in_GNet2_2_d <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bANet_p[AgeC, Net_domain] +
        bGANet_p[AgeC, Net_domain]*(Gender-1) +
        bIn_p[Inheritance] + bR_p[Residence] + 
        bK_p[Kinship] + bM_p * Market +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale ),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bANet_pos[AgeC[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]] +
        bGANet_pos[AgeC[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]]*(Gender[Harmonic_in_pos_idx]-1) +
        bIn_pos[Inheritance[Harmonic_in_pos_idx]] + bR_pos[Residence[Harmonic_in_pos_idx]] + 
        bK_pos[Kinship[Harmonic_in_pos_idx]] + bM_pos * Market[Harmonic_in_pos_idx] +
        bV_pos[VID[Harmonic_in_pos_idx]] + 
        bI_pos[ID[Harmonic_in_pos_idx]], 
      
      # Priors: hurdle part
      matrix[AgeC, Net_domain]: bANet_p ~ normal(0, 0.3),
      matrix[AgeC, Net_domain]: bGANet_p ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bR_p[Residence] ~ normal(0, 0.3),
      bK_p[Kinship] ~ normal(0, 0.3),
      bM_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      matrix[AgeC, Net_domain]: bANet_pos ~ normal(0, 0.2),
      matrix[AgeC, Net_domain]: bGANet_pos ~ normal(0, 0.2),
      bIn_pos[Inheritance] ~ normal(0, 0.2),
      bR_pos[Residence] ~ normal(0, 0.2),
      bK_pos[Kinship] ~ normal(0, 0.2),
      bM_pos ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet2_2_d <- precis(Model_harmonic_in_GNet2_2_d, depth = 3, prob = 0.90, 
                                    pars = c("bANet_p", "bGANet_p", "bANet_pos", "bGANet_pos", 
                                             "bIn_p", "bIn_pos", "bR_p", "bR_pos", 
                                             "bK_p", "bK_pos", "bM_p", "bM_pos",
                                             "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet2_2_d

##2.3 Triangle count ----
###2.3.1 Total ----
{set.seed(123)
  Model_triangle_GNet2_2 <- ulam(
    alist(
      Triangle ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bANet[AgeC,Net_domain] + 
        bGANet[AgeC,Net_domain]*(Gender-1) +
        bV[VID] + 
        bI[ID], 
      
      phi ~ exponential(1),
      
      matrix[AgeC,Net_domain]: bANet ~ normal(0, 0.3),
      matrix[AgeC,Net_domain]: bGANet ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet2_2 <- precis(Model_triangle_GNet2_2, depth = 3, prob = 0.90, 
                               pars = c("bANet", "bGANet", "V_bar"))
Pre_triangle_GNet2_2

###2.3.2 Direct ----
{set.seed(123)
  Model_triangle_GNet2_2_d <- ulam(
    alist(
      Triangle ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bANet[AgeC,Net_domain] + 
        bGANet[AgeC,Net_domain]*(Gender-1) +
        bIn[Inheritance] + bR[Residence] + 
        bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID], 
      
      phi ~ exponential(1),
      
      matrix[AgeC,Net_domain]: bANet ~ normal(0, 0.3),
      matrix[AgeC,Net_domain]: bGANet ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet2_2_d <- precis(Model_triangle_GNet2_2_d, depth = 3, prob = 0.90, 
                                 pars = c("bANet", "bGANet", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_triangle_GNet2_2_d

##2.4 Community size ----
###2.4.1 Total ----
{set.seed(123)
  Model_commsize_GNet2_2 <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bANet[AgeC,Net_domain] + 
        bGANet[AgeC,Net_domain]*(Gender-1) + 
        bV[VID] +
        bI[ID], 
      
      sigma ~ exponential(1),
      
      matrix[AgeC,Net_domain]: bANet ~ normal(0, 0.3),
      matrix[AgeC,Net_domain]: bGANet ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet2_2 <- precis(Model_commsize_GNet2_2, depth = 3, prob = 0.90, 
                               pars = c("bANet", "bGANet", "V_bar"))
Pre_commsize_GNet2_2

###2.4.2 Direct ----
{set.seed(123)
  Model_commsize_GNet2_2_d <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bANet[AgeC,Net_domain] + 
        bGANet[AgeC,Net_domain]*(Gender-1) + 
        bIn[Inheritance] + bR[Residence] + 
        bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID],
      
      sigma ~ exponential(1),
      
      matrix[AgeC,Net_domain]: bANet ~ normal(0, 0.3),
      matrix[AgeC,Net_domain]: bGANet ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet2_2_d <- precis(Model_commsize_GNet2_2_d, depth = 3, prob = 0.90, 
                                 pars = c("bANet", "bGANet", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_commsize_GNet2_2_d

#3 Gender differences stratified by network domains and residence patterns ----
##3.1 Betweenness ----
###3.1.1 Total ----
{set.seed(123)
  Model_between_GNet2_3 <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bRNet_p[Residence, Net_domain] +
        bGRNet_p[Residence, Net_domain]*(Gender-1) +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bRNet_pos[Residence[Between_pos_idx], Net_domain[Between_pos_idx]] +
        bGRNet_pos[Residence[Between_pos_idx], Net_domain[Between_pos_idx]]*(Gender[Between_pos_idx]-1) +
        bV_pos[VID[Between_pos_idx]] + 
        bI_pos[ID[Between_pos_idx]], 
      
      # Priors: hurdle part
      matrix[Residence, Net_domain]: bRNet_p ~ normal(0, 0.3),
      matrix[Residence, Net_domain]: bGRNet_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      matrix[Residence, Net_domain]: bRNet_pos ~ normal(0, 0.3),
      matrix[Residence, Net_domain]: bGRNet_pos ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet2_3 <- precis(Model_between_GNet2_3, depth = 3, prob = 0.90, 
                              pars = c("bRNet_p", "bGRNet_p", "bRNet_pos", "bGRNet_pos", 
                                       "V_bar_p", "V_bar_pos"))
Pre_between_GNet2_3

###3.1.2 Direct ----
{set.seed(123)
  Model_between_GNet2_3_d <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bRNet_p[Residence, Net_domain] +
        bGRNet_p[Residence, Net_domain]*(Gender-1) +
        bA_p[AgeC] + bIn_p[Inheritance] + 
        bK_p[Kinship] + bM_p * Market +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bRNet_pos[Residence[Between_pos_idx], Net_domain[Between_pos_idx]] +
        bGRNet_pos[Residence[Between_pos_idx], Net_domain[Between_pos_idx]]*(Gender[Between_pos_idx]-1) +
        bA_pos[AgeC[Between_pos_idx]] + bIn_pos[Inheritance[Between_pos_idx]] + 
        bK_pos[Kinship[Between_pos_idx]] + bM_pos * Market[Between_pos_idx] +
        bV_pos[VID[Between_pos_idx]] + 
        bI_pos[ID[Between_pos_idx]], 
      
      # Priors: hurdle part
      matrix[Residence, Net_domain]: bRNet_p ~ normal(0, 0.3),
      matrix[Residence, Net_domain]: bGRNet_p ~ normal(0, 0.3),
      bA_p[AgeC] ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bK_p[Kinship] ~ normal(0, 0.3),
      bM_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      matrix[Residence, Net_domain]: bRNet_pos ~ normal(0, 0.3),
      matrix[Residence, Net_domain]: bGRNet_pos ~ normal(0, 0.3),
      bA_pos[AgeC] ~ normal(0, 0.3),
      bIn_pos[Inheritance] ~ normal(0, 0.3),
      bK_pos[Kinship] ~ normal(0, 0.3),
      bM_pos ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet2_3_d <- precis(Model_between_GNet2_3_d, depth = 3, prob = 0.90, 
                                pars = c("bRNet_p", "bGRNet_p", "bRNet_pos", "bGRNet_pos", 
                                         "bA_p", "bA_pos", "bIn_p", "bIn_pos", 
                                         "bK_p", "bK_pos", "bM_p", "bM_pos",
                                         "V_bar_p", "V_bar_pos"))
Pre_between_GNet2_3_d

##3.2 Inward harmonic centrality ----
###3.2.1 Total ----
{set.seed(123)
  Model_harmonic_in_GNet2_3 <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bRNet_p[Residence, Net_domain] +
        bGRNet_p[Residence, Net_domain]*(Gender-1) +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale ),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bRNet_pos[Residence[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]] +
        bGRNet_pos[Residence[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]]*(Gender[Harmonic_in_pos_idx]-1) +
        bV_pos[VID[Harmonic_in_pos_idx]] + 
        bI_pos[ID[Harmonic_in_pos_idx]], 
      
      # Priors: hurdle part
      matrix[Residence, Net_domain]: bRNet_p ~ normal(0, 0.3),
      matrix[Residence, Net_domain]: bGRNet_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      matrix[Residence, Net_domain]: bRNet_pos ~ normal(0, 0.2),
      matrix[Residence, Net_domain]: bGRNet_pos ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet2_3 <- precis(Model_harmonic_in_GNet2_3, depth = 3, prob = 0.90, 
                                  pars = c("bRNet_p", "bGRNet_p", "bRNet_pos", "bGRNet_pos", 
                                           "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet2_3

###3.2.2 Direct ----
{set.seed(123)
  Model_harmonic_in_GNet2_3_d <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bRNet_p[Residence, Net_domain] +
        bGRNet_p[Residence, Net_domain]*(Gender-1) +
        bA_p[AgeC] + bIn_p[Inheritance] + 
        bK_p[Kinship] + bM_p * Market +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale ),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bRNet_pos[Residence[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]] +
        bGRNet_pos[Residence[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]]*(Gender[Harmonic_in_pos_idx]-1) +
        bA_pos[AgeC[Harmonic_in_pos_idx]] + bIn_pos[Inheritance[Harmonic_in_pos_idx]] + 
        bK_pos[Kinship[Harmonic_in_pos_idx]] + bM_pos * Market[Harmonic_in_pos_idx] +
        bV_pos[VID[Harmonic_in_pos_idx]] +
        bI_pos[ID[Harmonic_in_pos_idx]], 
      
      # Priors: hurdle part
      matrix[Residence, Net_domain]: bRNet_p ~ normal(0, 0.3),
      matrix[Residence, Net_domain]: bGRNet_p ~ normal(0, 0.3),
      bA_p[AgeC] ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bK_p[Kinship] ~ normal(0, 0.3),
      bM_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      matrix[Residence, Net_domain]: bRNet_pos ~ normal(0, 0.2),
      matrix[Residence, Net_domain]: bGRNet_pos ~ normal(0, 0.2),
      bA_pos[AgeC] ~ normal(0, 0.2),
      bIn_pos[Inheritance] ~ normal(0, 0.2),
      bK_pos[Kinship] ~ normal(0, 0.2),
      bM_pos ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet2_3_d <- precis(Model_harmonic_in_GNet2_3_d, depth = 3, prob = 0.90, 
                                    pars = c("bRNet_p", "bGRNet_p", "bRNet_pos", "bGRNet_pos", 
                                             "bA_p", "bA_pos", "bIn_p", "bIn_pos", 
                                             "bK_p", "bK_pos", "bM_p", "bM_pos",
                                             "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet2_3_d

##3.3 Triangle count ----
###3.3.1 Total ----
{set.seed(123)
  Model_triangle_GNet2_3 <- ulam(
    alist(
      Triangle ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bRNet[Residence,Net_domain] + 
        bGRNet[Residence,Net_domain]*(Gender-1) +
        bV[VID] + 
        bI[ID] , 
      
      phi ~ exponential(1),
      
      matrix[Residence,Net_domain]: bRNet ~ normal(0, 0.3),
      matrix[Residence,Net_domain]: bGRNet ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet2_3 <- precis(Model_triangle_GNet2_3, depth = 3, prob = 0.90, 
                               pars = c("bRNet", "bGRNet", "V_bar"))
Pre_triangle_GNet2_3

###3.3.2 Direct ----
{set.seed(123)
  Model_triangle_GNet2_3_d <- ulam(
    alist(
      Triangle ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bRNet[Residence,Net_domain] + 
        bGRNet[Residence,Net_domain]*(Gender-1) +
        bA[AgeC] + bIn[Inheritance] + 
        bK[Kinship] + bM * Market +
        bV[VID] +
        bI[ID] , 
      
      phi ~ exponential(1),
      
      matrix[Residence,Net_domain]: bRNet ~ normal(0, 0.3),
      matrix[Residence,Net_domain]: bGRNet ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet2_3_d <- precis(Model_triangle_GNet2_3_d, depth = 3, prob = 0.90, 
                                 pars = c("bRNet", "bGRNet", "bA", "bIn", "bK", "bM", "V_bar"))
Pre_triangle_GNet2_3_d

##3.4 Community size ----
###3.4.1 Total ----
{set.seed(123)
  Model_commsize_GNet2_3 <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bRNet[Residence,Net_domain] + 
        bGRNet[Residence,Net_domain]*(Gender-1) +
        bV[VID] + 
        bI[ID], 
      
      sigma ~ exponential(1),
      
      matrix[Residence,Net_domain]: bRNet ~ normal(0, 0.3),
      matrix[Residence,Net_domain]: bGRNet ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet2_3 <- precis(Model_commsize_GNet2_3, depth = 3, prob = 0.90, 
                               pars = c("bRNet", "bGRNet", "V_bar"))
Pre_commsize_GNet2_3

###3.4.2 Direct ----
{set.seed(123)
  Model_commsize_GNet2_3_d <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bRNet[Residence,Net_domain] + 
        bGRNet[Residence,Net_domain]*(Gender-1) +
        bA[AgeC] + bIn[Inheritance] + 
        bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID], 
      
      sigma ~ exponential(1),
      
      matrix[Residence,Net_domain]: bRNet ~ normal(0, 0.3),
      matrix[Residence,Net_domain]: bGRNet ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet2_3_d <- precis(Model_commsize_GNet2_3_d, depth = 3, prob = 0.90, 
                                 pars = c("bRNet", "bGRNet", "bA", "bIn", "bK", "bM", "V_bar"))
Pre_commsize_GNet2_3_d

#4 Gender differences stratified by network domains and kinship systems ----
##4.1 Betweenness ----
###4.1.1 Total ----
{set.seed(123)
  Model_between_GNet2_4 <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bKNet_p[Kinship, Net_domain] +
        bGKNet_p[Kinship, Net_domain]*(Gender-1) +
        bV_p[VID] +
        bI_p[ID], 
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bKNet_pos[Kinship[Between_pos_idx], Net_domain[Between_pos_idx]] +
        bGKNet_pos[Kinship[Between_pos_idx], Net_domain[Between_pos_idx]]*(Gender[Between_pos_idx]-1) +
        bV_pos[VID[Between_pos_idx]] +
        bI_pos[ID[Between_pos_idx]],
      
      # Priors: hurdle part
      matrix[Kinship, Net_domain]: bKNet_p ~ normal(0, 0.3),
      matrix[Kinship, Net_domain]: bGKNet_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      matrix[Kinship, Net_domain]: bKNet_pos ~ normal(0, 0.3),
      matrix[Kinship, Net_domain]: bGKNet_pos ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet2_4 <- precis(Model_between_GNet2_4, depth = 3, prob = 0.90, 
                              pars = c("bKNet_p", "bGKNet_p", "bKNet_pos", "bGKNet_pos", 
                                       "V_bar_p", "V_bar_pos"))
Pre_between_GNet2_4

###4.1.2 Direct ----
{set.seed(123)
  Model_between_GNet2_4_d <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bKNet_p[Kinship, Net_domain] +
        bGKNet_p[Kinship, Net_domain]*(Gender-1) +
        bA_p[AgeC] + bIn_p[Inheritance] + 
        bR_p[Residence] + bM_p * Market +
        bV_p[VID] + 
        bI_p[ID],
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bKNet_pos[Kinship[Between_pos_idx], Net_domain[Between_pos_idx]] +
        bGKNet_pos[Kinship[Between_pos_idx], Net_domain[Between_pos_idx]]*(Gender[Between_pos_idx]-1) +
        bA_pos[AgeC[Between_pos_idx]] + bIn_pos[Inheritance[Between_pos_idx]] + 
        bR_pos[Residence[Between_pos_idx]] + bM_pos * Market[Between_pos_idx] +
        bV_pos[VID[Between_pos_idx]] + 
        bI_pos[ID[Between_pos_idx]],
      
      # Priors: hurdle part
      matrix[Kinship, Net_domain]: bKNet_p ~ normal(0, 0.3),
      matrix[Kinship, Net_domain]: bGKNet_p ~ normal(0, 0.3),
      bA_p[AgeC] ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bR_p[Residence] ~ normal(0, 0.3),
      bM_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      matrix[Kinship, Net_domain]: bKNet_pos ~ normal(0, 0.3),
      matrix[Kinship, Net_domain]: bGKNet_pos ~ normal(0, 0.3),
      bA_pos[AgeC] ~ normal(0, 0.3),
      bIn_pos[Inheritance] ~ normal(0, 0.3),
      bR_pos[Residence] ~ normal(0, 0.3),
      bM_pos ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet2_4_d <- precis(Model_between_GNet2_4_d, depth = 3, prob = 0.90, 
                                pars = c("bKNet_p", "bGKNet_p", "bKNet_pos", "bGKNet_pos", 
                                         "bA_p", "bA_pos", "bIn_p", "bIn_pos", 
                                         "bR_p", "bR_pos", "bM_p", "bM_pos",
                                         "V_bar_p", "V_bar_pos"))
Pre_between_GNet2_4_d

##4.2 Inward harmonic centrality ----
###4.2.1 Total ----
{set.seed(123)
  Model_harmonic_in_GNet2_4 <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bKNet_p[Kinship, Net_domain] +
        bGKNet_p[Kinship, Net_domain]*(Gender-1) +
        bV_p[VID] + 
        bI_p[ID],
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale ),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bKNet_pos[Kinship[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]] +
        bGKNet_pos[Kinship[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]]*(Gender[Harmonic_in_pos_idx]-1) +
        bV_pos[VID[Harmonic_in_pos_idx]] + 
        bI_pos[ID[Harmonic_in_pos_idx]],
      
      # Priors: hurdle part
      matrix[Kinship, Net_domain]: bKNet_p ~ normal(0, 0.3),
      matrix[Kinship, Net_domain]: bGKNet_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      matrix[Kinship, Net_domain]: bKNet_pos ~ normal(0, 0.2),
      matrix[Kinship, Net_domain]: bGKNet_pos ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet2_4 <- precis(Model_harmonic_in_GNet2_4, depth = 3, prob = 0.90, 
                                  pars = c("bKNet_p", "bGKNet_p", "bKNet_pos", "bGKNet_pos", 
                                           "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet2_4

###4.2.2 Direct ----
{set.seed(123)
  Model_harmonic_in_GNet2_4_d <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bKNet_p[Kinship, Net_domain] +
        bGKNet_p[Kinship, Net_domain]*(Gender-1) +
        bA_p[AgeC] + bIn_p[Inheritance] + 
        bR_p[Residence] + bM_p * Market +
        bV_p[VID] + 
        bI_p[ID],
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale ),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bKNet_pos[Kinship[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]] +
        bGKNet_pos[Kinship[Harmonic_in_pos_idx], Net_domain[Harmonic_in_pos_idx]]*(Gender[Harmonic_in_pos_idx]-1) +
        bA_pos[AgeC[Harmonic_in_pos_idx]] + bIn_pos[Inheritance[Harmonic_in_pos_idx]] + 
        bR_pos[Residence[Harmonic_in_pos_idx]] + bM_pos * Market[Harmonic_in_pos_idx] +
        bV_pos[VID[Harmonic_in_pos_idx]] +
        bI_pos[ID[Harmonic_in_pos_idx]], 
      
      # Priors: hurdle part
      matrix[Kinship, Net_domain]: bKNet_p ~ normal(0, 0.3),
      matrix[Kinship, Net_domain]: bGKNet_p ~ normal(0, 0.3),
      bA_p[AgeC] ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bR_p[Residence] ~ normal(0, 0.3),
      bM_p ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      matrix[Kinship, Net_domain]: bKNet_pos ~ normal(0, 0.2),
      matrix[Kinship, Net_domain]: bGKNet_pos ~ normal(0, 0.2),
      bA_pos[AgeC] ~ normal(0, 0.2),
      bIn_pos[Inheritance] ~ normal(0, 0.2),
      bR_pos[Residence] ~ normal(0, 0.2),
      bM_pos ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet2_4_d <- precis(Model_harmonic_in_GNet2_4_d, depth = 3, prob = 0.90, 
                                    pars = c("bKNet_p", "bGKNet_p", "bKNet_pos", "bGKNet_pos", 
                                             "bA_p", "bA_pos", "bIn_p", "bIn_pos", 
                                             "bR_p", "bR_pos", "bM_p", "bM_pos",
                                             "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet2_4_d

##4.3 Triangle count ----
###4.3.1 Total ----
{set.seed(123)
  Model_triangle_GNet2_4 <- ulam(
    alist(
      Triangle ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bKNet[Kinship,Net_domain] + 
        bGKNet[Kinship,Net_domain]*(Gender-1) +
        bV[VID] + 
        bI[ID], 
      
      phi ~ exponential(1),
      
      matrix[Kinship,Net_domain]: bKNet ~ normal(0, 0.3),
      matrix[Kinship,Net_domain]: bGKNet ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet2_4 <- precis(Model_triangle_GNet2_4, depth = 3, prob = 0.90, 
                               pars = c("bKNet", "bGKNet", "V_bar"))
Pre_triangle_GNet2_4

###4.3.2 Direct ----
{set.seed(123)
  Model_triangle_GNet2_4_d <- ulam(
    alist(
      Triangle ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bKNet[Kinship,Net_domain] + 
        bGKNet[Kinship,Net_domain]*(Gender-1) +
        bA[AgeC] + bIn[Inheritance] + 
        bR[Residence] + bM * Market +
        bV[VID] +
        bI[ID], 
      
      phi ~ exponential(1),
      
      matrix[Kinship,Net_domain]: bKNet ~ normal(0, 0.3),
      matrix[Kinship,Net_domain]: bGKNet ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet2_4_d <- precis(Model_triangle_GNet2_4_d, depth = 3, prob = 0.90, 
                                 pars = c("bKNet", "bGKNet", "bA", "bIn", "bR", "bM", "V_bar"))
Pre_triangle_GNet2_4_d

##4.4 Community size ----
###4.4.1 Total ----
{set.seed(123)
  Model_commsize_GNet2_4 <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bKNet[Kinship,Net_domain] + 
        bGKNet[Kinship,Net_domain]*(Gender-1) +
        bV[VID] + 
        bI[ID], 
      
      sigma ~ exponential(1),
      
      matrix[Kinship,Net_domain]: bKNet ~ normal(0, 0.3),
      matrix[Kinship,Net_domain]: bGKNet ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet2_4 <- precis(Model_commsize_GNet2_4, depth = 3, prob = 0.90, 
                               pars = c("bKNet", "bGKNet", "V_bar"))
Pre_commsize_GNet2_4

###4.4.2 Direct ----
{set.seed(123)
  Model_commsize_GNet2_4_d <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bKNet[Kinship,Net_domain] + 
        bGKNet[Kinship,Net_domain]*(Gender-1) +
        bA[AgeC] + bIn[Inheritance] + 
        bR[Residence] + bM * Market +
        bV[VID] + 
        bI[ID],
      
      sigma ~ exponential(1),
      
      matrix[Kinship,Net_domain]: bKNet ~ normal(0, 0.3),
      matrix[Kinship,Net_domain]: bGKNet ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet2_4_d <- precis(Model_commsize_GNet2_4_d, depth = 3, prob = 0.90, 
                                 pars = c("bKNet", "bGKNet", "bA", "bIn", "bR", "bM", "V_bar"))
Pre_commsize_GNet2_4_d

#5 How market participation shape gender differences across network domains ----
##5.1 Betweenness ----
###5.1.1 Total ----
{set.seed(123)
  Model_between_GNet2_5 <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_domain] +
        bGNet_p[Net_domain] * (Gender - 1) +
        bMNet_p[Net_domain] * Market +
        bGMNet_p[Net_domain] * (Gender - 1) * Market + 
        bV_p[VID] +
        bI_p[ID], 
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bNet_pos[Net_domain[Between_pos_idx]] +
        bGNet_pos[Net_domain[Between_pos_idx]] * (Gender[Between_pos_idx] - 1) +
        bMNet_pos[Net_domain[Between_pos_idx]] * Market[Between_pos_idx] +
        bGMNet_pos[Net_domain[Between_pos_idx]] * (Gender[Between_pos_idx] - 1) * Market[Between_pos_idx] +
        bV_pos[VID[Between_pos_idx]] + 
        bI_pos[ID[Between_pos_idx]], 
      
      # Priors: hurdle part
      bNet_p[Net_domain] ~ normal(0, 0.3),
      bGNet_p[Net_domain] ~ normal(0, 0.3),
      bMNet_p[Net_domain] ~ normal(0, 0.3),
      bGMNet_p[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      bNet_pos[Net_domain] ~ normal(0, 0.3),
      bGNet_pos[Net_domain] ~ normal(0, 0.3),
      bMNet_pos[Net_domain] ~ normal(0, 0.3),
      bGMNet_pos[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet2_5 <- precis(Model_between_GNet2_5, depth = 3, prob = 0.90, 
                              pars = c("bNet_p", "bGNet_p", "bMNet_p", "bGMNet_p", 
                                       "bNet_pos", "bGNet_pos", "bMNet_pos", "bGMNet_pos", 
                                       "V_bar_p", "V_bar_pos"))
Pre_between_GNet2_5

###5.1.2 Direct ----
{set.seed(123)
  Model_between_GNet2_5_d <- ulam(
    alist(
      # Part 1: whether betweenness > 0
      Between_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_domain] +
        bGNet_p[Net_domain] * (Gender - 1) +
        bMNet_p[Net_domain] * Market +
        bGMNet_p[Net_domain] * (Gender - 1) * Market + 
        bA_p[AgeC] + bIn_p[Inheritance] + 
        bR_p[Residence] + bK_p[Kinship] +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive betweenness only
      Between_pos_log ~ normal(mu_pos, sigma_pos),
      vector[N_between_pos]: mu_pos <- 
        bNet_pos[Net_domain[Between_pos_idx]] +
        bGNet_pos[Net_domain[Between_pos_idx]] * (Gender[Between_pos_idx] - 1) +
        bMNet_pos[Net_domain[Between_pos_idx]] * Market[Between_pos_idx] +
        bGMNet_pos[Net_domain[Between_pos_idx]] * (Gender[Between_pos_idx] - 1) * Market[Between_pos_idx] +
        bA_pos[AgeC[Between_pos_idx]] + bIn_pos[Inheritance[Between_pos_idx]] + 
        bR_pos[Residence[Between_pos_idx]] + bK_pos[Kinship[Between_pos_idx]] +
        bV_pos[VID[Between_pos_idx]] + 
        bI_pos[ID[Between_pos_idx]], 
      
      # Priors: hurdle part
      bNet_p[Net_domain] ~ normal(0, 0.3),
      bGNet_p[Net_domain] ~ normal(0, 0.3),
      bMNet_p[Net_domain] ~ normal(0, 0.3),
      bGMNet_p[Net_domain] ~ normal(0, 0.3),
      bA_p[AgeC] ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bR_p[Residence] ~ normal(0, 0.3),
      bK_p[Kinship] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-1.4, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      sigma_pos ~ exponential(1),
      
      bNet_pos[Net_domain] ~ normal(0, 0.3),
      bGNet_pos[Net_domain] ~ normal(0, 0.3),
      bMNet_pos[Net_domain] ~ normal(0, 0.3),
      bGMNet_pos[Net_domain] ~ normal(0, 0.3),
      bA_pos[AgeC] ~ normal(0, 0.3),
      bIn_pos[Inheritance] ~ normal(0, 0.3),
      bR_pos[Residence] ~ normal(0, 0.3),
      bK_pos[Kinship] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_between_GNet2_5_d <- precis(Model_between_GNet2_5_d, depth = 3, prob = 0.90, 
                                pars = c("bNet_p", "bGNet_p", "bMNet_p", "bGMNet_p", 
                                         "bNet_pos", "bGNet_pos", "bMNet_pos", "bGMNet_pos", 
                                         "bA_p", "bA_pos", "bIn_p", "bIn_pos", 
                                         "bR_p", "bR_pos", "bK_p", "bK_pos",
                                         "V_bar_p", "V_bar_pos"))
Pre_between_GNet2_5_d

##5.2 Inward harmonic centrality ----
###5.2.1 Total ----
{set.seed(123)
  Model_harmonic_in_GNet2_5 <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_domain] +
        bGNet_p[Net_domain] * (Gender - 1) +
        bMNet_p[Net_domain] * Market +
        bGMNet_p[Net_domain] * (Gender - 1) * Market + 
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale ),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bNet_pos[Net_domain[Harmonic_out_pos_idx]] +
        bGNet_pos[Net_domain[Harmonic_out_pos_idx]] * (Gender[Harmonic_out_pos_idx] - 1) +
        bMNet_pos[Net_domain[Harmonic_out_pos_idx]] * Market[Harmonic_out_pos_idx] +
        bGMNet_pos[Net_domain[Harmonic_out_pos_idx]] * (Gender[Harmonic_out_pos_idx] - 1) * Market[Harmonic_out_pos_idx] +
        bV_pos[VID[Harmonic_out_pos_idx]] +
        bI_pos[ID[Harmonic_out_pos_idx]],
      
      # Priors: hurdle part
      bNet_p[Net_domain] ~ normal(0, 0.3),
      bGNet_p[Net_domain] ~ normal(0, 0.3),
      bMNet_p[Net_domain] ~ normal(0, 0.3),
      bGMNet_p[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      bNet_pos[Net_domain] ~ normal(0, 0.2),
      bGNet_pos[Net_domain] ~ normal(0, 0.2),
      bMNet_pos[Net_domain] ~ normal(0, 0.2),
      bGMNet_pos[Net_domain] ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet2_5 <- precis(Model_harmonic_in_GNet2_5, depth = 3, prob = 0.90, 
                                  pars = c("bNet_p", "bGNet_p", "bMNet_p", "bGMNet_p", 
                                           "bNet_pos", "bGNet_pos", "bMNet_pos", "bGMNet_pos", 
                                           "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet2_5

###5.2.2 Direct ----
{set.seed(123)
  Model_harmonic_in_GNet2_5_d <- ulam(
    alist(
      # Part 1: whether harmonic centrality > 0
      Harmonic_in_0 ~ bernoulli(p),
      logit(p) <- bNet_p[Net_domain] +
        bGNet_p[Net_domain] * (Gender - 1) +
        bMNet_p[Net_domain] * Market +
        bGMNet_p[Net_domain] * (Gender - 1) * Market + 
        bA_p[AgeC] + bIn_p[Inheritance] + 
        bR_p[Residence] + bK_p[Kinship] +
        bV_p[VID] + 
        bI_p[ID], 
      
      # Part 2: positive Harmonic centrality only
      Harmonic_in_pos ~ dgamma2(mu_pos, scale ),
      vector[N_harmonic_in_pos]: log(mu_pos) <- 
        bNet_pos[Net_domain_in_pos] +
        bGNet_pos[Net_domain_in_pos] * (Gender_in_pos - 1) +
        bMNet_pos[Net_domain_in_pos] * Market_in_pos +
        bGMNet_pos[Net_domain_in_pos] * (Gender_in_pos - 1) * Market_in_pos +
        bA_pos[AgeC_in_pos] + bIn_pos[Inheritance_in_pos] + 
        bR_pos[Residence_in_pos] + bK_pos[Kinship_in_pos] +
        bV_pos[VID_in_pos] + 
        bI_pos[ID_in_pos], 
      
      # Priors: hurdle part
      bNet_p[Net_domain] ~ normal(0, 0.3),
      bGNet_p[Net_domain] ~ normal(0, 0.3),
      bMNet_p[Net_domain] ~ normal(0, 0.3),
      bGMNet_p[Net_domain] ~ normal(0, 0.3),
      bA_p[AgeC] ~ normal(0, 0.3),
      bIn_p[Inheritance] ~ normal(0, 0.3),
      bR_p[Residence] ~ normal(0, 0.3),
      bK_p[Kinship] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV_p <<- V_bar_p + z_V_p*sigma_V_p,
      transpars> vector[ID]: bI_p <<- z_I_p*sigma_I_p,
      
      V_bar_p ~ normal(-0.5, 0.3),
      z_V_p[VID] ~ normal(0, 1),
      sigma_V_p ~ exponential(1),
      z_I_p[ID] ~ normal(0, 1),
      sigma_I_p ~ exponential(1),
      
      # Priors: positive part
      scale ~ exponential(1),
      
      bNet_pos[Net_domain] ~ normal(0, 0.2),
      bGNet_pos[Net_domain] ~ normal(0, 0.2),
      bMNet_pos[Net_domain] ~ normal(0, 0.2),
      bGMNet_pos[Net_domain] ~ normal(0, 0.2),
      bA_pos[AgeC] ~ normal(0, 0.2),
      bIn_pos[Inheritance] ~ normal(0, 0.2),
      bR_pos[Residence] ~ normal(0, 0.2),
      bK_pos[Kinship] ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV_pos <<- V_bar_pos + z_V_pos*sigma_V_pos,
      transpars> vector[ID]: bI_pos <<- z_I_pos*sigma_I_pos,
      
      V_bar_pos ~ normal(1.2, 0.3),
      z_V_pos[VID] ~ normal(0, 1),
      sigma_V_pos ~ exponential(1),
      z_I_pos[ID] ~ normal(0, 1),
      sigma_I_pos ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_harmonic_in_GNet2_5_d <- precis(Model_harmonic_in_GNet2_5_d, depth = 3, prob = 0.90, 
                                    pars = c("bNet_p", "bGNet_p", "bMNet_p", "bGMNet_p", 
                                             "bNet_pos", "bGNet_pos", "bMNet_pos", "bGMNet_pos", 
                                             "bA_p", "bA_pos", "bIn_p", "bIn_pos", 
                                             "bR_p", "bR_pos", "bK_p", "bK_pos",
                                             "V_bar_p", "V_bar_pos"))
Pre_harmonic_in_GNet2_5_d

##5.3 Triangle count ----
###5.3.1 Total ----
{set.seed(123)
  Model_triangle_GNet2_5 <- ulam(
    alist(
      Triangle ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bNet[Net_domain] +
        bGNet[Net_domain] * (Gender - 1) +
        bMNet[Net_domain] * Market +
        bGMNet[Net_domain] * (Gender - 1) * Market + 
        bV[VID] +
        bI[ID],
      
      phi ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.3),
      bGNet[Net_domain] ~ normal(0, 0.3),
      bMNet[Net_domain] ~ normal(0, 0.3),
      bGMNet[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet2_5 <- precis(Model_triangle_GNet2_5, depth = 3, prob = 0.90, 
                               pars = c("bNet", "bGNet", "bMNet", "bGMNet", "V_bar"))
Pre_triangle_GNet2_5

###5.3.2 Direct ----
{set.seed(123)
  Model_triangle_GNet2_5_d <- ulam(
    alist(
      Triangle ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bNet[Net_domain] +
        bGNet[Net_domain] * (Gender - 1) +
        bMNet[Net_domain] * Market +
        bGMNet[Net_domain] * (Gender - 1) * Market + 
        bA[AgeC] + bIn[Inheritance] + 
        bR[Residence] + bK[Kinship] +
        bV[VID] + 
        bI[ID], 
      
      phi ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.3),
      bGNet[Net_domain] ~ normal(0, 0.3),
      bMNet[Net_domain] ~ normal(0, 0.3),
      bGMNet[Net_domain] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-1.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_triangle_GNet2_5_d <- precis(Model_triangle_GNet2_5_d, depth = 3, prob = 0.90, 
                                 pars = c("bNet", "bGNet", "bMNet", "bGMNet", 
                                          "bA", "bIn", "bR", "bK", "V_bar"))
Pre_triangle_GNet2_5_d

##5.4 Community size ----
###5.4.1 Total ----
{set.seed(123)
  Model_commsize_GNet2_5 <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bNet[Net_domain] +
        bGNet[Net_domain] * (Gender - 1) +
        bMNet[Net_domain] * Market +
        bGMNet[Net_domain] * (Gender - 1) * Market + 
        bV[VID] +
        bI[ID], 
      
      sigma ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.3),
      bGNet[Net_domain] ~ normal(0, 0.3),
      bMNet[Net_domain] ~ normal(0, 0.3),
      bGMNet[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet2_5 <- precis(Model_commsize_GNet2_5, depth = 3, prob = 0.90, 
                               pars = c("bNet", "bGNet", "bMNet", "bGMNet", "V_bar"))
Pre_commsize_GNet2_5

###5.4.2 Direct ----
{set.seed(123)
  Model_commsize_GNet2_5_d <- ulam(
    alist(
      Community_size_log ~ dnorm(mu, sigma),
      mu <- bNet[Net_domain] +
        bGNet[Net_domain] * (Gender - 1) +
        bMNet[Net_domain] * Market +
        bGMNet[Net_domain] * (Gender - 1) * Market + 
        bA[AgeC] + bIn[Inheritance] + 
        bR[Residence] + bK[Kinship] +
        bV[VID] +
        bI[ID], 
      
      sigma ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.3),
      bGNet[Net_domain] ~ normal(0, 0.3),
      bMNet[Net_domain] ~ normal(0, 0.3),
      bGMNet[Net_domain] ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bK[Kinship] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V * sigma_V,
      transpars> vector[ID]: bI <<- z_I * sigma_I,
      
      V_bar ~ normal(2.0, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ), data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }

Pre_commsize_GNet2_5_d <- precis(Model_commsize_GNet2_5_d, depth = 3, prob = 0.90, 
                                 pars = c("bNet", "bGNet", "bMNet", "bGMNet", 
                                          "bA", "bIn", "bR", "bK", "V_bar"))
Pre_commsize_GNet2_5_d
