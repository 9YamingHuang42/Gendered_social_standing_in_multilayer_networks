# Analyses of indegree and PageRank

#1 Gender differences across network domains/types ----
##1.1 Indegree ----
c( mean = mean(Indiv.indicator.all$Degree.in) ,
   var = var(Indiv.indicator.all$Degree.in) , 
   ratio = var(Indiv.indicator.all$Degree.in)/mean(Indiv.indicator.all$Degree.in)) 
# var>>mean and ratio more than 2 (overdispersion)

###1.1.1 Total ----
# Stratify by network types
{set.seed(123)
  Model_degree_in_GNet1_1 <- ulam(
    alist(
      Degree_in ~ dgampois(lambda, phi) ,
      log(lambda) <- bNet[Net_type] + 
        bGNet[Net_type]*(Gender-1) +
        bV[VID] + 
        bI[ID], 
      
      phi ~ exponential(1),
      
      bNet[Net_type] ~ normal(0, 0.3),
      bGNet[Net_type] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet1_1 <- precis(Model_degree_in_GNet1_1, depth = 3, prob = 0.90, 
                                pars = c("bNet", "bGNet", "V_bar"))
Pre_degree_in_GNet1_1

# Stratify by network domains
{set.seed(123)
  Model_degree_in_GNet2_1 <- ulam(
    alist(
      Degree_in ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bNet[Net_domain] + 
        bGNet[Net_domain]*(Gender-1) +
        bV[VID] +
        bI[ID],
      
      phi ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.3),
      bGNet[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet2_1 <- precis(Model_degree_in_GNet2_1, depth = 3, prob = 0.90, 
                                pars = c("bNet", "bGNet", "V_bar"))
Pre_degree_in_GNet2_1

###1.1.2 Direct ----
# Stratify by network types
{set.seed(123)
  Model_degree_in_GNet1_1_d <- ulam(
    alist(
      Degree_in ~ dgampois(lambda, phi) ,
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
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet1_1_d <- precis(Model_degree_in_GNet1_1_d, depth = 3, prob = 0.90, 
                                  pars = c("bNet", "bGNet", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_degree_in_GNet1_1_d

# Stratify by network domains
{set.seed(123)
  Model_degree_in_GNet2_1_d <- ulam(
    alist(
      Degree_in ~ dgampois( lambda , phi ) ,
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
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet2_1_d <- precis(Model_degree_in_GNet2_1_d, depth = 3, prob = 0.90, 
                                  pars = c("bNet", "bGNet", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_degree_in_GNet2_1_d

##1.2 PageRank ----
###1.2.1 Total ----
# Stratify by network types
{set.seed(123)
  Model_pagerank_GNet1_1 <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bNet[Net_type] + 
        bGNet[Net_type]*(Gender-1) + 
        bV[VID] +
        bI[ID],
      
      scale ~ exponential(1),
      
      bGNet[Net_type] ~ normal(0, 0.2),
      bNet[Net_type] ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet1_1 <- precis(Model_pagerank_GNet1_1, depth = 3, prob = 0.90, 
                               pars = c("bNet", "bGNet", "V_bar"))
Pre_pagerank_GNet1_1

# Stratify by network domains
{set.seed(123)
  Model_pagerank_GNet2_1 <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bNet[Net_domain] + 
        bGNet[Net_domain]*(Gender-1) + 
        bV[VID] + 
        bI[ID], 
      
      scale ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.2),
      bGNet[Net_domain] ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet2_1 <- precis(Model_pagerank_GNet2_1, depth = 3, prob = 0.90, 
                               pars = c("bNet", "bGNet", "V_bar"))
Pre_pagerank_GNet2_1

###1.2.2 Direcct ----
# Stratify by network types
{set.seed(123)
  Model_pagerank_GNet1_1_d <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bNet[Net_type] + 
        bGNet[Net_type]*(Gender-1) + 
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + bM * Market +
        bV[VID] +
        bI[ID], 
      
      scale ~ exponential(1),
      
      bNet[Net_type] ~ normal(0, 0.2),
      bGNet[Net_type] ~ normal(0, 0.2),
      bA[AgeC] ~ normal(0, 0.2),
      bIn[Inheritance] ~ normal(0, 0.2),
      bR[Residence] ~ normal(0, 0.2),
      bK[Kinship] ~ normal(0, 0.2),
      bM ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet1_1_d <- precis(Model_pagerank_GNet1_1_d, depth = 3, prob = 0.90, 
                                 pars = c("bNet", "bGNet", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_pagerank_GNet1_1_d

# Stratify by network domains
{set.seed(123)
  Model_pagerank_GNet2_1_d <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bNet[Net_domain] + 
        bGNet[Net_domain]*(Gender-1) + 
        bA[AgeC] + bIn[Inheritance] +
        bR[Residence] + bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID], 
      
      scale ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.2),
      bGNet[Net_domain] ~ normal(0, 0.2),
      bA[AgeC] ~ normal(0, 0.2),
      bIn[Inheritance] ~ normal(0, 0.2),
      bR[Residence] ~ normal(0, 0.2),
      bK[Kinship] ~ normal(0, 0.2),
      bM ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet2_1_d <- precis(Model_pagerank_GNet2_1_d, depth = 3, prob = 0.90, 
                                 pars = c("bNet", "bGNet", "bA", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_pagerank_GNet2_1_d

#2 Gender differences stratified by network domains and age cohorts ----
##2.1 Indegree ----
###2.1.1 Total ----
{set.seed(123)
  Model_degree_in_GNet2_2 <- ulam(
    alist(
      Degree_in ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bANet[AgeC,Net_domain] + 
        bGANet[AgeC,Net_domain]*(Gender-1) +
        bV[VID] + 
        bI[ID], 
      
      phi ~ exponential(1),
      
      matrix[AgeC,Net_domain]: bANet ~ normal(0, 0.3),
      matrix[AgeC,Net_domain]: bGANet ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet2_2 <- precis(Model_degree_in_GNet2_2, depth = 3, prob = 0.90, 
                                pars = c("bANet", "bGANet", "V_bar"))
Pre_degree_in_GNet2_2

###2.1.2 Direct ----
{set.seed(123)
  Model_degree_in_GNet2_2_d <- ulam(
    alist(
      Degree_in ~ dgampois( lambda , phi ) ,
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
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet2_2_d <- precis(Model_degree_in_GNet2_2_d, depth = 3, prob = 0.90, 
                                  pars = c("bANet", "bGANet", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_degree_in_GNet2_2_d

##2.2 PageRank ----
###2.2.1 Total ----
{set.seed(123)
  Model_pagerank_GNet2_2 <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bANet[AgeC, Net_domain] + 
        bGANet[AgeC, Net_domain]*(Gender-1) + 
        bV[VID] + 
        bI[ID],
      
      scale ~ exponential(1),
      
      matrix[AgeC, Net_domain]: bANet ~ normal(0, 0.2),
      matrix[AgeC, Net_domain]: bGANet ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet2_2 <- precis(Model_pagerank_GNet2_2, depth = 3, prob = 0.90, 
                               pars = c("bANet", "bGANet", "V_bar"))
Pre_pagerank_GNet2_2

###2.2.2 Direct ----
{set.seed(123)
  Model_pagerank_GNet2_2_d <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bANet[AgeC, Net_domain] + 
        bGANet[AgeC, Net_domain]*(Gender-1) + 
        bIn[Inheritance] + bR[Residence] + 
        bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID], 
      
      scale ~ exponential(1),
      
      matrix[AgeC, Net_domain]: bANet ~ normal(0, 0.2),
      matrix[AgeC, Net_domain]: bGANet ~ normal(0, 0.2),
      bIn[Inheritance] ~ normal(0, 0.2),
      bR[Residence] ~ normal(0, 0.2),
      bK[Kinship] ~ normal(0, 0.2),
      bM ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet2_2_d <- precis(Model_pagerank_GNet2_2_d, depth = 3, prob = 0.90, 
                                 pars = c("bANet", "bGANet", "bIn", "bR", "bK", "bM", "V_bar"))
Pre_pagerank_GNet2_2_d

#3 Gender differences stratified by network domains and residence patterns ----
##3.1 Indegree ----
###3.1.1 Total ----
{set.seed(123)
  Model_degree_in_GNet2_3 <- ulam(
    alist(
      Degree_in ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bRNet[Residence,Net_domain] + 
        bGRNet[Residence,Net_domain]*(Gender-1) +
        bV[VID] + 
        bI[ID], 
      
      phi ~ exponential(1),
      
      matrix[Residence,Net_domain]: bRNet ~ normal(0, 0.3),
      matrix[Residence,Net_domain]: bGRNet ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet2_3 <- precis(Model_degree_in_GNet2_3, depth = 3, prob = 0.90, 
                                pars = c("bRNet", "bGRNet", "V_bar"))
Pre_degree_in_GNet2_3

###3.1.2 Direct ----
{set.seed(123)
  Model_degree_in_GNet2_3_d <- ulam(
    alist(
      Degree_in ~ dgampois( lambda , phi ) ,
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
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet2_3_d <- precis(Model_degree_in_GNet2_3_d, depth = 3, prob = 0.90, 
                                  pars = c("bRNet", "bGRNet", "bA", "bIn", "bK", "bM", "V_bar"))
Pre_degree_in_GNet2_3_d

##3.2 PageRank ----
###3.2.1 Total ----
{set.seed(123)
  Model_pagerank_GNet2_3 <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bRNet[Residence, Net_domain] + 
        bGRNet[Residence, Net_domain]*(Gender-1) + 
        bV[VID] + 
        bI[ID], 
      
      scale ~ exponential(1),
      
      matrix[Residence, Net_domain]: bRNet ~ normal(0, 0.2),
      matrix[Residence, Net_domain]: bGRNet ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet2_3 <- precis(Model_pagerank_GNet2_3, depth = 3, prob = 0.90, 
                               pars = c("bRNet", "bGRNet", "V_bar"))
Pre_pagerank_GNet2_3

###3.2.2 Direct ----
{set.seed(123)
  Model_pagerank_GNet2_3_d <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bRNet[Residence, Net_domain] + 
        bGRNet[Residence, Net_domain]*(Gender-1) + 
        bA[AgeC] + bIn[Inheritance] + 
        bK[Kinship] + bM * Market +
        bV[VID] + 
        bI[ID], 
      
      scale ~ exponential(1),
      
      matrix[Residence, Net_domain]: bRNet ~ normal(0, 0.2),
      matrix[Residence, Net_domain]: bGRNet ~ normal(0, 0.2),
      bA[AgeC] ~ normal(0, 0.2),
      bIn[Inheritance] ~ normal(0, 0.2),
      bK[Kinship] ~ normal(0, 0.2),
      bM ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet2_3_d <- precis(Model_pagerank_GNet2_3_d, depth = 3, prob = 0.90, 
                                 pars = c("bRNet", "bGRNet", "bA", "bIn", "bK", "bM", "V_bar"))
Pre_pagerank_GNet2_3_d

#4 Gender differences stratified by network domains and kinship systems ----
##4.1 Indegree ----
###4.1.1 Total ----
{set.seed(123)
  Model_degree_in_GNet2_4 <- ulam(
    alist(
      Degree_in ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bKNet[Kinship,Net_domain] + 
        bGKNet[Kinship,Net_domain]*(Gender-1) +
        bV[VID] +
        bI[ID],
      
      phi ~ exponential(1),
      
      matrix[Kinship,Net_domain]: bKNet ~ normal(0, 0.3),
      matrix[Kinship,Net_domain]: bGKNet ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet2_4 <- precis(Model_degree_in_GNet2_4, depth = 3, prob = 0.90, 
                                pars = c("bKNet", "bGKNet", "V_bar"))
Pre_degree_in_GNet2_4

###4.1.2 Direct ----
{set.seed(123)
  Model_degree_in_GNet2_4_d <- ulam(
    alist(
      Degree_in ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bKNet[Kinship,Net_domain] + 
        bGKNet[Kinship,Net_domain]*(Gender-1) +
        bA[AgeC] + bIn[Inheritance] + 
        bR[Residence] + bM * Market +
        bV[VID] + 
        bI[ID] ,
      
      phi ~ exponential(1),
      
      matrix[Kinship,Net_domain]: bKNet ~ normal(0, 0.3),
      matrix[Kinship,Net_domain]: bGKNet ~ normal(0, 0.3),
      bA[AgeC] ~ normal(0, 0.3),
      bIn[Inheritance] ~ normal(0, 0.3),
      bR[Residence] ~ normal(0, 0.3),
      bM ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet2_4_d <- precis(Model_degree_in_GNet2_4_d, depth = 3, prob = 0.90, 
                                  pars = c("bKNet", "bGKNet", "bA", "bIn", "bR", "bM", "V_bar"))
Pre_degree_in_GNet2_4_d

##4.2 PageRank ----
###4.2.1 Total ----
{set.seed(123)
  Model_pagerank_GNet2_4 <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bKNet[Kinship, Net_domain] + 
        bGKNet[Kinship, Net_domain]*(Gender-1) + 
        bV[VID] + 
        bI[ID],
      
      scale ~ exponential(1),
      
      matrix[Kinship, Net_domain]: bKNet ~ normal(0, 0.2),
      matrix[Kinship, Net_domain]: bGKNet ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet2_4 <- precis(Model_pagerank_GNet2_4, depth = 3, prob = 0.90, 
                               pars = c("bKNet", "bGKNet", "V_bar"))
Pre_pagerank_GNet2_4

###4.2.2 Direct ----
{set.seed(123)
  Model_pagerank_GNet2_4_d <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bKNet[Kinship, Net_domain] + 
        bGKNet[Kinship, Net_domain]*(Gender-1) + 
        bA[AgeC] + bIn[Inheritance] + 
        bR[Residence] + bM * Market +
        bV[VID] + 
        bI[ID],
      
      scale ~ exponential(1),
      
      matrix[Kinship, Net_domain]: bKNet ~ normal(0, 0.2),
      matrix[Kinship, Net_domain]: bGKNet ~ normal(0, 0.2),
      bA[AgeC] ~ normal(0, 0.2),
      bIn[Inheritance] ~ normal(0, 0.2),
      bR[Residence] ~ normal(0, 0.2),
      bM ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet2_4_d <- precis(Model_pagerank_GNet2_4_d, depth = 3, prob = 0.90, 
                                 pars = c("bKNet", "bGKNet", "bA", "bIn", "bR", "bM", "V_bar"))
Pre_pagerank_GNet2_4_d

#5 How market participation shape gender differences across network domains ----
##5.1 Indegree ----
###5.1.1 Total ----
{set.seed(123)
  Model_degree_in_GNet2_5 <- ulam(
    alist(
      Degree_in ~ dgampois( lambda , phi ) ,
      log( lambda ) <- bNet[Net_domain] +
        bGNet[Net_domain] * (Gender - 1) +
        bMNet[Net_domain] * Market +
        bGMNet[Net_domain] * (Gender - 1) * Market + 
        bV[VID] +
        bI[ID] , 
      
      phi ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.3),
      bGNet[Net_domain] ~ normal(0, 0.3),
      bMNet[Net_domain] ~ normal(0, 0.3),
      bGMNet[Net_domain] ~ normal(0, 0.3),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet2_5 <- precis(Model_degree_in_GNet2_5, depth = 3, prob = 0.90, 
                                pars = c("bNet", "bGNet", "bMNet", "bGMNet", "V_bar"))
Pre_degree_in_GNet2_5

###5.1.2 Direct ----
{set.seed(123)
  Model_degree_in_GNet2_5_d <- ulam(
    alist(
      Degree_in ~ dgampois( lambda , phi ) ,
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
      
      V_bar ~ normal(-0.6, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1) 
    ) , data = Model_list , 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_degree_in_GNet2_5_d <- precis(Model_degree_in_GNet2_5_d, depth = 3, prob = 0.90, 
                                  pars = c("bNet", "bGNet", "bMNet", "bGMNet", 
                                           "bA", "bIn", "bR", "bK", "V_bar"))
Pre_degree_in_GNet2_5_d

##5.2 PageRank ----
###5.2.1 Total ----
{set.seed(123)
  Model_pagerank_GNet2_5 <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bNet[Net_domain] +
        bGNet[Net_domain] * (Gender - 1) +
        bMNet[Net_domain] * Market +
        bGMNet[Net_domain] * (Gender - 1) * Market + 
        bV[VID] + 
        bI[ID],
      
      scale ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.2),
      bGNet[Net_domain] ~ normal(0, 0.2),
      bMNet[Net_domain] ~ normal(0, 0.2),
      bGMNet[Net_domain] ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet2_5 <- precis(Model_pagerank_GNet2_5, depth = 3, prob = 0.90, 
                               pars = c("bNet", "bGNet", "bMNet", "bGMNet", "V_bar"))
Pre_pagerank_GNet2_5

###5.2.2 Direct ----
{set.seed(123)
  Model_pagerank_GNet2_5_d <- ulam(
    alist(
      Page_rank ~ dgamma2(mu, scale),
      log(mu) <- bNet[Net_domain] +
        bGNet[Net_domain] * (Gender - 1) +
        bMNet[Net_domain] * Market +
        bGMNet[Net_domain] * (Gender - 1) * Market + 
        bA[AgeC] + bIn[Inheritance] + 
        bR[Residence] + bK[Kinship] +
        bV[VID] + 
        bI[ID], 
      
      scale ~ exponential(1),
      
      bNet[Net_domain] ~ normal(0, 0.2),
      bGNet[Net_domain] ~ normal(0, 0.2),
      bMNet[Net_domain] ~ normal(0, 0.2),
      bGMNet[Net_domain] ~ normal(0, 0.2),
      bA[AgeC] ~ normal(0, 0.2),
      bIn[Inheritance] ~ normal(0, 0.2),
      bR[Residence] ~ normal(0, 0.2),
      bK[Kinship] ~ normal(0, 0.2),
      
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V,
      transpars> vector[ID]: bI <<- z_I*sigma_I,
      
      V_bar ~ normal(-2.5, 0.3),
      z_V[VID] ~ normal(0, 1),
      sigma_V ~ exponential(1),
      z_I[ID] ~ normal(0, 1),
      sigma_I ~ exponential(1)
    ) , data = Model_list, 
    iter = 4500, warmup = 1000, chains = 4, cores = 4, 
    log_lik = TRUE, control = list(adapt_delta = 0.95) 
  ) }
Pre_pagerank_GNet2_5_d <- precis(Model_pagerank_GNet2_5_d, depth = 3, prob = 0.90, 
                                 pars = c("bNet", "bGNet", "bMNet", "bGMNet", 
                                          "bA", "bIn", "bR", "bK", "V_bar"))
Pre_pagerank_GNet2_5_d
