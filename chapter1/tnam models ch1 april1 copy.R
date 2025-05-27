library(tidyverse)
library(statnet)
library(dplyr)
library(reshape2)
library(tnam)
library(MASS) # glm.nb
library(GLMMadaptive) #negative.binomial()
library(modelsummary)

setwd('/autocorrelation models')
rel_df <- read.csv('lebanese_group_justrelationships.csv')
grpyear_df <- read.csv('diss_groupyear_0108_copy_edited0228.csv')

########################################################################
###                 fixing up the group year dataset                 ###
########################################################################
# need to limit relationship_df to have same years as groupyear_df
# first get rid of NAs from rel data
rel_df[rel_df == "#N/A"] <- NA
rel_df <- rel_df %>%
  filter(!is.na(group2_kld_id))
# create group.year identifier
rel_df$grp1year <- with(rel_df, paste(group1_kld_id, year, sep = '.'))
rel_df$grp2year <- with(rel_df, paste(group2_kld_id, year, sep = '.'))
grpyear_df$grpyear <- with(grpyear_df, paste(group1_kld_id, year, sep = '.'))

# exclude when these cols all == NA in grpyear_df
grpyear_df <- grpyear_df %>%
  filter(!if_all(attack1:idyear, is.na))


# values of rel_df$grp1year that are not in grpyear_df$grpyear
unique_vals1 <- unique(rel_df$grp1year[!rel_df$grp1year %in% grpyear_df$grpyear]) 
length(rel_df$grp1year[!rel_df$grp1year %in% grpyear_df$grpyear]) # checking that right amount is removed
rel_df <- rel_df %>%
  filter(!grp1year %in% unique_vals1)
unique_vals2 <- unique(rel_df$grp2year[!rel_df$grp2year %in% grpyear_df$grpyear])
rel_df <- rel_df %>%
  filter(!grp2year %in% unique_vals2)

# if current year is larger than group end then year-startyear
grpyear_df$dur <- with(grpyear_df, ifelse(year >= yearstart1, 
                                          year - yearstart1 +1, 
                                          NA))

# fill NA goal values for groups where years extend past edtg frame. affects ssnp
grpyear_df$edtg_ercsr[is.na(grpyear_df$edtg_ercsr) & grpyear_df$group1_kld_id=="L27"] <- 1
grpyear_df$edtg_pch[is.na(grpyear_df$edtg_pch) & grpyear_df$group1_kld_id=="L27"] <- 0
grpyear_df$edtg_sq[is.na(grpyear_df$edtg_sq) & grpyear_df$group1_kld_id=="L27"] <- 0
grpyear_df$edtg_tch[is.na(grpyear_df$edtg_tch) & grpyear_df$group1_kld_id=="L27"] <- 0
# change multiple bases from NA to 0 for ansar allah and ramzi nahra martyr org
grpyear_df$edtg_mul_bases[is.na(grpyear_df$edtg_mul_bases) & grpyear_df$group1_kld_id=="L08"] <- 0
grpyear_df$edtg_mul_bases[is.na(grpyear_df$edtg_mul_bases) & grpyear_df$group1_kld_id=="L26"] <- 0
# change jihad to 0 for hamas L16, hezbollah L17, pij l22
grpyear_df$jihad[grpyear_df$group1_kld_id=="L16"] <- 0
grpyear_df$jihad[grpyear_df$group1_kld_id=="L17"] <- 0
grpyear_df$jihad[grpyear_df$group1_kld_id=="L22"] <- 0



# smaller df with only variables i will potentially need
grpyear_small <-
  grpyear_df %>%
  dplyr::select(group1_kld_id, group1_name, year, yearstart1, yearend1, endtype1,
                edtg_endstr, dur, attacks_all, edtg_total_casualties, edtg_total_deaths, 
                suicide_all, edtg_mul_bases, left_all, right_all, nat_all, relig_all, 
                relig_filled, edtg_ercsr, edtg_pch, edtg_sq, edtg_tch, statespons, jihad, 
                govt, civwar_syria)
# duration squared
grpyear_small$dursq <- grpyear_small$dur^2
# turn L16 (hamas) to 0 (agrees w/ edtg)
grpyear_small$relig_filled[grpyear_small$group1_kld_id == 'L16'] <- 0
# changing some NAs
grpyear_small$edtg_tch[grpyear_small$group1_kld_id == 'L08'] <- 0
grpyear_small$edtg_pch[grpyear_small$group1_kld_id == 'L08'] <- 0
grpyear_small$edtg_ercsr[grpyear_small$group1_kld_id == 'L08'] <- 1 
# turn l27 ssnp to 1 for multiple bases 
grpyear_small$edtg_mul_bases[grpyear_small$group1_kld_id == 'L27'] <- 1 
# cell of tripoli 0 for multiple bases, suicide
grpyear_small$edtg_mul_bases[grpyear_small$group1_kld_id == 'L12'] <- 0
grpyear_small$suicide_all[grpyear_small$group1_kld_id == 'L12'] <- 0

# order proper
grpyear_small <- grpyear_small[order(grpyear_small$group1_kld_id),]

grpyear_small$log_atks <- log(grpyear_small$attacks_all + 1) + .0001
grpyear_small$dur_norm <- with(grpyear_small, (dur - mean(dur, na.rm = TRUE))/sd(dur, na.rm=TRUE))
grpyear_small$dursq_norm <- with(grpyear_small, (dursq - mean(dursq, na.rm = TRUE))/sd(dursq, na.rm=TRUE))

#####################################################
###             prepare data for tnam             ###
#####################################################

# first get all the nodal covariates into separate dfs of wide format
cov_list <- list()
columns <- colnames(grpyear_small[, 8:30])
for (column in columns){
  covariate_vector_list <- list()
  
  for (i in 2000:2016) {
    yeardata <- grpyear_small %>%
      #select only years, groups, and that column
      dplyr::select(year, group1_kld_id, !!sym(column)) %>%
      # filter to the year (of the loop)
      dplyr::filter(year == i)
    
    # vector of values for each group in the column (of the loop)
    vecvals <- yeardata[[column]]
    # assign names
    vecvals <- setNames(vecvals, yeardata$group1_kld_id)
    
    # put into covariate_vector_list
    covariate_vector_list[[as.character(paste0('t', i))]] <- vecvals
  }
  # name each list 
  cov_list[[column]] <- covariate_vector_list
}

# get matrices for yearly alliance networks
list_posmat <- list()
for(i in 1:17){
  print(i)
  
  reldf_i <- rel_df %>%
    dplyr::select(group1_kld_id, group2_kld_id, posrel, year) %>%
    filter(year == 2000 + i -1)
  
  reldf_ia <- reldf_i[rep(rownames(reldf_i), each = 2),]
  reldf_ia[c(FALSE, TRUE), 1:2] <- reldf_ia[c(FALSE, TRUE), 2:1]
  
  reldf_i_cast <- acast(reldf_ia, group1_kld_id ~ group2_kld_id, value.var = 'posrel')
  # breaks down bc each rel not double listed
  reldf_i_cast[is.na(reldf_i_cast)] <- 0
  # make diags NA
  #diag(reldf_i_cast) <- NA
  # turn into network
  #pos_nw <- network(reldf_i_cast, directed = FALSE)
  
  
  # create name for the list element
  list_name <- paste0("t", 2000 + i - 1)
  # store into list
  list_posmat[[list_name]] <- reldf_i_cast#pos_nw
}
# turn matrices to networks
for (i in 1:length(list_posmat)) {
  list_posmat[[i]] <- as.network(list_posmat[[i]])
}

# get matrices for yearly rival networks
list_negmat <- list()
for(i in 1:17){
  print(i)
  
  reldf_i <- rel_df %>%
    dplyr::select(group1_kld_id, group2_kld_id, negrel, year) %>%
    filter(year == 2000 + i -1)
  
  reldf_ia <- reldf_i[rep(rownames(reldf_i), each = 2),]
  reldf_ia[c(FALSE, TRUE), 1:2] <- reldf_ia[c(FALSE, TRUE), 2:1]
  
  reldf_i_cast <- acast(reldf_ia, group1_kld_id ~ group2_kld_id, value.var = 'negrel')
  # breaks down bc each rel not double listed
  reldf_i_cast[is.na(reldf_i_cast)] <- 0
  # make diags NA
  #diag(reldf_i_cast) <- NA
  # turn into network
  #pos_nw <- network(reldf_i_cast, directed = FALSE)
  
  
  # create name for the list element
  list_name <- paste0("t", 2000 + i - 1)
  # store into list
  list_negmat[[list_name]] <- reldf_i_cast#pos_nw
}
# turn matrices to networks
for (i in 1:length(list_negmat)) {
  list_negmat[[i]] <- as.network(list_negmat[[i]])
}

#####################################################
###             get tnam dataset               ###
#####################################################
set.seed(9986000)
# need to set a dv for tnam
atks_dv <- cov_list[["attacks_all"]]
atks_log_dv <- cov_list[["log_atks"]]
# all vars that will potentially be used
tnam_df <- tnamdata(
  atks_dv ~
    covariate(cov_list[["relig_all"]], coefname = "religious") +
    covariate(cov_list[["statespons"]], coefname = "statespons") +
    covariate(cov_list[["edtg_mul_bases"]], coefname = "mul_bases") +
    covariate(cov_list[["govt"]], coefname = "gov") +
    covariate(cov_list[["dur"]], coefname = "dur") +
    covariate(cov_list[["jihad"]], coefname = "jihad") +
    centrality(list_posmat, type = "eigenvector", directed = FALSE, coefname = "ally") +
    centrality(list_posmat, type = "outdegree", directed = FALSE, coefname = "ally") +
    degreedummy(list_posmat, type = "outdegree", directed = FALSE, deg = 0, reverse = TRUE, coefname = "ally") +
    netlag(atks_dv, list_posmat, coefname = "ally") +
    netlag(atks_dv, list_posmat, coefname = "ally", lag = 1) +
    netlag(atks_dv, list_posmat, pathdist = 2, coefname = "ally") +
    cliquelag(atks_dv, list_posmat, directed = FALSE, k.min = 3, k.max = Inf, lag = 0, coefname = "ally") +
    clustering(list_posmat, coefname = "ally", directed = FALSE) +
    centrality(list_negmat, type = "outdegree", directed = FALSE, coefname = "rival") +
    netlag(atks_dv, list_negmat, coefname = "rival") +
    netlag(atks_dv, list_negmat, coefname = "rival", lag = 1) +
    netlag(atks_dv, list_negmat, pathdist = 2, coefname = "rival") +
    clustering(list_negmat, coefname = "rival", directed = FALSE) +
    cliquelag(atks_dv, list_negmat, directed = FALSE, k.min = 3, k.max = Inf, lag = 0, coefname = "rival") +
    degreedummy(list_negmat, type = "outdegree", directed = FALSE, deg = 0, reverse = TRUE, coefname = "rival") +
    centrality(list_posmat, type = "eigenvector", directed = FALSE, coefname = "ally", lag = 1) +
    centrality(list_posmat, type = "outdegree", directed = FALSE, coefname = "ally", lag = 1) +
    centrality(list_negmat, type = "outdegree", directed = FALSE, coefname = "rival", lag = 1) +
    covariate(cov_list[["statespons"]], coefname = "statespons", lag = 1))
# standardize evcent, use datawizard for use with modelsummary
# reason is so that it moves a standard deviation
tnam_df <- tnam_df %>%
  mutate(ally_evcent_standard = datawizard::standardize(eigenvector.ally)) 
tnam_df <- tnam_df %>%
  mutate(ally_evcent_standard_lag = datawizard::standardize(eigenvector.ally.lag1)) 


#####################################################
###                  main models                  ###
#####################################################
set.seed(9986000)
# degree centrality, no random effects
fit_deg_noRE <- glm.nb(response ~ outdegree.ally + cliquelag.ally.k.3.3 +
                         netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                         outdegree.rival + cliquelag.rival.k.3.3 +
                         netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                         covariate.religious + covariate.statespons + 
                         covariate.mul_bases + covariate.gov + covariate.dur,
                       data = tnam_df)
summary(fit_deg_noRE)
# checking overdispersion
sum(residuals(fit_deg_noRE, type = "pearson")^2) / fit_deg_noRE$df.residual

# eigenvector centrality, no random effects
fit_ev_noRE <- glm.nb(response ~ ally_evcent_standard + cliquelag.ally.k.3.3 +
                         netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                        outdegree.rival + cliquelag.rival.k.3.3 +
                         netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                         covariate.religious + covariate.statespons + 
                         covariate.mul_bases + covariate.gov + covariate.dur,
                       data = tnam_df)
summary(fit_ev_noRE)

# degree centrality, random effects on node
fit_deg_nodeRE <- mixed_model(response ~ outdegree.ally + cliquelag.ally.k.3.3 +
                                netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                                outdegree.rival + cliquelag.rival.k.3.3 +
                                netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                                covariate.religious + covariate.statespons + 
                                covariate.mul_bases + covariate.gov + covariate.dur,
                              random = ~1|node, 
                              family = GLMMadaptive::negative.binomial(),
                              data = tnam_df)

summary(fit_deg_nodeRE)

# eigenvector centrality, random effects on node
fit_ev_nodeRE <- mixed_model(response ~ ally_evcent_standard + cliquelag.ally.k.3.3 +
                                netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                                outdegree.rival + cliquelag.rival.k.3.3 +
                                netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                                covariate.religious + covariate.statespons + 
                                covariate.mul_bases + covariate.gov + covariate.dur,
                              random = ~1|node, 
                              family = GLMMadaptive::negative.binomial(),
                              data = tnam_df)


# table for latex
modelsummary(list(fit_deg_noRE, fit_ev_noRE, fit_deg_nodeRE, fit_ev_nodeRE), output = "latex",
             stars = c("+" = .1, "*"=.05, "**"=.01, "***"=0.001), exponentiate = TRUE,
             coef_rename = c("outdegree.ally" = "Degree Centrality", 
                             "ally_evcent_standard" = "Eigenvector Centrality",
                             "cliquelag.ally.k.3.3" = "Cliques",
                             "netlag.ally.pathdist1" = "Spatial Lag",
                             "netlag.ally.lag1.pathdist1" = "Spatial Lag with Temporal Lag",
                             "outdegree.rival" = "RDegree Centrality", 
                             "cliquelag.rival.k.3.3" = "RCliques",
                             "netlag.rival.pathdist1" = "RSpatial Lag",
                             "netlag.rival.lag1.pathdist1" = "RSpatial Lag with Temporal Lag",
                             "covariate.religious" = "Religious", 
                             "covariate.statespons" = "State Sponsorship",
                             "covariate.mul_bases" = "Multiple Bases",
                             "covariate.gov" = "Government",
                             "covariate.dur" = "Duration"))

# check that model summary is right
exp(fit_deg_noRE$coefficients)
exp(fit_deg_noRE$coefficients)*summary(fit_deg_noRE)$coefficients[,2]
summary(fit_deg_nodeRE)
exp(0.0642)*0.0952
exp(-0.0174)* 0.0255


#####################################################
###            main models no clique              ###
#####################################################
# i set the seed a lot just in case
set.seed(9986000)
fit_deg_noRE_noclique <- glm.nb(response ~ outdegree.ally + 
                         netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                         outdegree.rival + 
                         netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                         covariate.religious + covariate.statespons + 
                         covariate.mul_bases + covariate.gov + covariate.dur,
                       data = tnam_df)
fit_ev_noRE_noclique <- glm.nb(response ~ ally_evcent_standard + 
                        netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                        outdegree.rival + 
                        netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                        covariate.religious + covariate.statespons + 
                        covariate.mul_bases + covariate.gov + covariate.dur,
                      data = tnam_df)

fit_deg_nodeRE_noclique <- mixed_model(response ~ outdegree.ally + 
                                netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                                outdegree.rival + 
                                netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                                covariate.religious + covariate.statespons + 
                                covariate.mul_bases + covariate.gov + covariate.dur,
                              random = ~1|node, 
                              family = GLMMadaptive::negative.binomial(),
                              data = tnam_df)
fit_ev_nodeRE_noclique <- mixed_model(response ~ ally_evcent_standard + 
                               netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                               outdegree.rival + 
                               netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                               covariate.religious + covariate.statespons + 
                               covariate.mul_bases + covariate.gov + covariate.dur,
                             random = ~1|node, 
                             family = GLMMadaptive::negative.binomial(),
                             data = tnam_df)


# table for latex
modelsummary(list(fit_deg_noRE_noclique, fit_ev_noRE_noclique, fit_deg_nodeRE_noclique, fit_ev_nodeRE_noclique), output = "latex",
             stars = c("+" = .1, "*"=.05, "**"=.01, "***"=0.001), exponentiate = TRUE,
             coef_rename = c("outdegree.ally" = "Degree Centrality", 
                             "ally_evcent_standard" = "Eigenvector Centrality",
                             "netlag.ally.pathdist1" = "Spatial Lag",
                             "netlag.ally.lag1.pathdist1" = "Spatial Lag with Temporal Lag",
                             "outdegree.rival" = "RDegree Centrality", 
                             "netlag.rival.pathdist1" = "RSpatial Lag",
                             "netlag.rival.lag1.pathdist1" = "RSpatial Lag with Temporal Lag",
                             "covariate.religious" = "Religious", 
                             "covariate.statespons" = "State Sponsorship",
                             "covariate.mul_bases" = "Multiple Bases",
                             "covariate.gov" = "Government",
                             "covariate.dur" = "Duration"))

#####################################################
###   main models w/ jihad instead of religious   ###
#####################################################
set.seed(9986000)
fit_deg_noRE_jihad <- glm.nb(response ~ outdegree.ally + cliquelag.ally.k.3.3 +
                         netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                         outdegree.rival + cliquelag.rival.k.3.3 +
                         netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                         covariate.jihad + covariate.statespons + 
                         covariate.mul_bases + covariate.gov + covariate.dur,
                       data = tnam_df)
fit_ev_noRE_jihad <- glm.nb(response ~ ally_evcent_standard + cliquelag.ally.k.3.3 +
                        netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                        outdegree.rival + cliquelag.rival.k.3.3 +
                        netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                        covariate.jihad + covariate.statespons + 
                        covariate.mul_bases + covariate.gov + covariate.dur,
                      data = tnam_df)

fit_deg_nodeRE_jihad <- mixed_model(response ~ outdegree.ally + cliquelag.ally.k.3.3 +
                                netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                                outdegree.rival + cliquelag.rival.k.3.3 +
                                netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                                covariate.jihad + covariate.statespons + 
                                covariate.mul_bases + covariate.gov + covariate.dur,
                              random = ~1|node, 
                              family = GLMMadaptive::negative.binomial(),
                              data = tnam_df)
fit_ev_nodeRE_jihad <- mixed_model(response ~ ally_evcent_standard + cliquelag.ally.k.3.3 +
                               netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                               outdegree.rival + cliquelag.rival.k.3.3 +
                               netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                               covariate.jihad + covariate.statespons + 
                               covariate.mul_bases + covariate.gov + covariate.dur,
                             random = ~1|node, 
                             family = GLMMadaptive::negative.binomial(),
                             data = tnam_df)


# table for latex
modelsummary(list(fit_deg_noRE_jihad, fit_ev_noRE_jihad, fit_deg_nodeRE_jihad, fit_ev_nodeRE_jihad), output = "latex",
             stars = c("+" = .1, "*"=.05, "**"=.01, "***"=0.001), exponentiate = TRUE,
             coef_rename = c("outdegree.ally" = "Degree Centrality", 
                             "ally_evcent_standard" = "Eigenvector Centrality",
                             "cliquelag.ally.k.3.3" = "Cliques",
                             "netlag.ally.pathdist1" = "Spatial Lag",
                             "netlag.ally.lag1.pathdist1" = "Spatial Lag with Temporal Lag",
                             "outdegree.rival" = "RDegree Centrality", 
                             "cliquelag.rival.k.3.3" = "RCliques",
                             "netlag.rival.pathdist1" = "RSpatial Lag",
                             "netlag.rival.lag1.pathdist1" = "RSpatial Lag with Temporal Lag",
                             "covariate.jihad" = "Jihad", 
                             "covariate.statespons" = "State Sponsorship",
                             "covariate.mul_bases" = "Multiple Bases",
                             "covariate.gov" = "Government",
                             "covariate.dur" = "Duration"))



#####################################################
###                  netlag2                  ###
#####################################################
set.seed(9986000)
fit_deg_noRE_netlag2 <- glm.nb(response ~ outdegree.ally + cliquelag.ally.k.3.3 +
                         netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 + netlag.ally.pathdist2.decay0.5 +
                         outdegree.rival + cliquelag.rival.k.3.3 +
                         netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                         covariate.religious + covariate.statespons + 
                         covariate.mul_bases + covariate.gov + covariate.dur,
                       data = tnam_df)
fit_ev_noRE_netlag2 <- glm.nb(response ~ ally_evcent_standard + cliquelag.ally.k.3.3 +
                        netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 + netlag.ally.pathdist2.decay0.5 +
                        outdegree.rival + cliquelag.rival.k.3.3 +
                        netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                        covariate.religious + covariate.statespons + 
                        covariate.mul_bases + covariate.gov + covariate.dur,
                      data = tnam_df)
fit_deg_nodeRE_netlag2 <- mixed_model(response ~ outdegree.ally + cliquelag.ally.k.3.3 +
                                netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 + netlag.ally.pathdist2.decay0.5 +
                                outdegree.rival + cliquelag.rival.k.3.3 +
                                netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 +
                                covariate.religious + covariate.statespons + 
                                covariate.mul_bases + covariate.gov + covariate.dur,
                              random = ~1|node, 
                              family = GLMMadaptive::negative.binomial(),
                              data = tnam_df)
fit_ev_nodeRE_netlag2 <- mixed_model(response ~ ally_evcent_standard + cliquelag.ally.k.3.3 +
                               netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 + netlag.ally.pathdist2.decay0.5 +
                               outdegree.rival + cliquelag.rival.k.3.3 +
                               netlag.rival.pathdist1 + netlag.rival.lag1.pathdist1 + 
                               covariate.religious + covariate.statespons + 
                               covariate.mul_bases + covariate.gov + covariate.dur,
                             random = ~1|node, 
                             family = GLMMadaptive::negative.binomial(),
                             data = tnam_df)

summary(fit_ev_nodeRE_netlag2)
# table for latex
modelsummary(list(fit_deg_noRE_netlag2, fit_ev_noRE_netlag2, fit_deg_nodeRE_netlag2, fit_ev_nodeRE_netlag2), output = "latex",
             stars = c("+" = .1, "*"=.05, "**"=.01, "***"=0.001), exponentiate = TRUE,
             coef_rename = c("outdegree.ally" = "Degree Centrality", 
                             "ally_evcent_standard" = "Eigenvector Centrality",
                             "cliquelag.ally.k.3.3" = "Cliques",
                             "netlag.ally.pathdist1" = "Spatial Lag",
                             "netlag.ally.lag1.pathdist1" = "Spatial Lag with Temporal Lag",
                             "outdegree.rival" = "RDegree Centrality", 
                             "cliquelag.rival.k.3.3" = "RCliques",
                             "netlag.rival.pathdist1" = "RSpatial Lag",
                             "netlag.rival.lag1.pathdist1" = "RSpatial Lag with Temporal Lag",
                             "covariate.religious" = "Religious", 
                             "covariate.statespons" = "State Sponsorship",
                             "covariate.mul_bases" = "Multiple Bases",
                             "covariate.gov" = "Government",
                             "covariate.dur" = "Duration"))



#####################################################
###                  just ally                  ###
#####################################################
set.seed(9986000)
fit_deg_noRE_ally <- glm.nb(response ~ outdegree.ally + cliquelag.ally.k.3.3 +
                         netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                         covariate.religious + covariate.statespons + 
                         covariate.mul_bases + covariate.gov + covariate.dur,
                       data = tnam_df)
fit_ev_noRE_ally <- glm.nb(response ~ ally_evcent_standard + cliquelag.ally.k.3.3 +
                        netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                        covariate.religious + covariate.statespons + 
                        covariate.mul_bases + covariate.gov + covariate.dur,
                      data = tnam_df)
fit_deg_nodeRE_ally <- mixed_model(response ~ outdegree.ally + cliquelag.ally.k.3.3 +
                                netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                                covariate.religious + covariate.statespons + 
                                covariate.mul_bases + covariate.gov + covariate.dur,
                              random = ~1|node, 
                              family = GLMMadaptive::negative.binomial(),
                              data = tnam_df)
fit_ev_nodeRE_ally <- mixed_model(response ~ ally_evcent_standard + cliquelag.ally.k.3.3 +
                               netlag.ally.pathdist1 + netlag.ally.lag1.pathdist1 +
                               covariate.religious + covariate.statespons + 
                               covariate.mul_bases + covariate.gov + covariate.dur,
                             random = ~1|node, 
                             family = GLMMadaptive::negative.binomial(),
                             data = tnam_df)


# table for latex
modelsummary(list(fit_deg_noRE_ally, fit_ev_noRE_ally, fit_deg_nodeRE_ally, fit_ev_nodeRE_ally), output = "latex",
             stars = c("+" = .1, "*"=.05, "**"=.01, "***"=0.001), exponentiate = TRUE,
             coef_rename = c("outdegree.ally" = "Degree Centrality", 
                             "ally_evcent_standard" = "Eigenvector Centrality",
                             "cliquelag.ally.k.3.3" = "Cliques",
                             "netlag.ally.pathdist1" = "Spatial Lag",
                             "netlag.ally.lag1.pathdist1" = "Spatial Lag with Temporal Lag",
                             "covariate.religious" = "Religious", 
                             "covariate.statespons" = "State Sponsorship",
                             "covariate.mul_bases" = "Multiple Bases",
                             "covariate.gov" = "Government",
                             "covariate.dur" = "Duration"))

