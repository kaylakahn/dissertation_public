library(tidyverse)
library(statnet)
library(dplyr)
library(reshape2)
#library(xergm)
#library(btergm)
library(viridis)

setwd('/autocorrelation models')
rel_df <- read.csv('lebanese_group_justrelationships.csv')
grpyear_df <- read.csv('diss_groupyear_0108_copy_edited0228.csv')

########################################################################
###                preparing data               ###
########################################################################
### the following data preparation is copied from tnam models script
### some of the code may be unnecessary but ultimately need the networks 
### w/ some vertext attributes, which is what it does


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


# smaller df
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
grpyear_small$edtg_ercsr[grpyear_small$group1_kld_id == 'L08'] <-1 

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
for (i in 1:length(list_negmat)) {
  list_negmat[[i]] <- as.network(list_negmat[[i]])
}


library(statnet)
library(ggplot2)
library(ggnetwork)
#detach("package:igraph", unload=TRUE)

# 2016 ally network
dim(list_posmat[[17]])
net2016 <- network(list_posmat[[17]], matrix.type = 'adjacency', directed = F)
df2016 <- grpyear_small[grpyear_small$year == 2016,]
# see names. to know which abbrevs need to be made
df2016$group1_name
# abbreviations for use in node labels
abbreviations <- c("Abdullah Azzam Brigades" = "AAB",
                   "Al-Fatah" = "Fatah",
                   "ANSAR ALLAH" = "Ansar Allah",
                   "AL-SAIQA" = "Saiqa",
                   "Asbat al-Ansar" = "Asbat al-Ansar",
                   "BRIGADE OF AL-MUKHTAR AL-THAQAFI" = "Brig. al-Mukhtar al-Thaqafi",
                   "Democratic Front for the Liberation of Palestine" = "DFLP",
                   "Fatah al-Islam" = "Fatah al-Islam",
                   "Future Movement" = "FM",
                   "HAMAS (ISLAMIC RESISTANCE MOVEMENT)" = "Hamas",
                   "Hizballah" = "Hezbollah",
                   "Islamic Unification Movement" = "IUM",
                   "Jund al-Sham for Tawhid and Jihad" = "Jund al-Sham",
                   "Palestinian Islamic Jihad" = "PIJ",
                   "PFLP-GC" = "PFLP-GC",
                   "Popular Front for the Liberation of Palestine" = "PFLP",
                   "Syrian Social Nationalist Party" = "SSNP",
                   "Takfir wa Hijra" = "TwH")
# match abbreviations to group names
df2016$group1_abbrev <- abbreviations[df2016$group1_name]
# set attributes: abbrev, attack count, centrality measures
# calling packages because network/igraph loads causing issues
network::set.vertex.attribute(net2016, 'abbr', df2016$group1_abbrev)
network::set.vertex.attribute(net2016, 'atks', df2016$attacks_all)
df2016$evc <- evcent(net2016, gmode='graph')
df2016$degc <- degree(net2016, gmode='graph')
df2016$dc_norm <- with(df2016, (degc-min(degc))/(max(degc)-min(degc)))
network::set.vertex.attribute(net2016, 'evc', df2016$evc)
network::set.vertex.attribute(net2016, 'degc', df2016$degc)
network::set.vertex.attribute(net2016, 'dcn', df2016$dc_norm)

# same thing for 2000
dim(list_posmat[[1]])
net2000 <- network(list_posmat[[1]], matrix.type = 'adjacency', directed = F)
df2000 <- grpyear_small[grpyear_small$year == 2000,]
df2000$group1_name
abbr2000 <- c("Abu Nidal Organization" = "ANO", "Al-Fatah" = "Fatah",
              "AL-SADR BRIGADES" = "Sadr Brig.", "AL-SAIQA" = "Saiqa",
              "ANSAR ALLAH" = "Ansar Allah", "Asbat al-Ansar" = "Asbat al-Ansar",
              "Democratic Front for the Liberation of Palestine" = "DFLP",
              "HAMAS (ISLAMIC RESISTANCE MOVEMENT)" = "HAMAS",
              "Hizballah" = "Hezbollah",
              "Islamic Unification Movement" = "IUM",
              "Jama'at al-Nur" = "JN", 
              "Palestinian Islamic Jihad" = "PIJ",
              "PFLP-GC" = "PFLP-GC",
              "Popular Front for the Liberation of Palestine" = "PFLP",
              "Syrian Social Nationalist Party" = "SSNP",
              "Takfir wa Hijra" = "TwH")
df2000$group1_abbrev <- abbr2000[df2000$group1_name]
network::set.vertex.attribute(net2000, 'names2', df2000$group1_name)
network::set.vertex.attribute(net2000, 'abbr', df2000$group1_abbrev)
network::set.vertex.attribute(net2000, 'atks', df2000$attacks_all)
df2000$evc <- evcent(net2000)
df2000$degc <- degree(net2000)
network::set.vertex.attribute(net2000, 'evc', df2000$evc)
network::set.vertex.attribute(net2000, 'degc', df2000$degc)

# arbitrarily doing 2012
list_posmat[[13]]
net2012 <- network(list_posmat[[13]], matrix.type = 'adjacency', directed = F)
df2012 <- grpyear_small[grpyear_small$year == 2012,]
df2012$group1_name
df2012$group1_abbrev <- abbreviations[df2012$group1_name]
network::set.vertex.attribute(net2012, 'abbr', df2012$group1_abbrev)
network::set.vertex.attribute(net2012, 'atks', df2012$attacks_all)
df2012$evc <- evcent(net2012, gmode = 'graph')
df2012$degc <- degree(net2012, gmode = 'graph')
df2012$dc_norm <- with(df2012, (degc-min(degc))/(max(degc)-min(degc)))
network::set.vertex.attribute(net2012, 'evc', df2012$evc)
network::set.vertex.attribute(net2012, 'degc', df2012$degc)
network::set.vertex.attribute(net2012, 'dcn', df2012$dc_norm)


# arbitrarily doing 2005
net2005 <- network(list_posmat[[6]], matrix.type = 'adjacency', directed = F)
df2005 <- grpyear_small[grpyear_small$year == 2005,]
df2005$group1_name
abbr2005 <- c("Al-Fatah" = "Fatah", "AL-SAIQA" = "Saiqa", "ANSAR ALLAH" = "Ansar Allah", 
              "Asbat al-Ansar" = "Asbat al-Ansar",
              "Democratic Front for the Liberation of Palestine" = "DFLP",
              "HAMAS (ISLAMIC RESISTANCE MOVEMENT)" = "HAMAS",
              "Hizballah" = "Hezbollah", "Islamic Unification Movement" = "IUM",
              "Palestinian Islamic Jihad" = "PIJ",
              "PFLP-GC" = "PFLP-GC",
              "Popular Front for the Liberation of Palestine" = "PFLP",
              "Syrian Social Nationalist Party" = "SSNP",
              "Takfir wa Hijra" = "TwH")
df2005$group1_abbrev <- abbr2005[df2005$group1_name]
network::set.vertex.attribute(net2005, 'names2', df2005$group1_name)
network::set.vertex.attribute(net2005, 'abbr', df2005$group1_abbrev)
network::set.vertex.attribute(net2005, 'atks', df2005$attacks_all)
df2005$evc <- evcent(net2005, gmode='graph')
df2005$degc <- degree(net2005, gmode='graph')
df2005$dc_norm <- with(df2005, (degc-min(degc))/(max(degc)-min(degc)))
network::set.vertex.attribute(net2005, 'evc', df2005$evc)
network::set.vertex.attribute(net2005, 'degc', df2005$degc)
network::set.vertex.attribute(net2005, 'dcn', df2005$dc_norm)



# plotting the 2016 network
set.seed(9986000)
# establish object that will use the 2016 network and fr layout
net2016_fr <- ggnetwork(net2016, layout = 'fruchtermanreingold')
# plot, scale node size by eigenvector centrality
ggplot(data = net2016_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey50') + # grey ties 
  geom_nodes(aes(color = atks, size = (evc*100)^3)) + # node colored by attacks, size by ev centrality
  # define the breaks in node size scale
  scale_size_continuous(
    name = "EV\nCentrality",
    breaks = (c(0.1, 0.2, 0.3, 0.4, 0.5) * 100)^3,
    labels = function(x) signif((x)^(1/3) / 100, 2)
  ) +
  # define what the color gradient will be
  scale_color_gradient(name = "Attacks", low = "#414388FF",high = "#7ED7AFFF")+
  # add text to some nodes
  geom_nodetext_repel(aes(label = abbr), size = 2.8) +
  coord_fixed(ratio = 1) +
  # get rid of background grid and axis elements
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# plot 2016 network, scale node size by degree centrality
ggplot(data = net2016_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey50') +
  geom_nodes(aes(color = atks, size = degc)) +
  geom_nodetext_repel(aes(label = abbr), size = 2.8) +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 4), name = "Number\nof Allies") +
  scale_color_continuous(name = "Attacks", low = "#414388FF",high = "#7ED7AFFF") +
  coord_fixed(ratio = 1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
  
# 2012 plots. separate for eigenvector and degree centralities
net2012_fr <- ggnetwork(net2012, layout = 'fruchtermanreingold')
ggplot(data = net2012_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey35') +
  geom_nodes(aes(color = atks, size = (evc*20)^2)) +
  scale_color_continuous(name = "Attacks", 
                         labels = scales::number_format(scale = 1)) +
  scale_size_continuous(breaks = scales::pretty_breaks(4), name = "EV Cent.") +
  geom_nodetext_repel(aes(label = abbr), size = 3) +
  coord_fixed(ratio = 1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggplot(data = net2012_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey35') +
  geom_nodes(aes(color = atks, size = degc^2)) +
  scale_color_continuous(name = "Attacks", 
                         labels = scales::number_format(scale = 1)) +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 4), name = "Deg. Cent.") +
  geom_nodetext_repel(aes(label = abbr), size = 3) +
  coord_fixed(ratio = 1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# 2005 plots. separate for eigenvector and degree centralities
net2005_fr <- ggnetwork(net2005, layout = 'fruchtermanreingold')
ggplot(data = net2005_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey75') +
  geom_nodes(aes(color = atks, size = (evc*20)^2)) +
  geom_nodetext_repel(aes(label = abbr), size = 2.8) +
  scale_size_continuous(breaks = scales::pretty_breaks()((net2005_fr$evc*20)^2), name = "EV Cent.") +
  scale_color_continuous(name = "Attacks", 
                         labels = scales::number_format(scale = 1)) +
  coord_fixed(ratio = 1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

ggplot(data = net2005_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey75') +
  geom_nodes(aes(color = atks, size = degc^2))+
  scale_size_continuous(breaks = scales::pretty_breaks(5)(net2005_fr$degc^2), name = "Deg. Cent.") +
  scale_color_continuous(name = "Attacks", 
                         labels = scales::number_format(scale = 1)) +
  geom_nodetext_repel(aes(label = abbr), size = 2.8) +
  coord_fixed(ratio = 1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# 2000 plots. separate for eigenvector and degree centralities
set.seed(9986000)
net2000_fr <- ggnetwork(net2000, layout = 'fruchtermanreingold')
ggplot(data = net2000_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey50') +
  geom_nodes(aes(color = atks, size = (evc*100)^3)) +
  scale_size_continuous(
    name = "EV\nCentrality",
    breaks = (c(0.1, 0.2, 0.3, 0.4, 0.5) * 100)^3,
    labels = function(x) signif((x)^(1/3) / 100, 2)
  ) +
  scale_color_gradient(name = "Attacks", low = "#414388FF",high = "#7ED7AFFF")+
  geom_nodetext_repel(aes(label = abbr), size = 2.8) +
  coord_fixed(ratio = 1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggplot(data = net2000_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey50') +
  geom_nodes(aes(color = atks, size = degc)) +
  geom_nodetext_repel(aes(label = abbr), size = 2.8) +
  scale_size_continuous(breaks = scales::pretty_breaks(n = 4), name = "Number\nof Allies") +
  scale_color_continuous(name = "Attacks", low = "#414388FF",high = "#7ED7AFFF") +
  coord_fixed(ratio = 1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

#######################################
# rivalry plots
######################################
rival2016 <- network(list_negmat[[17]], matrix.type = 'adjacency', directed = F)
network::set.vertex.attribute(rival2016, 'abbr', df2016$group1_abbrev)
network::set.vertex.attribute(rival2016, 'atks', df2016$attacks_all)
df2016$degc <- degree(rival2016, gmode='graph')
network::set.vertex.attribute(rival2016, 'degc', df2016$degc)

rival2012 <- network(list_negmat[[13]], matrix.type = 'adjacency', directed = F)
network::set.vertex.attribute(rival2012, 'abbr', df2012$group1_abbrev)
network::set.vertex.attribute(rival2012, 'atks', df2012$attacks_all)
df2012$degc <- degree(rival2012, gmode='graph')
network::set.vertex.attribute(rival2012, 'degc', df2012$degc)

rival2005 <- network(list_negmat[[6]], matrix.type = 'adjacency', directed = F)
network::set.vertex.attribute(rival2005, 'abbr', df2005$group1_abbrev)
network::set.vertex.attribute(rival2005, 'atks', df2005$attacks_all)
df2005$degc <- degree(rival2005, gmode='graph')
network::set.vertex.attribute(rival2005, 'degc', df2005$degc)

rival2000 <- network(list_negmat[[1]], matrix.type = 'adjacency', directed = F)
network::set.vertex.attribute(rival2000, 'abbr', df2000$group1_abbrev)
network::set.vertex.attribute(rival2000, 'atks', df2000$attacks_all)
df2000$degc <- degree(rival2000, gmode='graph')
network::set.vertex.attribute(rival2000, 'degc', df2000$degc)

set.seed(9986000)
rival2000_fr <- ggnetwork(rival2000, layout = "circle")
rival2005_fr <- ggnetwork(rival2005, layout = "circle")
rival2012_fr <- ggnetwork(rival2012, layout = "circle")
rival2016_fr <- ggnetwork(rival2016, layout = "circle")

# plotting in one big square of 4 plots. will need to have one full legend 
range(c(rival2000_fr$atks, rival2005_fr$atks, rival2012_fr$atks, rival2016_fr$atks), na.rm = TRUE)
range(c(rival2000_fr$degc, rival2005_fr$degc, rival2012_fr$degc, rival2016_fr$degc), na.rm = TRUE)
attack_color <- scale_color_continuous(low = "#414388FF", high = "#7ED7AFFF", name = "Attacks", limits = c(0,23))
riv_size <- scale_size_continuous(breaks = scales::pretty_breaks(n = 6), name = "Number\nof Rivals", limits = c(0,5))


r2000 <- ggplot(data = rival2000_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey50') +
  geom_nodes(aes(color = atks, size = degc)) +
  geom_nodetext_repel(aes(label = abbr), size = 2.8) +
  attack_color +
  riv_size +
  coord_fixed(ratio = 1) +
  labs(title = "2000") +
  theme(plot.title = element_text(size = 10)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

r2005 <- ggplot(data = rival2005_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey50') +
  geom_nodes(aes(color = atks, size = degc)) +
  geom_nodetext_repel(aes(label = abbr), size = 2.8) +
  attack_color +
  riv_size +
  coord_fixed(ratio = 1) +
  labs(title = "2005") +
  theme(plot.title = element_text(size = 10)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

r2012 <- ggplot(data = rival2012_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey50') +
  geom_nodes(aes(color = atks, size = degc)) +
  geom_nodetext_repel(aes(label = abbr), size = 2.8) +
  attack_color +
  riv_size +
  coord_fixed(ratio = 1) +
  labs(title = "2012") +
  theme(plot.title = element_text(size = 10)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

r2016 <- ggplot(data = rival2016_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey50') +
  geom_nodes(aes(color = atks, size = degc)) +
  geom_nodetext_repel(aes(label = abbr), size = 2.8) +
  attack_color +
  riv_size +
  coord_fixed(ratio = 1) +
  labs(title = "2016") +
  theme(plot.title = element_text(size = 10)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

library(patchwork)
((r2000 + r2005) / (r2012 + r2016)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

