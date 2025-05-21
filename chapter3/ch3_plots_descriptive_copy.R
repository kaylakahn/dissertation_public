library(dplyr)
library(igraph)
library(sna) # load igraph and sna in this order
library(reshape2) # acast
library(stringr) # str_to_title
library(ggnetwork)
library(scales) #used when plotting
library(viridis) # pretty colors!
library(stargazer) # pretty tables!
library(tidyr)
library(patchwork) # arranging plots
library(survival) # making a km plot later
library(xtable) # making a summary stats table

# relationship data
reldf <- read.csv("data ch3/RELDATA_07-Feb-2025 17-57.csv")
reldf <- reldf[, 4:9]
reldf$ally_new[is.na(reldf$ally_new)] <- 0
reldf$gid1[reldf$gid1 == 460] <- 525 # pagad should be 525
reldf$gid2[reldf$gid2 == 460] <- 525
reldf$ally_new[reldf$ally_new == 2] <- 1 # turn 2s to 1s
# make all ids 3 digits
reldf$gid1 <- gsub("^([0-9]{2})$", "0\\1", reldf$gid1)
reldf$gid1 <- gsub("^([0-9])$", "00\\1", reldf$gid1)
reldf$gid2 <- gsub("^([0-9]{2})$", "0\\1", reldf$gid2)
reldf$gid2 <- gsub("^([0-9])$", "00\\1", reldf$gid2)
reldf$gid1 <- as.character(reldf$gid1)
reldf$gid2 <- as.character(reldf$gid2)

# covariate data
gydf <- read.csv("data ch3/ACTORDATA_12-Feb-2025 14.37.csv")
names(gydf)[names(gydf) == 'gname.x'] <- 'gname'
gydf$gid <- as.character(gydf$gid)
# make all ids 3 digits, put df in order by gid
gydf$gid <- gsub("^([0-9]{2})$", "0\\1", gydf$gid)
gydf$gid <- gsub("^([0-9])$", "00\\1", gydf$gid)

# make sure all unique_groups are in gydf$gid and vv
unique_groups <- unique(c(reldf$gid1, reldf$gid2))
unique_groups[!(unique_groups %in% gydf$gid)]
unique(gydf$gid[!gydf$gid %in% unique_groups])

# make a smaller version 
gydf_sml <- gydf %>%
  dplyr::select(gid, gname, year, y_start, y_end, end, EAP:polity, left:right, ercsr, pch:pop,
                gdppercapita, shr_trans, total_atks, total_casualties, endstr, 
                terrctrl, diversity, total_deaths, suicide, duration_new, csum_deaths, 
                csum_atks, vdem_final, islam, regioncat, goals, orientation)
# start and stop
gydf_sml$start <- gydf_sml$duration_new-1
gydf_sml$stop <- gydf_sml$duration_new
# logged deaths
gydf_sml$logdeaths <- log2(gydf_sml$total_deaths + 1)
gydf_sml$logdeathssq <- log2((gydf_sml$total_deaths^2) + 1)






#######################################
# creating networks
######################################
# rel data to adjacency matrices
list_allymats <- list()
for(i in 1970:2016){
  reldf_i <- reldf %>%
    dplyr::select(gid1, gid2, year, ally_new) %>%
    filter(year == i)
  
  reldf_adj <- reldf_i[rep(rownames(reldf_i), each = 2),]
  reldf_adj[c(FALSE, TRUE), 1:2] <- reldf_adj[c(FALSE, TRUE), 2:1]
  
  reldf_i_cast <- acast(reldf_adj, gid1 ~ gid2, value.var = 'ally_new')
  # make diags NA
  diag(reldf_i_cast) <- 0
  
  # create name for the list element
  list_name <- paste0(i, "_adjmat")
  # store into list
  list_allymats[[list_name]] <- reldf_i_cast
}

# 
loop_df <- data.frame()
# Loop through yearly networks and compute measures
for (i in names(list_allymats)) {
  # get network for that year
  yearnet <- graph_from_adjacency_matrix(list_allymats[[i]], mode = "undirected", diag = FALSE)
  # assign node names
  V(yearnet)$name <- rownames(list_allymats[[i]])
  # get nodes
  nodes <- V(yearnet)$name
  # get measures
  ev_cent <- eigen_centrality(yearnet)$vector # ev centrality
  deg_cent <- igraph::degree(yearnet, loops = FALSE) # degree centrality
  trans <- transitivity(yearnet, type = "local", isolates = "zero") # local trans
  neighbsize2 <- neighborhood_size(yearnet, order = 2)
  
  # for spatial lag
  # get gydf$duration_new for the right year
  year <- as.integer(gsub("_adjmat", "", i))
  durationvar <- gydf_sml[gydf_sml$year == year, "duration_new"]
  endvar <- gydf_sml[gydf_sml$year == year, "end"]
  # match nodes make sure right order
  durationvar <- durationvar[match(nodes, gydf_sml[gydf_sml$year == year, "gid"])]
  endvar <- endvar[match(nodes, gydf_sml[gydf_sml$year == year, "gid"])]
  
  # temporary df to bind into loopdf
  i_df <- data.frame(gid = nodes, year = year, ev_cent = ev_cent, deg_cent = deg_cent,
                     trans = trans, neighbsize2 = neighbsize2)
  # bind with main df outside loop
  loop_df <- rbind(loop_df, i_df)
}

# merge loop_df and gydf_sml
terrdf <- merge(gydf_sml, loop_df)




# table with stats




#######################################
# plots
######################################
library(ggnetwork)
library(ggplot2)
library(scales)
library(stringr)

net2016 <- graph_from_adjacency_matrix(list_allymats[["2016_adjmat"]], mode = "undirected", diag = FALSE)
net2016 <- network(list_allymats[["2016_adjmat"]], matrix.type = 'adjacency', directed = F)
# 2016 df
terrdf2016 <- terrdf[terrdf$year == 2016,]
terrdf2016$total_deaths[is.na(terrdf2016$total_deaths)] <- 0
# assign node names
get.vertex.attribute(net2016, 'vertex.names')
set.vertex.attribute(net2016, "gname", terrdf$gname[match(get.vertex.attribute(net2016, "vertex.names"), terrdf$gid)])
set.vertex.attribute(net2016, "total_deaths", terrdf$total_deaths[match(get.vertex.attribute(net2016, "vertex.names"), terrdf$gid)])
set.vertex.attribute(net2016, "degree_cent", degree(net2016, gmode = "graph"))

# vector for labels
# going to label only groups with >= 10 deaths
terrdf2016$label <- ifelse(terrdf2016$total_deaths >= 10, terrdf2016$gname, NA)

# see the names
terrdf2016$label[!is.na(terrdf2016$label)]
terrdf2016$label[!is.na(terrdf2016$label)] <- str_to_title(terrdf2016$label[!is.na(terrdf2016$label)])
# big casewhen for better labels
terrdf2016$label <- case_when(terrdf2016$label == "Abu Sayyaf Group (Asg)" ~ "Abu Sayyaf Group",
                              terrdf2016$label == "Al-Qaida In The Islamic Maghreb (Aqim)" ~ "AQIM",
                              terrdf2016$label == "Ansar Al-Dine" ~ "Ansar al-Dine",
                              terrdf2016$label == "Baloch Liberation Front(Blf)" ~ "Baloch Lib Front",
                              terrdf2016$label == "Baloch Republican Army(Bra)" ~ "Baloch Republican Army",
                              terrdf2016$label == "Barqa Province Of The Islamic State; Fezzan Province Of The Islamic State; Tripoli Province Of The Islamic State" ~ "IS Libya",
                              terrdf2016$label == "Caucasus Province Of The Islamic State" ~ "IS Caucasus",
                              terrdf2016$label == "Communist Party Of India-Maoists (Cpi-Maoist)" ~ "CPI-Maoist",
                              terrdf2016$label == "Democratic Front For The Liberation Of Rwanda (Fdlr)" ~ "FDLR",
                              terrdf2016$label == "Islamic State In Bangladesh" ~ "IS Bangladesh",
                              terrdf2016$label == "Islamic State Of Iraq And The Levant (Isil)" ~ "ISIL",
                              terrdf2016$label == "Khorasan Chapter Of The Islamic State" ~ "IS Khorasan",
                              terrdf2016$label == "Kurdistan Freedom Hawks (Tak)" ~ "TAK",
                              terrdf2016$label == "Kurdistan Workers Party(Pkk)" ~ "PKK",
                              terrdf2016$label == "Lashkar-E-Islam" ~ "Lashkar-e-Islam",
                              terrdf2016$label == "Lashkar-E-Jhangvi" ~ "Lashkar-e-Jhangvi",
                              terrdf2016$label == "Lashkar-E-Taiba (Let)" ~ "Lashkar-e-Taiba",
                              terrdf2016$label == "National Democratic Front Of Bodoland (Bhutan, Burma, India)" ~ "National Dem Front Of Bodoland",
                              terrdf2016$label == "National Liberation Army Of Colombia (Eln)" ~ "ELN",
                              terrdf2016$label == "New People's Army (Npa)" ~ "CPP",
                              terrdf2016$label == "Runda Kumpulan Kecil (Rkk)" ~ "Runda Kumpulan Kecil",
                              terrdf2016$label == "Sinai Province Of The Islamic State" ~ "IS Sinai",
                              terrdf2016$label == "Ta'ang National Liberation Army (Tnla) " ~ "Ta'ang Nat Lib Army",
                              terrdf2016$label == "Tehrik-I-Taliban Pakistan(Ttp)" ~ "TTP",
                              terrdf2016$label == "Al Qaeda In The Arabian Peninsula (Aqap)" ~ "AQAP"
)
# attach the labels to the network object
set.vertex.attribute(net2016, 'labels', terrdf2016$label)

set.seed(9986000)
net2016_fr <- ggnetwork(net2016, layout = "fruchtermanreingold")
# plot for 2016, node size scaled by degree centrality, colored by total deaths
ggplot(data = net2016_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey55') +
  geom_nodes(aes(color = log(total_deaths+1), size = degree_cent)) +
  scale_color_gradient(low = "#EF7F4FFF", high = "#9512A1FF") +
  geom_nodetext(aes(label = labels, size = 2.5)) + 
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(),axis.title = element_blank(),
        axis.line = element_blank())


# uncomment next line to view colors
#"#FEB72DFF", "#EF7F4FFF", "#D14E72FF", "#9512A1FF", "#5901A5FF"

# 1975 network

net1975 <- network(list_allymats[["1975_adjmat"]], matrix.type = 'adjacency', directed = F)
# 1975 df
terrdf1975 <- terrdf[terrdf$year == 1975,]
terrdf1975$total_deaths[is.na(terrdf1975$total_deaths)] <- 0
# assign node names
get.vertex.attribute(net1975, 'vertex.names')
set.vertex.attribute(net1975, "gname", terrdf1975$gname[match(get.vertex.attribute(net1975, "vertex.names"), terrdf1975$gid)])
set.vertex.attribute(net1975, "total_deaths", terrdf1975$total_deaths[match(get.vertex.attribute(net1975, "vertex.names"), terrdf1975$gid)])
set.vertex.attribute(net1975, "degree_cent", degree(net1975, gmode = "graph"))

# vector for labels
terrdf1975$label <- ifelse(terrdf1975$total_deaths >= 1, terrdf1975$gname, NA)

# see the names
terrdf1975$label[!is.na(terrdf1975$label)]
terrdf1975$label[!is.na(terrdf1975$label)] <- str_to_title(terrdf1975$label[!is.na(terrdf1975$label)])
# casewhen for rename
terrdf1975$label <- case_when(terrdf1975$label == "Ulster Volunteer Force (Uvf)" ~ "Ulster Volunteer Force",
                              terrdf1975$label == "Ulster Defence Association/Ulster Freedom Fighters" ~ "Ulster Defence Assoc",
                              terrdf1975$label == "South-West Africa People's Organization (Swapo)" ~ "SWAPO",
                              terrdf1975$label == "Revolutionary Armed Forces Of Colombia (Farc)" ~ "FARC",
                              terrdf1975$label == "Popular Liberation Army (Epl)" ~ "EPL",
                              terrdf1975$label == "Popular Front For The Liberation Of Palestine (Pflp)" ~ "PFLP",
                              terrdf1975$label == "National Liberation Army Of Colombia (Eln)" ~ "ELN",
                              terrdf1975$label == "Mujahideen-I-Khalq" ~ "Mujahideen-i-Khalq",
                              terrdf1975$label == "Montoneros (Argentina)" ~ "Montoneros",
                              terrdf1975$label == "Justice Commandos For The Armenian Genocide" ~ "Justice Commandos for the\nArmenian Genocide",
                              terrdf1975$label == "Irish Republican Army (Ira)" ~ "IRA",
                              terrdf1975$label == "Irish National Liberation Army (Inla)" ~ "INLA",
                              terrdf1975$label == "Guerrilla Army Of The Poor (Egp)" ~ "Guerrilla Army Of The Poor",
                              terrdf1975$label == "First Of October Antifascist Resistance Group (Grapo)" ~ "GRAPO",
                              terrdf1975$label == "Democratic Front For The Liberation Of Palestine" ~ "DFLP",
                              terrdf1975$label == "Basque Fatherland And Freedom (Eta)" ~ "ETA",
                              terrdf1975$label == "23rd Of September Communist League" ~ "23rd of September\nCommunist League",
                              .default = terrdf1975$label
)
set.vertex.attribute(net1975, 'labels', terrdf1975$label)

set.seed(9986000)
net1975_fr <- ggnetwork(net1975, layout = "fruchtermanreingold")
ggplot(data = net1975_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey55') +
  geom_nodes(aes(color = log2(total_deaths+1), size = degree_cent)) +
  scale_size_continuous(name = "Number\nof Allies", labels = label_number(accuracy = 1) ) +
  scale_color_gradient(low = "#EF7F4FFF", high = "#9512A1FF", name = "Fatalities\n(log)") +
  geom_nodetext_repel(aes(label = labels, size = 1.5), box.padding = .1) + 
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(),axis.title = element_blank(),
        axis.line = element_blank())



net2005 <- network(list_allymats[["2005_adjmat"]], matrix.type = 'adjacency', directed = F)
# 2005 df
terrdf2005 <- terrdf[terrdf$year == 2005,]
terrdf2005$total_deaths[is.na(terrdf2005$total_deaths)] <- 0
# assign node names
get.vertex.attribute(net2005, 'vertex.names')
set.vertex.attribute(net2005, "gname", terrdf2005$gname[match(get.vertex.attribute(net2005, "vertex.names"), terrdf2005$gid)])
set.vertex.attribute(net2005, "total_deaths", terrdf2005$total_deaths[match(get.vertex.attribute(net2005, "vertex.names"), terrdf2005$gid)])
set.vertex.attribute(net2005, "degree_cent", degree(net2005, gmode = "graph"))
set.vertex.attribute(net2005, "eigen_cent", evcent(net2005, gmode = "graph"))
set.vertex.attribute(net2005, "neighb2", terrdf2005$neighbsize2[match(get.vertex.attribute(net2005, "vertex.names"), terrdf2005$gid)])


# vector for labels
terrdf2005$label <- ifelse(terrdf2005$total_deaths >= 1, terrdf2005$gname, NA)

# see the names
terrdf2005$label[!is.na(terrdf2005$label)]
terrdf2005$label[!is.na(terrdf2005$label)] <- str_to_title(terrdf2005$label[!is.na(terrdf2005$label)])
# casewhen for names
terrdf2005$label <- case_when(terrdf2005$label == "Abu Sayyaf Group (Asg)" ~ "Abu Sayyaf Group",
                              terrdf2005$label == "Al-Aqsa Martyrs Brigade" ~ "AAMB",
                              terrdf2005$label == "Al-Arifeen" ~ "Al-Arifeen",
                              terrdf2005$label == "Al-Mansoorian" ~ "Al-Mansoorian",
                              terrdf2005$label == "Al-Nasireen Group" ~ "Al-Nasireen Group",
                              terrdf2005$label == "Al-Qaida In Iraq" ~ "Al-Qaida in Iraq",
                              terrdf2005$label == "Ansar Al-Islam" ~ "Ansar al-Islam",
                              terrdf2005$label == "Ansar Al-Sunnah Army" ~ "Ansar al-Sunnah Army",
                              terrdf2005$label == "Arbav Martyrs Of Khuzestan" ~ "Arbav Martyrs\nOf Khuzestan",
                              terrdf2005$label == "Armed Islamic Group (Gia)" ~ "GIA",
                              terrdf2005$label == "Baloch Liberation Army" ~ "Baloch Lib. Army",
                              terrdf2005$label == "Barisan Revolusi Nasional (Brn)" ~ "Barisan Revolusi Nasional",
                              terrdf2005$label == "Communist Party Of India-Maoists (Cpi-Maoist)" ~ "CPI-Maoist",
                              terrdf2005$label == "Dagestani Shari'ah Jamaat" ~ "Dagestani Shari'ah Jamaat",
                              terrdf2005$label == "Etnocacerista Movement" ~ "Etnocacerista Movement",
                              terrdf2005$label == "Hamas" ~ "Hamas",
                              terrdf2005$label == "Harkatul Jihad-E-Islami" ~ "Harkatul Jihad-e-Islami",
                              terrdf2005$label == "Hizbul Mujahideen (Hm)" ~ "Hizbul Mujahideen",
                              terrdf2005$label == "Jaish-E-Mohammad (Jem)" ~ "JeM",
                              terrdf2005$label == "Jama'atul Mujahideen Bangladesh (Jmb)" ~ "Jama'atul Mujahideen\nBangladesh",
                              terrdf2005$label == "Jemaah Islamiya (Ji)" ~ "JI",
                              terrdf2005$label == "Kach" ~ "Kach",
                              terrdf2005$label == "Karen National Union" ~ "Karen Natl Union",
                              terrdf2005$label == "Kurdistan Workers Party(Pkk)" ~ "PKK",
                              terrdf2005$label == "Lashkar-E-Jhangvi" ~ "Lashkar-e-Jhangvi",
                              terrdf2005$label == "Lashkar-E-Taiba (Let)" ~ "LeT",
                              terrdf2005$label == "Liberation Tigers Of Tamil Eelam (Ltte)" ~ "LTTE",
                              terrdf2005$label == "Lord's Resistance Army (Lra)" ~ "Lord's Resistance Army",
                              terrdf2005$label == "National Liberation Army Of Colombia (Eln)" ~ "ELN",
                              terrdf2005$label == "New People's Army (Npa)" ~ "CPP/NPA",
                              terrdf2005$label == "Party For The Liberation Of The Hutu People (Palipehutu)" ~ "Palipehutu",
                              terrdf2005$label == "Patriotic Resistance Front In Ituri (Frpi)" ~ "Patriotic Resistance\nFront in Ituri",
                              terrdf2005$label == "Pattani United Liberation Organization (Pulo)" ~ "PULO",
                              terrdf2005$label == "Popular Liberation Army (Epl)" ~ "EPL",
                              terrdf2005$label == "Popular Resistance Committees" ~ "PRC",
                              terrdf2005$label == "Purbo Banglar Communist Party (Pbcp)" ~ "PBCP",
                              terrdf2005$label == "Revolutionary Armed Forces Of Colombia (Farc)" ~ "FARC",
                              terrdf2005$label == "Riyad Us-Saliheyn Martyrs' Brigade" ~ "Riyadus Saliheyn\nMartyrs' Brigade",
                              terrdf2005$label == "Salafist Group For Preaching And Fighting (Gspc)" ~ "Salafist Group for\nPreaching and Fighting",
                              terrdf2005$label == "Sipah-E-Sahaba Pakistan" ~ "SSP",
                              terrdf2005$label == "Taliban" ~ "Taliban",
                              terrdf2005$label == "Ulster Volunteer Force (Uvf)" ~ "UVF",
                              terrdf2005$label == "United Jihad Council" ~ "United Jihad Council",
                              terrdf2005$label == "United Liberation Front Of Assam (Ulfa)" ~ "ULFA",
                              terrdf2005$label == "Al-Qaeda" ~ "Al-Qaeda",
                              .default = terrdf2005$label)


    
set.vertex.attribute(net2005, 'labels', terrdf2005$label)

set.seed(9986000)
net2005_fr <- ggnetwork(net2005, layout = "fruchtermanreingold")
# 2005 scale by degree
ggplot(data = net2005_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey55') +
  geom_nodes(aes(color = log2(total_deaths+1), size = degree_cent)) +
  scale_size_continuous(name = "Number\nof Allies", labels = label_number(accuracy = 1) ) +
  scale_color_gradient(low = "#EF7F4FFF", high = "#9512A1FF", name = "Fatalities\n(log)") +
  geom_nodetext_repel(aes(label = labels, size = 1.5), box.padding = .1) + 
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(),axis.title = element_blank(),
        axis.line = element_blank())

# 2005 scale by eigen
set.seed(9986000)
ggplot(data = net2005_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey55') +
  geom_nodes(aes(color = log2(total_deaths+1), size = eigen_cent*9)) +
  scale_size_continuous(name = "Eigenvector\nCentrality", labels = label_number(accuracy = 1) ) +
  scale_color_gradient(low = "#EF7F4FFF", high = "#9512A1FF", name = "Fatalities\n(log)") +
  geom_nodetext_repel(aes(label = labels, size = .3), box.padding = .1) + 
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(),axis.title = element_blank(),
        axis.line = element_blank())

# scale by neighborhood order 2
set.seed(9986000)
ggplot(data = net2005_fr, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color = 'grey55') +
  geom_nodes(aes(color = log2(total_deaths+1), size = neighb2)) +
  scale_size_continuous(name = "Neighborhood\nOrder 2", labels = label_number(accuracy = 1) ) +
  scale_color_gradient(low = "#EF7F4FFF", high = "#9512A1FF", name = "Fatalities\n(log)") +
  geom_nodetext_repel(aes(label = labels, size = 4), box.padding = .1) + 
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(),axis.title = element_blank(),
        axis.line = element_blank())



# PLOT for neighborhood order 2 of taliban gid 669
# neighborhood 1 and 2
library(intergraph)
net2005_igraph <- asIgraph(net2005)
V(net2005_igraph)$name <- get.vertex.attribute(net2005, "vertex.names")
# getting the order 1 nodes NOT including taliban node
order1 <- setdiff(unlist(ego(net2005_igraph, order = 1, nodes = "669", mode = "all")), 669)
# getting order 2 nodes NOT including order 1 or taliban nodes
order2 <- setdiff(unlist(ego(net2005_igraph, order = 2, nodes = "669", mode = "all")), c(669, order1))

# initialize vector for neighborhoods and assign different levels of neighborhood
taliban_neighborhoods <- rep("other", network.size(net2005))
# which id is taliban gid 669
which(get.vertex.attribute(net2005, "vertex.names") == "669") # 207
# work from outside inward to avoid overwriting
taliban_neighborhoods[order2] <- "order2"
taliban_neighborhoods[order1] <- "order1"
taliban_neighborhoods[207] <- "ego"

# assign neighborhood as attribute
set.vertex.attribute(net2005, "taliban_neighborhoods", taliban_neighborhoods)

# plot
set.seed(9986000)
# redo fr object now w/ new vtx attribute
net2005_fr <- ggnetwork(net2005, layout = "fruchtermanreingold")
ggplot(net2005_fr, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "grey55") +
  geom_nodes(aes(color = taliban_neighborhoods, size = taliban_neighborhoods)) +
  scale_color_manual(name = "Neighborhood",
    values = c("ego" = "#FDC926FF", "order1" = "#E76E5BFF", "order2" = "#A72197FF","other" = "#44039EFF")) +
  scale_size_manual(
    values = c("ego" = 4, "order1" = 3, "order2" = 2, "other" = 1)) +
  geom_nodetext_repel(aes(label = labels), size = 2.5, box.padding = .1) + 
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(),axis.title = element_blank(),
        axis.line = element_blank()) +
          guides(size = "none")







# plot lethality and degree distribution
terrdf_long <- terrdf %>%
  mutate(log2_total_deaths = log2(total_deaths + 1)) %>%
  pivot_longer(cols = c(log2_total_deaths, deg_cent), 
               names_to = "variable", values_to = "value")
# plot next to each other
ggplot(terrdf_long, aes(x = value, fill = variable, color = variable)) +
  geom_histogram(binwidth = 1, alpha = 0.6) +
  scale_fill_manual(values = c("#5901A5FF", "#9512A1FF")) +
  scale_color_manual(values = c("#5901A5FF", "#9512A1FF")) +
  facet_wrap(~variable, scales = "free", 
             labeller = labeller(variable = c(
               "log2_total_deaths" = "Logged Fatalities Distribution",
               "deg_cent" = "Alliance Count Distribution"
             ))) +
  labs(x = "Logged Value", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 12))

# plot alliance count dist and logged fatalities
# eliminating 0s to get a closer look at other values
terrdf_long <- terrdf %>%
  mutate(log2_total_deaths = log2(total_deaths + 1)) %>%
  pivot_longer(cols = c(log2_total_deaths, deg_cent), 
               names_to = "variable", values_to = "value") %>%
  mutate(value = ifelse(value == 0, NA, value)) 

ggplot(terrdf_long, aes(x = value, fill = variable, color = variable)) +
  geom_histogram(binwidth = 1, alpha = 0.6) +
  scale_fill_manual(values = c("#5901A5FF", "#9512A1FF")) +
  scale_color_manual(values = c("#5901A5FF", "#9512A1FF")) +
  facet_wrap(~variable, scales = "free", 
             labeller = labeller(variable = c(
               "log2_total_deaths" = "Logged Fatalities Distribution",
               "deg_cent" = "Alliance Count Distribution"
             ))) +
  labs(x = "Logged Value", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 12))





# Calculate the maximum duration per group (without considering year)
max_duration_df <- terrdf %>%
  group_by(gid) %>%
  summarise(max_duration = max(duration_new, na.rm = TRUE))

# Plot the maximum duration distribution
ggplot(max_duration_df, aes(x = max_duration)) +
  geom_histogram(binwidth = 1, alpha = 0.6, fill = "#EF7F4FFF", color = "#EF7F4FFF") +
  labs(x = "Maximum Duration", y = "Count", title = "Maximum Duration Per Group") +
  theme_minimal()



# plot the groups at risk each year
riskset_df <- terrdf %>%
  group_by(year) %>%
  mutate(riskset_alive = n_distinct(gid)) %>%
  slice(1) %>% # take first row of each group
  ungroup()

ggplot(riskset_df, aes(x = year, y = riskset_alive)) +
  geom_col(fill = "#9512A1FF") +
  labs(x = "Year", y = "Number of Groups at Risk", title = "Groups at Risk of Ending") +
  theme_minimal()

# plot groups that end each year
groupsdied_df <- terrdf %>%
  filter(end == 1) %>%
  group_by(year) %>%
  mutate(n_died = n_distinct(gid)) %>%
  slice(1) %>%
  ungroup()


# Kaplan-Meier:
plot(Surv(terrdf$start, terrdf$stop, terrdf$end) ~ 1, mark.time=FALSE, 
     lwd=c(2,1,1), col = "#D8576BFF",
     xlab="Time (Years)", ylab="Survival Probability")


# get data proper format for summary stats table
# redo orientation and goals
terrdf$orientation_real <- with(
  terrdf, ifelse(
    rel == 1, "rel", ifelse(
      nat == 1, "nat", ifelse(
        right == 1, "right", ifelse(
          left == 1, "left", NA
        )))))
terrdf$goals_real <- with(
  terrdf, ifelse(
    pch == 1, "pch", ifelse(
      tch == 1, "tch", ifelse(
        sq == 1, "sq", ifelse(
          ercsr == 1, "ercsr", NA
        )))))
  
# check
terrdf %>% dplyr::select(rel, nat, left, right, orientation_real)
terrdf %>% dplyr::select(pch, tch, sq, ercsr, goals_real)

# min 25 50 75 max
contvars <- terrdf %>%
  mutate(logpop = log2(pop),
         loggdppc = log2(gdppercapita),
         evcent100 = ev_cent*100) %>%
  dplyr::select(total_deaths,logdeaths, deg_cent, evcent100, neighbsize2, trans, diversity, 
                shr_trans, logpop, loggdppc, vdem_final)
binvars <- terrdf %>%
  dplyr::select(mul_bases, EAP, ECA, LAC, MENA, NA., SAS, SSA)

# creating tables. here getting 3 latex tables and finagling them into 1 in latex
xtable(t(round(sapply(contdf, function(x) {
  c(min = min(x, na.rm = TRUE),
    Q25 = quantile(x, 0.25, na.rm = TRUE),
    med = median(x, na.rm = TRUE),
    Q25 = quantile(x, 0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE))}), 2)))

xtable(t(sapply(binvars, function(x) {
  prop.table(table(x))}
  )))

xtable(round(100*prop.table(table(terrdf$orientation_real)),2))
xtable(round(100*prop.table(table(terrdf$goals_real)),2))







