library(lubridate)
library(purrr)
library(tidyr)
library(dplyr)

# only need this for torg and ucdp ids
edtg_monadic <- read.csv("/edtg_groups_merge_jul2.csv")
# main df to work with
edtg <- read.csv("/edtg_vertexdata_mergeconcatalias_oct30.csv")

# removing the groups that are ok to remove (Diss data ch2 doc for notes)
edtg <- filter(edtg, !(group1_id %in% c(782, 1037, 1682, 1305, 2074, 1188, 3371,
                                        1454, 191, 1988, 328, 2513, 2643, 348, 2786,
                                        1255, 238, 1902, 133)))


# match ystart yend and y_start y_end
edtg$yend <- edtg$y_end
edtg <- edtg %>% 
  group_by(gid) %>% 
  fill(y_end, .direction = "up")
# assign 2016 to NA end years
edtg$y_end[is.na(edtg$y_end)] <- 2016
# get years interval
edtg$start_ymd <- ymd(sprintf("%d-01-01", edtg$y_start))
edtg$end_ymd <- ymd(sprintf("%d-12-31", edtg$y_end))
edtg$years_int <- interval(edtg$start_ymd, edtg$end_ymd)





# ----------------------------------------------------------------
### this section will get a df of overlapping pairs of groups

# get list of permutations of group1_name
edtg_overlap_df <- do.call(expand.grid, rep(list(unique(edtg$gid)), 2))
# rename columns
edtg_overlap_df <- dplyr::rename(edtg_overlap_df, c(gid1 = Var1, gid2 = Var2))
# remove rows where it's self-self
edtg_overlap_df <- edtg_overlap_df[edtg_overlap_df$gid1 != edtg_overlap_df$gid2, ]
# remove rows where it's the same combination but different permutation
# margin = 1 to sort by row, t to transpose, duplicated for index of duplicated, ! to negate
edtg_overlap_df <- edtg_overlap_df[!duplicated(t(apply(edtg_overlap_df, 1, sort))),]
# assign name back in
edtg_overlap_df$gname1 <- edtg$gname.x[match(edtg_overlap_df$gid1, edtg$gid)]
edtg_overlap_df$gname2 <- edtg$gname.x[match(edtg_overlap_df$gid2, edtg$gid)]
# assign years and intervals back 
edtg_overlap_df$g1_y_start <- edtg$y_start[match(edtg_overlap_df$gid1, edtg$gid)]
edtg_overlap_df$g1_y_end <- edtg$y_end[match(edtg_overlap_df$gid1, edtg$gid)]
edtg_overlap_df$g1_int <- edtg$years_int[match(edtg_overlap_df$gid1, edtg$gid)]
edtg_overlap_df$g2_y_start <- edtg$y_start[match(edtg_overlap_df$gid2, edtg$gid)]
edtg_overlap_df$g2_y_end <- edtg$y_end[match(edtg_overlap_df$gid2, edtg$gid)]
edtg_overlap_df$g2_int <- edtg$years_int[match(edtg_overlap_df$gid2, edtg$gid)]
# assign mgar ids back in
edtg_overlap_df$group1_id <- edtg$group1_id[match(edtg_overlap_df$gid1, edtg$gid)]
edtg_overlap_df$group2_id <- edtg$group1_id[match(edtg_overlap_df$gid2, edtg$gid)]
edtg_overlap_df$group1_id_alt <- edtg$group1_id2[match(edtg_overlap_df$gid1, edtg$gid)]
edtg_overlap_df$group2_id_alt <- edtg$group1_id2[match(edtg_overlap_df$gid2, edtg$gid)]

# put torg and ucdp ids in
edtg_overlap_df$group1_ucdp <- edtg_monadic$ucdp[match(edtg_overlap_df$gid1, edtg_monadic$gid)]
edtg_overlap_df$group1_torg <- edtg_monadic$torg[match(edtg_overlap_df$gid1, edtg_monadic$gid)]
edtg_overlap_df$group2_ucdp <- edtg_monadic$ucdp[match(edtg_overlap_df$gid2, edtg_monadic$gid)]
edtg_overlap_df$group2_torg <- edtg_monadic$torg[match(edtg_overlap_df$gid2, edtg_monadic$gid)]

# see where the intervals overlap
edtg_overlap_df$overlaps <- int_overlaps(edtg_overlap_df$g1_int, edtg_overlap_df$g2_int)
# limit to overlapping groups
edtg_overlap_df <- filter(edtg_overlap_df, overlaps == TRUE)
# get highest start year between the two groups
edtg_overlap_df$overlap_y_start <- with(edtg_overlap_df, ifelse(g2_y_start >= g1_y_start, g2_y_start, g1_y_start))
# get lowest end year between the two groups
edtg_overlap_df$overlap_y_end <- with(edtg_overlap_df, ifelse(g2_y_end <= g1_y_end, g2_y_end, g1_y_end))
# expand to all years w/in start and end per group
edtg_overlap_df <- edtg_overlap_df %>% 
  group_by(gid1, gid2) %>%
  mutate(year = map2(overlap_y_start, overlap_y_end, seq, by = 1)) %>%
  unnest(cols = year)

# ----------------------------------------------------------------
### this section deals with merging in mgar data
mgar <- read.csv("/mgarfinal_edtg_ids_MAIN_OCT30.csv")

# match mgar into edtg_overlap_df
colnames(edtg_overlap_df)
reldf1 <- left_join(edtg_overlap_df, mgar, by = c("group1_id", "group2_id", "year"), na_matches = "never")
# check (mgar doesn't have gname. check id in monadicfinal to make sure it matches)
select(reldf1[!is.na(reldf1$X), ], c(gname1, gname2, group1_id, group2_id))
# get rid of NAs. NAs will remain in reldf2. should = nrow(edtg_overlap_df) at the end
reldf1 <- reldf1[!is.na(reldf1$X), ] 

# match relationships swapped
reldf2 <- left_join(edtg_overlap_df, mgar, 
                    by = c("group1_id" = "group2_id", "group2_id" = "group1_id", "year"), 
                    na_matches = "never")
# check
select(reldf2[!is.na(reldf2$X), ], c(group1_id, group2_id, gname1, gname2))
# remove any of reldf2 that exists in reldf1 
reldf2 <- anti_join(reldf2, reldf1, by = c("group1_id", "group2_id", "year"), na_matches = "never")
# check that nrow of final = nrow of edtg_overlap_df
nrow(reldf1) + nrow(reldf2) 
# concat df
reldf <- rbind(reldf1, reldf2)
# remove some dfs from env
rm(reldf1, reldf2, edtg_overlap_df)
# remove R memory not being used
gc()



# ----------------------------------------------------------------
### this section adds vanilla ally and rival columns based on mgar coding
# create ally and rival cols
#  6 = rivals, 7 = competitors, 8 = unknown.
# allies: coop 
# 1 allies: financial, material, logistical, personnel, surrogate cell
# 2 associates: material, financial, logistical, personnel
# 3 moderate to low assistance - financial, logistical, personnel
# 4 fan - sympathetic, statements of support, need to incorporate other vars to code this one
# 5 host 
which(reldf$reltype == 5 )
# 6 rivals - not actively engaged in violence. need to incorporate other vars
# 7 competitors - actively working to defeat the other
# assign ally rival cols 
reldf <- reldf %>%
  mutate(ally_new = 
           case_when(reltype %in% c(1, 2, 3, 5) ~ 1,
                     finsupport %in% c(1, 2) ~ 1,
                     matsupport %in% c(1, 2) ~ 1,
                     trainsupport %in% c(1, 2) ~ 1,
                     opsupport %in% c(1, 2) ~ 1,
                     territorialsupport %in% c(1, 2) ~ 1,
                     TRUE ~ NA),
         rival_new = 
           case_when(reltype == 7 ~ 1,
                     TRUE ~ NA))
# check. should be 2000-something
nrow(subset(reldf, ally_new==1))
# check col names and get rid of any unnecessary columns
colnames(reldf)
reldf <- select(reldf, !c(X, overlaps, overlap_y_start, overlap_y_end, confidence_rel, g1_int, g2_int, pairs))

# ----------------------------------------------------------------
### this section adds vanilla ally and rival columns based on mgar coding
library(prodlim)
secdf <- read.csv("/mgarfinal_edtg_ids_SECONDARY_oct30.csv")
# viewing to figure out combinations
select(reldf, group1_id, group1_id_alt, group2_id, group2_id_alt, gname1, gname2)

# create support condition in secdf
secdf$support_cond <- ifelse(secdf$reltype %in% c(1, 2, 3, 5) |
                               secdf$finsupport %in% c(1, 2) |
                               secdf$matsupport %in% c(1, 2) |
                               secdf$trainsupport %in% c(1, 2) |
                               secdf$opsupport %in% c(1, 2) |
                               secdf$territorialsupport %in% c(1, 2),
                             1, 0)
# assign backup reldf in case of mistakes. so dont have to read in again
reldf_backup <- reldf
# user function to update the ally variable
# not doing list/tuples because year could be same as id 
update_ally_rival_fn <- function(rel_id1, rel_id2, sec_id1, sec_id2) {
  reldf$dyad_year_id <- ifelse(!is.na(reldf[[rel_id1]]) & !is.na(reldf[[rel_id2]]),
                               paste(reldf[[rel_id1]], reldf[[rel_id2]], reldf$year, sep = "-"),
                               NA)
  secdf$dyad_year_id <- ifelse(!is.na(secdf[[sec_id1]]) & !is.na(secdf[[sec_id2]]),
                               paste(secdf[[sec_id1]], secdf[[sec_id2]], secdf$year, sep = "-"),
                               NA)
  # subset secdf to where support_cond = 1
  secdf_sub <- secdf[secdf$support_cond == 1, ]
  # when ally_new is na and when the dyad-year id is found in the secdf, turn ally_new to 1
  reldf$ally_new[is.na(reldf$ally_new) & reldf$dyad_year_id %in% secdf_sub$dyad_year_id] <- 1
  
  # re-subset secdf to where reltype = 7 (competitors)
  secdf_sub <- secdf[secdf$reltype == 7, ]
  reldf$rival_new[is.na(reldf$rival_new) & reldf$dyad_year_id %in% secdf_sub$dyad_year_id] <- 1
  
  # return the updated dataframe
  return(reldf)
}

# check that it works right before doing for real
print(n = 100,
      update_ally_rival_fn(rel_id1 = "group1_id_alt", sec_id1 = "group1_id", rel_id2 = "group2_id", sec_id2 = "group2_id") %>%
        select(ally_new, group1_id, group1_id_alt, group2_id, year) %>%
        filter(ally_new == 1 & !is.na(group1_id_alt)))
filter(secdf, c(group1_id == 2864 & group2_id == 158)) # no - so check if already was matched
filter(reldf, c(group1_id == 155 & group2_id == 158)) %>% select(group1_id, group2_id, group1_id_alt, ally_new, year) # the 1 is from prev match
filter(secdf, c(group1_id == 1447 & group2_id == 1552)) # yes
filter(secdf, c(group1_id == 1447 & group2_id == 3425)) # yes
filter(reldf, c(group1_id_alt == 1447 & group2_id == 3425)) %>% select(group1_id, group2_id, group1_id_alt, ally_new, year)
print(n = 100,
      update_ally_rival_fn(rel_id1 = "group1_id_alt", sec_id1 = "group1_id", rel_id2 = "group2_id", sec_id2 = "group2_id") %>%
        select(rival_new, group1_id, group1_id_alt, group2_id, year) %>%
        filter(rival_new == 1 & !is.na(group1_id_alt)))
filter(secdf, c(group1_id == 1447 & group2_id == 1554)) # yes
filter(reldf, c(group1_id_alt == 1447 & group2_id == 1554))%>% select(group1_id, group2_id, group1_id_alt, rival_new, year) # checking if it is new 1s

# apply the ally_new update function
# can check after each update to see if changes were made
length(which(reldf$ally_new == 1))
length(which(reldf$rival_new == 1))
# group 1 alt, group 2 main
reldf <- update_ally_rival_fn(rel_id1 = "group1_id_alt", sec_id1 = "group1_id", rel_id2 = "group2_id", sec_id2 = "group2_id")
# group 1 alt, group 2 main, swapped
reldf <- update_ally_rival_fn(rel_id1 = "group1_id_alt", sec_id1 = "group2_id", rel_id2 = "group2_id", sec_id2 = "group1_id")
# group 1 main, group 2 alt
reldf <- update_ally_rival_fn(rel_id1 = "group1_id", sec_id1 = "group1_id", rel_id2 = "group2_id_alt", sec_id2 = "group2_id")
# group 1 main, group 2 alt, swapped
reldf <- update_ally_rival_fn(rel_id1 = "group1_id", sec_id1 = "group2_id", rel_id2 = "group2_id_alt", sec_id2 = "group1_id")
# both alt
#reldf <- update_ally_rival_fn(rel_id1 = "group1_id_alt", sec_id1 = "group1_id", rel_id2 = "group2_id_alt", sec_id2 = "group2_id")
# both alt swap
#reldf <- update_ally_rival_fn(rel_id1 = "group1_id_alt", sec_id1 = "group2_id", rel_id2 = "group2_id_alt", sec_id2 = "group1_id")

# alts did not result in any update so making sure this is correct
secdf_alts <- filter(secdf, c(group1_id %in% c(na.omit(reldf$group1_id_alt), na.omit(reldf$group2_id_alt)) &
                                group2_id %in% c(na.omit(reldf$group1_id_alt), na.omit(reldf$group2_id_alt))))
# yes - reltype is only 0 or 4

# source cols
reldf$source_ally <- ifelse(reldf$ally_new == 1, "mgar", "NA")
reldf$source_rival <- ifelse(reldf$rival_new == 1, "mgar", "NA")

# create index so small (created below) can be merged back into main
reldf$index_id <- seq.int(nrow(reldf))



filename <- paste0("/DATA/reldf_edtgmgarmerge_MGARONLY_", format(Sys.time(), "%m%d%Y-%H%M"), ".csv")
#write.csv(reldf, filename)

colnames(reldf)
reldf_small <- select(reldf, c(gid1:gname2, group1_id:year, reltype, ally_new:index_id))
filename <- paste0("/DATA/reldf_edtgmgarmerge_MGARONLY_RELONLY_", format(Sys.time(), "%m%d%Y%-%H%M"), ".csv")
#write.csv(reldf_small, filename)


# ----------------------------------------------------------------



