library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)

# load lebanon cross sectional df
lebanon_df_groups <- read.csv("Lebanon_groups_kahn_monadic_042823.csv")


#----------------------------------
### this section turns the start and end year to a datetime in order to get an interval

# turn to datetime
lebanon_df_groups$start_ymd <- ymd(sprintf("%d-01-01", lebanon_df_groups$yearstart1))
lebanon_df_groups$end_ymd <- ymd(sprintf("%d-12-31", lebanon_df_groups$yearend1))
# get interval
lebanon_df_groups$years_int <- interval(lebanon_df_groups$start_ymd, lebanon_df_groups$end_ymd)
# ----------------------------------------------------------------
### this section will make a list of the permutations of n groups = 2 
### and limit to combinations
### and check which group combinations overlap in years

# get list of permutations of group1_name
group_overlap_df <- do.call(expand.grid, rep(list(lebanon_df_groups$group1_name), 2))

# remove where same combination but different permutation
# sort by row using apply with MARGIN=1, transpose (t) the output, 
# use duplicated to get the logical index of duplicate rows, 
# negate (!) to get the rows that are not duplicated, and subset the dataset.
group_overlap_df <- group_overlap_df[!duplicated(t(apply(group_overlap_df, 1, sort))),]


# rename df columns
group_overlap_df <- group_overlap_df %>%
                      rename(
                        group1 = Var1,
                        group2 = Var2
                      )
# assign yearstart1, yearend1, and years_int into overlap df
group_overlap_df$g1startyear <- lebanon_df_groups$yearstart1[match(group_overlap_df$group1, lebanon_df_groups$group1_name)]
group_overlap_df$g1endyear <- lebanon_df_groups$yearend1[match(group_overlap_df$group1, lebanon_df_groups$group1_name)]
group_overlap_df$g1_int <- lebanon_df_groups$years_int[match(group_overlap_df$group1, lebanon_df_groups$group1_name)]
group_overlap_df$g2startyear <- lebanon_df_groups$yearstart1[match(group_overlap_df$group2, lebanon_df_groups$group1_name)]
group_overlap_df$g2endyear <- lebanon_df_groups$yearend1[match(group_overlap_df$group2, lebanon_df_groups$group1_name)]
group_overlap_df$g2_int <- lebanon_df_groups$years_int[match(group_overlap_df$group2, lebanon_df_groups$group1_name)]

# see where the intervals overlap
group_overlap_df$overlaps <- int_overlaps(group_overlap_df$g1_int, group_overlap_df$g2_int)
# remove self overlaps and limit to only the overlapping groups
group_overlap_df <- group_overlap_df %>%
                      filter(!(group1==group2)) %>%
                      # limit to just overlapping groups
                      filter(overlaps==TRUE)
# overlap start and end
# if g2 start year is highest, use g2 start year, otherwise use g1 start year (use highest start year)
group_overlap_df$overlap_yearstart <- with(group_overlap_df, ifelse(g2startyear >= g1startyear, g2startyear, g1startyear))
# if g2 end year is lowest, use g2 end year, otherwise use g1 end year (use lowest end year)
group_overlap_df$overlap_yearend <- with(group_overlap_df, ifelse(g2endyear <= g1endyear, g2endyear, g1endyear))


# ------------------------------------------
### some preparation before bringing in relationship data
# column for dyad name. probably not necessary but just in case
group_overlap_df$dyadname <- with(group_overlap_df, paste(group1, group2))
# might need dyad id from other dataset
# first rename from group1_name to group1 to be able to match
lebanon_df_groups <- lebanon_df_groups %>% rename(group1 = group1_name)
# join in ids for group1
group_overlap_df <- left_join(group_overlap_df, dplyr::select(lebanon_df_groups, id_mgar, group1), by = "group1")
# renaming id column in overlap df to keep track of which id belongs with which group
group_overlap_df <- group_overlap_df %>% rename(group1_id = id_mgar)
# rename old df group1 to group2 to match for merging
lebanon_df_groups <- lebanon_df_groups %>% rename(group2 = group1)
# join in ids for gname 2
group_overlap_df <- left_join(group_overlap_df, dplyr::select(lebanon_df_groups, id_mgar, group2), by = "group2")
# rename ids for group2
group_overlap_df <- group_overlap_df %>% rename(group2_id = id_mgar)
# remove MG characters from ids. will make dyadid in code below
group_overlap_df$group1_id <- gsub("MG", "", group_overlap_df$group1_id)
group_overlap_df$group2_id <- gsub("MG", "", group_overlap_df$group2_id)


# expand years for each dyad name
group_overlap_df_long <- group_overlap_df %>%
                          # sequence of years for each corresponding start, end
                          mutate(dyadname, year = map2(overlap_yearstart, overlap_yearend, seq, by = 1)) %>%
                          # unnest the list column
                          unnest(cols = year)

#------------------------------------
# ### bringing in relationship data
# # df with mgar relationship data
# mgar_df_relyear <-  read.csv("/relationshipdyads.csv")
# # making sure the variable types match in both dfs
# mgar_df_relyear$group1_id <- as.character(mgar_df_relyear$group1_id)
# mgar_df_relyear$group2_id <- as.character(mgar_df_relyear$group2_id)
# 
# # creating a dyad-dyad-year id for mgar_df_relyear and for group_overlap_df_long to merge on
# group_overlap_df_long$dyadyearid <- with(group_overlap_df_long, paste(group1_id, group2_id, year, sep = ""))
# str(group_overlap_df_long$dyadyearid) # double checking that it is still chr
# mgar_df_relyear$dyadyearid <- with(mgar_df_relyear, paste(group1_id, group2_id, year, sep = ""))
# 
# 
# # merge in relationship data
# rel_df <- left_join(group_overlap_df_long, mgar_df_relyear, by = "dyadyearid")
# write.csv(rel_df, "lebanon_relationship_df.csv") # rename this in the folder asap so you do not accidentally overwrite it
# length(which(!is.na(rel_df$reltype)))


#-------------------------------------
# same as above chunk but with "final" dataset
library(data.table)
start <- Sys.time()
mgar_final_select <- as.data.frame(fread("/mgar_final.csv",
                      select = c("year", "group1_id", "group2_id", 
                      "grp1_yrfirst", "grp1_yrlast", "grp2_yrfirst", "grp2_yrlast",
                      "ccode1", "ccode2", 
                      "reltype","opsupp", "matsupp", "terrsupp", "trainsupp",	"finsupp",
                      "allies", "associates","supporters",	
                      "fans", "rivals", "competition", "hosts",  "enemies",
                      "relstart", "patron","opsupport", "territorialsupport",
                      "trainsupport", "matsupport", "finsupport",
                      "confidence_rel")))
print(Sys.time() - start)

# making sure the variable types match in both dfs
mgar_final_select$group1_id <- as.character(mgar_final_select$group1_id)
mgar_final_select$group2_id <- as.character(mgar_final_select$group2_id)

# creating a dyad-year id for mgar_df_relyear and for group_overlap_df_long to merge on
# sep must be " " or else 334-44 and 33-444 can get merged together
mgar_final_select$dyadyearid <- with(mgar_final_select, paste(group1_id, group2_id, year, sep = "-"))
group_overlap_df_long$dyadyearid <- with(group_overlap_df_long, paste(group1_id, group2_id, year, sep = "-"))

# merge
rel_df_zeros <- left_join(group_overlap_df_long, mgar_final_select, by = "dyadyearid")

# checking reltype 7
length(which(rel_df_zeros$reltype == 7))

#write.csv(rel_df_zeros, "lebanon_withfinal_df.csv") # rename in folder 



