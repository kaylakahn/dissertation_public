# packages
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(compare)


# datasets
edtg_mena_df <- read.csv("edtg_MENA.csv")
names_yrs_df <- read.csv("Lebanon_groups_kahn_monadic_042823.csv")
#mgar_mena_df <- read.csv("mgarfinal_mena.csv")

##########################
# keep only necessary columns,  limit to groups lasting 2001 or later
names_yrs_df <- names_yrs_df %>%
  select(id_mgar, id_edtg, group1_name, yearstart1, yearend1, endtype1) %>%
  filter(yearend1>2000)
# expand df for every year in interval
names_yrs_df$year <- mapply(seq, names_yrs_df$yearstart1, names_yrs_df$yearend1, SIMPLIFY = FALSE)
names_yrs_df <- names_yrs_df %>%
  unnest(year)
# limit to year 2000 or after
names_yrs_df <- names_yrs_df %>%
  filter(year > 1999, group1_name != "Arab Socialist Baath Party") # remove arab socialist baath party - should not be included rn

#############################
### left join the group attribute data ###
# put E before gid numbers in edtg_mena_df to match to edtg_id col in names_years_df
edtg_mena_df$gid <- paste("E", edtg_mena_df$gid, sep = "")
# put "edtg_" prefix for every col in edtg
original_cols <- colnames(edtg_mena_df)
colnames(edtg_mena_df) <- paste("edtg", original_cols, sep = "_")

# merge dfs
groupyear_df <- left_join(names_yrs_df, edtg_mena_df, by = c("id_edtg" = "edtg_gid", "year" = "edtg_year"))



#############################
### dealing with mgar ###
# use mgar subsetted to my groups
mgar_mygroups <- read.csv("mgarfinal_lebanon2000.csv")
# limit to 2000 
mgar_mygroups <- 
  mgar_mygroups %>%
  filter(year > 1999)
### the two commented out functions do the same thing - return indices
### checking these cols by to see what can be removed. details in separate text file
# #which(str_sub(colnames(mgar_mygroups), -1, -1)==1)
# #str_which(colnames(mgar_mygroups), "1$")
# str_subset(colnames(mgar_mygroups), "2$")
# str_subset(colnames(mgar_mygroups), "lag1$")
# str_subset(colnames(mgar_mygroups), "dyad$")
# str_subset(colnames(mgar_mygroups), "share")
# str_subset(colnames(mgar_mygroups), "support$")
# str_subset(colnames(mgar_mygroups), "supp$")
# str_subset(colnames(mgar_mygroups), "^type")
# str_subset(colnames(mgar_mygroups), "^cont")
# str_subset(colnames(mgar_mygroups), "cap")
# str_subset(colnames(mgar_mygroups), "repr") # the repression vars are dyadic
# str_subset(colnames(mgar_mygroups), "alli")
# remove columns that end in 2 -- these are for group 2. group2_id stays
# dont need lags, dyad vars
mgar_mygroups <- 
  mgar_mygroups %>%
  select(!str_subset(colnames(mgar_mygroups), "2$") & !str_subset(colnames(mgar_mygroups), "lag1")
         & !str_subset(colnames(mgar_mygroups), "dyad$") & !str_subset(colnames(mgar_mygroups), "share")
         & !str_subset(colnames(mgar_mygroups), "support$") & !str_subset(colnames(mgar_mygroups), "supp$")
         & !str_subset(colnames(mgar_mygroups), "^type") & !str_subset(colnames(mgar_mygroups), "^cont")
         & !str_subset(colnames(mgar_mygroups), "cap") & !str_subset(colnames(mgar_mygroups), "repr")
         & !str_subset(colnames(mgar_mygroups), "alli"))

# writing so i can have a look
#write.csv(mgar_mygroups, "mgar_mygroups_nomerge.csv")
#which(colnames(mgar_mygroups)=='reltype')
# double check all colnames to be removed
colnames(mgar_mygroups %>% select(c(11, 29:32, 56, 58:59, 61:81, 91:93, 95:108)))
# remove 
mgar_mygroups <- 
  mgar_mygroups %>%
  select(!c(11, 29:32, 56, 58:59, 61:81, 91:93, 95:108))



# check which variables are the same for group years
mgar_groupyear_CHECK <- 
  mgar_mygroups %>% 
  group_by(group1_id,year) %>%
  summarise_all(n_distinct)
which(colSums(mgar_groupyear_CHECK != 1) > 0) # group1_id, year, X, dyad_id, group2_id, grp2_yrfirst, grp2_yrlast
##### mgar wings. need to merge wings into main group
# mgar wings df
armedwings_df <- read.csv("mgar_other_toagg.csv") 
# get rid of the same vars
armedwings_df <- armedwings_df %>%
  filter(year > 1999) %>%
  select(!str_subset(colnames(armedwings_df), "2$") & !str_subset(colnames(armedwings_df), "lag1")
         & !str_subset(colnames(armedwings_df), "dyad$") & !str_subset(colnames(armedwings_df), "share")
         & !str_subset(colnames(armedwings_df), "support$") & !str_subset(colnames(armedwings_df), "supp$")
         & !str_subset(colnames(armedwings_df), "^type") & !str_subset(colnames(armedwings_df), "^cont")
         & !str_subset(colnames(armedwings_df), "cap") & !str_subset(colnames(armedwings_df), "repr")
         & !str_subset(colnames(armedwings_df), "alli"))
# double check all colnames to be removed again
colnames(armedwings_df %>% select(c(11, 29:32, 56, 58:59, 61:81, 91:93, 95:108)))
# remove these
armedwings_df <- 
  armedwings_df %>%
  select(!c(11, 29:32, 56, 58:59, 61:81, 91:93, 95:108))
# double checking 
armedwings_df_CHECK <- 
  armedwings_df %>% 
  group_by(group1_id,year) %>%
  summarise_all(n_distinct)
which(colSums(armedwings_df_CHECK != 1) > 0)


### add armedwings_df into mgar_mygroups
# MG to the start of the mgar ids to match groupyear_df
mgar_mygroups$group1_id <- paste("MG", mgar_mygroups$group1_id, sep ="")
# unique identifier group-year
mgar_mygroups$idyear <- paste(mgar_mygroups$group1_id, mgar_mygroups$year, sep = ".")
armedwings_df$idyear <- paste(armedwings_df$id_mgar, armedwings_df$year, sep = ".")
# slice first instance for each groupyear
length(unique(mgar_mygroups$idyear)) # 249
length(unique(armedwings_df$idyear)) # 72
mgar_mygroups_distinct <- distinct(mgar_mygroups, idyear, .keep_all = TRUE)
armedwings_df_distinct <- distinct(armedwings_df, idyear, .keep_all = TRUE)
# check that it ran correctly. should be same nrow as length above
nrow(mgar_mygroups_distinct)
nrow(armedwings_df_distinct)

# armed wings years should be same as main group end date
armedwings_df_distinct$grp1_yrlast <- mgar_mygroups_distinct$grp1_yrlast[match(armedwings_df_distinct$id_mgar, mgar_mygroups_distinct$group1_id)]
# remove years greater than g1 end year
armedwings_df_distinct <- 
  armedwings_df_distinct %>%
  filter(year <= grp1_yrlast)


# give mgar_mygroups_distinct id_mgar so cols match so that i can rbind
mgar_mygroups_distinct$id_mgar <- mgar_mygroups_distinct$group1_id
# rbind the dfs
mgar_combined <- rbind(mgar_mygroups_distinct, armedwings_df_distinct)

# summarise on attack cols (the wings and main groups attack counts added together)
mgar_combined  <- 
  mgar_combined  %>%
  group_by(idyear) %>%
  dplyr::summarise(across(.cols = c(attack1:maritimehijacking1), .fns = sum, na.rm = TRUE))

# make sure mgar_combined and mgar_mygroups_distinct have same groupyear ids
compare::compare(mgar_mygroups_distinct$idyear, mgar_combined$idyear, allowAll = TRUE)

# get ids where attacks are NA
NA_IDs <- mgar_mygroups_distinct[is.na(mgar_mygroups_distinct["attack1"]),"idyear"]
# replace attack1:maritimehijacking1 in mgar_mygroups_distinct df with those columns from mgar_combined
mgar_mygroups_distinct[match(mgar_combined$idyear, mgar_mygroups_distinct$idyear), 28:38] <- mgar_combined[2:12]
# put NAs back. if it was NA for the main group it has to stay that way
mgar_mygroups_distinct[mgar_mygroups_distinct$idyear %in% NA_IDs,28:38] <- NA

#######################

# rename mgar_id to group1_id
mgar_mygroups <-
  mgar_mygroups %>%
  dplyr:: rename(id_mgar = group1_id)
# merge in mgar data
groupyear_df <- join(groupyear_df, mgar_mygroups, by = c("id_mgar", "year"),
                     type = "left", match = "first")
# write to csv
#write.csv(groupyear_df, "diss_groupyear.csv")





