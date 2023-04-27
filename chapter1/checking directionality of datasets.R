mgar_df_relyear <-  read.csv("/relationshipdyads.csv")
mgar_df_relyear2 <-  read.csv("/relationshipdyads.csv")


# assign names for directionality
# group1 sender -> group 2 receiver
mgar_df_relyear$g1g2year <- with(mgar_df_relyear, paste(group1_id, group2_id, year, sep = ""))
# group2 sender -> group 1 receiver
mgar_df_relyear2$g2g1year <- with(mgar_df_relyear2, paste(group2_id, group1_id, year, sep = ""))



# assign reltype group2 from df 2 into df1
# this is to check if the variables are the same regardless of whether the group is the sender or receiver
# because the original data has dyads in both directions. but potentially unidirectional
mgar_df_relyear$reltype2 <- mgar_df_relyear2$reltype[match(mgar_df_relyear$g1g2year, mgar_df_relyear2$g2g1year)]


# check if any do not match
which(mgar_df_relyear$reltype != mgar_df_relyear$reltype2)

# do the reverse to double check
# basically: when mgar_df_relyear2$g2g1year is x value, find x value among mgar_df_relyear$g1g2year, 
# then find the reltype that corresponds with that row for mgar_df_relyear$g1g2year 
# and put that reltype into mgar_df_relyear2 where mgar_df_relyear2$g2g1year matches mgar_df_relyear$g1g2year
mgar_df_relyear2$reltype2 <- mgar_df_relyear$reltype[match(mgar_df_relyear2$g2g1year, mgar_df_relyear$g1g2year)]
# check if any do not match
which(mgar_df_relyear2$reltype != mgar_df_relyear2$reltype2)



# do the same but for support types
mgar_df_relyear$finsupp2 <- mgar_df_relyear2$finsupp[match(mgar_df_relyear$g1g2year, mgar_df_relyear2$g2g1year)]
which(mgar_df_relyear$finsupp != mgar_df_relyear$finsupp2)

mgar_df_relyear$trainsupp2 <- mgar_df_relyear2$trainsupp[match(mgar_df_relyear$g1g2year, mgar_df_relyear2$g2g1year)]
which(mgar_df_relyear$trainsupp != mgar_df_relyear$trainsupp2)

mgar_df_relyear$matsupp2 <- mgar_df_relyear2$matsupp[match(mgar_df_relyear$g1g2year, mgar_df_relyear2$g2g1year)]
which(mgar_df_relyear$matsupp != mgar_df_relyear$matsupp2)

mgar_df_relyear$terrsupp2 <- mgar_df_relyear2$terrsupp[match(mgar_df_relyear$g1g2year, mgar_df_relyear2$g2g1year)]
which(mgar_df_relyear$terrsupp != mgar_df_relyear$terrsupp2)

mgar_df_relyear$opsupp2 <- mgar_df_relyear2$opsupp[match(mgar_df_relyear$g1g2year, mgar_df_relyear2$g2g1year)]
which(mgar_df_relyear$opsupp != mgar_df_relyear$opsupp2)


