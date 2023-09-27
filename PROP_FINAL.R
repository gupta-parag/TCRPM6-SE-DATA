library(data.table)
library(stringr)


############################ FUNCTION TO CALCULATE PROPORTION FROM CENSUS AND THEN PROJECT IT TO MPO DATA #################################################

calc_prop <- function(x ,col_index = 1:3, total = 4, mpo_total = 4, geography = "bg"){
  x[x$TAZ_REG == 133, c(col_index,total) ] <-  x[x$TAZ_REG == 132,c(col_index,total) ] # TAZ 133 AND 426 have zero census data
  x[x$TAZ_REG == 426, c(col_index,total)] <-  x[x$TAZ_REG == 430,c(col_index,total) ] # hence used their neighbors to get the proportion
  
  x[x[,total] == 0, total] <- 1
  t <- x[ , col_index]/x[,total]
  colnames(t) <- str_c(colnames(t),"_prop")
  v <- round(t[,1:ncol(t)] * x[,mpo_total],0)
  colnames(v) <- gsub(colnames(t), pattern = "_prop", replacement = "_dist")
  x <- cbind(x,t,v)
  return(x)
}

######################################################################################################################################

#################################### JOINING DATA FRAMES WITH CENSUS GEOGRAPHY TO GET THE CROSS WALK #########################

join_frame <- function(x, geography){
  if(geography == "bg"){
    x <- as.data.frame(left_join(bg_taz_cross, x, by = "GEOID"))
  } else {
    x <- as.data.frame(left_join(ct_taz_cross, x, by = "GEOID"))
  }
  return(x)
}

##############################################################################################################################


####################################### DISAGGREGATING THE DATA ###########################################################


# WRITING THE SCALE AT WHICH THE DATA IS AVAILABLE
# BG - AGE, GENDER, INCOME, SCHOOL ENR, SF AND MF
#CT - RACE, HHSIZE, WORKER,

bg_taz_cross$GEOID <- as.character(bg_taz_cross$GEOID)
ct_taz_cross$GEOID <- as.character(ct_taz_cross$GEOID)

age_dist_f <- calc_prop(join_frame(age_dist, "bg"), col_index = 9:17, mpo_total = 5, total = 18)

gender_f<-calc_prop(join_frame(gender,"bg"), col_index = 10:11, total = 9, mpo_total = 5)

race_hisp_t_f <- calc_prop(join_frame(race_hisp_t, "ct"), col_index = 10:12, total = 9, mpo_total = 6)

hhsize_ff <-calc_prop( join_frame(hhsize_f, "ct"), col_index = 10:13, total = 9,mpo_total = 7)

hhwrkr_ff <- calc_prop(join_frame(hhwrkr_f,"ct"), col_index = 10:13, total = 9, mpo_total = 7)

inc_dist_f <- calc_prop(join_frame(inc_dist, "bg"), col_index = 10:14, total = 9, mpo_total = 6)

sf_mf_fff <- calc_prop(join_frame(sf_mf_ff, "ct"), col_index = 10:12, total = 9, mpo_total = 7)

child_ff <- calc_prop(join_frame(child_f,"ct"), col_index = 10:11, total = 9, mpo_total = 7)

######################################################################################################################################

######################################## COMPILING FINAL FILE ######################################

# THIS IS WITH PROPORTIONS
# final_data_comp <- cbind(age_dist_f[ , c(4:6,18:36)], gender_f[ , c(4:6,9,12:15)], 
#                          race_hisp_t_f[,c(2,6:7,9,13:18)], hhsize_ff[, c(2,6:7,9,14:21)],
#                          hhwrkr_ff[,c(2,6:7,9,14:21)], inc_dist_f[,c(4:6,9,15:24)], 
#                          sf_mf_fff[ , c(2,6:7,9,13:18)], child_ff[,c(2,6:7,9,11:15)])

#THIS IS JUST WITH TAZ_REG NUMBER AND PROP OF FIELDS
final <- cbind(age_dist_f[ , c(4:6,28:36)], gender_f[ , c(4,14:15)], 
               race_hisp_t_f[,c(2,16:18)], hhsize_ff[, c(2,18:21)],
               hhwrkr_ff[,c(2,18:21)], inc_dist_f[,c(4,20:24)], 
               sf_mf_fff[ , c(2,16:18)], child_ff[,c(2,14:15)])

# write.csv(final_data_comp, "finalise_this.csv", row.names = F)
write.csv(final, "TCRPM6_disaggregated.csv", row.names = F)
####################################################################################################

