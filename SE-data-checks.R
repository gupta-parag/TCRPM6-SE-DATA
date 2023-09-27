library(data.table)
library(dplyr)
library(foreign)
library(stringr)

############################ READING BASE AND HORIZON YEAR DATA OF TCRPM 5 to see whether the problems existed before or not ##################
tcrp5_base <- read.dbf('C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\revised-data\\R-FINAL\\TCRPM5-DATA\\TCRPM5_15\\TCRPM5_15.dbf')
tcrp5_hor <- read.dbf('C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\revised-data\\R-FINAL\\TCRPM5-DATA\\TCRPM5_45\\TCRPM5_45.dbf')
########################################################################################################################################


######################################### TRYING TO FIND THE EXTENT OF THE PROBLEM #####################################
base_gq <- tcrp5_base[ , c(1,6,43)] # getting base group quarter population to join to TCRPM 6 data


tcrp5_base_f <- tcrp5_base[,c(1,2,5:10)]
tcrp5_base_f$POP_SIZE <- tcrp5_base_f$HHSIZE_1*1 + tcrp5_base_f$HHSIZE_2*2 + tcrp5_base_f$HHSIZE_3 *3 +
  tcrp5_base_f$HHSIZE_4PL*4

tcrp5_base_f$delta <- tcrp5_base_f$POP_SIZE - tcrp5_base_f$POP

tcrp5_hor_f <- tcrp5_hor[,c(1,2,5:10)]
tcrp5_hor_f$POP_SIZE <- tcrp5_hor_f$HHSIZE_1*1 + tcrp5_hor_f$HHSIZE_2*2 + tcrp5_hor_f$HHSIZE_3 *3 +
  tcrp5_hor_f$HHSIZE_4PL*4
tcrp5_hor_f$delta <- tcrp5_hor_f$POP_SIZE - tcrp5_hor_f$POP

###########################################################################################################################################

########################### READING TCRPM6 DATA AND BALANCING HHSIZE ########################################################################


tcrpm6_data <- fread("C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\revised-data\\R-FINAL\\disProportionteThis\\TCRPM6_disaggregated_wo_GQ.csv")
sizes <- tcrpm6_data[ , c(20,2,21:24)]
sizes_b <- tcrpm6_data[ , c(20,2,21:24)]


sizes_b$POP_SIZE <- sizes_b$`1-person household:_dist` * 1 + 
  sizes_b$`2-person household:_dist` * 2 + 
  sizes_b$`3-person household:_dist` * 3 + 
  sizes_b$`4-or-more-person household:_dist` * 4
sizes_b$DELTA <- sizes_b$POP_SIZE - sizes_b$POP



sizes$POP_SIZE <- sizes$`1-person household:_dist` * 1 + 
  sizes$`2-person household:_dist` * 2 + 
  sizes$`3-person household:_dist` * 3 + 
 sizes$`4-or-more-person household:_dist` * 4

sizes$DELTA <- sizes$POP_SIZE - sizes$POP



# SHOULD WRITE A FUNCTION HERE TO BALANCE IT INSIDE THE FOR LOOP 
for( i in 1:nrow(sizes)){
  print(i)
  fourPersHH <- sizes$`4-or-more-person household:_dist`[i]
  threePersHH <- sizes$`3-person household:_dist`[i]
  twoPerHH <- sizes$`2-person household:_dist`[i]
  hhNeeded <- round((sizes$DELTA[i] / 3),0) + 10
  
  if(sizes$DELTA[i] > 0) { # to check if is positive
    if (fourPersHH > 0){
      if (fourPersHH > hhNeeded ) { # check if 4 person HH are more
        # to leave 10 - 4 households
        sizes$`1-person household:_dist`[i] <- sizes$`1-person household:_dist`[i] + ( hhNeeded - 10)
        sizes$`4-or-more-person household:_dist`[i] <- fourPersHH - ( hhNeeded - 10)
      } else if(fourPersHH < hhNeeded && fourPersHH > 10) {
        # getting all the 4 person hh that are there but less than the size
        sizes$`1-person household:_dist`[i] <- sizes$`1-person household:_dist`[i] + 
          (sizes$`4-or-more-person household:_dist`[i] - 10)
        sizes$`4-or-more-person household:_dist`[i] <- 10
      } else if(fourPersHH == hhNeeded && hhNeeded > 10){
        print("I am at equal")
        sizes$`1-person household:_dist`[i] <- sizes$`1-person household:_dist`[i] + 
          (hhNeeded - 10)
        sizes$`4-or-more-person household:_dist`[i] <- 10
      }
    }
  }
  
  
  sizes$POP_SIZE <- sizes$`1-person household:_dist` * 1 + 
    sizes$`2-person household:_dist` * 2 + 
    sizes$`3-person household:_dist` * 3 + 
    sizes$`4-or-more-person household:_dist` * 4
  sizes$DELTA_NEW <- sizes$POP_SIZE - sizes$POP
  
  if(sizes$DELTA_NEW[i] > 0) { # to check if is positive
    hhNeeded_2 <- round((sizes$DELTA_NEW[i] / 2),0) + 10
    if (threePersHH > 0){
      if (threePersHH > hhNeeded_2 ) { # check if 3 person HH are more
        # to leave 10 - 3 households
        sizes$`1-person household:_dist`[i] <- sizes$`1-person household:_dist`[i] + ( hhNeeded_2 - 10)
        sizes$`3-person household:_dist`[i] <- threePersHH - ( hhNeeded_2 - 10)
      } else if(threePersHH < hhNeeded_2 && threePersHH > 10) {
        # getting all the 4 person hh that are there but less than the size
        sizes$`1-person household:_dist`[i] <- sizes$`1-person household:_dist`[i] +
          (sizes$`3-person household:_dist`[i] - 10)
        sizes$`3-person household:_dist`[i] <- 10
      } else if(threePersHH == hhNeeded_2 && hhNeeded_2 > 10){
        print("I am at equal")
        sizes$`1-person household:_dist`[i] <- sizes$`1-person household:_dist`[i] +
          (hhNeeded_2 - 10)
        sizes$`3-person household:_dist` <- 10
      }
    }
  }
  
  sizes$POP_SIZE <- sizes$`1-person household:_dist` * 1 +
    sizes$`2-person household:_dist` * 2 +
    sizes$`3-person household:_dist` * 3 +
    sizes$`4-or-more-person household:_dist` * 4
    sizes$DELTA_NEW2 <- sizes$POP_SIZE - sizes$POP
   
    if(sizes$DELTA_NEW2[i] > 0) { # to check if is positive
      hhNeeded_3 <- round((sizes$DELTA_NEW2[i] / 1),0) + 10
      if (twoPerHH > 0){
        if (twoPerHH > hhNeeded_3 ) { # check if 3 person HH are more
          # to leave 10 - 3 households
          sizes$`1-person household:_dist`[i] <- sizes$`1-person household:_dist`[i] + ( hhNeeded_2 - 10)
          sizes$`2-person household:_dist`[i] <- twoPerHH - ( hhNeeded_2 - 10)
        } else if(twoPerHH < hhNeeded_3 && twoPerHH > 10) {
          # getting all the 4 person hh that are there but less than the size
          sizes$`1-person household:_dist`[i] <- sizes$`1-person household:_dist`[i] +
            (sizes$`2-person household:_dist`[i] - 10)
          sizes$`2-person household:_dist`[i] <- 10
        } else if(threePersHH == hhNeeded_3 && hhNeeded_3 > 10){
          print("I am at equal")
          sizes$`1-person household:_dist`[i] <- sizes$`1-person household:_dist`[i] +
            (hhNeeded_3 - 10)
          sizes$`3-person household:_dist` <- 10
        }
      }
    }

    sizes$POP_SIZE <- sizes$`1-person household:_dist` * 1 +
      sizes$`2-person household:_dist` * 2 +
      sizes$`3-person household:_dist` * 3 +
      sizes$`4-or-more-person household:_dist` * 4
    sizes$DELTA_NEW3 <- sizes$POP_SIZE - sizes$POP
}


# SHOULD WRITE A FUNCTION HERE TO BALANCE IT INSIDE THE FOR LOOP 
for( i in 1:nrow(sizes)){
  print(i)
  onePersHH <- sizes$`1-person household:_dist`[i]
  twoPersHH <- sizes$`2-person household:_dist`[i]
  threePerHH <- sizes$`3-person household:_dist`[i]
  hhNeeded <- abs(round((sizes$DELTA[i] / 3),0)) + 10
  
  if(sizes$DELTA[i] < -99) { # to check if it is negative
    print(sizes$DELTA[i])
    if (onePersHH > 0){
      if (onePersHH > hhNeeded ) { # check if 4 person HH are more
        # to leave 10 - 4 households
        sizes$`4-or-more-person household:_dist`[i] <- sizes$`4-or-more-person household:_dist`[i]  + 
          ( hhNeeded - 10)
        sizes$`1-person household:_dist`[i] <- onePersHH - ( hhNeeded - 10)
      } else if(onePersHH < hhNeeded && onePersHH > 10) {
        # getting all the 4 person hh that are there but less than the size
        sizes$`4-or-more-person household:_dist`[i] <- sizes$`4-or-more-person household:_dist`[i]  + 
          (sizes$`1-person household:_dist`[i] - 10)
        sizes$`1-person household:_dist`[i] <- 10
      } else if(onePersHH == hhNeeded && hhNeeded > 10){
        print("I am at equal")
        sizes$`4-or-more-person household:_dist`[i] <- sizes$`4-or-more-person household:_dist`[i] + 
          (sizes$`1-person household:_dist`[i] - 10) 
        sizes$`1-person household:_dist`[i] <- 10
      }
    }
  }
  
 
  sizes$POP_SIZE <- sizes$`1-person household:_dist` * 1 +
    sizes$`2-person household:_dist` * 2 +
    sizes$`3-person household:_dist` * 3 +
    sizes$`4-or-more-person household:_dist` * 4
  sizes$DELTA_NEW <- sizes$POP_SIZE - sizes$POP
  
  
  if(sizes$DELTA_NEW[i] < -99) { # to check if is positive
    hhNeeded_2 <- abs(round((sizes$DELTA_NEW[i] / 2),0)) + 10
    if (twoPersHH > 0){
      if (twoPersHH > hhNeeded_2 ) { # check if 3 person HH are more
        # to leave 10 - 3 households
        sizes$`4-or-more-person household:_dist`[i] <- sizes$`4-or-more-person household:_dist`[i]  + 
          ( hhNeeded_2 - 10)
        sizes$`2-person household:_dist`[i] <- twoPersHH - ( hhNeeded_2 - 10)
      } else if(twoPersHH < hhNeeded_2 && twoPersHH > 10) {
        # getting all the 4 person hh that are there but less than the size
        sizes$`4-or-more-person household:_dist`[i] <- sizes$`4-or-more-person household:_dist`[i] + 
          ( sizes$`2-person household:_dist`[i] - 10)
        sizes$`2-person household:_dist`[i] <- 10
      } else if(twoPersHH == hhNeeded_2 && hhNeeded_2 > 10){
        print("I am at equal")
        sizes$`4-or-more-person household:_dist`[i] <- sizes$`4-or-more-person household:_dist`[i]  + 
          ( sizes$`2-person household:_dist`[i] - 10)
        sizes$`2-person household:_dist` <- 10
      }
    }
  }
 
   sizes$POP_SIZE <- sizes$`1-person household:_dist` * 1 + 
    sizes$`2-person household:_dist` * 2 + 
    sizes$`3-person household:_dist` * 3 + 
    sizes$`4-or-more-person household:_dist` * 4
  sizes$DELTA_NEW2 <- sizes$POP_SIZE - sizes$POP
  
  if(sizes$DELTA_NEW2[i] < -99) { # to check if is positive
    hhNeeded_3 <- abs(round((sizes$DELTA_NEW2[i] / 1),0)) + 10
    if (threePerHH > 0){
      if (threePerHH > hhNeeded_3 ) { # check if 3 person HH are more
        # to leave 10 - 3 households
        sizes$`4-or-more-person household:_dist`[i] <- sizes$`4-or-more-person household:_dist`[i]  + 
          ( hhNeeded_3 - 10)
        sizes$`3-person household:_dist`[i] <- threePerHH - ( hhNeeded_3 - 10)
      } else if(threePerHH < hhNeeded_3 && threePerHH > 10) {
        # getting all the 4 person hh that are there but less than the size
        sizes$`4-or-more-person household:_dist`[i] <- sizes$`4-or-more-person household:_dist`[i] + 
          ( sizes$`3-person household:_dist`[i] - 10)
        sizes$`3-person household:_dist`[i] <- 10
      } else if(threePersHH == hhNeeded_3 && hhNeeded_3 > 10){
        print("I am at equal")
        sizes$`4-or-more-person household:_dist`[i] <- sizes$`4-or-more-person household:_dist`[i]  + 
          ( sizes$`3-person household:_dist`[i] - 10)
        sizes$`3-person household:_dist` <- 10
      }
    }
  }
  
  sizes$POP_SIZE <- sizes$`1-person household:_dist` * 1 + 
    sizes$`2-person household:_dist` * 2 + 
    sizes$`3-person household:_dist` * 3 + 
    sizes$`4-or-more-person household:_dist` * 4
  sizes$DELTA_NEW3 <- sizes$POP_SIZE - sizes$POP
}

#########################################################################################################################################

####################################### FINALIZING THE HHSIZE BY TAKING OUT COLUMNS THAT ARE NEEDED #####################################
rebalanced <- sizes[ , c(1,3:6)]
colnames(rebalanced)[2:5] <- str_c(colnames(rebalanced)[2:5], "_y")

rm(list=setdiff(ls(), "rebalanced"))


###########################################################################################################################################

