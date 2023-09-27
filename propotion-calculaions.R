library(sf)
library(foreign)
library(tidycensus)
library(leaflet)
library(dplyr)
library(data.table)


#loading all the tables and column available in ACS 5 year dataset
#x <- load_variables(year = 2021, dataset = "acs5")
x <- load_variables(year = 2021, dataset = "acs5")#
# housing units


#writing function to clean column names
clean_names <- function(frame_sf){
  if(class(frame_sf)[1] == "sf"){
    frame <- st_drop_geometry(frame_sf)
  } else {
    frame <- frame_sf 
  }
  
  frame_f <- frame[,!grepl(colnames(frame), pattern = "M$")]
  
  frame_names <- as.data.frame(gsub(colnames(frame_f)[3:ncol(frame_f)], pattern = "E$", replacement = ""))
  colnames(frame_names) <- "Variable_Name"
  
  frame_names <- left_join(frame_names, x[,c(1,2)] , 
                         by = c("Variable_Name" = "name"))
  frame_names$label <- gsub(frame_names$label, pattern = "Estimate!!Total:!!", replacement = "")
  frame_names$label <- gsub(frame_names$label, pattern = "!!", replacement = "")
  frame_names$label <- gsub(frame_names$label, pattern = "Estimate", replacement = "")
  
  colnames(frame_f)[3:ncol(frame_f)] <- frame_names$label 
  return(frame_f)
}


################## Getting age data and gender data of TCRPM region#######################################

age_expt <- get_acs(geography = "block group", year = 2021, state = "12", 
                    county = c("061","111", "085"), table = "B01001", output = "wide", 
                    geometry = T)

age <- get_acs(geography = "block group", year = 2021, state = "12", 
               county = c("061","111", "085"), table = "B01001", output = "wide", 
               geometry = T)

age_f <- st_drop_geometry(age)
age_f <- age_f[,!grepl(colnames(age_f), pattern = "M$")]

age_names <- as.data.frame(gsub(colnames(age_f)[3:ncol(age_f)], pattern = "E$", replacement = ""))
colnames(age_names) <- "Variable_Name"

age_names <- left_join(age_names, x[,c(1,2)] , 
          by = c("Variable_Name" = "name"))

age_names$label <- gsub(age_names$label, pattern = "Estimate!!Total:!!", replacement = "")
age_names$label <- gsub(age_names$label, pattern = "!!", replacement = "")
age_names$label <- gsub(age_names$label, pattern = "Estimate", replacement = "")

colnames(age_f)[3:ncol(age_f)] <- age_names$label 



age_dist <- age_f[ , c(1,2)]
age_dist$Less_5 <- age_f$`Male:Under 5 years` + age_f$`Female:Under 5 years`

age_dist$FiveTo14 <- age_f$`Male:5 to 9 years` + age_f$`Male:10 to 14 years` + 
                    age_f$`Female:5 to 9 years` + age_f$`Female:10 to 14 years`

age_dist$FifTo17 <- age_f$`Male:15 to 17 years` + age_f$`Female:15 to 17 years`

age_dist$EighTo24 <- age_f$`Male:18 and 19 years` + age_f$`Male:20 years` + age_f$`Male:21 years`+
                      age_f$`Male:22 to 24 years` + age_f$`Female:18 and 19 years` + age_f$`Female:20 years` +
                      age_f$`Female:21 years` + age_f$`Female:22 to 24 years`
age_dist$twenFiveTo34 <- age_f$`Male:25 to 29 years` + age_f$`Male:30 to 34 years` + 
                          age_f$`Female:25 to 29 years` + age_f$`Female:30 to 34 years`

age_dist$thirFiveTo49 <- age_f$`Male:35 to 39 years` + age_f$`Male:40 to 44 years` + 
                            age_f$`Male:45 to 49 years` + age_f$`Female:35 to 39 years` + 
                          age_f$`Female:40 to 44 years` + age_f$`Female:45 to 49 years`

age_dist$fiftyTo64 <- age_f$`Male:50 to 54 years` + age_f$`Male:55 to 59 years` + 
                      age_f$`Male:60 and 61 years` + age_f$`Male:62 to 64 years` + 
                    age_f$`Female:50 to 54 years` + age_f$`Female:55 to 59 years` + 
                        age_f$`Female:60 and 61 years`+ age_f$`Female:62 to 64 years`

age_dist$sixty5To79 <- age_f$`Male:65 and 66 years` + age_f$`Male:67 to 69 years` + 
                      age_f$`Male:70 to 74 years` + age_f$`Male:75 to 79 years` + 
                      age_f$`Female:65 and 66 years` + age_f$`Female:67 to 69 years` + 
                        age_f$`Female:70 to 74 years` + age_f$`Female:75 to 79 years`
age_dist$EightyPlus <- age_f$`Male:80 to 84 years` + age_f$`Male:85 years and over` + 
                        age_f$`Female:80 to 84 years` + age_f$`Female:85 years and over`


age_dist <- left_join(age_dist,age[,c(1,3)], by = "GEOID")
colnames(age_dist)[12] <- "Total"
age_dist <- age_dist[ , -ncol(age_dist)]

##############################################################################################################




####################### Gender data #########################
gender <- age_f[ , c(1,2,3,4,28)]
##########################################################



################################# Race\ethnicity data is by census tract ############################
race_hisp <- get_acs(geography = "tract", year = 2021, state = "12", 
                     county = c("061","111", "085"), # requestiing total, white, hispanic, 
                     variables = c("B01001_001" , "B01001H_001", "B01001I_001"), output = "wide", 
                     geometry = T)

race_hisp_t <- clean_names(race_hisp)
colnames(race_hisp_t) <- c( "GEOID", "NAME", "Total", "White", "Hispanic")
race_hisp_t$Other <- race_hisp_t$Total - race_hisp_t$White - race_hisp_t$Hispanic # computing other

#################################################################################################



####################################### Household size ########################################
hhsize <- get_acs(geography = "tract", year = 2021, state = "12", 
               county = c("061","111", "085"), table = "B08201", output = "wide", 
               geometry = T)
hhsize_f <- clean_names(hhsize)
hhsize_f <- hhsize_f[ , c(1,2,3,9,15,21,27)]

####################################################################################



leaflet() %>% 
  addTiles() %>%
    addPolygons(data = st_transform(hhsize, 4326))

###################################### Household workers###########################################

hhwrkr <- get_acs(geography = "tract", year = 2021, state = "12", 
                  county = c("061","111", "085"), table = "B08202", output = "wide", 
                  geometry = T)
hhwrkr_f <- clean_names(hhwrkr)
hhwrkr_f <- hhwrkr_f[ , c(1:7)]

###########################################################################################


######################################## Household income ##############################################
hhinc <- get_acs(geography = "block group", year = 2021, state = "12", 
                  county = c("061","111", "085"), table = "B19001", output = "wide", 
                  geometry = T)
hhinc_f <- clean_names(hhinc)
inc_dist <- hhinc_f[ , c(1,2,3)]
inc_dist$Less25k <- hhinc_f[ ,4] + hhinc_f[ ,5] + hhinc_f[ ,6] + hhinc_f[ ,7]
inc_dist$twnety5to50k <- hhinc_f[ ,8] + hhinc_f[ ,9] + hhinc_f[ ,10] + hhinc_f[ ,11] + hhinc_f[,12]
inc_dist$fiftyto75k <- hhinc_f[ ,13] + hhinc_f[ ,14] 
inc_dist$sevety5to100k <-  hhinc_f[ ,15] 
inc_dist$hundrekPlus <-  hhinc_f[ ,16] + hhinc_f[ ,17] + hhinc_f[ ,18] + hhinc_f[ ,19] 
#############################################################################################
 

######################################### Additonal Data not required ##############################
#group quarters
# gq <- get_acs(geography = "tract", year = 2021, state = "12", 
#                  county = c("061","111", "085"), variables= c("B26001_001"), 
#               output = "wide", 
#                  geometry = T)
# #gq total population
# gq_f <- clean_names(gq)


# school enrollment
# schEnr <- get_acs(geography = "block group", year = 2021, state = "12", 
#                  county = c("061","111", "085"), table = "B14002", output = "wide", 
#                  geometry = T)
# schEnr_f <- clean_names(schEnr)

######################################################################################################

############################ Single family and Multi family units (Subject tables begin from here) #########################################

sf_mf <- get_acs(geography = "tract", year = 2021, state = "12", 
                 county = c("061","111", "085"), table = "S2504", output = "wide", 
                 geometry = T)

sf_mf_f <- clean_names(sf_mf)
sf_mf_f <- sf_mf_f[ , c(1:10)]
sf_mf_f$Single_Fam <- sf_mf_f$`Occupied housing unitsOccupied housing unitsUNITS IN STRUCTURE1, detached` + 
                      sf_mf_f$`Occupied housing unitsOccupied housing unitsUNITS IN STRUCTURE1, attached`      
sf_mf_f$Multi_Fam <- sf_mf_f$`Occupied housing unitsOccupied housing unitsUNITS IN STRUCTURE2 apartments` + 
                    sf_mf_f$`Occupied housing unitsOccupied housing unitsUNITS IN STRUCTURE3 or 4 apartments`+
                    sf_mf_f$`Occupied housing unitsOccupied housing unitsUNITS IN STRUCTURE5 to 9 apartments` + 
                    sf_mf_f$`Occupied housing unitsOccupied housing unitsUNITS IN STRUCTURE10 or more apartments`

sf_mf_f$Mob_Units <- sf_mf_f$`Occupied housing unitsOccupied housing unitsUNITS IN STRUCTUREMobile home or other type of housing`

sf_mf_ff <- sf_mf_f[, c(1:3, 11:13)]
colnames(sf_mf_ff)[3] <- "Total"
#########################################################################################################

######################################## HH Children(Subject tables begin from here) ########################################################

child <-  clean_names(get_acs(geography = "tract", year = 2021, state = "12", 
                          county = c("061","111", "085"), table = "S1101", output = "wide", 
                          geometry = T))
child_f <- child[ , c(1,2,3,7)]
child_f$No_Child <- child_f$`TotalHOUSEHOLDSTotal households` - child_f$`TotalAGE OF OWN CHILDRENHouseholds with own children of the householder under 18 years`
colnames(child_f)[3:4] <- c("Total_households", "With_Child")
######################################################################################################

####################################### CROSS WALK BEGINS HERE ####################################
newPopHHdata <- read.csv("C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\revised-data\\R-FINAL\\disProportionteThis\\DataToProportionate.csv")
bg_crossWalk <- fread("C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\cross-walkj\\tazCrossWalk_BG.csv")
ct_crosswalk <- fread("C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\cross-walkj\\tazCrossWalk_CT.csv")

#BG crosswalk
#bg_crossWalk$taz_diff <- bg_crossWalk$TAZ_REG - bg_crossWalk$TAZ_REG_1
options(scipen =999)
bg_croos_rev <- bg_crossWalk[ , c(3:4,15,44:48,51)]
bg_croos_rev <- bg_croos_rev[,c(1,2,3,5,6,9)]


RevBg_croos_rev <- left_join(bg_croos_rev, newPopHHdata[ , c(1,3:4)], by = "TAZ_REG")
bg_croos_rev <- RevBg_croos_rev[,c(1,2,3,7,8,6)] 

bg_croos_rev_f <- split(bg_croos_rev, bg_croos_rev$TAZ_REG)
bg_taz <- lapply(bg_croos_rev_f, function(x){
  z <- x %>% top_n(1, Rev_Area)
   return(z)
})
bg_taz_cross <- bind_rows(bg_taz, .id = "column_label")

#CT crosswalk
  
ct_croos_rev <- ct_crosswalk[ , c(3:4,33:34,42:43,53)]

ct_croos_rev <- left_join(ct_croos_rev, newPopHHdata[,-2], by = "TAZ_REG")
ct_croos_rev <- ct_croos_rev[,c(1,2,5,7,8,9)]

ct_croos_rev_f <- split(ct_croos_rev, ct_croos_rev$TAZ_REG)

ct_taz <- lapply(ct_croos_rev_f, function(x){
  z <- x %>% top_n(1, Rev_Area)
  return(z)
})
ct_taz_cross <- bind_rows(ct_taz, .id = "column_label")
################################################################################################


