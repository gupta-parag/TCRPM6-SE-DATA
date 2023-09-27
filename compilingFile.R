library(data.table)
library(dplyr)
library(foreign)
library(sf)


############################################ READING ALL THE DATA NEEDED ################################################

nodes <- read.dbf("C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\revised-data\\R-FINAL\\network\\nodes.dbf") # for POINT_X and POINT_Y field
tcrpm5_data <- read.dbf("C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\revised-data\\R-FINAL\\TCRPM5-DATA\\TCRPM5_45\\TCRPM5_45.dbf") # 45 data
tcrpm6_file <- st_read("C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\revised-data\\R-FINAL\\tazSplits\\shapefile",
                       layer = "TCRPM6") # TCRPM6 shapefile to join the final data

# final disaggregated data
se_Data <- as.data.frame(fread("C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\revised-data\\R-FINAL\\disProportionteThis\\TCRPM6_disaggregated_wo_GQ.csv"))

# employment data
emp <- fread("C:\\Projects_2023\\TCRPM6\\SE-DATA-PROP\\revised-data\\R-FINAL\\disProportionteThis\\EMP_TO_JOIN.csv")
############################################################################################################################################

####################################### MODIFYING FILES #########################################################
nodes_f <- nodes[nodes$N %in% tcrpm6_file$TAZ_REG, c(1:3)]
colnames(nodes_f)[2:3] <- c("POINT_X", "POINT_Y")
tcrpm6_file_f <- tcrpm6_file[ , c(1:29)]
colnames(se_Data)
colnames(tcrpm5_data)
##################################################################################################################

######################################## Changing Column Names to TCRPM5 column names #########################################

# age
colnames(se_Data)[4:12] <- colnames(tcrpm5_data)[23:31]

# workers
colnames(se_Data)[26:29] <- colnames(tcrpm5_data)[16:19]

# income
colnames(se_Data)[31:35] <- colnames(tcrpm5_data)[11:15]

#size
colnames(se_Data)[21:24] <- colnames(tcrpm5_data)[7:10]

#houses 
colnames(se_Data)[37:39] <- colnames(tcrpm5_data)[20:22]

# households with children
colnames(se_Data)[41:42] <- colnames(tcrpm5_data)[c(33,32)]

#Male, female
colnames(se_Data)[14:15] <- colnames(tcrpm5_data)[c(37:38)]

#race
#Male, female
colnames(se_Data)[17:19] <- colnames(tcrpm5_data)[c(35,34,36)]

se_Data_f <- se_Data[ ,-which(colnames(se_Data) == "TAZ_REG")]
se_Data_f <-  cbind(se_Data[,1], se_Data_f)
colnames(se_Data_f)[c(1,3)] <- c("TAZ_REG", "TOTAL_HOUS")

#######################################################################################################################


################################################ COMPILING FILE #######################################################
getting_there <- left_join(se_Data_f, tcrpm6_file_f, by = "TAZ_REG") %>%
                left_join(nodes_f, by = c("TAZ_REG" = "N")) %>%
                  left_join(emp, by = c("TAZ_REG" = "TAZ")) %>%
                left_join(tcrpm5_data[,c(1,39:41,43,71,72,74,77:96)], by = "TAZ_REG")

#printing column names to get their order
colnames(getting_there)
colnames(tcrpm5_data)

# final order of columns, this is same as TCRPM5 
final_file <- getting_there[ , c(1,36,37,39,38,2,18:21,26:30,22:25,31:33,4:12,35,34,16,15,
                                 17,13:14,85:87,3,88 ,40:41,68:84,42:49,89,90,50,91,65:66,
                                 92:94,95:111)]
# replacing column namesthat are ending with .y with nothing
colnames(final_file) <- gsub(colnames(final_file), 
                             pattern = ".y$", replacement = '')

# checking column names
colnames(final_file)

# RUN SE DATA CHECKS FILE before running this script
ff <- left_join(final_file, rebalanced, by = "TAZ_REG") #joining the rebalaced HHSZIE with the disaggregated data

#reasssinging hhsize that came from SE-data checks script
ff$HHSIZE_1 <- ff$`1-person household:_dist_y`
ff$HHSIZE_2 <- ff$`2-person household:_dist_y`
ff$HHSIZE_3 <- ff$`3-person household:_dist_y`
ff$HHSIZE_4PL <- ff$`4-or-more-person household:_dist_y`

ff_f <- ff[ , -c(97:100)]

ff_ff <- left_join(tcrpm6_file[,1], ff_f, by = "TAZ_REG") # joining with the shapefiles


st_write(ff_ff, "TCRPM6_SE_DATA.shp") # writing finfal shapefile
##################################################################################################################






