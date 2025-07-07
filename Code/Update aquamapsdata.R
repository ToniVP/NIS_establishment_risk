###############################################################################!
## Working with SQL databases: Updating existing aquamapsdata from R package ##!
###############################################################################
remove(list=ls())
#Libraries and functions
source("./Code/Libraries and functions.R")

#Prior to running this code you should have installed the R package "aquamapsdata": https://raquamaps.github.io/aquamapsdata/articles/intro.html
#The data in the R package is from 2019, the updated distribution maps (2021) should be requested to the AquaMaps team: https://www.aquamaps.org/

#New species distribution data is provided in SQL format. The goal of this script is to update the tables stored in the local 
#SQL repository created when installing the "aquamapsdata" package, to be able to access the data with the in-built package functions, for an easier workflow

# Create and connect to an SQLite database (change path as needed, to your local dependencies and folder) 

#The first connection is the dependencies where aquamapsdata is installed, with "am.db" being the locally stored full data, 
#and where the new data should be substituted to be accessed with the package functions
con_first <- dbConnect(RSQLite::SQLite(), dbname = "C:/Users/avipo/AppData/Local/aquamaps/aquamaps/am.db") #This also serves to access all data very easily with dplyr!!

#This one is the duplicated file, in order to check what's inside and to make trials, in order to not touch the original local repository
#At the end, this one will be merged with the original one from the package dependencies
con <- dbConnect(RSQLite::SQLite(), dbname = "./aquamapsdata/am.db")

#know which tables are inside
dbListTables(con_first)
dbListTables(con) 

# 1. Table creation: All native range for fishes ------
#Delete table if existing
# drop_table_query <- paste("DROP TABLE IF EXISTS", "hcaf_species_native")
# dbExecute(con, drop_table_query)

#Table creation query
create_table_query <- "CREATE TABLE `hcaf_species_native`(                                              
 `SpeciesID` varchar(50) DEFAULT NULL,                                         
`CsquareCode` varchar(10) DEFAULT NULL,                                          
`CenterLat` double DEFAULT NULL,                                                
`CenterLong` double DEFAULT NULL,                                                
`Probability` float DEFAULT NULL,                                              
`FAOAreaYN` tinyint(1) DEFAULT NULL,                                             
`BoundBoxYN` tinyint(1) DEFAULT NULL                                             
);"

#Execute the query to create the table
dbExecute(con, create_table_query)

#Fill the new database with the values
sql_file <- "./aquamapsdata/hcaf_species_native_fish.sql"  # Replace with the path to your .sql file

# values = paste(sql_commands[,2], sep = "", collapse = "\n")
# 
# insert_query = paste(sql_commands[1,1], values, collapse = "\n")

sql_commands <- readLines(sql_file, warn = F, encoding = "UTF-8") #select only those commands where they list fish species
insert_query <- sql_commands[grepl("INSERT", sql_commands)]
trial = substr(insert_query, 1, 10); all(trial == trial)

for (i in 1:length(insert_query)) {
  tryCatch({dbExecute(con, insert_query[i])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  #print(i)
  print(i/length(insert_query)*100)
}

#Check the data
#dbListFields(con, "hcaf_species_native") #know which columns are inside
all_native_range = dbGetQuery(con, "select * from hcaf_species_native") %>% collect()

# 2. Table creation: All info per cell ------

# drop_table_query <- paste("DROP TABLE IF EXISTS", "hcaf_species_native")
# dbExecute(con, drop_table_query)

#Table creation query
create_table_query <- "CREATE TABLE hcaf_r (ID TEXT DEFAULT NULL,CsquareCode TEXT DEFAULT NULL,LOICZID TEXT DEFAULT NULL,NLimit TEXT DEFAULT NULL,Slimit TEXT DEFAULT NULL,WLimit TEXT DEFAULT NULL,ELimit TEXT DEFAULT NULL,CenterLat TEXT DEFAULT NULL,CenterLong TEXT DEFAULT NULL,CellArea TEXT DEFAULT NULL,
                        OceanArea TEXT DEFAULT NULL,PWater TEXT DEFAULT NULL,ClimZoneCode TEXT DEFAULT NULL,FAOAreaM TEXT DEFAULT NULL,FAOAreaIn TEXT DEFAULT NULL,CountryMain TEXT DEFAULT NULL,CountrySecond TEXT DEFAULT NULL,CountryThird TEXT DEFAULT NULL,CountrySubMain TEXT DEFAULT NULL,CountrySubSecond TEXT DEFAULT NULL,
                        CountrySubThird TEXT DEFAULT NULL,EEZ TEXT DEFAULT NULL,LME TEXT DEFAULT NULL,LMEBorder TEXT DEFAULT NULL,MEOW TEXT DEFAULT NULL,OceanBasin TEXT DEFAULT NULL,IslandsNo TEXT DEFAULT NULL,Area0_20 TEXT DEFAULT NULL,Area20_40 TEXT DEFAULT NULL,Area40_60 TEXT DEFAULT NULL,Area60_80 TEXT DEFAULT NULL,
                        Area80_100 TEXT DEFAULT NULL,AreaBelow100 TEXT DEFAULT NULL,ElevationMin TEXT DEFAULT NULL,ElevationMax TEXT DEFAULT NULL,ElevationMean TEXT DEFAULT NULL,ElevationSD TEXT DEFAULT NULL,DepthMin TEXT DEFAULT NULL,DepthMax TEXT DEFAULT NULL,DepthMean TEXT DEFAULT NULL,DepthSD TEXT DEFAULT NULL,SSTAnMean TEXT DEFAULT NULL,
                        SBTAnMean TEXT DEFAULT NULL,SalinityMean TEXT DEFAULT NULL,SalinityBMean TEXT DEFAULT NULL,PrimProdMean TEXT DEFAULT NULL,IceConAnn TEXT DEFAULT NULL,OxyMean TEXT DEFAULT NULL,OxyBMean TEXT DEFAULT NULL,LandDist TEXT DEFAULT NULL,Shelf TEXT DEFAULT NULL,Slope TEXT DEFAULT NULL,Abyssal TEXT DEFAULT NULL,
                        TidalRange TEXT DEFAULT NULL,Coral TEXT DEFAULT NULL,Estuary TEXT DEFAULT NULL,Seamount TEXT DEFAULT NULL,MPA TEXT DEFAULT NULL);"

#Execute the query to create the table
dbExecute(con, create_table_query)

#Fill the new database with the values
sql_file <- "./aquamapsdata/hcaf.sql"  # Replace with the path to your .sql file

#sql_commands <- read.delim(sql_file, header = F, sep = "\t", row.names = NULL)
sql_commands = readLines(sql_file,  warn = F, encoding = "UTF-8")

insert_query <- unlist(strsplit(paste(sql_commands, sep = "", collapse = "\n"), ";"))

for (i in 1:length(insert_query)) {
  tryCatch({dbExecute(con, insert_query[i])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  #print(i)
  print(i/length(insert_query)*100)
}

#Check the data
#dbListFields(con, "hcaf_species_native") #know which columns are inside
hcaf = dbGetQuery(con, "select * from hcaf_r") %>% collect()

# 3. Table creation: Envelope for fish ---------------------------------------

create_table_query <- "CREATE TABLE hspen_r (SpeciesID TEXT DEFAULT NULL,Speccode TEXT DEFAULT NULL,LifeStage TEXT DEFAULT NULL,FAOAreas TEXT DEFAULT NULL,FAOComplete TEXT DEFAULT NULL,NMostLat TEXT DEFAULT NULL,SMostLat TEXT DEFAULT NULL,WMostLong TEXT DEFAULT NULL,EMostLong TEXT DEFAULT NULL,BB_inc TEXT DEFAULT NULL,LME TEXT DEFAULT NULL,DepthYN TEXT DEFAULT NULL,
                        DepthMin TEXT DEFAULT NULL,DepthPrefMin TEXT DEFAULT NULL,DepthPrefMax TEXT DEFAULT NULL,DepthMax TEXT DEFAULT NULL,MeanDepth TEXT DEFAULT NULL,Pelagic TEXT DEFAULT NULL,TempYN TEXT DEFAULT NULL,TempMin TEXT DEFAULT NULL,TempPrefMin TEXT DEFAULT NULL,TempPrefMax TEXT DEFAULT NULL,TempMax TEXT DEFAULT NULL,SalinityYN TEXT DEFAULT NULL,SalinityMin TEXT DEFAULT NULL,
                        SalinityPrefMin TEXT DEFAULT NULL,SalinityPrefMax TEXT DEFAULT NULL,SalinityMax TEXT DEFAULT NULL,PrimProdYN TEXT DEFAULT NULL,PrimProdMin TEXT DEFAULT NULL,PrimProdPrefMin TEXT DEFAULT NULL,PrimProdPrefMax TEXT DEFAULT NULL,PrimProdMax TEXT DEFAULT NULL,IceConYN TEXT DEFAULT NULL,IceConMin TEXT DEFAULT NULL,IceConPrefMin TEXT DEFAULT NULL,IceConPrefMax TEXT DEFAULT NULL,
                        IceConMax TEXT DEFAULT NULL,OxyYN TEXT DEFAULT NULL,OxyMin TEXT DEFAULT NULL,OxyPrefMin TEXT DEFAULT NULL,OxyPrefMax TEXT DEFAULT NULL,OxyMax TEXT DEFAULT NULL,LandDistYN TEXT DEFAULT NULL,LandDistMin TEXT DEFAULT NULL,LandDistPrefMin TEXT DEFAULT NULL,LandDistPrefMax TEXT DEFAULT NULL,LandDistMax TEXT DEFAULT NULL,Remark TEXT DEFAULT NULL,DateCreated TEXT DEFAULT NULL,
                        DateModified TEXT DEFAULT NULL,Expert TEXT DEFAULT NULL,DateExpert TEXT DEFAULT NULL,Envelope TEXT DEFAULT NULL,MapData TEXT DEFAULT NULL,Effort TEXT DEFAULT NULL,Layer TEXT DEFAULT NULL,UsePoints TEXT DEFAULT NULL,`Rank` TEXT DEFAULT NULL,MapOpt TEXT DEFAULT NULL,ExtnRuleYN TEXT DEFAULT NULL,IPCC TEXT DEFAULT NULL,Reviewed TEXT DEFAULT NULL);"


#Execute the query to create the table
dbExecute(con, create_table_query)
dbListTables(con_first) 

#Fill the new database with the values
sql_file <- "./aquamapsdata/hspen_fish.sql"  # Replace with the path to your .sql file

#sql_commands <- read.delim(sql_file, header = F, sep = "\t", row.names = NULL)
sql_commands = readLines(sql_file,  warn = F, encoding = "UTF-8")

insert_query <- unlist(strsplit(paste(sql_commands, sep = "", collapse = "\n"), ";"))

for (i in 1:length(insert_query)) {
  tryCatch({dbExecute(con, insert_query[i])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  #print(i)
  print(i/length(insert_query)*100)
}

#Check the data
#dbListFields(con, "hcaf_species_native") #know which columns are inside
hspen = dbGetQuery(con, "select * from hspen_r") %>% collect()

# 4. Table creation: Summary of species occurrences ---------------------------------------

#Delete table if existing
# drop_table_query <- paste("DROP TABLE IF EXISTS", "speciesoccursum_r")
# dbExecute(con, drop_table_query)

#Creation table query
create_table_query <- "CREATE TABLE speciesoccursum_r (
    SpeciesID TEXT DEFAULT NULL, AphiaID TEXT DEFAULT NULL, SpeciesID_SLB TEXT DEFAULT NULL,
    SPECIESID_2016 TEXT DEFAULT NULL, SpeciesID_2016a TEXT DEFAULT NULL,
    Reviewed TEXT DEFAULT NULL, expert_id TEXT DEFAULT NULL, SpecCode TEXT DEFAULT NULL,
    Genus TEXT DEFAULT NULL, Species TEXT DEFAULT NULL, FBname TEXT DEFAULT NULL,
    OccurRecs TEXT DEFAULT NULL, OccurCells TEXT DEFAULT NULL, StockDefs TEXT DEFAULT NULL,
    Kingdom TEXT DEFAULT NULL, Phylum TEXT DEFAULT NULL, Class TEXT DEFAULT NULL,
    `Order` TEXT DEFAULT NULL, Family TEXT DEFAULT NULL, map_beforeafter TEXT DEFAULT NULL,
    map_seasonal TEXT DEFAULT NULL, with_gte_5 TEXT DEFAULT NULL, with_gte_6 TEXT DEFAULT NULL,
    with_gt_66 TEXT DEFAULT NULL, no_of_cells_3 TEXT DEFAULT NULL, no_of_cells_5 TEXT DEFAULT NULL,
    no_of_cells_0 TEXT DEFAULT NULL, database_id TEXT DEFAULT NULL, picname TEXT DEFAULT NULL,
    authname TEXT DEFAULT NULL, entered TEXT DEFAULT NULL, total_native_csc_cnt TEXT DEFAULT NULL,
    deepwater TEXT DEFAULT NULL, m_mammals TEXT DEFAULT NULL, angling TEXT DEFAULT NULL,
    diving TEXT DEFAULT NULL, dangerous TEXT DEFAULT NULL, m_invertebrates TEXT DEFAULT NULL,
    algae TEXT DEFAULT NULL, seabirds TEXT DEFAULT NULL, freshwater TEXT DEFAULT NULL,
    highseas TEXT DEFAULT NULL, countriesintroduced TEXT DEFAULT NULL, invasive TEXT DEFAULT NULL,
    resilience TEXT DEFAULT NULL, iucn_id TEXT DEFAULT NULL, iucn_code TEXT DEFAULT NULL,
    iucn_version TEXT DEFAULT NULL, provider TEXT DEFAULT NULL, no_default TEXT DEFAULT NULL);"


#Execute the query to create the table
dbExecute(con, create_table_query)
dbListTables(con) 

#Fill the new database with the values
sql_fdile <- "./aquamapsdata/speciesoccursum_fish.sql"  # Replace with the path to your .sql file

#sql_commands <- read.delim(sql_file, header = F, sep = "\t", row.names = NULL)
sql_commands = readLines(sql_file,  warn = F, encoding = "UTF-8")

sql_commands = paste(sql_commands, sep = "", collapse = "\n")

insert_query <- unlist(strsplit(sql_commands, "INSERT", fixed = T)); #insert_query[1200]
trial = substr(insert_query, 1, 10); all(trial == trial)

for (i in 1:length(insert_query)) {
  tryCatch({dbExecute(con, paste("INSERT", insert_query[i], sep = ""))}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  #print(i)
  print(i/length(insert_query)*100)
}

#Check the data
#dbListFields(con, "hcaf_species_native") #know which columns are inside
species_occur_sum = dbGetQuery(con, "select * from speciesoccursum_r") %>% collect()

# 5. NEW table creation: Native range of NIS ---------------------------------------

#Delete table if existing
# drop_table_query <- paste("DROP TABLE IF EXISTS", "speciesoccursum_r")
# dbExecute(con, drop_table_query)

#Creation table query
create_table_query <- "CREATE TABLE `NIS_native_range`(                                              
 `SpeciesID` varchar(50) DEFAULT NULL,                                         
`CsquareCode` varchar(10) DEFAULT NULL,                                          
`CenterLat` double DEFAULT NULL,                                                
`CenterLong` double DEFAULT NULL,                                                
`Probability` float DEFAULT NULL,                                              
`FAOAreaYN` tinyint(1) DEFAULT NULL,                                             
`BoundBoxYN` tinyint(1) DEFAULT NULL                                             
);"

#Execute the query to create the table
dbExecute(con, create_table_query)

#Fill the new database with the values
sql_file <- "./aquamapsdata/hcaf_species_native_NIS_list.sql"  # Replace with the path to your .sql file

#sql_commands <- read.delim(sql_file, header = F, sep = "\t", row.names = NULL)
sql_commands = readLines(sql_file,  warn = F, encoding = "UTF-8")

insert_query <- unlist(strsplit(paste(sql_commands, sep = "", collapse = "\n"), ";")); insert_query[12]
trial = substr(insert_query, 1, 10); all(trial == trial)

for (i in 1:length(insert_query)) {
  tryCatch({dbExecute(con, insert_query[i])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  print(i)
  print(i/length(insert_query)*100)
}

NIS_nat_range = dbGetQuery(con, "select * from NIS_native_range") %>% collect()

# 6. NEW table creation: Suitable range of NIS ---------------------------------------

#Delete table if existing
# drop_table_query <- paste("DROP TABLE IF EXISTS", "speciesoccursum_r")
# dbExecute(con, drop_table_query)

#Creation table query
create_table_query <- "CREATE TABLE `NIS_suitable_range`(                                              
 `SpeciesID` varchar(50) DEFAULT NULL,                                         
`CsquareCode` varchar(10) DEFAULT NULL,                                          
`CenterLat` double DEFAULT NULL,                                                
`CenterLong` double DEFAULT NULL,                                                
`Probability` float DEFAULT NULL,                                              
`FAOAreaYN` tinyint(1) DEFAULT NULL,                                             
`BoundBoxYN` tinyint(1) DEFAULT NULL                                             
);"

#Execute the query to create the table
dbExecute(con, create_table_query)

#Fill the new database with the values
sql_file <- "./aquamapsdata/hcaf_species_suitable_NIS_list.sql"  # Replace with the path to your .sql file

#sql_commands <- read.delim(sql_file, header = F, sep = "\t", row.names = NULL)
sql_commands = readLines(sql_file,  warn = F, encoding = "UTF-8")

insert_query <- unlist(strsplit(paste(sql_commands, sep = "", collapse = "\n"), ";")); #insert_query[12:13]
trial = substr(insert_query, 1, 10); all(trial == trial)

for (i in 1:length(insert_query)) {
  tryCatch({dbExecute(con, insert_query[i])}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  #print(i)
  print(i/length(insert_query)*100)
}

NIS_suit_range = dbGetQuery(con, "select * from NIS_suitable_range") %>% collect()

# 7. Substitute the tables in the original sql repository (am.db) from the aquamapsdata dependencies ---------------------
dbListTables(con_first)

#Update all the information per cell
dbWriteTable(con_first, "hcaf_r", hcaf, overwrite = TRUE) 
all_cells = as.data.frame(am_hcaf() %>% collect()); identical(hcaf, all_cells) #test that it's working
rm(hcaf, all_cells)

#Update environmental envelopes
dbWriteTable(con_first, "hspen_r", hspen, overwrite = TRUE) #update the species environmental ennvelope
all_env = as.data.frame(am_hspen() %>% collect()); identical(hspen, all_env) #test that it's working
rm(hspen, all_env)

#Update species occurrences
dbWriteTable(con_first, "speciesoccursum_r", species_occur_sum, overwrite = TRUE) #update all the species information

#Check data
all_occur = as.data.frame(am_search_exact() %>% filter(Class %in% c("Actinopterygii", "Holocephali", "Elasmobranchii", 
                                                                    "Myxini", "Cephalaspidomorphi", "Sarcopterygii"))); identical(species_occur_sum, all_occur) #test that it's working
rm(species_occur_sum, all_occur)

