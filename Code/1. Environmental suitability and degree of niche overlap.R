#############################################################################
## 1. Obtention of environmental suitability (Component II) and degree of  ##
##    niche overlap (Component III) for all NIS in each cell worldwide     ##!
#############################################################################
remove(list=ls())
#Libraries and functions
source("./Code/Libraries and functions.R")

#1: Data preparation ----------
#1.1: Load and store all the information for half-degree cells worldwide, as well as fish species names from AquaMaps ----------------
#All fish present in AquaMaps
all_fish <- as.data.frame(am_search_exact() %>% filter(Class %in% c("Actinopterygii", "Holocephali", "Elasmobranchii", 
                                                                    "Myxini", "Cephalaspidomorphi", "Sarcopterygii"))) 

all_fish$Species = paste(all_fish$Genus, all_fish$Species, sep = " ")

#Convert needed columns to numeric
str(all_fish)
colnames(all_fish)
all_fish <- all_fish %>% mutate(across(c("expert_id","OccurRecs","OccurCells", 33:45), as.numeric))

#Information for all cells
all_cells = as.data.frame(am_hcaf() %>% collect()) #convert the slq into a data.frame with all the information for each unique cell

#Convert needed columns to numeric
str(all_cells)
colnames(all_cells)
all_cells <- all_cells %>% mutate(across(c("ID", 4:12, 26:58), as.numeric))
#write.table(all_cells, "./Data/All_cells_AquaMaps.txt", row.names = T,  sep="\t")

# #Environmental envelope of all fishes
# all_env = as.data.frame(am_hspen() %>% collect())
# #Convert needed columns to numeric
# str(all_env)
# colnames(all_env)
# all_env <- all_env %>% mutate(across(c(6:9, 12:48, 59), as.numeric))

#1.2: Get the overall pairwise distances matrix between all fishes ------------
#Get a list with all fish of interest
species_tax_hab = merge(rfishbase::load_taxa(), species(all_fish$Species, fields = c("SpecCode",
                                                                                     "Genus",
                                                                                     "Species", "Fresh",
                                                                                     "Brack",
                                                                                     "Saltwater",
                                                                                     "DemersPelag",
                                                                                     "AnaCat")), by = c("SpecCode", "Species", "Genus"))

#Merge all fish from AquaMaps with the habitat characteristics from FishBase
species_tax_hab = merge(species_tax_hab %>% select(-Family, -Order, -Class, -SuperClass, -Subfamily),
                        all_fish %>% select(1:2, 8:10, 17:19),
                        by = c("SpecCode", "Species", "Genus")) %>% select(9,1,2:3, 10:13, 4:8)


#Filter species by habitat -- remove those strictly from freshwater and deeper waters
fish_filt <- species_tax_hab %>% dplyr::filter(Brack == 1 & Saltwater == 1 | Brack == 0 & Saltwater == 1,
                                               !DemersPelag %in% c("bathypelagic", "bathydemersal"))
#write.table(fish_filt, file = "./Data/fish_sp_filtered.txt", row.names = T,  sep="\t")

#Get the traits for all the potentially present species
traitsFULL<-read.csv("./Data/Final_traits_num.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Full cleaned traits data
head(traitsFULL); traitsFULL <- traitsFULL%>%mutate_if(is.character, as.factor)

#Check which species do not have trait information
trait = traitsFULL[rownames(traitsFULL) %in% unique(fish_filt$Species),]

non_found <- setdiff(unique(fish_filt$Species), rownames(trait))
#which(rownames(traitsFULL) == "Gadus")
non_found_genus <- strsplit(non_found, " "); non_found_genus <- unlist(lapply(non_found_genus, function(x) x[1]))
non_found <- traitsFULL[rownames(traitsFULL) %in% non_found_genus,]; 

trait = rbind(trait, non_found)

#Remove those traits which are not needed in the computation
colnames(trait)
trait = trait %>% select(-temperature, -length_infinity, -natural_mortality, -weight_infinity, 
                         -max_body_depth, -max_body_width, -lower_jaw_length, -min_caudal_pedoncule_depth)
#write.table(trait, "./Data/All_traits_clean.txt", row.names = T,  sep="\t")

#Load clean trait data and compute the global pairwise dissimilarity matrix
trait = read.csv("./Data/All_traits_clean.txt",header=T,dec=".",sep="\t", check.names = FALSE)

#This function takes a long time to run (~3 days), the use of an HPC cluster is recommended
dist_matrix <- as.matrix(gawdis(trait, w.type = "optimized", opti.maxiter = 300,
                                silent = T,
                                groups = c(1,2,3,4,5,6,7,8,9,
                                           10,10,10,
                                           11,11,11,11,11,
                                           12,12,12,
                                           13,13,13,13,13), fuzzy = c(10,11,12,13)))

#saveRDS(dist_matrix, file = "./Data/dist_matrix_gawdis.R")

#2: Obtain the environmental suitability (Component II) and degree of niche overlap (Component III) for the potential range of NIS --------------------
#2.1: Load all needed data ----------
all_cells = read.csv("./Data/All_cells_AquaMaps.txt",header=T,dec=".",sep="\t", check.names = FALSE)

#Load all the needed datasets
NIS_ASH = as.data.frame(con %>% dplyr::tbl("NIS_suitable_range")); #str(NIS_ASH) #Suitable range for NIS
Aqua_native = as.data.frame(con %>% dplyr::tbl("NIS_native_range")); #str(Aqua_native) #Native range for NIS
needed_cells = unique(NIS_ASH$CsquareCode)

#Takes 1-2min to load
all_sp_native = con %>% dplyr::tbl("hcaf_species_native") %>% 
  dplyr::filter(CsquareCode %in% needed_cells, Probability >= 0.75) %>%  collect(); rm(needed_cells) #Native range for all fish species

dist_matrix = as.data.frame(readRDS("./Data/dist_matrix_gawdis.R")) #Pairwise functional distances
fish_filt = read.csv("./Data/fish_sp_filtered.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Fish species with available trait data

#Get the list of NIS to analyze
NIS_list = fish_filt[fish_filt$SpeciesID %in% unique(Aqua_native$SpeciesID),c("Species", "SpeciesID")] #All present NIS species

#2.2: Calculation of Components II & III plus other metrics in each cell - BIG LOOP -------
# Takes a substantial time to run entirely (~5 days), it is recommended to use an HPC cluster. Time is not exact as it depends
# on the number of species present in each cell. 

# Approximate running times:
# ~30 sec x each 100 observations; 
# ~2 min to obtain distinctiveness x 50 cells
# ~72 min to obtain distinctiveness x 4025 cells

# The final output is a list with a dataframe for each species which includes information about the cell itself, but also: 
# 1: Environmental suitability of potential range of each NIS within each half-degree cell
# 2: Degree of niche overlap (i.e. functional distinctiveness) with the potential native community in each half-degree cell
# 3: Native richness
# 4: Surface of that cell falling within an MPA

# START LOOP METRICS PER CELL
system.time(data_list <- foreach(i = 1:nrow(NIS_list), 
                                 .packages = c("dplyr", "vegan","stringr","mFD", "DBI", "RSQLite"), 
                                 .errorhandling = "pass")%do%{ # START LOOP METRICS X CELL
                                   
                                   print(paste("Start:", 
                                               paste(NIS_list$Species[i], 
                                                     paste(i/nrow(NIS_list)*100, "%", sep = ""), sep = " "), sep = " "))
                                   
                                   nat_cells = Aqua_native[Aqua_native$SpeciesID %in% NIS_list$SpeciesID[i], c("CenterLat", "CenterLong")] #Obtain the centroids of all native range cells
                                   
                                   ASH_cells = NIS_ASH[NIS_ASH$SpeciesID %in% NIS_list$SpeciesID[i],] %>% 
                                     dplyr::filter(!paste(CenterLat, CenterLong) %in% paste(nat_cells$CenterLat, nat_cells$CenterLong)) #Obtain the centroids of all suitable cells outside native range
                                   
                                   prob_occur = ASH_cells %>% select(-FAOAreaYN, -BoundBoxYN) %>% 
                                     mutate(uniq_coords = paste(CenterLat, CenterLong), Valid_name = NIS_list$Species[i]) #%>% filter(Probability > 0.5)
                                   
                                   #key <- am_search_fuzzy(NIS_list$Species[i])$key #Get the NIS identifier
                                   
                                   key = NIS_list$SpeciesID[i]
                                   
                                   #NIS_env = all_env %>% filter(SpeciesID == key) # Obtain the environmental envelope of this NIS to filter unsuitable areas
                                   
                                   ASH_cells_info = all_cells %>% filter(paste(CenterLat, CenterLong) %in% paste(ASH_cells$CenterLat, ASH_cells$CenterLong)) %>% 
                                     select(1:2, 4:9, 23:25, 38:46, 48:49, 51, 58) %>% #Select columns of interest, environmental variables, location, MPA %...
                                     mutate(uniq_coords = paste(CenterLat, CenterLong))
                                   
                                   prob_occur = merge(prob_occur, ASH_cells_info, by = c("uniq_coords", "CenterLat","CenterLong", "CsquareCode")) %>% 
                                     select(7,5:6,4,8,1:3,9:28) #%>% mutate(Di = 0); 
                                   
                                   prob_occur = prob_occur[sample(10),]
                                   
                                   cells_sp = prob_occur$CsquareCode
                                   
                                   print("Species & Metrics x cell")
                                   #The most efficient thing to do is to obtain a list of all species x cell and then compute the metrics in parallel
                                   
                                   system.time(metrics <- foreach(j = 1:length(cells_sp),
                                                                  .packages = c("dplyr", "vegan","stringr","mFD", "DBI", "RSQLite"), 
                                                                  .errorhandling = "pass")%do%{
                                                                    
                                                                    #write the connection to the local database for each core in the case that you 
                                                                    #run the computation in parallel whitin an HPC cluster               
                                                                    
                                                                    tryCatch({
                                                                      
                                                                      cell = cells_sp[j]
                                                                      sp_in_cell = all_sp_native %>% dplyr::filter(CsquareCode %in% cell) %>% dplyr::pull(SpeciesID)
                                                                      
                                                                      #Extract the list of species occuring within the same cells as our species of interest
                                                                      #sp_in_cell = sp_cell_list[[j]] #most likely species to co-occur with NIS in this cell
                                                                      
                                                                      #Bind the NIS to that list to compute niche overlap with potential native species
                                                                      fish_in_cell = c(sp_in_cell, key)
                                                                      
                                                                      #Get the fish names corresponding to those IDs
                                                                      fish_in_cell = fish_filt %>% dplyr::filter(SpeciesID %in% fish_in_cell) %>% dplyr::select(1:7)
                                                                      
                                                                      if(nrow(fish_in_cell) > 1){
                                                                        sp_in_dist_matr <- rownames(dist_matrix[rownames(dist_matrix) %in% fish_in_cell$Species,
                                                                                                                colnames(dist_matrix) %in% fish_in_cell$Species]);
                                                                        
                                                                        non_found <- setdiff(fish_in_cell$Species, sp_in_dist_matr)
                                                                        
                                                                        non_found_genus <- strsplit(non_found, " "); non_found_genus <- unique(unlist(lapply(non_found_genus, function(x) x[1])))
                                                                        
                                                                        #Compute functional distinctiveness in this specific cell based on the global functional distances matrix
                                                                        dist_matrix_cell <- as.data.frame(dist_matrix[rownames(dist_matrix) %in% c(fish_in_cell$Species, non_found_genus),
                                                                                                                      colnames(dist_matrix) %in% c(fish_in_cell$Species, non_found_genus)]);
                                                                        
                                                                        
                                                                        sp <- fish_in_cell[!fish_in_cell$Species %in% NIS_list$Species[i], "Species"]; #total richness within cell
                                                                        
                                                                        sp_traits = rownames(dist_matrix_cell %>% filter(!rownames(.) %in% NIS_list$Species[i]))
                                                                        
                                                                        dis <- unlist(sapply(sp_traits, function(x) dist_matrix_cell[NIS_list$Species[i], x]))
                                                                        
                                                                        disW <- sum(dis); DiW <- disW/length(sp_traits); #prob_occur$Di[j] = DiW
                                                                        
                                                                        #Compute functional distinctiveness scaled within this specific cell based on the global functional distances matrix
                                                                        dist_matrix_cell_st= standr(dist_matrix_cell)
                                                                        
                                                                        dis_st <- unlist(sapply(sp_traits, function(x) dist_matrix_cell_st[NIS_list$Species[i], x]))
                                                                        
                                                                        disW_st <- sum(dis_st); Di_st <- disW_st/length(sp_traits); #prob_occur$Di[j] = DiW
                                                                        
                                                                        
                                                                        # Obtain a "fake" presence/absence matrix for each cell
                                                                        traitcomm = dist_matrix_cell %>% dplyr::filter(!rownames(.) %in% NIS_list$Species[i]) %>%
                                                                          select(-NIS_list$Species[i]); 
                                                                        
                                                                        if(ncol(traitcomm) <= 1){traitcomm[,] = 1; rownames(traitcomm) = cells_sp[j]} else {traitcomm = as.data.frame(traitcomm[1,]); traitcomm[,] = 1; rownames(traitcomm) = cells_sp[j]}
                                                                        
                                                                        sp_absent = as.data.frame(dist_matrix[1, which(!colnames(dist_matrix) %in% colnames(traitcomm))]); sp_absent[,] = 0
                                                                        
                                                                        traitcomm = cbind(traitcomm, sp_absent) #get an expanded matrix with only the species presences within that cell
                                                                        
                                                                        data_x_cell = data.frame(cbind(SpeciesID = prob_occur$SpeciesID[j], Valid_name = prob_occur$Valid_name[j], CsquareCode = prob_occur$CsquareCode[j],
                                                                                                       uniq_coords = prob_occur$uniq_coords[j], Di = DiW, Di_scaled_cell = Di_st, rich = length(sp), rich_with_trait = length(sp_traits)))
                                                                        
                                                                      } else {
                                                                        
                                                                        data_x_cell = data.frame(cbind(SpeciesID = prob_occur$SpeciesID[j], Valid_name = prob_occur$Valid_name[j], CsquareCode = prob_occur$CsquareCode[j],
                                                                                                       uniq_coords = prob_occur$uniq_coords[j], Di = 0, Di_scaled_cell = 0, rich = 0, rich_with_trait = 0))
                                                                        
                                                                        prova = as.data.frame(dist_matrix[1,]); prova[,] = 0; rownames(prova) = cells_sp[j]
                                                                        
                                                                        
                                                                      }
                                                                      
                                                                    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                                                                    
                                                                    print(paste(NIS_list$Species[i], round(j/length(cells_sp)*100, digits = 2)))
                                                                    
                                                                    list(data_x_cell, traitcomm)
                                                                    
                                                                    #data_x_cell
                                                                    
                                                                  }) 
                                   
                                   #dbDisconnect(con)
                                   
                                   metrics = do.call(bind_rows, lapply(metrics, `[[`, 1 ))
                                   
                                   data_all = merge(prob_occur, metrics, by = c("SpeciesID", "Valid_name", "CsquareCode", "uniq_coords"))
                                   
                                   data_NIS = list(data_all); names(data_NIS) = c("data_all")
                                   
                                   filename = paste("./Data/All_species_metrics/", #Metrics for single species, saved individually
                                                    paste(NIS_list$Species[i], "metrics_occur.R", sep = "_"), sep = "")

                                   saveRDS(data_NIS, file = filename)
                                   
                                   data_NIS
                                   
                                 }) #END LOOP METRICS X CELL

rm(nat_cells, ASH_cells, data_x_cell, prob_occur, data_NIS, data_all, cells_sp)

names(data_list) = NIS_list$Species

#saveRDS(data_list, file="./Data/sp_metrics_occur.R")


#3: Repeat calculation for the native range of NIS, needed for future corrections --------
#3.1: Load all needed data -------
all_cells = read.csv("./Data/All_cells_AquaMaps.txt",header=T,dec=".",sep="\t", check.names = FALSE) #information for all cells
Aqua_native = as.data.frame(con %>% dplyr::tbl("NIS_native_range")); #str(Aqua_native) #Native range for NIS
needed_cells = unique(Aqua_native$CsquareCode)

#Takes 1-2min to load
all_sp_native = con %>% dplyr::tbl("hcaf_species_native") %>%
  dplyr::filter(CsquareCode %in% needed_cells, Probability >= 0.75) %>%  collect(); rm(needed_cells) #Native range for all fish species

dist_matrix = as.data.frame(readRDS("./Data/dist_matrix_gawdis.R")) #Pairwise functional distances
fish_filt = read.csv("./Data/fish_sp_filtered.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Fish species with available trait data

#Get the list of NIS to analyze
NIS_list = fish_filt[fish_filt$SpeciesID %in% unique(Aqua_native$SpeciesID),c("Species", "SpeciesID")] #All present NIS species

#3.2: Calculation of Components II & III for the native range of NIS - BIG LOOP --------------
#Final outputs are the same as before, but for cells whitin the native range of NIS

# Approximate running times:
# ~30 sec x each 100 observations; 
# ~2 min to obtain distinctiveness x 50 cells
# ~72 min to obtain distinctiveness x 4025 cells

# START LOOP METRICS PER CELL
system.time(data_list <- foreach(i = 1:nrow(NIS_list),
                                 .packages = c("dplyr", "vegan","stringr","mFD", "DBI", "RSQLite"),
                                 .errorhandling = "pass")%do%{ # START LOOP METRICS X CELL
                                   
                                   print(paste("Start:", 
                                               paste(NIS_list$Species[i], 
                                                     paste(i/nrow(NIS_list)*100, "%", sep = ""), sep = " "), sep = " "))
                                   
                                   nat_cells = Aqua_native[Aqua_native$SpeciesID %in% NIS_list$SpeciesID[i],] #Obtain the centroids of all cells inside native range
                                   
                                   prob_occur = nat_cells %>% select(-FAOAreaYN, -BoundBoxYN) %>% 
                                     mutate(uniq_coords = paste(CenterLat, CenterLong), Valid_name = NIS_list$Species[i]) #%>% filter(Probability > 0.5)
                                   
                                   key = NIS_list$SpeciesID[i] #Get the NIS identifier
                                    
                                   nat_cells_info = all_cells %>% filter(paste(CenterLat, CenterLong) %in% paste(nat_cells$CenterLat, nat_cells$CenterLong)) %>% 
                                     select(1:2, 4:9, 23:25, 38:46, 48:49, 51, 58) %>% #Select columns of interest, environmental variables, location, MPA %...
                                     mutate(uniq_coords = paste(CenterLat, CenterLong))
                                   
                                   prob_occur = merge(prob_occur, nat_cells_info, by = c("uniq_coords", "CenterLat","CenterLong", "CsquareCode")) %>% 
                                     select(7,5:6,4,8,1:3,9:28) #%>% mutate(Di = 0); 
                                   
                                   # prob_occur = prob_occur[sample(5),]
                                   
                                   cells_sp = prob_occur$CsquareCode
                                   
                                   print("Species & Metrics x cell")
                                   #The most efficient thing to do is to obtain a list of all species x cell and then compute the metrics in parallel
                                   
                                   system.time(metrics <- foreach(j = 1:length(cells_sp),
                                                                  .packages = c("dplyr", "vegan","stringr","mFD", "DBI", "RSQLite"),
                                                                  .errorhandling = "pass")%do%{
                                                                    
                                                                    #write the connection to the local database for each core
                                                                    
                                                                    tryCatch({
                                                                      
                                                                      cell = cells_sp[j]
                                                                      sp_in_cell = all_sp_native %>% dplyr::filter(CsquareCode %in% cell) %>%  dplyr::pull(SpeciesID)
                                                                      
                                                                      #Extract the list of species occuring within the same cells as our species of interest
                                                                      #sp_in_cell = sp_cell_list[[j]] #most likely species to co-occur with NIS in this cell
                                                                      
                                                                      #Bind the NIS to that list to compute niche overlap with potential native species
                                                                      fish_in_cell = c(sp_in_cell, key)
                                                                      
                                                                      #Get the fish names corresponding to those IDs
                                                                      fish_in_cell = fish_filt %>% dplyr::filter(SpeciesID %in% fish_in_cell) %>% dplyr::select(1:7)
                                                                      
                                                                      if(nrow(fish_in_cell) > 1){
                                                                        sp_in_dist_matr <- rownames(dist_matrix[rownames(dist_matrix) %in% fish_in_cell$Species,
                                                                                                                colnames(dist_matrix) %in% fish_in_cell$Species]);
                                                                        
                                                                        non_found <- setdiff(fish_in_cell$Species, sp_in_dist_matr)
                                                                        
                                                                        non_found_genus <- strsplit(non_found, " "); non_found_genus <- unlist(lapply(non_found_genus, function(x) x[1]))
                                                                        
                                                                        #Compute functional distinctiveness in this specific cell based on the global functional distances matrix
                                                                        dist_matrix_cell <- as.data.frame(dist_matrix[rownames(dist_matrix) %in% c(fish_in_cell$Species, non_found_genus),
                                                                                                                      colnames(dist_matrix) %in% c(fish_in_cell$Species, non_found_genus)]);
                                                                        
                                                                        
                                                                        
                                                                        sp <- fish_in_cell[!fish_in_cell$Species %in% NIS_list$Species[i], "Species"];
                                                                        
                                                                        dis <- unlist(sapply(sp, function(x) dist_matrix_cell[NIS_list$Species[i], x]))
                                                                        
                                                                        disW <- sum(dis); DiW <- disW/length(sp); #prob_occur$Di[j] = DiW
                                                                        
                                                                        # Obtain a "fake" presence/absence matrix for each cell
                                                                        traitcomm = dist_matrix_cell %>% dplyr::filter(!rownames(.) %in% NIS_list$Species[i]) %>%
                                                                          select(-NIS_list$Species[i]);

                                                                        if(ncol(traitcomm) <= 1){traitcomm[,] = 1; rownames(traitcomm) = cells_sp[j]} else {traitcomm = as.data.frame(traitcomm[1,]); traitcomm[,] = 1; rownames(traitcomm) = cells_sp[j]}

                                                                        sp_absent = as.data.frame(dist_matrix[1, which(!colnames(dist_matrix) %in% colnames(traitcomm))]); sp_absent[,] = 0

                                                                        traitcomm = cbind(traitcomm, sp_absent) #get an expanded matrix with only the species presences within that cell

                                                                        data_x_cell = data.frame(cbind(SpeciesID = prob_occur$SpeciesID[j], Valid_name = prob_occur$Valid_name[j], CsquareCode = prob_occur$CsquareCode[j],
                                                                                                       uniq_coords = prob_occur$uniq_coords[j], Di = DiW, rich = length(sp_in_cell)))

                                                                      } else {

                                                                        data_x_cell = data.frame(cbind(SpeciesID = prob_occur$SpeciesID[j], Valid_name = prob_occur$Valid_name[j], CsquareCode = prob_occur$CsquareCode[j],
                                                                                                       uniq_coords = prob_occur$uniq_coords[j], Di = 0, rich = 0))

                                                                        prova = as.data.frame(dist_matrix[1,]); prova[,] = 0; rownames(prova) = cells_sp[j]


                                                                      }
                                                                      
                                                                    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                                                                    
                                                                    print(paste(NIS_list$Species[i], round(j/length(cells_sp)*100, digits = 2)))
                                                                    
                                                                    list(data_x_cell)
                                                                    
                                                                    #data_x_cell
                                                                    
                                                                  })
                                   
                                   metrics = do.call(bind_rows, lapply(metrics, `[[`, 1 ))
                                   
                                   data_all = merge(prob_occur, metrics, by = c("SpeciesID", "Valid_name", "CsquareCode", "uniq_coords"))
                                   
                                   data_NIS = list(data_all); names(data_NIS) = c("data_all")
                                   
                                   filename = paste("./Data/All_species_metrics_native/",
                                                    paste(NIS_list$Species[i], "metrics_native.R", sep = "_"), sep = "")
                                   
                                   saveRDS(data_NIS, file = filename)
                                   
                                   data_NIS
                                   
                                 })

rm(nat_cells, ASH_cells, data_x_cell, prob_occur, data_NIS, data_all, cells_sp)

names(data_list) = NIS_list$Species

saveRDS(data_list, file = "./Data/data_list_NIS_native.R")


#4: Store the generated data in the local dataset (i.e. SQL connection) --------
#4.1: All the metrics for the potential suitable range of NIS ---------
sp_data = list.files(path = "./Data/All_species_metrics/", pattern = "_metrics_occur.R", all.files = T)

data_species = lapply(sp_data, function(x){filename = paste("./All_species_metrics/", x, sep = ""); 
file = readRDS(filename); file = file[[1]]
return(file)})

data_species = do.call(bind_rows, data_species) %>% mutate(Di = as.numeric(Di), rich = as.numeric(rich), 
                                                           Di_scaled_cell = as.numeric(Di_scaled_cell), rich_with_trait = as.numeric(rich_with_trait),
                                                           range = "potential") %>% filter(rich > 0)

dbWriteTable(con_first, "NIS_potential_occur_metrics", data_species, overwrite = T) #Gather the NIS suitable range
trial = as.data.frame(con_first %>% dplyr::tbl("NIS_potential_occur_metrics")); dplyr::all_equal(trial, data_species) #Check if that works
rm(trial, data_species)

#4.2: All the metrics for the native range of NIS -------------
sp_data = list.files(path = "./Data/All_species_metrics_native/", pattern = "_metrics_native.R", all.files = T)

data_species = lapply(sp_data, function(x){filename = paste("./Data/All_species_metrics_native/", x, sep = ""); 
file = readRDS(filename); file = file[[1]]
return(file)})

data_species = do.call(bind_rows, data_species) %>% mutate(Di = as.numeric(Di), rich = as.numeric(rich), 
                                                           Di_scaled_cell = as.numeric(Di_scaled_cell), rich_with_trait = as.numeric(rich_with_trait),
                                                           range = "native") %>% filter(rich > 0)

dbWriteTable(con, "NIS_native_metrics", data_species, overwrite = T) #Gather the NIS suitable range
trial = as.data.frame(con %>% dplyr::tbl("NIS_native_metrics")); dplyr::all_equal(trial, data_species) #Check if that works
rm(trial, data_species)

#4.3: Unify and store the potential range metrics with the native range metrics for the NIS --------
potential = as.data.frame(con %>% dplyr::tbl("NIS_potential_occur_metrics"))
native = as.data.frame(con %>% dplyr::tbl("NIS_native_metrics"))
all = rbind(potential, native)

dbWriteTable(con, "NIS_ALL_range_metrics", all, overwrite = T) #Gather the NIS suitable range
trial = as.data.frame(con %>% dplyr::tbl("NIS_ALL_range_metrics")); dplyr::all_equal(trial, all) #Check if that works
rm(trial, all)


#4.4: Correct the data for some species (e.g. native range, introduced...) ----------
#Load data from local source
prob_occur = as.data.frame(con %>% dplyr::tbl("NIS_ALL_range_metrics"));

#List of lessepsian species from WRiMS and literature
#This step was needed because for some NIS, AquaMaps showed as the native or actual range the Mediterranean, therefore were corrected
lessepsian <- c("Alepes djedaba", "Apogonichthyoides pharaonis", "Atherinomorus forskalii", "Callionymus filamentosus", 
                "Cynoglossus sinusarabici", "Decapterus russelli", "Equulites klunzingeri", "Fistularia commersonii", 
                "Hemiramphus far", "Heniochus intermedius", "Herklotsichthys punctatus", "Himantura uarnak", "Crenidens crenidens",
                "Hyporhamphus affinis", "Jaydia smithi", "Lagocephalus guentheri", "Lagocephalus sceleratus", 
                "Lagocephalus spadiceus", "Lagocephalus suezensis", "Lutjanus fulviflamma", "Muraenesox cinereus", "Nemipterus randalli", "Pteragogus pelycus",
                "Parexocoetus mento", "Petroscirtes ancylodon", "Parupeneus forsskali", "Upeneus pori", 
                "Saurida lessepsianus", "Hyporhamphus affinis", "Oxyurichthys papuensis", "Ostorhinchus fasciatus", 
                "Platycephalus indicus", "Plotosus lineatus", "Priacanthus hamrur", 
                "Pterois miles", "Rhabdosargus haffara", "Sargocentron rubrum", "Saurida lessepsianus", 
                "Saurida undosquamis", "Scarus ghobban", "Scomberomorus commerson", "Siganus luridus", "Siganus rivulatus", 
                "Sillago sihama", "Sphyraena chrysotaenia", "Sphyraena flavicauda", "Stephanolepis diaspros", "Terapon puta", 
                "Torquigener flavimaculosus", "Upeneus moluccensis", "Bregmaceros atlanticus")

#Merge the metrics dataset with ecoregions, provinces and realms (Spalding et al 2007)
ecoregions = readOGR("./Data/MEOW/meow_ecos.shp"); 
ecoregions = as.data.frame(ecoregions) %>% select(ECO_CODE, ECOREGION, PROVINCE, REALM) %>% rename(MEOW = ECO_CODE)
prob_occur = merge(prob_occur, ecoregions, by = "MEOW")

prob_occur = prob_occur %>% mutate(range = case_when(Valid_name %in% lessepsian & PROVINCE %in% "Mediterranean Sea" ~ "potential",
                                                     Valid_name %in% "Neogobius melanostomus" & ECOREGION %in% "Baltic Sea" ~ "potential",
                                                     Valid_name %in% c("Stephanolepis diaspros", "Rhinecanthus aculeatus", "Callionymus filamentosus", "Cephalopholis argus") &
                                                       ECOREGION %in% "Arabian (Persian) Gulf" ~ "native", 
                                                     Valid_name %in% c("Cephalopholis argus", "Lutjanus kasmira", "Pomacanthus imperator") & ECOREGION %in% "Hawaii" ~ "potential", 
                                                     Valid_name %in% c("Rhinecanthus aculeatus") & REALM %in% c("Tropical Atlantic", "Temperate Northern Atlantic", "Temperate Southern Africa") ~ "potential",
                                                     Valid_name %in% c("Parupeneus forsskali", "Jaydia smithi") & REALM %in% c("Western Indo-Pacific") ~ "native", 
                                                     Valid_name %in% c("Parupeneus forsskali", "Jaydia smithi") & ECOREGION %in% c("Malacca Strait", "Andaman Sea Coral Coast") ~ "native", 
                                                     Valid_name %in% "Iniistius twistii" & REALM %in% "Western Indo-Pacific" ~ "potential",
                                                     Valid_name %in% "Lagocephalus guentheri" & REALM %in% c("Temperate South America", "Tropical Atlantic") ~ "potential",
                                                     Valid_name %in% c("Pomacanthus annularis") & PROVINCE %in% c("Red Sea and Gulf of Aden", "Somali/Arabian", "Western Indian Ocean") ~ "potential",
                                                     Valid_name %in% c("Pteragogus pelycus") & PROVINCE %in% c("Red Sea and Gulf of Aden", "Somali/Arabian", "Western Indian Ocean") ~ "native",
                                                     TRUE ~ range))

#write.table(prob_occur, "./Data/Initial_metrics.txt", sep="\t")
