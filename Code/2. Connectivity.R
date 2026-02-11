#######################################################################################
## 2. Obtain the connectivity between source and recipient areas of NIS (Component I) #!
#######################################################################################
remove(list=ls())
#Libraries and functions
source("./Code/Libraries and functions.R")

#1: Obtain a clean list of recipient and native ecoregions -----------------
#This information will be used later, when joining all risk components in the same data frame
#1.1: Recipient regions ----
ecoregions = readOGR("./Data/MEOW/meow_ecos.shp"); ecoregions = as.data.frame(ecoregions)
ecoregions = ecoregions %>% select(ECO_CODE, ECOREGION, PROVINCE, REALM) %>% 
  mutate(REALM = ifelse(REALM == "Tropical Eastern Pacific", "Tropical East Pacific", REALM))

#Invasibility risk matrix from Seebens et al (2013), used to obtain clean names for ecoregions, this is not the port-to-port matrix
risk_inv_ecoreg = read.csv("./Data/Connectivity/matrix_invasion_risk.txt",header=T,dec=".",sep="\t", check.names = FALSE) 
rownames(risk_inv_ecoreg) = risk_inv_ecoreg[,1]; risk_inv_ecoreg[,1] = NULL
risk_inv_ecoreg = risk_inv_ecoreg[1:15,]

#Get a list of the ecoregions' code within the needed realms/provinces -- those which already match
provinces = unique(ecoregions[ecoregions$PROVINCE %in% rownames(risk_inv_ecoreg), "PROVINCE"])
realms = unique(ecoregions[ecoregions$REALM %in% rownames(risk_inv_ecoreg), "REALM"])
regions = unique(c(provinces, realms))

ECO_CODEs_list <- sapply(regions, function(reg){
  
  rlm = unique(ecoregions[ecoregions$REALM %in% reg, "ECO_CODE"])
  
  prov = unique(ecoregions[ecoregions$PROVINCE %in% reg, "ECO_CODE"])
  
  ecoreg = unique(ecoregions[ecoregions$ECOREGION %in% reg, "ECO_CODE"])
  
  ecde = c(rlm, prov, ecoreg); return(ecde)
  
})

#Get a list of the ecoregions' code within the needed realms/provinces -- those which DO NOT match
reg_diff = setdiff(rownames(risk_inv_ecoreg), regions)

reg_diff_list = list ("North East Pacific" =  c("Temperate Northern Pacific"),
                      "South East Pacific" = c("Warm Temperate Southeastern Pacific","Juan Fernández and Desventuradas", 
                                               "Juan Fernandez and Desventuradas",
                                               "Chiloense", "Channels and Fjords of Southern Chile"),
                      "North West Pacific"= c("Temperate Northern Pacific"),
                      "Tropical East Atlantic" = c("West African Transition","Gulf of Guinea","St. Helena and Ascension Islands"),
                      "Tropical West Atlantic" = c("Warm Temperate Northwest Atlantic","Tropical Northwestern Atlantic",
                                                   "North Brazil Shelf","Tropical Southwestern Atlantic"),
                      "South East Atlantic" = c("Temperate Southern Africa"),
                      "South West Atlantic" = c("Warm Temperate Southwestern Atlantic","North Patagonian Gulfs",
                                                "Patagonian Shelf", "Malvinas/Falklands"),
                      "North West Atlantic" = c("Cold Temperate Northwest Atlantic", "West Greenland Shelf", "Northern Grand Banks–Southern Labrador",
                                                "Northern Labrador", "Baffin Bay–Davis Strait", "Hudson Complex", "Lancaster Sound", "High Arctic Archipelago",
                                                "Beaufort–Amundsen–Viscount Melville–Queen Maud"))

ECO_CODEs_diff <- sapply(reg_diff_list, function(reg){
  
  rlm = unique(ecoregions[ecoregions$REALM %in% reg, "ECO_CODE"])
  
  prov = unique(ecoregions[ecoregions$PROVINCE %in% reg, "ECO_CODE"])
  
  ecoreg = unique(ecoregions[ecoregions$ECOREGION %in% reg, "ECO_CODE"])
  
  ecde = c(rlm, prov, ecoreg); return(ecde)
  
})

ECO_CODEs_list = c(ECO_CODEs_list, ECO_CODEs_diff)

#saveRDS(ECO_CODEs_list, file = ".Data/Connectivity/Regions.R")

#1.2: Source regions -----
Aqua_native = as.data.frame(con %>% dplyr::tbl("NIS_native_range")); #Native range for NIS
ecoregions = readOGR("./Data/MEOW/meow_ecos.shp"); ecoregions = as.data.frame(ecoregions) 
fish_filt = read.csv("./Data/fish_sp_filtered.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Fish species with available trait data
NIS_list = fish_filt[fish_filt$SpeciesID %in% unique(Aqua_native$SpeciesID),c("Species", "SpeciesID")]
all_cells = as.data.frame(am_hcaf() %>% collect()) #convert the slq into a data.frame with all the information for each unique cell
all_cells <- all_cells %>% mutate(across(c("ID", 4:12, 26:58), as.numeric))

nat_range = read.csv("./Data/Initial_metrics.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Fish species with available trait data
nat_range = nat_range %>% filter(range == "native")

ALL_nat_regions = list(); 

for (i in 1:nrow(NIS_list)){
  print(i)
  #Obtain the centroids of all native range cells
  nat_cells = nat_range[nat_range$SpeciesID %in% NIS_list$SpeciesID[i], c("CenterLat", "CenterLong")] 
  
  #Obtain the ecoregion where those cells belong
  cell_nat_ecoreg = all_cells %>% filter(paste(CenterLat, CenterLong) %in% paste(nat_cells$CenterLat, nat_cells$CenterLong)) %>% 
    mutate(Species = NIS_list$Species[i], SpeciesID = NIS_list$SpeciesID[i]) %>% 
    select(Species, SpeciesID, MEOW) %>% rename(ECO_CODE = MEOW) 
  
  cell_nat_ecoreg = merge(cell_nat_ecoreg, ecoregions, by = "ECO_CODE") %>% rename(ECO_CODE_nat = ECO_CODE) %>% mutate(nat_region = ECOREGION) %>%
    select(-PROVINCE, -REALM, -ECOREGION) %>%  
    distinct()
  
  regions_NIS_nat = list(unique(cell_nat_ecoreg$nat_region)); names(regions_NIS_nat) = NIS_list$Species[i]
  ALL_nat_regions = append(ALL_nat_regions, regions_NIS_nat)
}

#saveRDS(ALL_nat_regions, file = "./Data/Connectivity/Native_regions.R")

#2: Obtain the connectivity values at diferent levels (ecoregion, province and realm) based on the single port-to-port connections -----------------
port_list = read.csv("./Ports_data/ports_link_list.txt",header=T,dec=".",sep="\t", check.names = FALSE) #List of ports
port_tot_risk = read.csv("./Ports_data/port_total_risk.txt",header=T,dec=".",sep=" ", check.names = FALSE)
colnames(port_tot_risk) = c("Port", "risk")
port_matrix = read.csv("./Ports_data/port-to-port_risk.txt",header=T,dec=".",sep=";", check.names = FALSE)

#Find the ports falling within each ecoregion
ecoregions = read_sf("./MEOW/meow_ecos.shp"); 
ports_spatial = SpatialPointsDataFrame(coords = port_list[,c(3,4)], data = port_list,
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Merge with ecoregion
port_list = port_list %>% mutate(ecoregion = over(ports_spatial, ecoregions[,2])[,1], 
                                 MEOW = over(ports_spatial, ecoregions[,1])[,1],
                                 province = over(ports_spatial, ecoregions[,4])[,1],
                                 realm = over(ports_spatial, ecoregions[,6])[,1])

port_list[is.na(port_list$ecoregion), "ecoregion"] = "Gulf of St. Lawrence - Eastern Scotian Shelf" #Canadian ports in inland waters
port_list[is.na(port_list$MEOW), "MEOW"] = 20037
port_list[is.na(port_list$province), "province"] = "Cold Temperate Northwest Atlantic"
port_list[is.na(port_list$realm), "realm"] = "Temperate Northern Atlantic"
# colnames(port_list)[14:15] = c("ecoregion", "MEOW")
ecoreg_ports = port_list %>% select(Port, ecoregion, MEOW, province, realm)

ecoreg_risk = base::merge(port_matrix, ecoreg_ports, by.x = "port_orig", by.y = "Port", no.dups = T); colnames(ecoreg_risk)[4:7] = c("ecoreg_origin", "MEOW_origin",
                                                                                                                                     "province_orig", "realm_orig")
ecoreg_risk = merge(ecoreg_risk, ecoreg_ports, by.x = "port_dest", by.y = "Port", no.dups = T); colnames(ecoreg_risk)[8:11] = c("ecoreg_dest", "MEOW_dest",
                                                                                                                                "province_dest", "realm_dest")
coln <- c(colnames(port_matrix)); ecoreg_risk <- ecoreg_risk[which(!duplicated(ecoreg_risk[,coln])),]

risk_aggregation = function(risk){agg_risk = 1 - prod(1 - risk); return(agg_risk)}

#Include the man-made structures as increasing the connectivity, 100% of possibilities to invade
#Define the critical ecoregions
ecoregions = as.data.frame(ecoregions) %>% rename(ecoregion = ECOREGION, MEOW = ECO_CODE)
suez_canal = ecoregions[which(ecoregions$ECO_CODE_X %in% c("31","32", "33", "34")), "ecoregion"] #Ecoregions directly connected with the Suez Canal
panama_canal = ecoregions[which(ecoregions$ECO_CODE_X %in% c("65","66", "67", "68")), "ecoregion"] #Ecoregions directly connected with the Panama Canal
kiel_canal = "Baltic Sea"
panama_pacific = ecoregions[which(ecoregions$ECO_CODE_X %in% c("167","168", "169","170")), "ecoregion"]

#Ecoregion risk
ecoreg_risk = as.data.frame(ecoreg_risk %>% group_by(ecoreg_origin, MEOW_origin, ecoreg_dest, MEOW_dest,
                                                     province_orig, province_dest, realm_orig, realm_dest) %>% 
                              summarise_at(vars(risk), .funs = risk_aggregation))

ecoreg_risk_canals = ecoreg_risk %>%  mutate(risk = case_when(province_orig %in% "Red Sea and Gulf of Aden" & ecoreg_dest %in% suez_canal ~ 1,
                                                              ecoreg_origin %in% panama_pacific & ecoreg_dest %in% panama_canal ~ 1,
                                                              ecoreg_origin %in% "North Sea" & ecoreg_dest %in% "Baltic Sea" ~ 1,
                                                              TRUE ~ risk))
#saveRDS(ecoreg_risk_canals, file = "risk_matr_ecoreg_canals.R")

#Province risk
province_risk_canals = ecoreg_risk_canals %>% group_by(province_orig, province_dest, realm_orig, realm_dest) %>% 
  summarise_at(vars(risk), .funs = risk_aggregation)
#saveRDS(province_risk_canals, file = "risk_matr_province_canals.R")

#Realm risk
realm_risk = ecoreg_risk_canals %>% group_by(realm_orig, realm_dest) %>% 
  summarise_at(vars(risk), .funs = risk_aggregation)
#saveRDS(realm_risk, file = "risk_matr_realm.R")

#3: Merge connectivity data with env. suitability, niche overlap and the previously computed metrics at a cell level ------------
#Load data
prob_occur = read.csv("./Data/Initial_metrics.txt",header=T,dec=".",sep="\t", check.names = FALSE);
fish_filt = read.csv("./Data/fish_sp_filtered.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Fish species with available trait data
Aqua_native = as.data.frame(con %>% dplyr::tbl("NIS_native_range")) #Native range for NIS
NIS_list = fish_filt[fish_filt$SpeciesID %in% unique(Aqua_native$SpeciesID),c("Species", "SpeciesID")] #All present NIS species

#Get the list of NIS
ALL_nat_regions = readRDS("./Data/Connectivity/Native_regions.R"); 
ECO_CODEs_list = readRDS("./Data/Connectivity/Regions.R"); 
ecoregions = readOGR("./Data/MEOW/meow_ecos.shp"); ecoregions = as.data.frame(ecoregions) %>% rename(ecoregion = ECOREGION, MEOW = ECO_CODE)

#Connectivity between regions
ecoreg_risk = readRDS("./Data/Connectivity/risk_matr_ecoreg.R")
province_risk = readRDS("./Data/Connectivity/risk_matr_province_canals.R")
realm_risk = readRDS("./Data/Connectivity/risk_matr_realm.R")

#Add the connectivity to the whole database
#Parallelized loop -- it has to be parallelized from the inside!! Way faster
#This loop takes ~30-60 min to run
system.time(prob_occur_inv<- foreach(i = 1:nrow(NIS_list), .packages = c("dplyr","doParallel","foreach"), 
                                     .combine = rbind, .errorhandling = "stop")%do%{
                                       
                                       print(paste("Start:", 
                                                   paste(NIS_list$Species[i], 
                                                         paste(round(i/nrow(NIS_list)*100, digits = 3), "%", sep = ""), sep = " "), sep = " "))                                        
                                       
                                       temp = prob_occur %>% filter(Valid_name %in% NIS_list$Species[i]) %>% mutate(Di_scaled = standr(Di),
                                                                                                                    introd_area = 0, inv_risk = 0)
                                       
                                       
                                       one_sp = foreach(j = 1:nrow(temp), .packages = c("dplyr","doParallel","foreach"), .combine = rbind, .errorhandling = "stop")%dopar%{
                                         
                                         print(j)
                                         
                                         line = temp[j,]
                                         
                                         nat_areas = unlist(ALL_nat_regions[NIS_list$Species[i]]); nat_areas = nat_areas[complete.cases(nat_areas)]
                                         nat_prov = unique(ecoregions[which(ecoregions$ecoregion %in% nat_areas), "PROVINCE"])
                                         nat_realm = unique(ecoregions[which(ecoregions$ecoregion %in% nat_areas), "REALM"])
                                         #nat_areas = Aqua_native %>% filter(Species %in% NIS_list$Species[i]) %>% pull(MEOW)
                                         
                                         line$native_area = paste(nat_areas, collapse = "; ")
                                         
                                         # introd_area = names(ECO_CODEs_list)[which(sapply(ECO_CODEs_list,
                                         #                                                  function(x) temp$MEOW[j] %in% x))]
                                         
                                         introd_area = ecoregions[which(ecoregions$MEOW == line$MEOW), "ecoregion"]
                                         introd_prov = ecoregions[which(ecoregions$MEOW == line$MEOW), "PROVINCE"]
                                         introd_realm = ecoregions[which(ecoregions$MEOW == line$MEOW), "REALM"]
                                         
                                         line$introd_area = ifelse(length(introd_area) == 0, "NO_NAME", introd_area)
                                         
                                         inv_risk = risk_aggregation(risk = ecoreg_risk %>% 
                                                                       filter(ecoreg_origin %in% nat_areas & ecoreg_dest %in% introd_area) %>% pull(risk))
                                         
                                         inv_risk_pr = risk_aggregation(risk = province_risk %>% 
                                                                          filter(province_orig %in% nat_prov & province_dest %in% introd_prov) %>% pull(risk))
                                         
                                         inv_risk_rlm = risk_aggregation(risk = realm_risk %>% 
                                                                           filter(realm_orig %in% nat_realm & realm_dest %in% introd_realm) %>% pull(risk))
                                         
                                         line$inv_risk = ifelse(line$introd_area == "NO_NAME", NA, inv_risk)
                                         
                                         line = line %>% mutate(#inv_risk = ifelse(line$introd_area == "NO_NAME", NA, inv_risk),
                                           inv_risk_pr = ifelse(line$introd_area == "NO_NAME", NA, inv_risk_pr),
                                           inv_risk_rlm = ifelse(line$introd_area == "NO_NAME", NA, inv_risk_rlm))
                                         
                                         #line$inv_risk = ifelse(line$introd_area == "NO_NAME", NA, inv_risk)
                                         
                                         line
                                         
                                       }
                                       
                                       no_name_temp = one_sp %>% filter(introd_area %in% "NO_NAME")
                                       #one_sp = one_sp %>% mutate(inv_risk = ifelse(!introd_area == "NO_NAME" & is.na(inv_risk), 0, inv_risk))
                                       prob = one_sp %>% filter(!introd_area %in% "NO_NAME") #%>% mutate(inv_risk_scaled = standr(inv_risk)) %>% 
                                       #mutate(inv_risk_scaled = ifelse(is.na(inv_risk), 0, inv_risk))#avoid NAs
                                       #prob= rbind(prob_occur_inv, temp); no_name = rbind(no_name, no_name_temp)
                                       
                                       prob
                                       
                                     }); stopImplicitCluster()

#Save the generated data
prob_occur = prob_occur_inv
#write.table(prob_occur_inv, "./Data/Joint_components.txt", sep="\t")