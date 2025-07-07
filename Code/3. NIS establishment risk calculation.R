#################################################################################################
## 3. Calculation of potential establishment risk of NIS based on the three previous components #!
#################################################################################################
remove(list=ls())
#Libraries and functions
source("./Code/Libraries and functions.R")

#1: Data preparation ----------------------
#1.1: Add other singular characteristics of species (aquaculture, ornamental, Lessepsian...) -----
prob_occur = as.data.frame(read.csv("./Data/Joint_components.txt",header=T,dec=".",sep="\t", check.names = FALSE)) #Invasibility risk matrix
prob_occur$introd_area = NULL

fish_filt = read.csv("./Data/fish_sp_filtered.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Fish species with available trait data
Aqua_native = as.data.frame(con %>% dplyr::tbl("NIS_native_range")) #Native range for NIS
NIS_list = fish_filt[fish_filt$SpeciesID %in% unique(Aqua_native$SpeciesID),c("Species", "SpeciesID")] #All present NIS species
Aqua_native = merge(Aqua_native, NIS_list, by = c("SpeciesID")) %>% distinct(.)

uncert = c("Petroscirtes breviceps", "Pomacanthus annularis", "Lutjanus analis", "Amphiprion ocellaris",
           "Apolemichtys trimaculatus", "Chaetodon ephippium", "Chaetodon lunula", "Chaetodon rafflesii", "Chaetodon vagabundus",
           "Dascyllus aruanus","Dunckerocampus dactyliophorus", "Heniochus singularius","Heniochus varius", "Hippocampus coronatus", "Hippocampus kuda",
           "Lactoria cornuta", "Monodactylus argenteus", "Naso lituratus", "Pampus argenteus", "Paracanthurus hepatus", "Parachaetodon ocellatus",
           "Pomacentrus coelestis", "Pseudanthias pleurotaenia", "Pygoplites diacanthus", "Zebrasoma flavescens", "Bregmaceros atlanticus")

range_exp = c("Conger conger", "Sparisoma cretense", "Knipowitschia caucasica", "Sarpa salpa", "Gobius cruentatus", "Gasterosteus aculeatus", 
              "Seriola fasciata")


#Add uncertainty and range expansion as status
prob_occur = prob_occur %>% mutate(status = case_when(Valid_name %in% range_exp ~ "range expansion",
                                                      Valid_name %in% uncert ~ "uncertain",
                                                      TRUE ~ "introd"))

#Aquaculture, ornamental and lessepsian
aquacult = read.csv("./Data/Aquaculture_fish.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Fish species with available trait data
aquacult = c(aquacult$Valid_name, "Diploprion bifasciatum", "Epinephelus fuscoguttatus", "Micropogonias undulatus", "Parexocoetus mento",
             "Sparus aurata", "Takifugu rubripes", "Oncorhynchus mykiss")

#Lessepsian species
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

#Ornamental species
ornamental <- c("Amphiprion ocellaris", "Acanthurus monroviae", "Apolemichtys trimaculatus", "Acanthurus guttatus", 
                "Chaetodon ephippium", "Chaetodon lunula", "Chaetodon rafflesii", "Chaetodon vagabundus", 
                "Chiloscyllium punctatum", "Dascyllus aruanus", "Dascyllus trimaculatus", "Dunckerocampus dactyliophorus", 
                "Epinephelus ongus", "Heniochus intermedius", "Heniochus singularius", "Heniochus varius", "Hippocampus coronatus", "Lactoria cornuta", 
                "Monodactylus argenteus", "Naso lituratus", "Pampus argenteus", "Paracanthurus hepatus", "Parachaetodon ocellatus", 
                "Pomacanthus annularis", "Pomacanthus imperator", "Pomacentrus coelestis", "Pseudanthias pleurotaenia", "Petroscirtes breviceps",
                "Pterois antennata", "Pterois radiata", "Pterois volitans", "Pygoplites diacanthus", "Rhinecanthus aculeatus", 
                "Zanclus cornutus", "Zebrasoma flavescens", "Zebrasoma scopas", "Hippocampus kuda")

#Intentional fishing release
intentional = c("Alosa sapidissima", "Oncorhynchus mykiss")

#Ballast water
ballast = c("Forsterygion varium", "Neogobius melanostomus", "Forsterygion lapillum", "Acanthogobius flavimanus", "Omobranchus punctatus" )

#Add extra categories
prob_occur = prob_occur %>% mutate(pathway = case_when(Valid_name %in% aquacult ~ "aquaculture",
                                                       Valid_name %in% ornamental ~ "ornamental",
                                                       Valid_name %in% lessepsian ~ "lessepsian",
                                                       Valid_name %in% range_exp ~ "range expansion",
                                                       TRUE ~ "uncertain",),
                                   aquaculture = case_when(Valid_name %in% aquacult ~ "YES",
                                                           TRUE ~ "NO"),
                                   lessepsian = case_when(Valid_name %in% lessepsian ~ "YES",
                                                          TRUE ~ "NO"),
                                   ornamental = case_when(Valid_name %in% ornamental ~ "YES",
                                                          TRUE ~ "NO"))

#write.table(prob_occur, "./Data/Joint_components.txt", sep="\t") #Just in case you would like to save the data in each step

#1.2: Scale the connectivity between 0 and 1 per species ----
#prob_occur = as.data.frame(read.csv("./Data/Joint_components.txt",header=T,dec=".",sep="\t", check.names = FALSE))
standr_inv_risk = data.frame(); #prob_occur = prob_occur %>% filter(rich_with_trait > 10) %>% select(-c(10:27))

for(i in 1:nrow(NIS_list)){
  
  temp = prob_occur %>% filter(Valid_name %in% NIS_list$Species[i] & range == "potential") %>% 
    mutate(inv_risk_scaled = standr(inv_risk_pr),
           rescaled_cell_Di = standr(Di_scaled_cell)) #This was a trial, not used at the end
  
  temp = temp %>% mutate(inv_risk_scaled = ifelse(is.na(inv_risk_scaled),0,inv_risk_scaled)) #avoid NAs
  
  standr_inv_risk = rbind(standr_inv_risk, temp)
  print(i)
  
}

prob_occur = prob_occur %>% filter(range == "native") %>% mutate(inv_risk_scaled = 0, rescaled_cell_Di = 0)
prob_occur = rbind(prob_occur, standr_inv_risk); rm(standr_inv_risk)
#write.table(prob_occur, "./Data/Joint_components.txt", sep="\t")

#2: Compute the establishment risk index -------
#prob_occur = as.data.frame(read.csv("./Data/Joint_components.txt",header=T,dec=".",sep="\t", check.names = FALSE)) #Invasibility risk matrix
risk = data.frame()
for(i in 1:nrow(NIS_list)){
  
  temp = prob_occur %>% filter(Valid_name %in% NIS_list$Species[i] & range == "potential") %>% 
    mutate(risk_sum  = Probability + Di_scaled_cell + inv_risk_scaled,
           risk_prod = Probability*Di_scaled_cell*inv_risk_scaled
    )
  
  #Standardize the obtained risk values
  temp = temp %>% mutate(risk_scaled = standr(risk_sum),
                         risk_scaled_prod = standr(risk_prod)) %>% mutate(risk_scaled_prod = ifelse(is.na(risk_scaled_prod), 0, risk_scaled_prod))#avoid NAs
  
  risk = rbind(risk, temp)
  print(i)
  
}

prob_occur = prob_occur %>% filter(range == "native") %>% mutate(risk_sum = 0, risk_prod = 0, 
                                                                 risk_scaled = 0, risk_scaled_prod = 0)
prob_occur = rbind(prob_occur, risk)
#write.table(prob_occur, "./Data/NIS_risk_metrics.txt", sep="\t")

