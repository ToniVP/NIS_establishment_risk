#########################################################################################################
## 6. Comparison of risk scores with current established ranges of NIS (Figure 6, Figure S5, Table S3) ##!
#########################################################################################################
remove(list=ls())
#Libraries and functions
source("./Code/Libraries and functions.R")
source("./Code/Figures data.R")

#1: Extract current established NIS from WRiMS database ------
#Search criteria: Status: Valid / Origin: Alien / Occurrence: Established and Established and expanding / Taxon Staus: Accepted
#lower or equal to: Gnathostomata / Environment: Marine or brackish, Terrestrial NO / Only accepted names / 19 Jan 2026

estab = read.csv("./Data/WRiMS/NIS_list_WRiMS_established.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Full cleaned traits data
detectwild = read.csv("./Data/WRiMS/NIS_list_WRiMS_detect_wild.txt",header=T,dec=".",sep="\t", check.names = FALSE) %>% select(colnames(estab)) #Full cleaned traits data
estexp = read.csv("./Data/WRiMS/NIS_list_WRiMS_estexp.txt",header=T,dec=".",sep="\t", check.names = FALSE) %>% select(colnames(estab)) #Full cleaned traits data

species = rbind(estab, estexp, detectwild)
head(species);

#modify the localities with the corresponding LME
unique(species$Locality)
mediterranean <- c(unique(str_subset(species$Locality, "Mediterranean Sea", negate = F)),
                   unique(str_subset(species$Locality, "Ionian", negate = F)),
                   unique(str_subset(species$Locality, "Aegean", negate = F)),
                   unique(str_subset(species$Locality, "Adriatic", negate = F)),
                   unique(str_subset(species$Locality, "Tyrrhenian", negate = F)),
                   unique(str_subset(species$Locality, "Levantine", negate = F)),
                   "Israeli Exclusive Economic Zone", "Turkey", "Marmara Sea", "Spanish part of the Balearic Sea",
                   "Western Mediterranean", "Greece, with Cyclades and more islands", "Disputed area: Palestinian Exclusive Economic Zone",
                   "Italy", "Greece", "Italian part of the Ligurian Sea", "Algerian part of the Alboran Sea",
                   "French part of the Ligurian Sea")

caribbean <- c(unique(str_subset(species$Locality, "Caribbean", negate = F)), "Bahamas part of the North Atlantic Ocean",
               "Dominican Republic part of the North Atlantic Ocean")


n_sea <- c(unique(str_subset(species$Locality, "North Sea", negate = F)),
           "Belgian Exclusive Economic Zone")

baltic <- c(unique(str_subset(species$Locality, "Baltic Sea", negate = F)),
            "Finnish Exclusive Economic Zone", "Estonian Exclusive Economic Zone",
            "Estonian part of the Gulf of Finland", "Germany", "Sweden")

celtic <- c("Ireland", "United Kingdom part of the Celtic Sea")


species<- species %>% mutate(Region = case_when(Locality %in% mediterranean ~ "Mediterranean Sea",
                                                Locality %in% caribbean ~ "Caribbean Sea",
                                                Locality %in% n_sea ~ "North Sea",
                                                Locality %in% celtic ~ "Celtic-Biscay Shelf",
                                                Locality %in% baltic ~ "Baltic Sea",
                                                Locality %in% "Chinese part of the Eastern China Sea" ~ "East China Sea",
                                                Locality %in% "Chinese part of the South China Sea" ~ "South China Sea",
                                                Locality %in% "Chinese part of the Yellow Sea" ~ "Yellow Sea",
                                                Locality %in% "Ukrainian part of the Black Sea" ~ "Black Sea",
                                                Locality %in% "Saudi Arabian part of the Red Sea" ~ "Red Sea",
                                                Locality %in% c("United States part of the Gulf of Mexico", "Gulf of Mexico",
                                                                "Mexican part of the Gulf of Mexico") ~ "Gulf of Mexico",
                                                Locality %in% "Iceland" ~ "Iceland Shelf and Sea",
                                                Locality %in% c("Canadian part of the Coastal Waters of Southeast Alaska and British Columbia",
                                                                "Canadian part of the North Pacific Ocean") ~ "Gulf of Alaska",
                                                Locality %in% c("Portuguese Exclusive Economic Zone", "Portuguese part of the North Atlantic Ocean",
                                                                "Spanish part of the North Atlantic Ocean")~ "Iberian Coastal",
                                                Locality %in% "Colombian part of the North Pacific Ocean" ~ "Pacific Central-American Coastal",
                                                Locality %in% "United States part of the North Pacific Ocean" ~ "California Current",
                                                Locality %in% c( "Brazilian part of the South Atlantic Ocean",
                                                                 "Brazilian part of the North Atlantic Ocean") ~ "South Brazil Shelf",
                                                Locality %in% "Faeroe part of the North Atlantic Ocean" ~ "Faroe Plateau"))

#Create a new dataframe with duplicated rows for those localities which fell into multiple LMEs
species_multi <- species %>% dplyr::filter(Locality %in% c("United States part of the North Atlantic Ocean", 
                                                           #"Brazilian part of the South Atlantic Ocean",
                                                           #"Brazilian part of the North Atlantic Ocean",
                                                           #"United Kingdom",
                                                           "Australia",
                                                           "Norwegian Exclusive Economic Zone"
)) %>% 
  rowwise() %>%
  dplyr::mutate(Region = list(case_when(
    Locality == "United States part of the North Atlantic Ocean" ~ c("Northeast US Continental Shelf", "Southeast US Continental Shelf"),
    # Locality == "Brazilian part of the South Atlantic Ocean" ~ c("East Brazil Shelf", "South Brazil Shelf"),
    # Locality == "Brazilian part of the North Atlantic Ocean" ~ c("North Brazil Shelf", "East Brazil Shelf"),
    # Locality == "United Kingdom" ~ c("North Sea", "Celtic-Biscay Shelf"),
    Locality == "Australia" ~ c("East Central Australian Shelf", "Southeast Australian Shelf"),
    Locality == "Norwegian Exclusive Economic Zone" ~ c("Barents Sea", "Norwegian Sea")
  ))) %>%
  tidyr::unnest(Region) %>%
  ungroup() %>%
  mutate(Region = as.character(Region))

#Deal with NAs (freshwater species)
species = rbind(species,species_multi)
species_NA <- species[which(is.na(species$Region)), ]
species <- species[which(!is.na(species$Region)), ]; 
sort(unique(species_NA$Locality))

#Delete strict freshwater species
habitat = species(species$ScientificName, fields = c("SpecCode",
                                                     "Genus",
                                                     "Species", "Fresh",
                                                     "Brack",
                                                     "Saltwater",
                                                     "DemersPelag",
                                                     "AnaCat")) %>% filter(Brack == 1 & Saltwater == 1 | Brack == 0 & Saltwater == 1)

species_WRiMS <- species %>% select(ScientificName, Genus, Family, Order, Region) %>% distinct() %>% filter(ScientificName %in% habitat$Species) %>% 
  mutate(Source = "WRiMS") %>% dplyr::rename("Taxon" = "ScientificName")

LME = read.csv("./Data/WRiMS/LME_ecoregions.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Filtered species list
LME = LME %>% select(Region, Identifyer) %>% distinct(.) 
species_WRiMS = merge(species_WRiMS, LME, by = c("Region"), all.x = T)
NIS_list_wr = species_WRiMS

#2: Compare establishment risk scores with established range of NIS ---------
LME_ecoreg = read.csv("./LME_ecoregions.txt",header=T,dec=".",sep="\t", check.names = FALSE) #Filtered species list
LME_ecoreg = LME_ecoreg %>% select(-ECOREGION, -ECO_CODE)
species_WRiMS = merge(NIS_list_wr, LME_ecoreg, by = c("Region", "Identifyer")) %>% distinct(.)

#Subset the species present in the risk data
NIS_metrics = prob_occur %>% filter(Valid_name %in% unique(species_WRiMS$Taxon), range == "potential", !pathway == "range expansion")
establ_NIS_list = species_WRiMS %>% filter(Taxon %in% unique(NIS_metrics$Valid_name)) %>% select(c("Region", "Identifyer", "Taxon", "PROVINCE", "REALM"))
colnames(establ_NIS_list) = c("Region_establ","Identifyer","Taxon","PROVINCE_establ","REALM_establ")

#See how many overlap is with our risk and the actual introduced area
prop_risk_establ = data.frame(); NIS_check = unique(establ_NIS_list$Taxon); estab_range = data.frame();
most_category = data.frame()
for (i in 1:length(NIS_check)){ #START LOOP COMPARISON
  check_sp = NIS_metrics %>% filter(Valid_name == NIS_check[i]); establ_NIS = establ_NIS_list %>% filter(Taxon == NIS_check[i])
  check_sp = check_sp %>% 
    mutate(category = ntile(risk_scaled, 4),
           category_prod = ntile(risk_prod, 4)) %>% 
    mutate(category = as.factor(case_when(category == 4 ~ "High",
                                          category == 3 ~ "Moderate-high",
                                          category == 2 ~ "Low-moderate",
                                          category == 1 ~ "Low")),
           category_prod = as.factor(case_when(category_prod == 4 ~ "High",
                                               category_prod == 3 ~ "Moderate-high",
                                               category_prod == 2 ~ "Low-moderate",
                                               category_prod == 1 ~ "Low"))) %>% 
    mutate(category=fct_relevel(category,c("High", "Moderate-high", "Low-moderate", "Low")),
           category_prod = fct_relevel(category_prod,c("High", "Moderate-high", "Low-moderate", "Low"))) %>% 
    select(Valid_name, CsquareCode, CenterLong, CenterLat, REALM, PROVINCE, ECOREGION, pathway,
           Di_scaled_cell, Probability, inv_risk_scaled, risk_scaled, risk_scaled_prod, category, category_prod)
  
  prova_sp = check_sp %>% filter(PROVINCE %in% unique(establ_NIS$PROVINCE_establ)) %>% mutate(level = "Province")
  
  if(nrow(prova_sp) == 0){prova_sp = check_sp %>% filter(REALM %in% unique(establ_NIS$REALM_establ)) %>% mutate(level = "Realm")}
  else{prova_sp = check_sp %>% filter(PROVINCE %in% unique(establ_NIS$PROVINCE_establ)) %>% mutate(level = "Province")}
  
  
  proportion <- data.frame(prop.table(table(prova_sp$category)) * 100, check.names = FALSE) %>% rename("category" = "Var1") %>% 
    mutate(Valid_name = NIS_check[i], level = prova_sp$level[1], Region_establ = paste(unique(establ_NIS$Region_establ), collapse = ";"),
           pathway = prova_sp$pathway[1], N_cells = nrow(prova_sp))
  
  sum_comp = prova_sp %>% group_by(Valid_name, category) %>% 
    summarise_at(.vars = c("Probability", "Di_scaled_cell", 
                           "inv_risk_scaled", "risk_scaled", "risk_scaled_prod"), .funs = mean)
  
  proportion = merge(proportion, sum_comp, by = c("Valid_name", "category"), all.x = T)
  max_cat = proportion[which.max(proportion$Freq), c("Valid_name", "category", "Freq")]
  `2nd_max_cat` = proportion %>% filter(Freq == sort(proportion$Freq, decreasing=T)[2]) %>% 
    select( c("category", "Freq")) %>% rename("category_2nd" = "category", "Freq_2nd" = "Freq"); max_cat = cbind(max_cat, `2nd_max_cat`)
  
  if(max_cat$Freq_2nd[1] == 0){max_cat = max_cat[1,]; max_cat[1,c(4,5)] = NA} #deal with duplicate 0s
  
  prop_risk_establ = rbind(prop_risk_establ, proportion); estab_range = rbind(estab_range, prova_sp); most_category = rbind(most_category, max_cat)
} #END LOOP COMPARISON

# Figure S5: Find the proportion of risk categories in established range of NIS -------------
risk_sp = c(lighten("red2", amount = 0.3), "orange2", "gold", "yellow"); names(risk_sp) = c("High", "Moderate-high", "Low-moderate", "Low")

comparison_establ_NIS <- ggplot(prop_risk_establ, aes(x = fct_rev(Valid_name), y = Freq, fill = as.factor(category))) + 
  geom_bar(position="stack", stat = "identity", width = 0.6) +
  #scale_color_manual(values = effect_color) + 
  scale_fill_manual(values = risk_sp) + #facet_wrap(vars(level), strip.position = "bottom") + 
  theme_minimal() + #scale_y_continuous(breaks = c(0.0, 0.25, 0.5, 0.75, 1)) + 
  theme(legend.position = "top", text = element_text(size = 15), 
        #axis.text.x = element_text(angle = 50, hjust = 1),
        axis.text.y = element_text(size = 12, color = "grey15"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  #scale_x_discrete(labels = c(levels(as.factor(sort(prop_risk_establ$Valid_name, decreasing = T))))) + 
  coord_flip()+ 
  labs(x = NULL, y = "Proportion of cells per risk category", fill = "Risk category"); comparison_establ_NIS

# ggsave(file="./Drafts/Submissions/AppEco/Figures/FigS5_NIS_comparison.png", plot= comparison_establ_NIS, width=10, height=11, dpi = 600)

# Figure 6: Find the most dominant risk category in the established range -------------
most_category = tidyr::complete(most_category, category); 
count_category = most_category %>% filter(!is.na(Freq)) %>% 
  group_by(category) %>% tally %>% complete(category); count_category[is.na(count_category)] = 0

count_category = count_category %>% mutate(perc = round((n/sum(n)) * 100, 2));

count_category_2nd = most_category %>% filter(!is.na(Freq_2nd)) %>% 
  group_by(category_2nd) %>% tally %>% complete(category_2nd); count_category_2nd[is.na(count_category_2nd)] = 0

count_category_2nd = count_category_2nd %>% mutate(perc = round((n/sum(n)) * 100, 2));

#Dominant category
summary_establ_NIS <- ggplot(most_category, mapping = aes(x = category, y = Freq, fill = as.factor(category), color = as.factor(category))) + 
  geom_violin(width = 0.5, alpha = 0.6, size = 0.4, color = NA) + 
  geom_boxplot(width = 0.5, alpha=0.4, size = 0.5) +
  scale_color_manual(values = risk_colors, guide = NULL) + ylim(0,100) + 
  scale_fill_manual(values = risk_sp, guide = NULL) + labs(fill = "Risk category") +
  geom_text(count_category,
            mapping = aes(x = category, y = 0, label = n, fontface = 2), size = 5) +
  theme_minimal() + #scale_y_continuous(breaks = c(0.0, 0.25, 0.5, 0.75, 1)) + 
  theme(legend.position = "top", text = element_text(size = 15), 
        #axis.text.x = element_text(angle = 50, hjust = 1),
        axis.text = element_text(size = 12, color = "grey15"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  #coord_flip()+ 
  labs(title = "Dominant risk category in cells from established range (N = 50)", x = NULL, y = NULL); summary_establ_NIS

# ggsave(file="./Drafts/Submissions/AppEco/Figures/Fig6_dominant_category.png", plot= summary_establ_NIS, width=10, height=8, dpi = 600)

# Table S3 -------------
NIS_established <- dcast(prop_risk_establ, Valid_name + Region_establ + level~ category, value.var = "Freq")
colnames(NIS_established) = c("Taxon","Region of establishment (LME)", "Geographic scale", "High", "Moderate-high", "Low-moderate", "Low")

#write.xlsx(NIS_established, './Drafts/Submissions/AppEco/Figures/TableS3_established_NIS_comparison.xlsx', rowNames=TRUE, overwrite = T)
