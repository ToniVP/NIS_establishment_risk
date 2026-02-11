###############################################!
## DATA PREPARATION TO OBTAIN THE FIGURES #####!
###############################################
#IMPORTANT: Loading this script is essential for producing all outputs (figures, models and tables)
#1: Load datasets ----------
prob_occur = as.data.frame(read.csv("./Data/NIS_risk_metrics.txt",header=T,dec=".",sep="\t", check.names = FALSE)) 
fish_filt = read.csv("./Data/fish_sp_filtered.txt",header=T,dec=".",sep="\t", check.names = FALSE) 
Aqua_native = as.data.frame(con %>% dplyr::tbl("NIS_native_range"))
NIS_list = fish_filt[fish_filt$SpeciesID %in% unique(Aqua_native$SpeciesID),c("Species", "SpeciesID")]
Aqua_native = merge(Aqua_native, NIS_list, by = c("SpeciesID")) %>% distinct(.)

#2: Create the REALMS and provinces polygons -----------------
ecoregions = readOGR("./Data/MEOW/meow_ecos.shp");

realm = aggregate(ecoregions, by = c("REALM", "RLM_CODE"))
realm@data$id = realm$RLM_CODE
realm.pnt = fortify(realm, RLM_CODE = "id")
sf.rlm <- plyr::join(realm.pnt, realm@data, by="id"); 

#Correct the names, they were messed up with the polygons
sf.rlm$REALM = as.factor(sf.rlm$REALM); 
levels(sf.rlm$REALM) = c("Arctic", "Temperate Northern Atlantic", "Temperate Northern Pacific", "Western Indo-Pacific",
                         "Tropical Eastern Pacific", "Central Indo-Pacific", "Eastern Indo-Pacific", "Temperate Southern Africa",
                         "Tropical Atlantic", "Southern Ocean", "Temperate South America", "Temperate Australasia")

provinces = aggregate(ecoregions, by = c("PROVINCE", "PROV_CODE"))
provinces@data$id =  provinces$PROV_CODE
prov.pnt = fortify(provinces, PROV_CODE = "id")
sf.prov <- plyr::join(prov.pnt, provinces@data, by="id")

#Polygon for the world map
projection = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
worldMap <- st_as_sf(getMap(resolution = "high"))
#worldMap <- st_transform(worldMap, projection)
worldMap <- sf:::as_Spatial(worldMap)

#3: Create the color gradients used in the plots ----------
#Realms
ecoregions = as.data.frame(ecoregions)
#show_col(myColors)
realm_colors = c(hue_pal(l = 30)(12)); names(realm_colors) = unique(prob_occur$REALM)
realm_fill = c(hue_pal(l = 70)(12)); names(realm_fill) = unique(prob_occur$REALM)

#Effect of components
#show_col(effect_color)
effect_fill = lighten(c("#EEA236FF", "#46B8DAFF", "#D43F3AFF"), amount = 0.2); names(effect_fill) = c("Niche overlap", "Environmental suitability", "Connectivity")
effect_color = darken(c("orange4", "dodgerblue4", "darkred"), amount = 0.4); names(effect_color) = c("Niche overlap", "Environmental suitability", "Connectivity")

#Risk categories for NIS
#show_col(effect_color)
risk_colors = darken(c("darkred", "darkorange3", "darkgoldenrod4", "gold2"), amount = 0.3) ; names(risk_colors) = c("High", "Moderate-high", "Low-moderate", "Low")
risk_fill = c("darksalmon", "peachpuff", "lightgoldenrod1", "lemonchiffon"); names(risk_fill) = c("High", "Moderate-high", "Low-moderate", "Low")

risk_points = c("red3", "orange2", "gold", "yellow"); names(risk_points) = c("High", "Moderate-high", "Low-moderate", "Low")

#4: Common color palette and theme for the maps ---------------------
theme_map = 
  theme_minimal() + 
  theme(
    legend.position="bottom",
    legend.margin=margin(0,0,0,0),
    legend.key.height= unit(0.50, 'cm'),
    legend.key.width= unit(2, 'cm'),
    legend.text=element_text(size=20),
    legend.background = element_rect(fill = "#FFFFFF", color = NA) ,
    legend.title = element_text(size = 25),
    legend.justification = "center",
    text = element_text(color = "#22211d"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, margin=margin(0,0,-15,0), size = 18),
    panel.grid.major = element_line(color = "white", linewidth = 0.2),
    panel.grid.minor = element_line(color = "white", linewidth = 0.2),
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.border = element_blank())

#5: Fill the non-present cells in the dataframe with 0s or NAs ------------
#Information for all cells
all_cells = as.data.frame(am_hcaf() %>% collect()) #convert the slq into a data.frame with all the information for each unique cell

#Convert needed columns to numeric
str(all_cells)
colnames(all_cells)
all_cells <- all_cells %>% mutate(across(c("ID", 4:12, 26:58), as.numeric))

cell_list = unique(prob_occur$CsquareCode)

cells_in_realm = all_cells %>% filter(!is.na(MEOW)) %>% dplyr::select(CsquareCode, CenterLong, CenterLat, MPA) %>% 
  mutate(risk = 0)


