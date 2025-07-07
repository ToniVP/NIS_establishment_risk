###########################################################################################
## 4. Figures 1 to 4: Global patterns of establishment risk and components' contribution ##! 
##    Figures S1, S2 & S3                                                                ##!
###########################################################################################
remove(list=ls())
#Libraries and functions
source("./Code/Libraries and functions.R")
source("./Code/Figures data.R")

# Figure 1: Global risk patterns -----------
#Aggregate the risk values per cell
NIS_metrics_global = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>% 
                                     group_by(CsquareCode, CenterLong, CenterLat, REALM, MEOW) %>% 
                                     summarise_at(.vars = c("MPA", "Probability", "rescaled_cell_Di", "Di_scaled_cell", "Di_scaled", "rich", "rich_with_trait","inv_risk",
                                                            "inv_risk_pr", "inv_risk_rlm", "inv_risk_scaled", 
                                                            "risk_sum", "risk_scaled", "risk_prod"), 
                                                  .funs = sum)) 

#Obtain the map
  map_sum <- ggplot() + theme_map +
    scale_fill_gradientn(colours=rev(brewer.pal(5, "Spectral")),
                         values= standr(getJenksBreaks(NIS_metrics_global[,"risk_scaled"], k = 5)), 
                         #breaks= c(0, 0.25, 0.5, 0.75, 1), 
                         na.value = "aliceblue") +
    geom_tile(data = cells_in_realm, aes(x=CenterLong, y=CenterLat), color = "#2B83BA", fill = "#2B83BA") +
    geom_tile(data = NIS_metrics_global, aes(x=CenterLong, y=CenterLat, fill = NIS_metrics_global[,"risk_scaled"])) + #risk
    geom_polygon(data = sf.rlm, size=2, aes(x = long, y = lat, group=group), fill = NA, color = "gray30",
                 linetype=1, alpha = 1) +
    geom_polygon(data = worldMap, fill = NA, colour = "saddlebrown", size = 0.1, aes(long, lat, group = group)) + 
    geom_polygon(data = worldMap, fill = "oldlace", colour = NA, size = 0, aes(long, lat, group = group)) + 
    scale_color_manual(values = realm_colors) + scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) + 
    guides(color = "none", fill = guide_colorbar(title = "Aggregated invasion risk  ", title.position = "left", title.vjust = 0.7)) + 
    labs(x = "Lon",y="Lat"); #map_sum


#Latitudinal plot of risk
lat_metrics = as.data.frame(NIS_metrics_global %>% group_by(CenterLat) %>% 
                              summarise_at(.vars = c("MPA", "Probability", "Di_scaled_cell", "Di_scaled", "rich","inv_risk_scaled", 
                                                     "inv_risk", "risk_sum", "risk_scaled", "risk_prod"), 
                                           .funs = mean))

lat_richness = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>% 
                               group_by(CenterLat) %>% 
                               summarise_at(.vars = c("rich", "rich_with_trait"), 
                                            .funs = mean)) 

lat_risk <- ggplot() + 
  theme_minimal() +
  scale_color_gradientn(colours = rev(brewer.pal(5, "Spectral")),
                        values = standr(getJenksBreaks(lat_metrics[,"risk_scaled"], k = 5)), na.value = "gray30",
                        guide_legend(title = "Invasion risk", title.position = "top", title.vjust = 0.5)) +
  geom_line(data = lat_metrics, mapping = aes(x = CenterLat, y = risk_scaled, color = risk_scaled), size = 1.2) +
  coord_flip() + 
  scale_y_continuous(position = "right") + 
  scale_x_continuous(expand = c(0, 0), position = "top") + 
  labs(x = "Lat", y="Mean establishment risk") + 
  theme(legend.position = "none", aspect.ratio = 5, text = element_text(size = 25), 
        panel.grid.minor = element_line(color = "white", linewidth = 0.2),
        panel.grid.major = element_line(color = "white", linewidth = 0.2),
        panel.background = element_rect(fill = lighten("white", 0.2), color = NA)); lat_risk

#Join both plots
Fig1 = ggarrange(map_sum, lat_risk, ncol = 2, nrow = 1, widths = c(5.5,1), align = "h")
# ggsave(file="./Plots/Fig1.svg", plot=Fig1, width=25, height=15)
# ggsave(file="./Plots/Fig1.png", plot=Fig1, width=30, height=15, dpi = 300)

# Figure 2: Violin plots of risk x realm -----------------
#Order the realms by latitude
realms_levels = rev(c("Arctic", "Temperate Northern Atlantic", "Temperate Northern Pacific", "Tropical Atlantic", "Western Indo-Pacific", 
                      "Tropical Eastern Pacific", "Central Indo-Pacific", "Eastern Indo-Pacific", "Temperate Southern Africa",
                      "Temperate South America", "Temperate Australasia", "Southern Ocean"))

#Summarise metrics by realm to obtain the boxplots
risk_violin = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>% 
                              group_by(CsquareCode, CenterLong, CenterLat, REALM, MEOW) %>% 
                              summarise_at(.vars = c("MPA", "Probability", "Di", "Di_scaled", "rich_with_trait","inv_risk_scaled", "inv_risk", "risk_sum", "risk_scaled", "risk_prod"), 
                                           .funs = mean)) %>% mutate(REALM = fct_reorder(REALM, risk_scaled))

risk_violin = risk_violin %>% mutate(REALM = fct_relevel(REALM, realms_levels))

#richness at suitable range
suit_NIS_rich = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>% 
                                group_by(CsquareCode, Valid_name, CenterLong, CenterLat, REALM) %>% summarise(n = n()) %>% 
                                group_by(CsquareCode, CenterLong, CenterLat, REALM) %>% summarise(n = as.numeric(n())) %>% rename(pot_rich = n, cell = CsquareCode)) %>% 
  group_by(REALM) %>% summarise_at(.vars = c("pot_rich"), .funs = list(mean, max)) %>% 
  rename(mean_NIS_rich = fn1, max_NIS = fn2) %>% mutate(REALM = fct_relevel(REALM, levels(risk_violin$REALM)))

# Plot Figure 2
risk_realm <-  ggplot(risk_violin, aes(x=as.factor(REALM), y=risk_scaled, fill=as.factor(REALM), color = as.factor(REALM)))+ 
  scale_fill_manual(values = realm_fill) + 
  scale_color_manual(values = realm_colors) + 
  labs(y = "Mean risk", x = NULL) + geom_hline(yintercept = -0.04) + 
  geom_text(suit_NIS_rich,
            mapping = aes(x = REALM, y = -0.2, label = round(mean_NIS_rich, digits = 2), fontface = 2), color = "red3", size = 6.5) +
  geom_text(suit_NIS_rich,
            mapping = aes(x = REALM, y = -0.1, label = round(max_NIS, digits = 2), fontface = 2), color = "navy", size = 6.5) + 
  geom_violin(width = 0.5, alpha = 0.6, size = 0.4, color = NA) + 
  geom_boxplot(width = 0.7, alpha=0.4, size = 0.5) + 
  scale_y_continuous(breaks = c(0.0, 0.25, 0.5, 0.75, 1)) + 
  theme_minimal() + theme(legend.position = "none",  
                          axis.text.x = element_text(size = 18, colour = "black"),
                          axis.title.x = element_text(size = 20),
                          axis.text.y = element_text(size = 20, color = "grey15"),
                          axis.line.x = element_line(), 
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank()) + #ggtitle("Risk x Realm")
  scale_x_discrete(labels = c(levels(as.factor(risk_violin$REALM))), position = "bottom") + 
  coord_flip(); risk_realm

# ggsave(file="./Plots/Supp_risk_realm.svg", plot=ovrll_map_sum, width=15, height=10)
# ggsave(file="./Plots/Fig2_sum_risk_realm.png", plot=risk_realm, width=15, height=10)

# Figure 3: Contribution of each components to establishment risk --------
#Decompose the risk
decomp_risk = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>% 
                              mutate(
                                Di_eff = (Di_scaled_cell/risk_sum)*100,
                                Prob_eff = (Probability/risk_sum)*100,
                                inv_eff = (inv_risk_scaled/risk_sum)*100
                              ) %>% 
                              group_by(CsquareCode, CenterLong, CenterLat, MEOW, REALM) %>% 
                              summarise_at(.vars = c("MPA", "Di_eff", "Prob_eff", "inv_eff", "risk_scaled", "risk_sum", "rich_with_trait"), .funs = mean)) %>% 
  mutate(REALM = fct_reorder(REALM, risk_scaled)) %>% rename(Connectivity = inv_eff, Env_suitability = Prob_eff, Niche_overlap = Di_eff)


decomp_realm = decomp_risk %>% group_by(REALM) %>% summarise_at(.vars = c("Niche_overlap", "Env_suitability", "Connectivity", "risk_scaled"), .funs = mean)
decomp_realm = melt(decomp_realm, id = "REALM"); colnames(decomp_realm)[2] = c("Component") 
levels(decomp_realm$Component) = c("Niche overlap", "Environmental suitability", "Connectivity", "risk_scaled")

Fig3 <- ggplot(decomp_realm %>% filter(!Component == "risk_scaled"), aes(fill=Component, x=REALM, y = value, color = Component)) + 
  geom_text(decomp_realm %>% filter(Component == "risk_scaled"), 
            mapping = aes(x = REALM, y = 1.1, label = round(value, digits = 2), fontface = 2), color = "black", size = 4.5) + 
  geom_bar(position="fill", stat = "identity", size = 0.75) + 
  scale_color_manual(values = effect_color) + 
  scale_fill_manual(values = effect_fill) + 
  theme_minimal() + scale_y_continuous(breaks = c(0.0, 0.25, 0.5, 0.75, 1)) + 
  theme(legend.position = "top", text = element_text(size = 12),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(size = 14, color = "grey15"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  scale_x_discrete(labels = c(levels(as.factor(decomp_realm$REALM))), position = "bottom") + 
  guides(color = "none") +
  coord_flip()+ 
  labs(x = NULL, y = "Contribution to risk (%)", fill = "Component"); Fig3

# ggsave(file="./Plots/Fig3.svg", plot=Fig3, width=18, height=11)
# ggsave(file="./Plots/Fig3_decomp_realm.png", plot=Fig3, width=10, height=6, dpi = 300)

# Figure 4: Map of each component's contribution --------
#Function to plot several maps
plot_decomp_maps = function(variable){
  
  p <- ggplot() + theme_map + 
    scale_fill_gradientn(colours=rev(brewer.pal(5, "Spectral")), values = c(0,0.25,0.5,0.75,1),
                         #breaks = c(0,25,50,75,100), 
                         limits= c(0, 100)) +
    geom_tile(data = cells_in_realm, aes(x=CenterLong, y=CenterLat), color = "#2B83BA", fill = "#2B83BA") +
    geom_tile(data = decomp_risk, aes(x=CenterLong, y=CenterLat, fill = decomp_risk[,variable])) + #risk
    geom_polygon(data = sf.rlm, size=1, aes(x = long, y = lat, group=group), fill = NA, color = "gray30",
                 linetype=1, alpha = 1) +
    geom_polygon(data = worldMap, fill = NA, colour = "saddlebrown", size = 0.1, aes(long, lat, group = group)) + 
    geom_polygon(data = worldMap, fill = "oldlace", colour = NA, size = 0, aes(long, lat, group = group)) + 
    scale_color_manual(values = realm_colors) + scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) + 
    guides(color = "none", fill = guide_colorbar(title = "Contribution to risk (%) ", title.position = "top", title.vjust = 0.5)) + 
    theme(plot.margin = margin(t = 10, b = 10)) + 
    labs(x = "Lon",y="Lat"); #p
  
  comp_map = p
  
  return(comp_map)
  
}

variables = c("Connectivity", "Env_suitability", "Niche_overlap")

decomp_maps = list(Connectivity = plot_decomp_maps(variable = "Connectivity"),
                   Env_suitability = plot_decomp_maps(variable = "Env_suitability"),
                   Niche_overlap = plot_decomp_maps(variable = "Niche_overlap"))

Fig4 = ggarrange(plotlist = decomp_maps,
                 ncol = 1, nrow = 3, common.legend = T, legend = "bottom",
                 labels = c("A", "B", "C")); #Fig4

# ggsave(file="./Plots/Fig4.svg", plot=Fig4, width=18, height=11)
# ggsave(file="./Plots/Fig4_comp_maps.png", plot=Fig4, width=12.5, height=20, dpi = 300)

# Figure S1: Maps of mean risk and variation --------
#Obtain the mean and sd of risk per cell
mean_metrics_global = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>% 
                                      group_by(CsquareCode, CenterLong, CenterLat, REALM, MEOW) %>% 
                                      summarise_at(.vars = c("risk_scaled"), 
                                                   .funs = list(mean = mean, sd = sd, var = var, coeff_var = cv), na.rm = T))

# Fig S1A
mean_risk_map <- ggplot() + theme_map +
  scale_fill_gradientn(colours =rev(brewer.pal(5, "Spectral")),
                       values= c(0, 0.25, 0.5, 0.75, 1),
                       breaks= c(0, 0.25, 0.5, 0.75, 1),
                       na.value = "aliceblue",
  ) +
  geom_tile(data = cells_in_realm, aes(x=CenterLong, y=CenterLat), color = "#2B83BA", fill = "#2B83BA") +
  geom_tile(data = mean_metrics_global, aes(x=CenterLong, y=CenterLat, fill = mean_metrics_global[,"mean"])) + #risk
  geom_polygon(data = sf.rlm, size=1, aes(x = long, y = lat, group=group), fill = NA, color = "gray30",
               linetype=1, alpha = 1) +
  geom_polygon(data = worldMap, fill = NA, colour = "saddlebrown", size = 0.1, aes(long, lat, group = group)) + 
  geom_polygon(data = worldMap, fill = "oldlace", colour = NA, size = 0, aes(long, lat, group = group)) + 
  scale_color_manual(values = realm_colors) + scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) + 
  guides(color = "none", fill = guide_colorbar(title = "Mean risk   ", title.position = "left", title.vjust = 0.7)) + 
  labs(x = "Lon",y="Lat"); mean_risk_map

#Fig S1B
sd_risk_map <- ggplot() + theme_map +
  scale_fill_gradientn(colours=rev(brewer.pal(5, "Spectral")),
                       values= standr(getJenksBreaks(mean_metrics_global[,"coeff_var"], k = 5)), 
                       #breaks= c(0, 0.25, 0.5, 0.75, 1), 
                       na.value = "gray85") +
  geom_tile(data = cells_in_realm, aes(x=CenterLong, y=CenterLat), color = "#2B83BA", fill = "#2B83BA") +
  geom_tile(data = mean_metrics_global, aes(x=CenterLong, y=CenterLat, fill = mean_metrics_global[,"coeff_var"])) + #risk
  geom_polygon(data = sf.rlm, size=1, aes(x = long, y = lat, group=group), fill = NA, color = "gray30",
               linetype=1, alpha = 1) +
  geom_polygon(data = worldMap, fill = NA, colour = "saddlebrown", size = 0.1, aes(long, lat, group = group)) + 
  geom_polygon(data = worldMap, fill = "oldlace", colour = NA, size = 0, aes(long, lat, group = group)) + 
  scale_color_manual(values = realm_colors) + scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) + 
  guides(color = "none", fill = guide_colorbar(title = "Risk variation  ", title.position = "left", title.vjust = 0.7)) + 
  labs(x = "Lon",y="Lat"); #sd_risk_map


FigS1 = ggarrange(mean_risk_map, sd_risk_map, ncol = 1, nrow = 2, common.legend = F, 
                  labels = c("A", "B")); #FigS1
# ggsave(file="./Plots/FigS1.svg", plot=FigS1, width=12.5, height=10)
# ggsave(file="./Plots/FigS1.png", plot=FigS1, width=12.5, height=15)

# Figure S2: Included cells and native range of NIS -------------
#Fig S2A: included cells
cells_map <- ggplot() + theme_map +
  geom_tile(data = cells_in_realm, aes(x=CenterLong, y=CenterLat), fill = "aliceblue") +
  geom_tile(data = NIS_metrics_global, aes(x=CenterLong, y=CenterLat), fill = "tomato3") + #risk
  geom_polygon(data = sf.rlm, size=0.5, aes(x = long, y = lat, group=group, color = REALM), fill = NA,
               linetype=1, alpha = 1) +
  scale_color_manual(values = realm_colors) + 
  geom_polygon(data = worldMap, fill = NA, colour = "saddlebrown", size = 0.3, aes(long, lat, group = group)) + 
  geom_polygon(data = worldMap, fill = "oldlace", colour = NA, size = 0, aes(long, lat, group = group)) + 
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0, 0)) + theme(legend.text = element_text(size = 9),
                                                                                      legend.title = element_text(size = 12)) + 
  labs(x = "Lon",y="Lat"); #cells_map


#Fig S2B
#richness at native range
pot_NIS_rich = as.data.frame(prob_occur %>% filter(range == "native") %>% filter(!pathway == "range expansion") %>% 
                               group_by(CsquareCode, Valid_name, CenterLong, CenterLat, REALM) %>% summarise(n = n()) %>% 
                               group_by(CsquareCode, CenterLong, CenterLat, REALM) %>% summarise(n = as.numeric(n())) %>% rename(pot_rich = n, cell = CsquareCode)) 

native_areas <- ggplot() + 
  theme_map + 
  scale_fill_gradientn(colours=brewer.pal(4, "BuGn"),
                       values= standr(getJenksBreaks(pot_NIS_rich[,5], k = 5)), na.value = "gray30") +
  geom_tile(data = cells_in_realm, aes(x=CenterLong, y=CenterLat), color = "white", fill = "white") +
  geom_tile(data = pot_NIS_rich, aes(x=CenterLong, y=CenterLat, fill = pot_rich)) + #risk
  geom_polygon(data = sf.rlm, size=0.5, aes(x = long, y = lat, group=group), fill = NA, color = "gray30", 
               linetype=1, alpha = 1) +
  geom_polygon(data = worldMap, fill = NA, colour = "saddlebrown", size = 0.1, aes(long, lat, group = group)) + 
  geom_polygon(data = worldMap, fill = "oldlace", colour = NA, size = 0, aes(long, lat, group = group)) + 
  scale_color_manual(values = realm_colors) + 
  guides(color = "none", fill = guide_colorbar(title = "Number of NIS per cell in native range", title.position = "top", title.vjust = 0.5)) + 
  labs(x = "Lon",y="Lat"); #native_areas


FigS2 = ggarrange(cells_map, native_areas, ncol = 1, nrow = 2, common.legend = F, 
                  labels = c("A", "B")) #FigS2
# ggsave(file="./Plots/FigS2.svg", plot=FigS2, width=12.5, height=10)
# ggsave(file="./Plots/FigS2.png", plot=FigS2, width=12.5, height=15)

# Figure S3: Scatterplot aggregated risk vs mean risk coloured by realm -------------
risk_versus = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>% 
                              group_by(CsquareCode, CenterLong, CenterLat, REALM, MEOW, PROVINCE) %>% 
                              summarise_at(.vars = c("risk_scaled"), 
                                           .funs = list(sum, mean))) %>% rename(sum = fn1, mean = fn2) %>% mutate(REALM = fct_relevel(REALM, rev(realms_levels)))

FigS3 = ggplot(risk_versus, aes(x = mean, y = sum, color = REALM)) + 
  theme_minimal() + scale_color_manual(values = realm_fill) + 
  geom_point() + #guides(size = "none", color = "none") + 
  labs(x= "Mean risk x cell", y= "Aggregated risk x cell", color = "Realm"); FigS3

# ggsave(file="./Plots/Fig. S3.svg", plot=risk_metrics_versus, width=18, height=14)
# ggsave(file="./Plots/Fig. S3.png", plot=risk_metrics_versus, width=10, height=8)





# Table S1: Summary of included NIS, risk, components and number of cells ----------
NIS_high_risk = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>% 
                                group_by(Valid_name) %>% 
                                summarise_at(.vars = c("Probability", "Di_scaled_cell", "Di_scaled", "rich_with_trait", "rich",
                                                       "inv_risk_scaled", "risk_prod", "risk_sum", "risk_scaled"), .funs = list(mean = mean, sd = sd))) %>% 
  mutate(category = ntile(risk_scaled_mean,4),
         category_prod = ntile(risk_prod_mean, 4)) %>% rename(Species = Valid_name) %>%
  mutate(category = as.factor(case_when(category == 4 ~ "High",
                                        category == 3 ~ "Moderate-high",
                                        category == 2 ~ "Low-moderate",
                                        category == 1 ~ "Low")),
         category_prod = as.factor(case_when(category_prod == 4 ~ "High",
                                             category_prod == 3 ~ "Moderate-high",
                                             category_prod == 2 ~ "Low-moderate",
                                             category_prod == 1 ~ "Low"))) %>% 
  mutate(category=fct_relevel(category,c("High", "Moderate-high", "Low-moderate", "Low")),
         category_prod = fct_relevel(category_prod,c("High", "Moderate-high", "Low-moderate", "Low")))

#Obtain number of suitable cells
n_suit_cells = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>% 
                               group_by(CsquareCode, Valid_name) %>% summarise(n = n()) %>% 
                               group_by(Valid_name) %>% summarise(n = as.numeric(n())) %>% rename(Species = Valid_name, n_suit_cells = n)) %>% 
  mutate(size = rescale(n_suit_cells, to = c(1, 15)))

#Merge all dataframes
NIS_list_risk = merge(NIS_list, NIS_high_risk, by = "Species"); NIS_list_risk = merge(NIS_list_risk, n_suit_cells, by = "Species")

#save table
colnames(NIS_list_risk)
NIS_list_risk = NIS_list_risk %>% select(Species, Probability_mean, Di_scaled_cell_mean, inv_risk_scaled_mean, risk_scaled_mean, risk_scaled_sd,
                                         category, n_suit_cells)

#write.table(NIS_list_risk, "Table_S1_NIS_list.txt", sep="\t")
