##########################################################################################################
## 5. Relationship between cumulative human impacts, MPAs and establishment risk (Figure 5 & Figure S4) ##!
##########################################################################################################
remove(list=ls())
#Libraries and functions
source("./Code/Libraries and functions.R")
source("./Code/Figures data.R")

#1: Obtain the cumulative human impacts in each cell --------------
#Take only the cells and associated coordinates
cells_and_coords = prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>%
  select(CsquareCode, CenterLong, CenterLat, REALM, PROVINCE, ECOREGION, MEOW) %>% distinct(.)

rasters = list.files(path = "./Data/Cumul_impact/", pattern = ".tif", all.files = T)

import_rast = lapply(rasters, function(x){filename = paste("./Data/Cumul_impact/", x, sep = ""); 
file = projectRaster(raster(filename), crs = "+proj=longlat +datum=WGS84 +no_defs", res = 0.5);
return(file)})

all_imp = raster::stack(import_rast)
plot(all_imp$commercial_fishing_pelagic_high_bycatch_2013_impact)

impacts = c("art_fish", "demersal_destructive", "demersal_low_bycatch", "pelagic_high_bycatch", "cumul_imp", "direct_human", "shipping", "sst")

cum_impact_data = cells_and_coords

#extract the impacts and merge them with database
for(i in 1:length(impacts)){
  print(impacts[i])
  #Extract the cumulative impact index metrics for each cell
  coords = cum_impact_data %>% select(CsquareCode, CenterLong, CenterLat) %>% distinct(.)
  coords$var <- raster::extract(all_imp[[i]], coords[,2:3], method = 'simple', small = T, na.rm = T, exact = T, cellnumbers = F)
  
  #Deal with NAs; 
  varNA <- coords[which(is.na(coords[,4])),];
  
  #find the coordinates of the nearest nonNA cell in the raster
  nas <- varNA[,2:3]
  q = 50000 #we set a maximum distance for the function to look at coordinates, this is in meters
  
  repeat{
    print(q)
    #With this nearestLand function, we calculate the nearest point in a raster file which is not NA
    #To deal with possible NA's in our data due to mistmatch between in-situ points and the satellite coordinates.
    land <- as.data.frame(nearestLand(nas, all_imp[[i]], q)); colnames(land) <- c("CenterLong", "CenterLat")
    dup <- c(which(is.na(land)))
    q = q + 10000
    if(length(dup)==0){break}
  } #We repeat it until there are not more NAs increasing the maximum distance by 1km each iteration
  
  coordinates(land)= ~CenterLong + CenterLat
  varNA$var <- raster::extract(all_imp[[i]],land,method = 'bilinear', small = T, na.rm = T, exact = T, cellnumbers = F);
  
  coords = coords %>% filter(!is.na(var))
  coords = rbind(coords, varNA); colnames(coords)[which(colnames(coords) == "var")] = impacts[i]
  
  cum_impact_data = merge(cum_impact_data, coords, by = c("CsquareCode", "CenterLong", "CenterLat"))
  
}

#write.table(cum_impact_data, "./Data/cumulative_impacts.txt", sep="\t")

#2: GAMMs to observe the relationship between cumulative human impacts and establishment risk --------------
#Focus on the management perspective, divide the cells into categories according to their values of risk
risk_management = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion") %>%
                                  group_by(CsquareCode, CenterLong, CenterLat, REALM, MEOW) %>%
                                  summarise_at(.vars = c("MPA", "Probability", "Di_scaled_cell", "Di_scaled", "rich", "rich_with_trait","inv_risk",
                                                         "inv_risk_pr", "inv_risk_rlm", "inv_risk_scaled", 
                                                         "risk_sum", "risk_scaled", "risk_prod"), .funs = mean)) %>% 
  mutate(category = ntile(risk_scaled, 4), #Classify cells within categories
         category_prod = ntile(risk_prod, 4), 
         protected = ifelse(is.na(MPA), "Outside MPA", "Within MPA")) %>% #Add category to see if cell is in or out MPAs
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


#Subset of data for modelling
cumul_impact = read.csv("./Data/cumulative_impacts.txt",header=T,dec=".",sep="\t", check.names = FALSE)
prob_occur_test = as.data.frame(prob_occur %>% filter(range == "potential") %>% filter(!pathway == "range expansion"))
prob_occur_test = merge(prob_occur_test, cumul_impact, by = colnames(cumul_impact)[c(1:4,7)]); prob_occur_test$SpeciesID = as.factor(prob_occur_test$SpeciesID)

# Fit the model with all observations, comparing risk with cumulative impact index, with species as random factor
model_ALL <- gam(risk_scaled ~ s(cumul_imp, k = 3) + s(CenterLong, CenterLat, k = 10) + s(SpeciesID, bs = "re"), data=prob_occur_test)

summary(model_ALL); plot.gam(model_ALL)
gam.check(model_ALL)

#Save overall model
#saveRDS(model_ALL, file = "./Data/Models/GAMM_overall.R")

#Model diagnostics
reg <- model_ALL
hist(resid(reg)) #Histogram of residuals

#observed vs fitted residuals
plot(fitted(reg),resid(reg), 
     col = "black", xlab = "Fitted class", ylab = "Residuals", cex.lab=1.5, cex.axis=1.5)
abline(h=0, lty='dashed')

#qqplot
qqnorm(resid(reg), main='')
qqline(resid(reg))

#2.1: Repeat models for each realm -----------------
models_realm = list(); realms = unique(prob_occur_test$REALM)

#Loop to fit GAMMs
system.time(for(i in 1:length(realms)){#START LOOP MODELS X REALM
  print(paste(realms[i], paste(i, length(realms), sep = "/"), sep = " "))
  
  test_realm = as.data.frame(prob_occur_test %>% filter(range == "potential") %>% 
                               filter(!pathway == "range expansion", REALM == realms[i])); test_realm$SpeciesID = as.factor(test_realm$SpeciesID)
  
  mod <- gam(risk_scaled ~ s(cumul_imp, k = 3) + s(CenterLong, CenterLat) + s(SpeciesID, bs = "re"), 
             data=test_realm, method = "REML")
  
  #list of models
  models_realm = append(models_realm, list(mod)); names(models_realm)[i] = realms[i]}) #END LOOP MODELS X REALM
#This loop takes ~30 min to run

#save all models
#saveRDS(models_realm, file = "./Data/Models/GAMM_realms.R")

#Extract smooth lines and partial effects for each realm
models_realm = readRDS("./Data/Models/GAMM_realms.R")

#Empty dataframes to fill
resid = data.frame(); lines = data.frame(); models_eval = data.frame();
preds_all = data.frame(); part_plots_realm = list()

#With this loop we will obtain a partial plot for each realm, which will be merged in Figure 5
system.time(for(i in 1:length(realms)){
  print(paste(realms[i], paste(i, length(realms), sep = "/"), sep = " "))
  mod = models_realm[[i]]
  
  print("Visualization")
  #Get lines and points for the model
  viz_fit <- mgcViz::getViz(mod); trt_fit <- plot(viz_fit, select = c(1), res = T) + l_fitLine() + l_points() + l_ciLine() + l_rug()
  plot <- as.data.frame(trt_fit[["plots"]][[1]]$data$fit %>% mutate(REALM = realms[i]))
  res <- as.data.frame(trt_fit[["plots"]][[1]]$data$res %>% mutate(REALM = realms[i]))
  plot$variable <- trt_fit[["plots"]][[1]][["ggObj"]][["labels"]][[1]]
  res$variable <- trt_fit[["plots"]][[1]][["ggObj"]][["labels"]][[1]]
  # resid <- bind_rows(resid, res)
  # lines <- bind_rows(lines, plot)
  
  print("Model fits")
  #Get the summary for model fits
  #R-squared
  models.r.sq <- summary(mod)$r.sq
  
  #deviance explained
  models.deviance <- summary(mod)$dev.expl
  
  #P values and AIC
  p.val <- t(data.frame(p.value = summary(mod)$s.pv)); aic <- AIC(mod); p.val <- cbind(p.val[,1], AIC = aic)
  colnames(p.val)[1] <- "p_value_cumul_imp";
  
  df.model.eval <- as.data.frame(cbind(r.sq=models.r.sq, dev.explain=models.deviance, p.val, N = summary(mod)$n)); rownames(df.model.eval)<- realms[i]
  df.model.eval$p_value_cumul_imp = ifelse(df.model.eval$p_value_cumul_imp < 0.001, "<0.001", as.character(round(df.model.eval$p_value_cumul_imp, digits = 3)))
  
  print("Partial effects plots")
  #Predictions
  pred = get_gam_predictions(mod, series = cumul_imp); pred$REALM = realms[i]
  
  #Plot partial effects and marginal point distribution
  part = ggplot(pred, aes(cumul_imp, risk_scaled, group = as.factor(REALM), color = as.factor(REALM))) + theme_minimal() + 
    geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, group = as.factor(REALM), fill = as.factor(REALM)), color = NA, alpha = 0.1) +
    #labs(x = "Cumulative human impacts index", y = "NIS establishment risk") +
    ggtitle(realms[i]) + 
    #title(realms[i]) + 
    geom_line(aes(linetype = "solid"), size = 1.5) + guides(linetype="none", fill = "none") + 
    scale_color_manual(values = realm_colors) + ylim(min(pred$risk_scaled) - 0.1, max(pred$risk_scaled) + 0.1) + 
    scale_fill_manual(values = realm_fill) + 
    #facet_wrap( ~ REALM, nrow = 4, scales = ("free_x")) + 
    # new_scale_color() + 
    # scale_color_manual(values = realm_fill) + 
    geom_point(data = prob_occur_test %>% filter(REALM %in% realms[i]),
               mapping = aes(x = cumul_imp, y = risk_scaled, fill = as.factor(REALM), color = as.factor(REALM)), alpha = 0) + 
    annotate(geom="text", x = 0, y=max(pred$risk_scaled) + 0.05, hjust = -0.1,
             label= paste("p = ", df.model.eval$p_value_cumul_imp, sep = ""),
             color="black", size = 6) + 
    annotate(geom="text", x = 0, y=max(pred$risk_scaled) + 0.08, hjust = -0.1,
             label= paste("N = ", summary(mod)$n, sep = ""),
             color="black", size = 6) +
    theme(legend.background = element_rect(fill = "transparent", color = NA), 
          legend.key = element_rect(fill = "transparent", color = NA),
          axis.title = element_blank(),
          axis.text = element_text(size = 15), plot.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm"),
          plot.title = element_text(hjust = 0, size = 20), legend.position = "none") +
    labs(color  = pred$fit); #realms_part
  
  part = ggMarginal(part, groupFill = T, groupColour = T, alpha = 0.1); #part
  
  #Bind all dataframes together
  resid <- bind_rows(resid, res)
  lines <- bind_rows(lines, plot)
  models_eval <- rbind(models_eval, df.model.eval)
  preds_all = rbind(preds_all, pred)
  part_plots_realm = append(part_plots_realm, list(part)); names(part_plots_realm)[i] = realms[i]
  
})

#Save Table S2: Models summary
#write.xlsx(models_eval, 'TableS2_REALMS_models_eval.xlsx', rowNames=TRUE, overwrite = T)

# Figure 5: Join all partial plots together ------------
#Plot the smooth line for the overall model
model_ALL = readRDS("./Data/Models/GAMM_overall.R")
preds = get_gam_predictions(model_ALL, series = cumul_imp)

#Plot overall trends
overall <- preds %>%
  ggplot(aes(cumul_imp, risk_scaled)) + theme_minimal() + ylim(min(preds$risk_scaled) - 0.1, max(preds$risk_scaled) + 0.1) +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, group = .idx), fill = "gray90", alpha = 0.05) +
  ggtitle("Trend across all realms") +
  labs(x = "Cumulative human impacts index", y = "NIS establishment risk") +
  geom_line(aes(linetype = "solid"), color = "gray20", size = 1) + guides(linetype="none") + 
  geom_point(prob_occur_test, mapping = aes(x = cumul_imp, y = risk_scaled), alpha = 0) +
  annotate(geom="text", x = 0, y=max(preds$risk_scaled) + 0.06, hjust = -0.1,
           label= "p = <0.001",
           color="black", size = 7) + 
  annotate(geom="text", x = 0, y=max(preds$risk_scaled) + 0.08, hjust = -0.1,
           label= paste("N = ", summary(model_ALL)$n, sep = ""),
           color="black", size = 7) + 
  theme(legend.background = element_rect(fill = "transparent", color = NA), 
        legend.key = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15), plot.margin = margin(0.1, 0.1, 0.1, 0.1, unit = "cm"),
        plot.title = element_text(hjust = 0, size = 20), legend.position = "none"); #overall

overall = ggMarginal(overall, fill = "gray90", color = "gray20"); #overall

#Reorder realm plots in list by latitude
realms = c("Arctic", "Temperate Northern Atlantic", "Temperate Northern Pacific", "Western Indo-Pacific",
           "Tropical Eastern Pacific", "Central Indo-Pacific", "Eastern Indo-Pacific", "Temperate Southern Africa",
           "Tropical Atlantic", "Temperate South America", "Temperate Australasia", "Southern Ocean")

part_plots_realm = part_plots_realm[realms]

Fig5_bot = ggarrange(plotlist = part_plots_realm, ncol = 4, nrow = 3)
Fig5_bot = annotate_figure(Fig5_bot, left = text_grob("NIS establishment risk", rot = 90, size = 30),
                           bottom = text_grob("Cumulative human impact index", size = 30))

Fig5= ggarrange(
  ggarrange(ggplot() + theme_void(), overall, ggplot() + theme_void(), ncol = 1, nrow = 3, heights = c(1,5,1)),
  ggplot() + theme_void(),
  Fig5_bot,
  ncol = 3, nrow = 1, widths = c(1,0.1, 2), labels = c("A", "B")); Fig5

# ggsave(file="./Plots/Fig5_gam_outputs.svg", plot= Fig5, width=11, height=8)
# ggsave(file="./Plots/Fig5_gam_outputs.png", plot= Fig5, width=30, height=15, dpi = 300)  

# Figure S4: Risk categories and MPAs -------
MPA_risk_bar = risk_management %>% group_by(protected, category) %>% summarise(n = n()) %>% 
  mutate(perc = ifelse(protected == "Outside MPA", 
                       n/nrow(risk_management %>% filter(protected =="Outside MPA")) * 100,
                       n/nrow(risk_management %>% filter(protected =="Within MPA")) * 100),
         label_n = as.character(n))

#Overall plot
protected_cells <- ggplot(MPA_risk_bar, aes(x = protected, y = n, fill = category)) + 
  geom_bar(position="dodge", stat = "identity", width = 0.6) +
  scale_fill_manual(values = risk_points) + 
  theme_minimal() + 
  theme(legend.position = "right", text = element_text(size = 15), 
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(size = 12, color = "grey15"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x = NULL, y = "Total cells", fill = "Risk category"); protected_cells

#Number of cells within or outside MPAs per realm
MPA_risk_rlm = risk_management %>% group_by(REALM, protected, category) %>% summarise(n = n()) %>% 
  mutate(perc = ifelse(protected == "Outside MPA", 
                       n/nrow(risk_management %>% filter(protected =="Outside MPA")) * 100,
                       n/nrow(risk_management %>% filter(protected =="Within MPA")) * 100),
         label_n = as.character(n))


realm_risk_within <- ggplot(MPA_risk_rlm %>% filter(protected == "Within MPA"), aes(x = protected, y = n, fill = category)) + 
  geom_bar(position="dodge", stat = "identity", width = 0.6) + 
  scale_fill_manual(values = risk_points) + facet_wrap(vars(REALM), strip.position = "bottom") + 
  theme_minimal() +  
  theme(legend.position = "none", text = element_text(size = 15), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, color = "grey15"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x = NULL, y = "Total cells", fill = "Risk category"); realm_risk_within

FigS4 = ggarrange(protected_cells, realm_risk_within, ncol = 1, nrow = 2, labels = c("A", "B"), heights = c(1,2)); FigS4

# ggsave(file="./Plots/Supp_MPA_plot.svg", plot= FigS4, width=11, height=10)
# ggsave(file="./Plots/Supp_MPA_plot.png", plot= FigS4, width=11, height=10, dpi = 600) 