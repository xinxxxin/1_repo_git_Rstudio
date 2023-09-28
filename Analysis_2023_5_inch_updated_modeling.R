# Load packages
library(ecospat)
library(car)
library(MASS)
library(caret)
library(pROC)
library(sjPlot)
library(pscl)
library(cowplot)
library(devtools)
library(grid)

# For cross-validation on the plots with given stand age
source("glm_cv_for_given_standage.R")

# For visualization
# For element_markdown
remotes::install_github("rpkgs/gg.layers")
library(gg.layers)

library(cowplot)

library(visreg)
devtools::install_github("strengejacke/sjstats")

# Load data
# Feature
###########################################################################################################
# Change on 03/09/2020                                                                                    #
# The relationship between stand age and charcoal hearth should be investigated                           #
###########################################################################################################
# Stand age
stand_age_smt <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/stand_age_smt.csv")

# Charcoal hearth
hearth_density_500m <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/hearth_density_500m.csv")
nearest_hearth_dist <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/nearest_hearth_dist.csv")
hearth_density_1km <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/hearth_density_1km.csv")

########################################################################################################
# Change on 03/16/2020
# Merge stand age to hearth density
# 3 CFI plots do not have stand age, 13033, 13065, 13066
########################################################################################################
hearth_density_1km <- merge(stand_age_smt[, 1:2], hearth_density_1km, by = "PlotID")
hearth_density_500m <- merge(stand_age_smt[, 1:2], hearth_density_500m, by = "PlotID")

# Plot 13033, 13065, and 13066 do not have stand age, 13033 missing, 13066 still in stem exclusion


# Climate, soil, and topography
smt_feature <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_feature.csv")
smt_feature$bedrock <- as.factor(smt_feature$bedrock)
smt_feature$SoilOrd <- as.factor(smt_feature$SoilOrd)
smt_feature$SoilSubOrd <- as.factor(smt_feature$SoilSubOrd)

# Presence/absence and abundance (> 5.0 inch tree)
smt_chestnutoak <- read.csv("D:/chapter2_backup/Plot_Tree/Presence_Absence/Modeling/smt_chestnutoak.csv")
smt_redoak <- read.csv("D:/chapter2_backup/Plot_Tree/Presence_Absence/Modeling/smt_redoak.csv")
smt_redmaple <- read.csv("D:/chapter2_backup/Plot_Tree/Presence_Absence/Modeling/smt_redmaple.csv")
smt_scarletoak <- read.csv("D:/chapter2_backup/Plot_Tree/Presence_Absence/Modeling/smt_scarletoak.csv")
smt_pitchpine <- read.csv("D:/chapter2_backup/Plot_Tree/Presence_Absence/Modeling/smt_pitchpine.csv")
smt_whiteoak <- read.csv("D:/chapter2_backup/Plot_Tree/Presence_Absence/Modeling/smt_whiteoak.csv")

# Presence/absence and abundance (> 7.8 inch tree)
smt_chestnutoak <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_chestnutoak_78_inch.csv")
smt_redoak <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_redoak_78_inch.csv")
smt_redmaple <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_redmaple_78_inch.csv")
smt_scarletoak <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_scarletoak_78_inch.csv")
smt_pitchpine <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_pitchpine_78_inch.csv")
smt_whiteoak <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_whiteoak_78_inch.csv")

###################################################################################################################
# Compare chestnut oak and red maple between two thresholds of DBH
# 04/16/2020
###################################################################################################################
# chestnut oak
smt_chestnutoak <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_chestnutoak_78_inch.csv")
table(smt_chestnutoak$chestnutoak_occr)
summary(smt_chestnutoak$chestnutoak_totalba)
summary(smt_chestnutoak$chestnutoak_totaln)

smt_chestnutoak <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_chestnutoak.csv")
table(smt_chestnutoak$chestnutoak_occr)
summary(smt_chestnutoak$chestnutoak_totalba)
summary(smt_chestnutoak$chestnutoak_totaln)

# Red maple
smt_redmaple <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_redmaple_78_inch.csv")
table(smt_redmaple$redmaple_occr)
summary(smt_redmaple$redmaple_totalba)
summary(smt_redmaple$redmaple_totaln)

smt_redmaple <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/smt_redmaple.csv")
table(smt_redmaple$redmaple_occr)
summary(smt_redmaple$redmaple_totalba)
summary(smt_redmaple$redmaple_totaln)
################################################################################################################################


# Merge feature to presence and absence for each data
# Check NA's
chestnutoak <- merge(smt_chestnutoak, hearth_density_1km, by = "PlotID")
chestnutoak <- merge(chestnutoak, nearest_hearth_dist, by = "PlotID")
chestnutoak <- merge(chestnutoak, smt_feature, by = "PlotID", all = TRUE)
#########################################################################################################################################
# NA's check on Apr 6th, 2020
# 4 NA's from FIA and 4 NA's from CFI
# FIA NA's from smt_feature because the most recent FIA does not have those four plots so that the extracted values were not available  #
# CFI NA's from chestnutoak because the CFI plots experienced clearcut, or other treatments in past 5 years                             #
# Plot 13016 clearcut (no trees), Plot 13011 Cut(shelterwood removal), Plot 13065 and 13066 new plots, Plot 13033 no stand age          #
# Other considering dropped plots: 13056 (Selection), 21702 (Seed Tree)

# In summary:
# CFI 13011 (CUT and Shelterwood Removal), Not appeared as NA
# CFI 13015 (Shelterwood), Not appeared as NA
# CFI 13016 (Clearcut and no trees), 
# CFI 13033 (Missing stand age), 
# CFI 13056 (Selection), Not appeared as NA
# CFI 13065 (New plot), 
# CFI 13066 (New plot), 
# CFI 21702 (Seed tree), Not appeared as NA

# Change on 04/28/2020
# total_ba and total_n can be zero because of the threshold of DBH 
smt_chestnutoak <- smt_chestnutoak[smt_chestnutoak$total_ba != 0, ]
smt_redoak <- smt_redoak[smt_redoak$total_ba != 0, ]
smt_scarletoak <- smt_scarletoak[smt_scarletoak$total_ba != 0, ]
smt_whiteoak <- smt_whiteoak[smt_whiteoak$total_ba != 0, ]
smt_pitchpine <- smt_pitchpine[smt_pitchpine$total_ba != 0, ]
smt_redmaple <- smt_redmaple[smt_redmaple$total_ba != 0, ]

#########################################################################################################################################


#########################################################################################################################################
# Dimension reduction based on PCA and LDA for all feature
# The predictor variables will be the same for all species
#########################################################################################################################################
# Reorganize the all features
library(factoextra)
feature <- smt_feature[, c(1, 3:25)]
feature[, 25:36] <- smt_feature[, c(26, 29:31, 34:39, 42, 43)]
feature[, 37:48] <- smt_feature[, c(2, 28, 32, 33, 41, 44, 47:52)]

# Load Wang Tongli's climate data
climate_tongli_smt <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/climate_tongli_smt.csv")
feature <- merge(feature, climate_tongli_smt[, -c(2,3)], by = "PlotID")


######################################################################################################################################
# 04/27/20 PCA does not work because PCs are highly correlated too.
######################################################################################################################################
# Reorganize the all features
library(factoextra)
feature <- smt_feature[, c(1, 3:25)]
feature[, 25:36] <- smt_feature[, c(26, 29:31, 34:39, 42, 43)]
feature[, 37:48] <- smt_feature[, c(2, 28, 32, 33, 41, 44, 47:52)]

# Load Wang Tongli's climate data
climate_tongli_smt <- read.csv("F:/LiDAR/Plot_Tree/Presence_Absence/Modeling/climate_tongli_smt.csv")
feature <- merge(feature, climate_tongli_smt[, -c(2,3)], by = "PlotID")

# Remove correlated variables
library(caret)
feature_90m <- feature[, c(1, 25:71)]
feature_cor <- cor(feature_90m)
hc <- findCorrelation(feature_cor, cutoff = 0.5, exact = TRUE)
hc <- sort(hc)
reduced_feature <- feature_90m[, -c(hc)]


library(caret)
feature_90m_sub <- feature[, c(1, 2:48)]
feature_90m_sub <- merge(feature_90m_sub, hearth_density_500m, by = "PlotID")
feature_90m_sub <- merge(feature_90m_sub, nearest_hearth_dist, by = "PlotID" )
feature_cor_sub <- cor(feature_90m_sub)
hc_sub <- findCorrelation(feature_cor_sub, cutoff = 0.5, exact = TRUE)
hc_sub <- sort(hc_sub)
reduced_feature_sub <- feature_90m_sub[, -c(hc_sub)]

# Rename predictors for plotting in the end
names(reduced_feature_sub) <- c("PlotID", "FDAY", "SMRPB", "BULK", "DEPTH", "EROD", "OM", "SOILSP", "TPI", "TRASP", "EST", "COUNT", "DIST")

###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
# Date: 04/13/2023
# Change: Start with the predictor with the highest mean absolute pair-wise correlation and then remove the correlated variables sequentially
# Method: Run findCorrelation function in caret package and then start with the 1st predictor

# Export the correlation matrix as a csv file and manually remove correlated variables
feature_cor_new <- feature_cor_sub[2:51, 2:51]
write.csv(feature_cor_new, "feature_cor_sub.csv", row.names = TRUE)
write.csv(findCorrelation(feature_cor_new, cutoff = 0.7, exact = TRUE), "order_correlated_variables.csv", row.names = FALSE)

# After manual variable selection
feature_new <- feature_90m_sub[, c("PlotID", "mmax", "fday", "pratio", "bulkdensity", "depthtobedrock", "om", "clay", "permeability", 
                                   "sieve200","err15", "imi", "rough27", "soilslope", "tpi150", "tpi2000", "trasp", "EST", "COUNT", "DIST")]

names(feature_new) <- c("PlotID", "MMAX", "FDAY", "PRATIO", "BULK", "DEPTH", "OM", "CLAY", "PERM", 
                        "SIEVE200","ERR15", "IMI", "ROUGH", "SOILSP", "TPI150", "TPI2000", "TRASP", "EST", "COUNT", "DIST")

# Change the unit of distance from m to km
# Change the EST to stand age
feature_new$DIST <- feature_new$DIST/1000
feature_new$EST <- 2023 - feature_new$EST

# Date: 08/26/2023
# Change: Rescale  PERMEADABILITY from 0-100% to 0-1 to avoid the very high fitted coefficients in GLM. PRATIO is in 0-1 scale.
feature_new$PERM  <- feature_new$PERM/100

# Merge occurrence&abundance with predictors
# Ignore those 8 NA's when merging the data
# Ignore those 8 NA's when merging the data
chestnutoak <- merge(smt_chestnutoak, feature_new, by = "PlotID")
redoak <- merge(smt_redoak, feature_new, by = "PlotID")
redmaple <- merge(smt_redmaple, feature_new, by = "PlotID")
scarletoak <- merge(smt_scarletoak, feature_new, by = "PlotID")
pitchpine <- merge(smt_pitchpine, feature_new, by = "PlotID")
whiteoak <- merge(smt_whiteoak, feature_new, by = "PlotID")

# Convert presence/absence to factors
chestnutoak$chestnutoak_occr <- as.factor(chestnutoak$chestnutoak_occr)
redoak$redoak_occr <- as.factor(redoak$redoak_occr)
redmaple$redmaple_occr <- as.factor(redmaple$redmaple_occr)
scarletoak$scarletoak_occr <- as.factor(scarletoak$scarletoak_occr)
pitchpine$pitchpine_occr <- as.factor(pitchpine$pitchpine_occr)
whiteoak$whiteoak_occr <- as.factor(whiteoak$whiteoak_occr)

##############################################################################################################################
# Date: 09/22/2023
# Change: Removed the plots that are far away from Michaux State Forest because they are not surrounded by forests

# Create the lists of plots close to and far away from Michaux State Forest 
plot_close <- data.frame(PlotID = c(2984, 5803, 1435, 1923, 2151, 3210, 6111, 1294, 2262, 5647, 870, 2164, 1644, 5141,
                                    6112, 573, 1544))
plot_far <- data.frame(PlotID = c(3151, 364, 1809, 1421, 1076, 2750, 5796, 1737, 2761, 1584, 4895, 431, 1757, 1591,
                                  4746, 1193, 483, 718, 2072, 3584))

# Check the distance to nearest hearths for the plots that are far from Michaux State Forest

# Chestnut oak
# Parametric models
# Logistic regression for presence/absence
# With legacy
chestnutoak$ESTGP <- "Old"
chestnutoak$ESTGP[chestnutoak$EST < 120] <- "Young"

glm_chestnutoak_occr <- glm(chestnutoak_occr ~ EST*DIST + EST*COUNT + DIST*COUNT + MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                              SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP,
                            data = chestnutoak[chestnutoak$DIST < 5, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_chestnutoak_occr, direction = "both")

glm_chestnutoak_occr_new <- glm(chestnutoak_occr ~ EST + DIST + COUNT + MMAX + 
                                  PRATIO + DEPTH + SIEVE200 + ERR15 + SOILSP + TPI150 + EST:DIST + 
                                  EST:COUNT,
                                data = chestnutoak[chestnutoak$DIST < 5, -c(1:9)],
                                family = binomial(link = "logit"))

summary(glm_chestnutoak_occr_new)
pR2(glm_chestnutoak_occr_new)
vif(glm_chestnutoak_occr_new)
anova(glm_chestnutoak_occr_new, test = "Chisq")

# Quickly check modeling abundance
glm_chestnutoak_ba <- glm(chestnutoak_totalba ~ EST*DIST + EST*COUNT + DIST*COUNT + MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                            SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP,
                          data = chestnutoak[chestnutoak$DIST < 5, ], family = "gaussian")
stepAIC(glm_chestnutoak_ba, direction = "both")

glm_chestnutoak_ba_new <- glm(chestnutoak_totalba ~ EST + DIST + COUNT + DIST:COUNT,
                              data = chestnutoak[chestnutoak$DIST < 5, ],
                              family = "gaussian")

summary(glm_chestnutoak_ba_new)


# Cross-validation
set.seed(123)
glm_chestnutoak_occr_new_cv <- ecospat.cv.glm(glm_chestnutoak_occr_new, K = 10)
glm_chestnutoak_occr_new_cv$obs <- as.factor(glm_chestnutoak_occr_new_cv$obs)
auc(glm_chestnutoak_occr_new_cv$obs, glm_chestnutoak_occr_new_cv$predictions)

# Cross-validation on plots < 113 years
auc(glm_chestnutoak_occr_new_cv$obs[glm_chestnutoak_occr_new$data$EST <= 113], 
    glm_chestnutoak_occr_new_cv$predictions[glm_chestnutoak_occr_new$data$EST <= 113])


# Quickly check response
visreg(glm_chestnutoak_occr_new, "DIST", by = "EST", data = chestnutoak[, -c(1:9)],
       scale = "response", gg = TRUE, line = list(size = .8),
       rug = 0,
       overlay = TRUE,  band = FALSE, nn = 200,
       print.cond = TRUE, type = "contrast") +
  scale_x_continuous(name = "Distance to nearest charcoal hearth (km)") +
  scale_y_continuous(name = "Probability of presence", breaks = c(0, 0.5, 1)) +
  scale_color_manual( values = c("#FF5733", "#FFC300", "#154360")) +
  labs(color = "Mean stand <br>age (year)")+
  theme_classic() +
  ggtitle("Chestnut oak") +
  theme_classic() +
  theme(plot.title = element_text(color = "black", size = 10, face = "bold", hjust = 0.5),
        legend.position = c(0.8, 0.5),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_markdown(size = 9),
        axis.title.y = element_markdown(size = 9)) +
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "grey95", color = "white"),
        strip.text = element_markdown(face = "bold"),
        strip.text.y = element_markdown(face = "bold"),
        panel.border = element_blank(),
        legend.title = element_markdown(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(.4, 'cm')) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))



# Without legacy
glm_chestnutoak_occr_aic <- glm(chestnutoak_occr ~ MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                                  SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP,
                                data = chestnutoak[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_chestnutoak_occr_aic, direction = "both")


glm_chestnutoak_occr_new_wo <- glm(chestnutoak_occr ~ MMAX + SIEVE200 + SOILSP + IMI,
                                   data = chestnutoak[, -c(1:9)],
                                   family = binomial(link = "logit"))
summary(glm_chestnutoak_occr_new_wo)
pR2(glm_chestnutoak_occr_new_wo)
vif(glm_chestnutoak_occr_new_wo)
anova(glm_chestnutoak_occr_new_wo, test = "Chisq")

# Cross-validation
set.seed(456)
glm_chestnutoak_occr_new_wo_cv <- ecospat.cv.glm(glm_chestnutoak_occr_new_wo, K = 5)
glm_chestnutoak_occr_new_wo_cv$obs <- as.factor(glm_chestnutoak_occr_new_wo_cv$obs)
auc(glm_chestnutoak_occr_new_wo_cv$obs, glm_chestnutoak_occr_new_wo_cv$predictions)

# Cross-validation on plots < 113 years
auc(glm_chestnutoak_occr_new_wo_cv$obs[glm_chestnutoak_occr_new_wo$data$EST <= 113], 
    glm_chestnutoak_occr_new_wo_cv$predictions[glm_chestnutoak_occr_new_wo$data$EST <= 113])

# Date: 08/26/2023
# Addition of legacy after stepwise variable selection
glm_chestnutoak_occr_new_wo_add <- glm(chestnutoak_occr ~ MMAX + SIEVE200 + SOILSP + IMI,
                                       data = chestnutoak[, -c(1:9)],
                                       family = binomial(link = "logit"))
  
summary(glm_chestnutoak_occr_new_wo_add)
pR2(glm_chestnutoak_occr_new_wo_add)
vif(glm_chestnutoak_occr_new_wo_add)
anova(glm_chestnutoak_occr_new_wo_add, test = "Chisq")



##############################################################################################################################
# Date: 07/09/2023
# Change: Performed variance partitioning for SDM with charcoal production legacy (CPL)
# Note: Fitting one model with legacy alone, fitting one model without legacy, and then run varpart on these two models and the
#       model with all variables fitted earlier

# Variation partitioning
# Chestnut oak
glm_chestnutoak_occr_new_leg <- glm(chestnutoak_occr ~ DIST*EST + COUNT*EST,
                                    data = chestnutoak[, -c(1:9)],
                                    family = binomial(link = "logit"))

glm_chestnutoak_occr_new_nonleg <- glm(chestnutoak_occr ~ MMAX + PRATIO + DEPTH + SIEVE200 +
                                         ERR15 + SOILSP + TPI150,
                                       data = chestnutoak[, -c(1:9)],
                                       family = binomial(link = "logit"))

# Legacy:non-legacy
ecospat.varpart(glm_chestnutoak_occr_new_leg,
                glm_chestnutoak_occr_new_nonleg,
                glm_chestnutoak_occr_new)

# Northern red oak
# Parametric models
# Logistic regression for presence/absence
# With legacy
glm_redoak_occr <- glm(redoak_occr ~ DIST*EST + COUNT*EST + DIST*COUNT + MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                         SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP + EST + COUNT + DIST,
                       data = redoak[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_redoak_occr, direction = "both")


glm_redoak_occr_new <- glm(redoak_occr ~ DIST*EST + COUNT + PRATIO + OM + CLAY + ERR15 + IMI + ROUGH + TPI2000 + TRASP,
                           data = redoak[, -c(1:9)],
                           family = binomial(link = "logit"))
summary(glm_redoak_occr_new)
pR2(glm_redoak_occr_new)
vif(glm_redoak_occr_new)
anova(glm_redoak_occr_new, test = "Chisq")

# Cross-validation
set.seed(123)
glm_redoak_occr_new_cv <- ecospat.cv.glm(glm_redoak_occr_new, K = 10)
glm_redoak_occr_new_cv$obs <- as.factor(glm_redoak_occr_new_cv$obs)
auc(glm_redoak_occr_new_cv$obs, glm_redoak_occr_new_cv$predictions)

# Without legacy
glm_redoak_occr_aic <- glm(redoak_occr ~ MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                             SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP,
                           data = redoak[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_redoak_occr_aic, direction = "both")

glm_redoak_occr_new_wo <- glm(redoak_occr ~ IMI + ROUGH,
                              data = redoak[, -c(1:9)],
                              family = binomial(link = "logit"))
summary(glm_redoak_occr_new_wo)
pR2(glm_redoak_occr_new_wo)
vif(glm_redoak_occr_new_wo)
anova(glm_redoak_occr_new_wo, test = "Chisq")

# Cross-validation
set.seed(456)
glm_redoak_occr_new_wo_cv <- ecospat.cv.glm(glm_redoak_occr_new_wo, K = 10)
glm_redoak_occr_new_wo_cv$obs <- as.factor(glm_redoak_occr_new_wo_cv$obs)
auc(glm_redoak_occr_new_wo_cv$obs, glm_redoak_occr_new_wo_cv$predictions)

##############################################################################################################################
# Date: 07/09/2023
# Change: Performed variance partitioning for SDM with charcoal production legacy (CPL)
# Note: Fitting one model with legacy alone, fitting one model without legacy, and then run varpart on these two models and the
#       model with all variables fitted earlier

# Northern red oak
glm_redoak_occr_new_leg <- glm(redoak_occr ~ DIST*EST + COUNT,
                               data = redoak[, -c(1:9)],
                               family = binomial(link = "logit"))

glm_redoak_occr_new_nonleg <- glm(redoak_occr ~ PRATIO + OM + CLAY + ERR15 + IMI + ROUGH + TPI2000 + TRASP,
                                  data = redoak[, -c(1:9)],
                                  family = binomial(link = "logit"))

# Legacy:non-legacy
ecospat.varpart(glm_redoak_occr_new_leg,
                glm_redoak_occr_new_nonleg,
                glm_redoak_occr_new)

# Red Maple
# Parametric models
# Logistic regression for presence/absence
# With legacy
glm_redmaple_occr <- glm(redmaple_occr ~ DIST*EST + COUNT*EST + DIST*COUNT + MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                           SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP + EST + COUNT + DIST,
                         data = redmaple[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_redmaple_occr, direction = "both")


glm_redmaple_occr_new <- glm(redmaple_occr ~ DIST*EST + IMI + TPI150 + TPI2000,
                             data = redmaple[, -c(1:9)],
                             family = binomial(link = "logit"))
summary(glm_redmaple_occr_new)
pR2(glm_redmaple_occr_new)
vif(glm_redmaple_occr_new)
anova(glm_redmaple_occr_new, test = "Chisq")

# Cross-validation
set.seed(123)
glm_redmaple_occr_new_cv <- ecospat.cv.glm(glm_redmaple_occr_new, K = 10)
glm_redmaple_occr_new_cv$obs <- as.factor(glm_redmaple_occr_new_cv$obs)
auc(glm_redmaple_occr_new_cv$obs, glm_redmaple_occr_new_cv$predictions)

# Without legacy
glm_redmaple_occr_aic <- glm(redmaple_occr ~ MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                               SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP,
                             data = redmaple[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_redmaple_occr_aic, direction = "both")


glm_redmaple_occr_new_wo <- glm(redmaple_occr ~ FDAY + DEPTH + IMI + ROUGH + TPI2000,
                                data = redmaple[, -c(1:9)],
                                family = binomial(link = "logit"))
summary(glm_redmaple_occr_new_wo)
pR2(glm_redmaple_occr_new_wo)
vif(glm_redmaple_occr_new_wo)
anova(glm_redmaple_occr_new_wo, test = "Chisq")

# Cross-validation
set.seed(456)
glm_redmaple_occr_new_wo_cv <- ecospat.cv.glm(glm_redmaple_occr_new_wo, K = 10)
glm_redmaple_occr_new_wo_cv$obs <- as.factor(glm_redmaple_occr_new_wo_cv$obs)
auc(glm_redmaple_occr_new_wo_cv$obs, glm_redmaple_occr_new_wo_cv$predictions)

##############################################################################################################################
# Date: 07/09/2023
# Change: Performed variance partitioning for SDM with charcoal production legacy (CPL)
# Note: Fitting one model with legacy alone, fitting one model without legacy, and then run varpart on these two models and the
#       model with all variables fitted earlier

# Red maple
glm_redmaple_occr_new_leg <- glm(redmaple_occr ~ DIST*EST,
                                 data = redmaple[, -c(1:9)],
                                 family = binomial(link = "logit"))

glm_redmaple_occr_new_nonleg <- glm(redmaple_occr ~ IMI + TPI150 + TPI2000,
                                    data = redmaple[, -c(1:9)],
                                    family = binomial(link = "logit"))

# Legacy:non-legacy
ecospat.varpart(glm_redmaple_occr_new_leg,
                glm_redmaple_occr_new_nonleg,
                glm_redmaple_occr_new)

# Scarlet oak
# Parametric models
# Logistic regression for presence/absence
# With legacy

glm_scarletoak_occr <- glm(scarletoak_occr ~ DIST*EST + COUNT*EST + DIST*COUNT + MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                             SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP + EST + COUNT + DIST,
                           data = scarletoak[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_scarletoak_occr, direction = "both")


glm_scarletoak_occr_new <- glm(scarletoak_occr ~ DIST*COUNT + FDAY + BULK + DEPTH + CLAY + PERM + IMI + TRASP,
                              data = scarletoak[, -c(1:9)],
                              family = binomial(link = "logit"))
summary(glm_scarletoak_occr_new)
pR2(glm_scarletoak_occr_new)
vif(glm_scarletoak_occr_new)
anova(glm_scarletoak_occr_new, test = "Chisq")

# Cross-validation
set.seed(123)
glm_scarletoak_occr_new_cv <- ecospat.cv.glm(glm_scarletoak_occr_new, K = 10)
glm_scarletoak_occr_new_cv$obs <- as.factor(glm_scarletoak_occr_new_cv$obs)
auc(glm_scarletoak_occr_new_cv$obs, glm_scarletoak_occr_new_cv$predictions)

# Without legacy
glm_scarletoak_occr_aic <- glm(scarletoak_occr ~ MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                                 SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP,
                               data = scarletoak[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_scarletoak_occr_aic, direction = "both")

glm_scarletoak_occr_new_wo <- glm(scarletoak_occr ~ MMAX + PRATIO + CLAY + PERM + SIEVE200 + IMI  + TPI150 + TPI2000,
                                  data = scarletoak[, -c(1:9)],
                                  family = binomial(link = "logit"))
summary(glm_scarletoak_occr_new_wo)
pR2(glm_scarletoak_occr_new_wo)
vif(glm_scarletoak_occr_new_wo)
anova(glm_scarletoak_occr_new_wo, test = "Chisq")

# Cross-validation
set.seed(456)
glm_scarletoak_occr_new_wo_cv <- ecospat.cv.glm(glm_scarletoak_occr_new_wo, K = 10)
glm_scarletoak_occr_new_wo_cv$obs <- as.factor(glm_scarletoak_occr_new_wo_cv$obs)
auc(glm_scarletoak_occr_new_wo_cv$obs, glm_scarletoak_occr_new_wo_cv$predictions)

##############################################################################################################################
# Date: 07/09/2023
# Change: Performed variance partitioning for SDM with charcoal production legacy (CPL)
# Note: Fitting one model with legacy alone, fitting one model without legacy, and then run varpart on these two models and the
#       model with all variables fitted earlier

# Scarlet oak
glm_scarletoak_occr_new_leg <- glm(scarletoak_occr ~ DIST*COUNT,
                                   data = scarletoak[, -c(1:9)],
                                   family = binomial(link = "logit"))

glm_scarletoak_occr_new_nonleg <- glm(scarletoak_occr ~ FDAY + BULK + DEPTH + CLAY + PERM + IMI + TRASP,
                                      data = scarletoak[, -c(1:9)],
                                      family = binomial(link = "logit"))

# Legacy:non-legacy
ecospat.varpart(glm_scarletoak_occr_new_leg,
                glm_scarletoak_occr_new_nonleg,
                glm_scarletoak_occr_new)


# Pitch pine
# Parametric models
# Logistic regression for presence/absence
# With legacy

glm_pitchpine_occr <- glm(pitchpine_occr ~ DIST*EST + COUNT*EST + DIST*COUNT + MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                            SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP + EST + COUNT + DIST,
                          data = pitchpine[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_pitchpine_occr, direction = "both")

# DIST was not significant but we still let it in the model
glm_pitchpine_occr_new <- glm(pitchpine_occr ~ DIST + PRATIO + OM + CLAY + PERM + SIEVE200 + IMI + TRASP + MMAX,
                              data = pitchpine[, -c(1:9)],
                              family = binomial(link = "logit"))
summary(glm_pitchpine_occr_new)
pR2(glm_pitchpine_occr_new)
vif(glm_pitchpine_occr_new)
anova(glm_pitchpine_occr_new, test = "Chisq")

# Cross-validation
set.seed(123)
glm_pitchpine_occr_new_cv <- ecospat.cv.glm(glm_pitchpine_occr_new, K = 10)
glm_pitchpine_occr_new_cv$obs <- as.factor(glm_pitchpine_occr_new_cv$obs)
auc(glm_pitchpine_occr_new_cv$obs, glm_pitchpine_occr_new_cv$predictions)

# Without legacy
glm_pitchpine_occr_aic <- glm(pitchpine_occr ~ MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                                SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP,
                              data = pitchpine[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_pitchpine_occr_aic, direction = "both")

glm_pitchpine_occr_new_wo <- glm(pitchpine_occr ~ MMAX + PRATIO + OM + CLAY + PERM + SIEVE200 + IMI + TRASP,
                                 data = pitchpine[, -c(1:9)],
                                 family = binomial(link = "logit"))
summary(glm_pitchpine_occr_new_wo)
pR2(glm_pitchpine_occr_new_wo)
vif(glm_pitchpine_occr_new_wo)
anova(glm_pitchpine_occr_new_wo, test = "Chisq")

# Cross-validation
set.seed(456)
glm_pitchpine_occr_new_wo_cv <- ecospat.cv.glm(glm_pitchpine_occr_new_wo, K = 10)
glm_pitchpine_occr_new_wo_cv$obs <- as.factor(glm_pitchpine_occr_new_wo_cv$obs)
auc(glm_pitchpine_occr_new_wo_cv$obs, glm_pitchpine_occr_new_wo_cv$predictions)

##############################################################################################################################
# Date: 07/09/2023
# Change: Performed variance partitioning for SDM with charcoal production legacy (CPL)
# Note: Fitting one model with legacy alone, fitting one model without legacy, and then run varpart on these two models and the
#       model with all variables fitted earlier

# Pitch pine
glm_pitchpine_occr_new_leg <- glm(pitchpine_occr ~ DIST,
                                  data = pitchpine[, -c(1:9)],
                                  family = binomial(link = "logit"))

glm_pitchpine_occr_new_nonleg <- glm(pitchpine_occr ~ PRATIO + OM + CLAY + PERM + SIEVE200 + IMI + TRASP + MMAX,
                                     data = pitchpine[, -c(1:9)],
                                     family = binomial(link = "logit"))

# Legacy:non-legacy
ecospat.varpart(glm_pitchpine_occr_new_leg,
                glm_pitchpine_occr_new_nonleg,
                glm_pitchpine_occr_new)


# White oak
# Parametric models
# Logistic regression for presence/absence
# With legacy

glm_whiteoak_occr <- glm(whiteoak_occr ~ DIST*EST + COUNT*EST + DIST*COUNT + MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                           SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP + EST + COUNT + DIST,
                         data = whiteoak[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_whiteoak_occr, direction = "both")


glm_whiteoak_occr_new <- glm(whiteoak_occr ~ DIST + COUNT + BULK + DEPTH + PERM + IMI + TPI2000 + TRASP,
                             data = whiteoak[, -c(1:9)],
                             family = binomial(link = "logit"))
summary(glm_whiteoak_occr_new)
pR2(glm_whiteoak_occr_new)
vif(glm_whiteoak_occr_new)
anova(glm_whiteoak_occr_new, test = "Chisq")

# Cross-validation
set.seed(123)
glm_whiteoak_occr_new_cv <- ecospat.cv.glm(glm_whiteoak_occr_new, K = 10)
glm_whiteoak_occr_new_cv$obs <- as.factor(glm_whiteoak_occr_new_cv$obs)
auc(glm_whiteoak_occr_new_cv$obs, glm_whiteoak_occr_new_cv$predictions)

# Without legacy
glm_whiteoak_occr_aic <- glm(whiteoak_occr ~ MMAX + FDAY + PRATIO + BULK + DEPTH + OM + CLAY + PERM +
                               SIEVE200 + ERR15 + IMI + ROUGH + SOILSP + TPI150 + TPI2000 + TRASP,
                             data = whiteoak[, -c(1:9)], family = binomial(link = "logit"))
stepAIC(glm_whiteoak_occr_aic, direction = "both")

glm_whiteoak_occr_new_wo <- glm(whiteoak_occr ~ MMAX + BULK + DEPTH + PERM + ERR15 + IMI + ROUGH + TPI2000 + TRASP,
                                data = whiteoak[, -c(1:9)],
                                family = binomial(link = "logit"))
summary(glm_whiteoak_occr_new_wo)
pR2(glm_whiteoak_occr_new_wo)
vif(glm_whiteoak_occr_new_wo)
anova(glm_whiteoak_occr_new_wo, test = "Chisq")

# Cross-validation
set.seed(456)
glm_whiteoak_occr_new_wo_cv <- ecospat.cv.glm(glm_whiteoak_occr_new_wo, K = 10)
glm_whiteoak_occr_new_wo_cv$obs <- as.factor(glm_whiteoak_occr_new_wo_cv$obs)
auc(glm_whiteoak_occr_new_wo_cv$obs, glm_whiteoak_occr_new_wo_cv$predictions)

##############################################################################################################################
# Date: 07/09/2023
# Change: Performed variance partitioning for SDM with charcoal production legacy (CPL)
# Note: Fitting one model with legacy alone, fitting one model without legacy, and then run varpart on these two models and the
#       model with all variables fitted earlier

# White oak
glm_whiteoak_occr_new_leg <- glm(whiteoak_occr ~ DIST + COUNT,
                                 data = whiteoak[, -c(1:9)],
                                 family = binomial(link = "logit"))

glm_whiteoak_occr_new_nonleg <- glm(whiteoak_occr ~ BULK + DEPTH + PERM + IMI + TPI2000 + TRASP,
                                    data = whiteoak[, -c(1:9)],
                                    family = binomial(link = "logit"))

# Legacy:non-legacy
ecospat.varpart(glm_whiteoak_occr_new_leg,
                glm_whiteoak_occr_new_nonleg,
                glm_whiteoak_occr_new)


###########################################################################################################################################################
# Change on 06/22/2023
# Visualizing response curve
# Marginal effects of charcoal making on species presence/absence

library(gridExtra)
library(grid)
library(visreg)
# Chestnut oak
chestnutoak_marginal <- visreg(glm_chestnutoak_occr_new, "DIST", by = "EST", data = chestnutoak[, -c(1:9)],
                               scale = "response", gg = TRUE, line = list(size = .8),
                               rug = 0,
                               overlay = TRUE,  band = FALSE, nn = 200,
                               print.cond = TRUE, type = "contrast") +
  scale_x_continuous(name = "Distance to nearest charcoal hearth (km)") +
  scale_y_continuous(name = "Probability of presence", breaks = c(0, 0.5, 1)) +
  scale_color_manual( values = c("#FF5733", "#FFC300", "#154360")) +
  labs(color = "Mean stand <br>age (year)")+
  theme_classic() +
  ggtitle("Chestnut oak") +
  theme_classic() +
  theme(plot.title = element_text(color = "black", size = 10, face = "bold", hjust = 0.5),
        legend.position = c(0.8, 0.5),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_markdown(size = 9),
        axis.title.y = element_markdown(size = 9)) +
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "grey95", color = "white"),
        strip.text = element_markdown(face = "bold"),
        strip.text.y = element_markdown(face = "bold"),
        panel.border = element_blank(),
        legend.title = element_markdown(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(.4, 'cm')) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# Northern red oak
redoak_marginal <- visreg(glm_redoak_occr_new, "DIST", by = "EST", data = redoak[, -c(1:9)],
                          scale = "response", gg = TRUE, line = list(size = .8),
                          rug = 0,
                          overlay = TRUE,  band = FALSE, nn = 200,
                          print.cond = TRUE, type = "contrast") +
  scale_x_continuous(name = "Distance to nearest charcoal hearth (km)") +
  scale_y_continuous(name = "Probability of presence", breaks = c(0, 0.5, 1)) +
  scale_color_manual( values = c("#FF5733", "#FFC300", "#154360")) +
  labs(color = "Mean stand <br>age (year)")+
  theme_classic() +
  ggtitle("Northern red oak") +
  theme_classic() +
  theme(plot.title = element_text(color = "black", size = 10, face = "bold", hjust = 0.5),
        legend.position = c(0.8, 0.5),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_markdown(size = 9),
        axis.title.y = element_markdown(size = 9)) +
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "grey95", color = "white"),
        strip.text = element_markdown(face = "bold"),
        strip.text.y = element_markdown(face = "bold"),
        panel.border = element_blank(),
        legend.title = element_markdown(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(.4, 'cm')) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# Scarlet oak
scarletoak_marginal <- visreg(glm_scarletoak_occr_new, "DIST", by = "COUNT", data = scarletoak[, -c(1:9)],
                              scale = "response", gg = TRUE, line = list(size = .8),
                              rug = 0,
                              overlay = TRUE,  band = FALSE, nn = 200, 
                              print.cond = TRUE, type = "contrast") +
  scale_x_continuous(name = "Distance to nearest charcoal hearth (km)") +
  scale_y_continuous(name = "Probability of presence", breaks = c(0, 0.5, 1)) +
  scale_color_manual( values = c("#FF5733", "#FFC300", "#154360")) +
  labs(color = "Mean number of<br>charcoal hearths")+
  ggtitle("Scarlet oak") +
  theme_classic() +
  theme(plot.title = element_text(color = "black", size = 10, face = "bold", hjust = 0.5),
        legend.position = c(0.8, 0.5),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_markdown(size = 9),
        axis.title.y = element_markdown(size = 9)) +
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "grey95", color = "white"),
        strip.text = element_markdown(face = "bold"),
        strip.text.y = element_markdown(face = "bold"),
        panel.border = element_blank(),
        legend.title = element_markdown(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(.4, 'cm')) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# White oak
whiteoak_marginal <- visreg(glm_whiteoak_occr_new, "DIST", data = whiteoak[, -c(1:9)],
                            scale = "response", gg = TRUE, line.par = list(size = .8, col = "#FF5733"), 
                            rug = 0, 
                            overlay = TRUE,  band = FALSE, nn = 200,
                            print.cond = TRUE, type = "contrast") +
  scale_x_continuous(name = "Distance to nearest charcoal hearth (km)") +
  scale_y_continuous(name = "Probability of presence", breaks = c(0, 0.5, 1), limits = c(0, 1)) +
  #scale_color_manual(values = c("#FF5733")) +
  labs(color = "Mean stand <br>age (year)")+
  ggtitle("White oak") +
  theme_classic() +
  theme(plot.title = element_text(color = "black", size = 10, face = "bold", hjust = 0.5),
        legend.position = c(0.8, 0.5),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_markdown(size = 9),
        axis.title.y = element_markdown(size = 9)) +
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "grey95", color = "white"),
        strip.text = element_markdown(face = "bold"),
        strip.text.y = element_markdown(face = "bold"),
        panel.border = element_blank(),
        legend.title = element_markdown(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(.4, 'cm')) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# Pitch pine
pitchpine_marginal <- visreg(glm(pitchpine_occr ~ DIST + PRATIO + OM + CLAY + PERM + SIEVE200 + IMI + TRASP + MMAX,
                                 data = pitchpine[, -c(1:9)],
                                 family = binomial(link = "logit")), "DIST", data = pitchpine[, -c(1:9)],
                             scale = "response", gg = TRUE, line.par = list(size = .8, col = "#FF5733"), 
                             rug = 0, 
                             overlay = TRUE,  band = FALSE, nn = 200,
                             print.cond = TRUE, type = "contrast") +
  scale_x_continuous(name = "Distance to nearest charcoal hearth (km)") +
  scale_y_continuous(name = "Probability of presence", breaks = c(0, 0.5, 1), limits = c(0, 1)) +
  #scale_color_manual(values = c("#FF5733")) +
  labs(color = "Mean stand <br>age (year)")+
  ggtitle("Pitch pine") +
  theme_classic() +
  theme(plot.title = element_text(color = "black", size = 10, face = "bold", hjust = 0.5),
        legend.position = c(0.8, 0.5),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        axis.title.x = element_markdown(size = 9),
        axis.title.y = element_markdown(size = 9)) +
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "grey95", color = "white"),
        strip.text = element_markdown(face = "bold"),
        strip.text.y = element_markdown(face = "bold"),
        panel.border = element_blank(),
        legend.title = element_markdown(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(.4, 'cm')) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# Red maple
redmaple_marginal <- visreg(glm_redmaple_occr_new, "DIST", by = "EST", data = redmaple[, -c(1:9)],
                            scale = "response", gg = TRUE, line.par = list(size = .8), 
                            rug = 0, 
                            overlay = TRUE,  band = FALSE, nn = 200,
                            print.cond = TRUE, type = "contrast") +
  scale_x_continuous(name = "Distance to nearest charcoal hearth (km)") +
  scale_y_continuous(name = "Probability of presence", breaks = c(0, 0.5, 1), limits = c(0, 1)) +
  scale_color_manual( values = c("#FF5733", "#FFC300", "#154360")) +
  labs(color = "Mean stand <br>age (year)")+
  ggtitle("Red maple") +
  theme_classic() +
  theme(plot.title = element_text(color = "black", size = 10, face = "bold", hjust = 0.5),
    legend.position = c(0.8, 0.5),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8), 
    axis.title.x = element_markdown(size = 9),
    axis.title.y = element_markdown(size = 9)) +
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "grey95", color = "white"),
        strip.text = element_markdown(face = "bold"),
        strip.text.y = element_markdown(face = "bold"),
        panel.border = element_blank(),
        legend.title = element_markdown(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(.4, 'cm')) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# Combine all response curves
marginal_grid_all <- plot_grid(chestnutoak_marginal, redoak_marginal, scarletoak_marginal,
                               whiteoak_marginal, pitchpine_marginal, redmaple_marginal,
                               nrow = 3)

marginal_grid_all

# Save the response curve
ggsave("Response_curve_all.png", width = 16, height = 15, units = "cm", dpi = 200)

# Permutation test on all coefficients
# Chestnut oak example
library(prettyglm)
pretty_coefficients(model_object = glm_chestnutoak_occr_new,
                    type_iii = 'LR', 
                    significance_level = 0.05, 
                    vimethod = 'permute', 
                    target = 'chestnutoak_occr', 
                    metric = 'auc',
                    return_data = FALSE,
                    pred_wrapper = predict.glm, 
                    reference_class = 0)

pretty_coefficients(model_object = glm_redoak_occr_new,
                    type_iii = 'LR', 
                    significance_level = 0.05, 
                    vimethod = 'permute', 
                    target = 'redoak_occr', 
                    metric = 'auc',
                    return_data = FALSE,
                    pred_wrapper = predict.glm, 
                    reference_class = 0)

pretty_coefficients(model_object = glm_scarletoak_occr_new,
                    type_iii = 'LR', 
                    significance_level = 0.05, 
                    vimethod = 'permute', 
                    target = 'scarletoak_occr', 
                    metric = 'auc',
                    return_data = FALSE,
                    pred_wrapper = predict.glm, 
                    reference_class = 0)

pretty_coefficients(model_object = glm_whiteoak_occr_new,
                    type_iii = 'LR', 
                    significance_level = 0.05, 
                    vimethod = 'permute', 
                    target = 'whiteoak_occr', 
                    metric = 'auc',
                    return_data = FALSE,
                    pred_wrapper = predict.glm, 
                    reference_class = 0)

pretty_coefficients(model_object = glm_pitchpine_occr_new,
                    type_iii = 'LR', 
                    significance_level = 0.05, 
                    vimethod = 'permute', 
                    target = 'pitchpine_occr', 
                    metric = 'auc',
                    return_data = FALSE,
                    pred_wrapper = predict.glm, 
                    reference_class = 0)

pretty_coefficients(model_object = glm_redmaple_occr_new,
                    type_iii = 'LR', 
                    significance_level = 0.05, 
                    vimethod = 'permute', 
                    target = 'redmaple_occr', 
                    metric = 'auc',
                    return_data = FALSE,
                    pred_wrapper = predict.glm, 
                    reference_class = 0)

###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
# Change on 07/07/2023
# Model fit and predictive power
Model_fit_and_predictive_power <- read.csv("D:/chapter2_backup/Plot_Tree/Presence_Absence/Modeling/Model fit and predictive power.csv")

chestnutoak_rsq <- ggplot(Model_fit_and_predictive_power[1:2, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

chestnutoak_AUC <- ggplot(Model_fit_and_predictive_power[1:2, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("10-fold CV") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

chestnutoak_fit_pred <- grid.arrange(chestnutoak_rsq, chestnutoak_AUC, ncol = 2,
                                     top = textGrob("Chestnut oak", x = 0.5, hjust = 0.5, 
                                                    gp = gpar(fontface = "bold", fontsize = 10)))

redoak_rsq <- ggplot(Model_fit_and_predictive_power[3:4, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

redoak_AUC <- ggplot(Model_fit_and_predictive_power[3:4, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("10-fold CV") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

redoak_fit_pred <- grid.arrange(redoak_rsq, redoak_AUC, ncol = 2,
                                top = textGrob("Northern red oak", x = 0.5, hjust = 0.5, 
                                               gp = gpar(fontface = "bold", fontsize = 10)))

scarletoak_rsq <- ggplot(Model_fit_and_predictive_power[5:6, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

scarletoak_AUC <- ggplot(Model_fit_and_predictive_power[5:6, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("10-fold CV") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

scarletoak_fit_pred <- grid.arrange(scarletoak_rsq, scarletoak_AUC, ncol = 2,
                                    top = textGrob("Scarlet oak", x = 0.5, hjust = 0.5, 
                                                   gp = gpar(fontface = "bold", fontsize = 10)))


whiteoak_rsq <- ggplot(Model_fit_and_predictive_power[7:8, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

whiteoak_AUC <- ggplot(Model_fit_and_predictive_power[7:8, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("10-fold CV") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

whiteoak_fit_pred <- grid.arrange(whiteoak_rsq, whiteoak_AUC, ncol = 2,
                                  top = textGrob("White oak", x = 0.5, hjust = 0.5, 
                                                 gp = gpar(fontface = "bold", fontsize = 10)))

pitchpine_rsq <- ggplot(Model_fit_and_predictive_power[9:10, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

pitchpine_AUC <- ggplot(Model_fit_and_predictive_power[9:10, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("10-fold CV") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

pitchpine_fit_pred <- grid.arrange(pitchpine_rsq, pitchpine_AUC, ncol = 2,
                                   top = textGrob("Pitch pine", x = 0.5, hjust = 0.5, 
                                                  gp = gpar(fontface = "bold", fontsize = 10)))

redmaple_rsq <- ggplot(Model_fit_and_predictive_power[11:12, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

redmaple_AUC <- ggplot(Model_fit_and_predictive_power[11:12, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("10-fold CV") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

redmaple_fit_pred <- plot_grid(redmaple_rsq, redmaple_AUC, ncol = 2)
redmaple_fit_title <- ggdraw() + draw_label(
  "Red maple",
  fontface = 'bold',
  size = 10,
  hjust = 0.5)
redmaple_fit_pred <- plot_grid(redmaple_fit_title, redmaple_fit_pred, 
                               ncol = 1,
                               rel_heights = c(0.1, 1))


# Combine all boxplots
fit_pred_all <- plot_grid(chestnutoak_fit_pred, redoak_fit_pred, scarletoak_fit_pred,
                          whiteoak_fit_pred, pitchpine_fit_pred, redmaple_fit_pred,
                          nrow = 2)

fit_pred_all

# Save the response curve
ggsave("fit_and_prediction_all.png", fit_pred_all, width = 24, height = 16, units = "cm",  dpi = 200)

###############################################################################################################################
# Date: 2023/07/11
# Change: Calculated AUC for the plots < 113 years

# Cross-validation on plots < 113 years
auc(glm_chestnutoak_occr_new_cv$obs[glm_chestnutoak_occr_new$data$EST <= 113], 
    glm_chestnutoak_occr_new_cv$predictions[glm_chestnutoak_occr_new$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_chestnutoak_occr_new_wo_cv$obs[glm_chestnutoak_occr_new_wo$data$EST <= 113], 
    glm_chestnutoak_occr_new_wo_cv$predictions[glm_chestnutoak_occr_new_wo$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_redoak_occr_new_cv$obs[glm_redoak_occr_new$data$EST <= 113], 
    glm_redoak_occr_new_cv$predictions[glm_redoak_occr_new$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_redoak_occr_new_wo_cv$obs[glm_redoak_occr_new_wo$data$EST <= 113], 
    glm_redoak_occr_new_wo_cv$predictions[glm_redoak_occr_new_wo$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_scarletoak_occr_new_cv$obs[glm_scarletoak_occr_new$data$EST <= 113], 
    glm_scarletoak_occr_new_cv$predictions[glm_scarletoak_occr_new$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_scarletoak_occr_new_wo_cv$obs[glm_scarletoak_occr_new_wo$data$EST <= 113], 
    glm_scarletoak_occr_new_wo_cv$predictions[glm_scarletoak_occr_new_wo$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_whiteoak_occr_new_cv$obs[glm_whiteoak_occr_new$data$EST <= 113], 
    glm_whiteoak_occr_new_cv$predictions[glm_whiteoak_occr_new$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_whiteoak_occr_new_wo_cv$obs[glm_whiteoak_occr_new_wo$data$EST <= 113], 
    glm_whiteoak_occr_new_wo_cv$predictions[glm_whiteoak_occr_new_wo$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_pitchpine_occr_new_cv$obs[glm_pitchpine_occr_new$data$EST <= 113], 
    glm_pitchpine_occr_new_cv$predictions[glm_pitchpine_occr_new$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_pitchpine_occr_new_wo_cv$obs[glm_pitchpine_occr_new_wo$data$EST <= 113], 
    glm_pitchpine_occr_new_wo_cv$predictions[glm_pitchpine_occr_new_wo$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_redmaple_occr_new_cv$obs[glm_redmaple_occr_new$data$EST <= 113], 
    glm_redmaple_occr_new_cv$predictions[glm_redmaple_occr_new$data$EST <= 113])

# Cross-validation on plots < 113 years
auc(glm_redmaple_occr_new_wo_cv$obs[glm_redmaple_occr_new_wo$data$EST <= 113], 
    glm_redmaple_occr_new_wo_cv$predictions[glm_redmaple_occr_new_wo$data$EST <= 113])

# Load data
Model_fit_and_predictive_power_cpl <- read.csv("D:/chapter2_backup/Plot_Tree/Presence_Absence/Modeling/Model_fit_and_predictive_power_cpl.csv")

chestnutoak_cpl_AUC <- ggplot(Model_fit_and_predictive_power_cpl[1:2, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Chestnut oak") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

redoak_cpl_AUC <- ggplot(Model_fit_and_predictive_power_cpl[3:4, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Northern red oak") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

scarletoak_cpl_AUC <- ggplot(Model_fit_and_predictive_power_cpl[5:6, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Scarlet oak") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

whiteoak_cpl_AUC <- ggplot(Model_fit_and_predictive_power_cpl[7:8, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("White oak") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

pitchpine_cpl_AUC <- ggplot(Model_fit_and_predictive_power_cpl[9:10, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Pitch pine") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

redmaple_cpl_AUC <- ggplot(Model_fit_and_predictive_power_cpl[11:12, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("Model") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  scale_x_discrete(labels = c("SDM<sub>BASE</sub>", "SDM<sub>CPL</sub>")) +
  ggtitle("Red maple") +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
        legend.position="none",
        axis.title = element_text(size = 8),
        axis.text.x = element_markdown(size = 7))

# Combine all boxplots
cpl_AUC_all <- plot_grid(chestnutoak_cpl_AUC, redoak_cpl_AUC, scarletoak_cpl_AUC,
                         whiteoak_cpl_AUC, pitchpine_cpl_AUC, redmaple_cpl_AUC,
                         nrow = 2)

cpl_AUC_all

# Save the response curve
ggsave("cpl_AUC_all.png", cpl_AUC_all, width = 12, height = 12, units = "cm",  dpi = 200)




############################################################################################################################
# Plotting results of variation partitioning
# Date: 06/01/2020
############################################################################################################################
# Load data
variation_partitioning_plot <- read.csv("D:/chapter2_backup/Plot_Tree/Presence_Absence/Modeling/variation partitioning_plot.csv")

# Create bar chart
vp <- ggplot(variation_partitioning_plot, aes(fill =  OrderSDM, x = OrderSpecies, y = Deviance)) +
  geom_bar(position = "stack", stat = "identity", color = "black", width = 0.7) +
  scale_y_continuous(name = "Partial deviance explained (%)",
                     limits = c(-.1, .9),
                     breaks = c(-0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                     labels = c("-10%", 0, "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")) +
  scale_x_discrete(name = "",
                   breaks = c("A", "B", "C", "D", "E", "F"), 
                   labels = c("Chestnut\noak", "Northern\nred oak", "Scarlet\noak",
                              "White\noak", "Pitch\npine", "Red\nmaple")) +
  scale_fill_manual(name = "", values = c("#00AFBB",  "grey", "#E69F00"), labels = c(expression(V[BASEF]), expression(V[JF]), expression(V[CPLF]))) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
        legend.position = c(.75, 1.15),
        legend.direction = "horizontal",
        legend.box.just = "right",
        legend.box.margin = margin(t = 4, unit = "cm"),
        #axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9), 
        axis.title.x = element_markdown(size = 9),
        axis.title.y = element_markdown(size = 9),
        legend.title = element_markdown(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(.4, 'cm')) + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

vp

# Save the response curve
ggsave("Variation_partitioning.png", vp, width = 12, height = 10, units = "cm",  dpi = 200)












































































###########################################################################################################################################################
# Change on 02/19/2020
# Test spearman correlation
feature_cor_sub_check <- cor(feature_90m_sub[, -c(50, 51)], method = "spearman")
hc_sub_check <- findCorrelation(feature_cor_sub_check, cutoff = 0.5, exact = TRUE)
hc_sub_check <- sort(hc_sub_check)
reduced_feature_sub_check <- feature_90m_sub[, -c(hc_sub_check, 50, 51)]
head(reduced_feature_sub_check)


# Rename predictors for plotting in the end
names(reduced_feature) <- c("PlotID", "BULK", "DEPTH", "EROD", "OM", "ROUGH", "SOILSP", "TPI", "TRASP", "DD0", "EFFP")
names(hearth_density_500m) <- c("PlotID", "EST", "COUNT")
names(nearest_hearth_dist) <- c("PlotID", "DIST")

# Ignore those 8 NA's when merging the data
chestnutoak <- merge(smt_chestnutoak, reduced_feature_sub, by = "PlotID")
chestnutoak_pre_index <- chestnutoak$chestnutoak_occr == "1"
chestnutoak$chestnutoak_occr[chestnutoak_pre_index] <- "presence"
chestnutoak$chestnutoak_occr[!chestnutoak_pre_index] <- "absence"

redoak <- merge(smt_redoak, reduced_feature_sub, by = "PlotID")
redoak_pre_index <- redoak$redoak_occr == "1"
redoak$redoak_occr[redoak_pre_index] <- "presence"
redoak$redoak_occr[!redoak_pre_index] <- "absence"

redmaple <- merge(smt_redmaple, reduced_feature_sub, by = "PlotID")
redmaple_pre_index <- redmaple$redmaple_occr == "1"
redmaple$redmaple_occr[redmaple_pre_index] <- "presence"
redmaple$redmaple_occr[!redmaple_pre_index] <- "absence"

scarletoak <- merge(smt_scarletoak, reduced_feature_sub, by = "PlotID")
scarletoak_pre_index <- scarletoak$scarletoak_occr == "1"
scarletoak$scarletoak_occr[scarletoak_pre_index] <- "presence"
scarletoak$scarletoak_occr[!scarletoak_pre_index] <- "absence"

pitchpine <- merge(smt_pitchpine, reduced_feature_sub, by = "PlotID")
pitchpine_pre_index <- pitchpine$pitchpine_occr == "1"
pitchpine$pitchpine_occr[pitchpine_pre_index] <- "presence"
pitchpine$pitchpine_occr[!pitchpine_pre_index] <- "absence"

whiteoak <- merge(smt_whiteoak, reduced_feature_sub, by = "PlotID")
whiteoak_pre_index <- whiteoak$whiteoak_occr == "1"
whiteoak$whiteoak_occr[whiteoak_pre_index] <- "presence"
whiteoak$whiteoak_occr[!whiteoak_pre_index] <- "absence"

# Convert presence/absence to factors
chestnutoak$chestnutoak_occr <- as.factor(chestnutoak$chestnutoak_occr)
redoak$redoak_occr <- as.factor(redoak$redoak_occr)
redmaple$redmaple_occr <- as.factor(redmaple$redmaple_occr)
scarletoak$scarletoak_occr <- as.factor(scarletoak$scarletoak_occr)
pitchpine$pitchpine_occr <- as.factor(pitchpine$pitchpine_occr)
whiteoak$whiteoak_occr <- as.factor(whiteoak$whiteoak_occr)


###########################################################################################################################################################
# Change on 10/23/2020
# Test plot type
TYPE_FIA <- read.csv("F:/Chapter2/Plot_Tree/Presence_Absence/Modeling/TYPE_FIA.csv")
TYPE_CFI <- read.csv("F:/Chapter2/Plot_Tree/Presence_Absence/Modeling/TYPE_CFI.csv")
plot_type <- rbind(TYPE_CFI, TYPE_FIA)


# Plot correlation
library(corrplot)
library(RColorBrewer)
corrplot(cor(reduced_feature), type = "upper", order = "hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

###########################################################################################################################################################
# Chestnut oak
# Parametric models
# Logistic regression for presence/absence
# With legacy
# SP name should be changed 05/26/20
glm_chestnutoak_occr <- glm(chestnutoak_occr ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SP + TPI + TRASP + TYPE,
                            data = chestnutoak[, -c(1:5, 7:9)], family = binomial)
stepAIC(glm_chestnutoak_occr, direction = "both",  k = 3.8415)
# Rename SOILSP to as SP for plotting using glm object
names(chestnutoak)[17] <- "SP"

# Change the unit of distance from m to km
# Change the scale of SMRPT from 0,1 to 0,100
chestnutoak$DIST <- chestnutoak$DIST/1000
chestnutoak$SMRPB <- chestnutoak$SMRPB*100

###########################################################################################################################################################
# Change on 10/23/2020
# Test plot type
chestnutoak <- merge(chestnutoak, plot_type, by = "PlotID")
chestnutoak$TYPE <- as.factor(chestnutoak$TYPE)

glm_chestnutoak_occr_final <- glm(chestnutoak_occr ~ DIST + EST + FDAY + SMRPB + SP,
                                  data = chestnutoak[, -c(1:9)],
                                  family = binomial)
summary(glm_chestnutoak_occr_final)
pR2(glm_chestnutoak_occr_final)
vif(glm_chestnutoak_occr_final)
anova(glm_chestnutoak_occr_final, test = "Chisq")


###########################################################################################################################################################
# Change on 02/19/2020
# Test dist < 10km
summary(
  glm(chestnutoak_occr ~ DIST + EST + FDAY + SMRPB + SP + TYPE,
                                  data = chestnutoak[chestnutoak$DIST < 10, -c(1:9)],
                                  family = binomial))

###########################################################################################################################################################
# Change on 02/28/2020
# Test dist < 5km and 2km from charcoal hearth layers
smtplots_2km <- read.csv("F:/Chapter2/Plot_Tree/Presence_Absence/Modeling/smtplots_2km.csv")
smtplots_5km <- read.csv("F:/Chapter2/Plot_Tree/Presence_Absence/Modeling/smtplots_5km.csv")
# Fist check the change in correlation between elevation and charcoal hearth vars
feature_90m_sub_check_2km <- merge(feature_90m_sub, smtplots_2km[, c(1, 4)], by = "PlotID")
feature_90m_sub_check_5km <- merge(feature_90m_sub, smtplots_5km[, c(1, 4)], by = "PlotID")

summary(
  glm(chestnutoak_occr ~ DIST + EST + FDAY + SMRPB + SP,
      data = chestnutoak[(chestnutoak$PlotID)%in%(feature_90m_sub_check_5km$PlotID), -c(1:9)],
      family = binomial)
  )

###########################################################################################################################################################
# Change on 02/28/2020
# Test elevation
chestnutoak_check_elev <- merge(chestnutoak, feature_90m_sub[, c("PlotID", "elev")], by = "PlotID")

glm_chestnutoak_occr_check_elev <- glm(chestnutoak_occr ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SP + TPI + TRASP + elev,
                            data = chestnutoak_check_elev[, -c(1:9)], family = binomial)
stepAIC(glm_chestnutoak_occr, direction = "both",  k = 3.8415)

summary(
  glm(chestnutoak_occr ~ DIST + EST + FDAY + SMRPB + SP + elev,
      data = chestnutoak_check_elev[, -c(1:9)],
      family = binomial)
)




# Cross-validation
set.seed(1234)
glm_chestnutoak_occr_final_cv <- ecospat.cv.glm(glm_chestnutoak_occr_final, K = 5)
glm_chestnutoak_occr_final_cv$obs <- as.factor(glm_chestnutoak_occr_final_cv$obs)
auc(glm_chestnutoak_occr_final_cv$obs, glm_chestnutoak_occr_final_cv$predictions)

# Without legacy
glm_chestnutoak_occr_final_wo <- glm(chestnutoak_occr ~ FDAY + SMRPB + SOILSP,
                                  data = chestnutoak[, -c(1:5, 7:9)],
                                  family = binomial)
summary(glm_chestnutoak_occr_final_wo)
pR2(glm_chestnutoak_occr_final_wo)
vif(glm_chestnutoak_occr_final_wo)
anova(glm_chestnutoak_occr_final_wo, test = "Chisq")

# Cross-validation
set.seed(1234)
glm_chestnutoak_occr_final_wo_cv <- ecospat.cv.glm(glm_chestnutoak_occr_final_wo, K = 5)
glm_chestnutoak_occr_final_wo_cv$obs <- as.factor(glm_chestnutoak_occr_final_wo_cv$obs)
auc(glm_chestnutoak_occr_final_wo_cv$obs, glm_chestnutoak_occr_final_wo_cv$predictions)

# Variation partitioning
glm_chestnutoak_occr_final_leg <- glm(chestnutoak_occr ~ DIST + EST,
                                  data = chestnutoak[, -c(1:5, 7:9)],
                                  family = binomial)

glm_chestnutoak_occr_final_nonleg <- glm(chestnutoak_occr ~ FDAY + SMRPB + SP,
                                    data = chestnutoak[, -c(1:5, 7:9)],
                                    family = binomial)

glm_chestnutoak_occr_final_both <- glm(chestnutoak_occr ~ DIST + EST + FDAY + SMRPB + SP,
                                       data = chestnutoak[, -c(1:5, 7:9)],
                                       family = binomial)

# Legacy:non-legacy
ecospat.varpart(glm_chestnutoak_occr_final_leg,
                glm_chestnutoak_occr_final_nonleg,
                glm_chestnutoak_occr_final_both)


# Binned residual plot for logistic regression because the data are discrete and so are the residuals
library(arm)
binnedplot(fitted(glm_chestnutoak_occr_final), resid(glm_chestnutoak_occr_final, type = "response"))

# Logistic regression for abundance
# Logistic regression
glm_chestnutoak_abun_ba <- glm(chestnutoak_n ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SP + TPI + TRASP,
                               weights = chestnutoak$total_n,
                               data = chestnutoak,
                               family = binomial(link = "logit"))
stepAIC(glm_chestnutoak_abun_ba, direction = "both", k = 3.8415)

###########################################################################################################################################################
# Change on 02/19/2020
# Test dist < 10km for modeling abundance

plot(chestnutoak$DIST, chestnutoak$chestnutoak_n)
summary(glm(chestnutoak_totaln/total_n ~ DIST + EST + FDAY + SMRPB + SP + TYPE,
            weights = chestnutoak$total_n[chestnutoak$DIST < 15],
            data = chestnutoak[chestnutoak$DIST < 15, ],
            family = binomial)
  )

glm_chestnutoak_abun_ba_final <- glm(chestnutoak_ba ~ DIST + EST + FDAY + SP,
                                     family = quasibinomial,
                                     data = chestnutoak)

summary(glm_chestnutoak_abun_ba_final)
anova(glm_chestnutoak_abun_ba_final, test = "Chisq")

###########################################################################################################################################################
# Northern red oak
# Parametric models
# Logistic regression for presence/absence
# With legacy
glm_redoak_occr <- glm(redoak_occr ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SOILSP + TPI + TRASP,
                       data = redoak[ -c(1:5, 7:9)], family = binomial)
stepAIC(glm_redoak_occr, direction = "both", k = 3.8415)

# Change on 10/23/2020
# Test plot type
redoak <- merge(redoak, plot_type, by = "PlotID")
redoak$TYPE <- as.factor(redoak$TYPE)

glm_redoak_occr_final <- glm(redoak_occr ~ COUNT,
                             data = redoak[, -c(1:5, 7:9)],
                             family = binomial)
summary(glm_redoak_occr_final)
pR2(glm_redoak_occr_final)
vif(glm_redoak_occr_final)
anova(glm_redoak_occr_final, test = "Chisq")

###########################################################################################################################################################
# Change on 02/19/2020
# Test dist < 10km
summary(glm(redoak_occr ~ COUNT,
            data = redoak[redoak$DIST < 10000, -c(1:5, 7:9)],
            family = binomial))

# Cross-validation
set.seed(1234)
glm_redoak_occr_final_cv <- ecospat.cv.glm(glm_redoak_occr_final, K = 5)
glm_redoak_occr_final_cv$obs <- as.factor(glm_redoak_occr_final_cv$obs)
auc(glm_redoak_occr_final_cv$obs, glm_redoak_occr_final_cv$predictions)

# Without legacy
glm_redoak_occr_final_wo <- glm(redoak_occr ~ 1, 
                                data = redoak[, -c(1:5, 7:9)],
                                family = binomial)
summary(glm_redoak_occr_final_wo)
pR2(glm_redoak_occr_final_wo)
vif(glm_redoak_occr_final_wo)
anova(glm_redoak_occr_final_wo, test = "Chisq")

# Variation partitioning
glm_redoak_occr_final_w <- glm(redoak_occr ~ COUNT,
                               data = redoak[, -c(1:5, 7:9)],
                               family = binomial)

# Legacy:intercept
ecospat.varpart(glm_redoak_occr_final_w,
                glm_redoak_occr_final_wo,
                glm_redoak_occr_final)

# Cross-validation
set.seed(1234)
glm_redoak_occr_final_wo_cv <- ecospat.cv.glm(glm_redoak_occr_final_wo, K = 5)
glm_redoak_occr_final_wo_cv$obs <- as.factor(glm_redoak_occr_final_wo_cv$obs)
auc(glm_redoak_occr_final_wo_cv$obs, glm_redoak_occr_final_wo_cv$predictions)

# Binned residual plot for logistic regression because the data are discrete and so are the residuals
library(arm)
binnedplot(fitted(glm_redoak_occr_final), resid(glm_redoak_occr_final, type = "response"))

# Logistic regression for abundance
# Logistic regression
glm_redoak_abun_ba <- glm(redoak_ba ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SOILSP + TPI + TRASP,
                          data = redoak,
                          family = binomial)
stepAIC(glm_redoak_abun_ba, direction = "both", k = 3.8415)

glm_redoak_abun_ba_final <- glm(redoak_ba ~ COUNT,
                                family = quasibinomial,
                                data = redoak)
summary(glm_redoak_abun_ba_final)
anova(glm_redoak_abun_ba_final, test = "Chisq")

###########################################################################################################################################################
# Scarlet oak
# Parametric models
# Logistic regression for presence/absence
# With legacy
glm_scarletoak_occr <- glm(scarletoak_occr ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SOILSP + TPI + TRASP,
                           data = scarletoak[ -c(1:5, 7:9)], family = binomial)
stepAIC(glm_scarletoak_occr, direction = "both",  k = 3.8415)

# Change the unit of distance from m to km
# Change the scale of SMRPT from 0,1 to 0,100
scarletoak$DIST <- scarletoak$DIST/1000
scarletoak$SMRPB <- scarletoak$SMRPB*100

# Change on 10/23/2020
# Test plot type
scarletoak <- merge(scarletoak, plot_type, by = "PlotID")
scarletoak$TYPE <- as.factor(scarletoak$TYPE)

glm_scarletoak_occr_final <- glm(scarletoak_occr ~ DIST + SMRPB + DEPTH + TPI + TYPE,
                                 data = scarletoak[, -c(1:5, 7:9)],
                                 family = binomial)
summary(glm_scarletoak_occr_final)
pR2(glm_scarletoak_occr_final)
vif(glm_scarletoak_occr_final)
anova(glm_scarletoak_occr_final, test = "Chisq")

###########################################################################################################################################################
# Change on 02/19/2020
# Test dist < 10km
summary(glm(scarletoak_occr ~ DIST + SMRPB + DEPTH + TPI + TYPE,
                                 data = scarletoak[scarletoak$DIST < 10, -c(1:5, 7:9)],
                                 family = binomial))


# Cross-validation
set.seed(1234)
glm_scarletoak_occr_final_cv <- ecospat.cv.glm(glm_scarletoak_occr_final, K = 5)
glm_scarletoak_occr_final_cv$obs <- as.factor(glm_scarletoak_occr_final_cv$obs)
auc(glm_scarletoak_occr_final_cv$obs, glm_scarletoak_occr_final_cv$predictions)

# Without legacy
glm_scarletoak_occr_final_wo <- glm(scarletoak_occr ~ SMRPB + DEPTH + TPI,
                                    data = scarletoak[, -c(1:5, 7:9)],
                                    family = binomial)
summary(glm_scarletoak_occr_final_wo)
pR2(glm_scarletoak_occr_final_wo)
vif(glm_scarletoak_occr_final_wo)
anova(glm_scarletoak_occr_final_wo, test = "Chisq")

# Cross-validation
set.seed(1234)
glm_scarletoak_occr_final_wo_cv <- ecospat.cv.glm(glm_scarletoak_occr_final_wo, K = 5)
glm_scarletoak_occr_final_wo_cv$obs <- as.factor(glm_scarletoak_occr_final_wo_cv$obs)
auc(glm_scarletoak_occr_final_wo_cv$obs, glm_scarletoak_occr_final_wo_cv$predictions)

# Variation partitioning
glm_scarletoak_occr_final_leg <- glm(scarletoak_occr ~ DIST,
                                     data = scarletoak[, -c(1:5, 7:9)],
                                     family = binomial)

glm_scarletoak_occr_final_nonleg <- glm(scarletoak_occr ~ SMRPB + DEPTH + TPI,
                                        data = scarletoak[, -c(1:5, 7:9)],
                                        family = binomial)

glm_scarletoak_occr_final_both <- glm(scarletoak_occr ~ DIST + SMRPB + DEPTH + TPI,
                                      data = scarletoak[, -c(1:5, 7:9)],
                                      family = binomial)

# Legacy:non-legacy
ecospat.varpart(glm_scarletoak_occr_final_leg,
                glm_scarletoak_occr_final_nonleg,
                glm_scarletoak_occr_final_both)

# Binned residual plot for logistic regression because the data are discrete and so are the residuals
library(arm)
binnedplot(fitted(glm_scarletoak_occr_final), resid(glm_scarletoak_occr_final, type = "response"))

# Logistic regression for abundance
# Logistic regression
glm_scarletoak_abun_ba <- glm(scarletoak_ba ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SOILSP + TPI + TRASP,
                              data = scarletoak,
                              family = binomial)
stepAIC(glm_scarletoak_abun_ba, direction = "both", k = 3.8415)

glm_scarletoak_abun_ba_final <- glm(scarletoak_ba ~ SMRPB + DEPTH + DIST,
                                    family = quasibinomial,
                                    data = scarletoak)
summary(glm_scarletoak_abun_ba_final)
anova(glm_scarletoak_abun_ba_final, test = "Chisq")

###########################################################################################################################################################
# White oak
# Parametric models
# Logistic regression for presence/absence
# With legacy
glm_whiteoak_occr <- glm(whiteoak_occr ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SOILSP + TPI + TRASP,
                         data = whiteoak[ -c(1:5, 7:9)], family = binomial)
stepAIC(glm_whiteoak_occr, direction = "both",  k = 3.8415)

# Change the unit of distance from m to km
# Change the scale of SMRPB from 0,1 to 0,100
# Change the scale of EROD from 0,1 to 0,100
whiteoak$DIST <- whiteoak$DIST/1000
whiteoak$SMRPB <- whiteoak$SMRPB*100
whiteoak$EROD <- whiteoak$EROD*100

# Change on 10/23/2020
# Test plot type
whiteoak <- merge(whiteoak, plot_type, by = "PlotID")
whiteoak$TYPE <- as.factor(whiteoak$TYPE)

# Final model
glm_whiteoak_occr_final <- glm(whiteoak_occr ~ DIST + COUNT + DEPTH + EROD + TPI + TRASP + TYPE,
                               data = whiteoak[, -c(1:5, 7:9)],
                               family = binomial)
summary(glm_whiteoak_occr_final)
pR2(glm_whiteoak_occr_final)
vif(glm_whiteoak_occr_final)
anova(glm_whiteoak_occr_final, test = "Chisq")

###########################################################################################################################################################
# Change on 02/19/2020
# Test dist < 10km
summary(glm(whiteoak_occr ~ DIST + COUNT + DEPTH + EROD + TPI + TRASP + TYPE,
                               data = whiteoak[whiteoak$DIST < 10, -c(1:5, 7:9)],
                               family = binomial))

# Cross-validation
set.seed(1234)
glm_whiteoak_occr_final_cv <- ecospat.cv.glm(glm_whiteoak_occr_final, K = 5)
glm_whiteoak_occr_final_cv$obs <- as.factor(glm_whiteoak_occr_final_cv$obs)
auc(glm_whiteoak_occr_final_cv$obs, glm_whiteoak_occr_final_cv$predictions)

# Without legacy
glm_whiteoak_occr_final_wo <- glm(whiteoak_occr ~ DEPTH + EROD + TPI + TRASP,
                                  data = whiteoak[, -c(1:5, 7:9)],
                                  family = binomial)
summary(glm_whiteoak_occr_final_wo)
pR2(glm_whiteoak_occr_final_wo)
vif(glm_whiteoak_occr_final_wo)
anova(glm_whiteoak_occr_final_wo, test = "Chisq")

# Cross-validation
set.seed(1234)
glm_whiteoak_occr_final_wo_cv <- ecospat.cv.glm(glm_whiteoak_occr_final_wo, K = 5)
glm_whiteoak_occr_final_wo_cv$obs <- as.factor(glm_whiteoak_occr_final_wo_cv$obs)
auc(glm_whiteoak_occr_final_wo_cv$obs, glm_whiteoak_occr_final_wo_cv$predictions)

# Variation partitioning
glm_whiteoak_occr_final_leg <- glm(whiteoak_occr ~ DIST + COUNT,
                                   data = whiteoak[, -c(1:5, 7:9)],
                                   family = binomial)

glm_whiteoak_occr_final_nonleg <- glm(whiteoak_occr ~ DEPTH + EROD + TPI + TRASP,
                                      data = whiteoak[, -c(1:5, 7:9)],
                                      family = binomial)

glm_whiteoak_occr_final_both <- glm(whiteoak_occr ~ DIST + COUNT + DEPTH + EROD + TPI + TRASP,
                                    data = whiteoak[, -c(1:5, 7:9)],
                                    family = binomial)

# Legacy:non-legacy
ecospat.varpart(glm_whiteoak_occr_final_leg,
                glm_whiteoak_occr_final_nonleg,
                glm_whiteoak_occr_final_both)


# Binned residual plot for logistic regression because the data are discrete and so are the residuals
library(arm)
binnedplot(fitted(glm_whiteoak_occr_final), resid(glm_whiteoak_occr_final, type = "response"))

# Logistic regression for abundance
# Logistic regression
glm_whiteoak_abun_ba <- glm(whiteoak_ba ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SOILSP + TPI + TRASP,
                            data = whiteoak,
                            family = binomial)
stepAIC(glm_whiteoak_abun_ba, direction = "both", k = 3.8415)

glm_whiteoak_abun_ba_final <- glm(whiteoak_ba ~ DIST + SOILSP,
                                  family = quasibinomial,
                                  data = whiteoak)
summary(glm_whiteoak_abun_ba_final)
anova(glm_whiteoak_abun_ba_final, test = "Chisq")

###########################################################################################################################################################
# Pitch pine
# Parametric models
# Logistic regression for presence/absence
# With legacy

glm_pitchpine_occr <- glm(pitchpine_occr ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SOILSP + TPI + TRASP,
                               data = pitchpine[ -c(1:5, 7:9)], family = binomial)
stepAIC(glm_pitchpine_occr, direction = "both",  k = 3.8415)

# Change the unit of distance from m to km
# Change the scale of SMRPB from 0,1 to 0,100
pitchpine$DIST <- pitchpine$DIST/1000
pitchpine$SMRPB <- pitchpine$SMRPB*100

# Change on 10/23/2020
# Test plot type
pitchpine <- merge(pitchpine, plot_type, by = "PlotID")
pitchpine$TYPE <- as.factor(pitchpine$TYPE)

# Final model
glm_pitchpine_occr_final <- glm(pitchpine_occr ~ DIST + SMRPB + TRASP,
                                data = pitchpine[, -c(1:5, 7:9)],
                                family = binomial)
summary(glm_pitchpine_occr_final)
pR2(glm_pitchpine_occr_final)
vif(glm_pitchpine_occr_final)
anova(glm_pitchpine_occr_final, test = "Chisq")

###########################################################################################################################################################
# Change on 02/19/2020
# Test dist < 20km
summary(glm(pitchpine_occr ~ DIST + SMRPB + TRASP,
                                data = pitchpine[pitchpine$DIST < 20, -c(1:5, 7:9)],
                                family = binomial))


# Cross-validation
set.seed(1234)
glm_pitchpine_occr_final_cv <- ecospat.cv.glm(glm_pitchpine_occr_final, K = 5)
glm_pitchpine_occr_final_cv$obs <- as.factor(glm_pitchpine_occr_final_cv$obs)
auc(glm_pitchpine_occr_final_cv$obs, glm_pitchpine_occr_final_cv$predictions)

# Without legacy
glm_pitchpine_occr_final_wo <- glm(pitchpine_occr ~ SMRPB + TRASP,
                                   data = pitchpine[, -c(1:5, 7:9)],
                                   family = binomial)
summary(glm_pitchpine_occr_final_wo)
pR2(glm_pitchpine_occr_final_wo)
vif(glm_pitchpine_occr_final_wo)
anova(glm_pitchpine_occr_final_wo, test = "Chisq")

# Cross-validation
set.seed(1234)
glm_pitchpine_occr_final_wo_cv <- ecospat.cv.glm(glm_pitchpine_occr_final_wo, K = 5)
glm_pitchpine_occr_final_wo_cv$obs <- as.factor(glm_pitchpine_occr_final_wo_cv$obs)
auc(glm_pitchpine_occr_final_wo_cv$obs, glm_pitchpine_occr_final_wo_cv$predictions)

# Variation partitioning
glm_pitchpine_occr_final_leg <- glm(pitchpine_occr ~ DIST,
                                    data = pitchpine[, -c(1:5, 7:9)],
                                    family = binomial)

glm_pitchpine_occr_final_nonleg <- glm(pitchpine_occr ~ SMRPB + TRASP,
                                       data = pitchpine[, -c(1:5, 7:9)],
                                       family = binomial)

glm_pitchpine_occr_final_both <- glm(pitchpine_occr ~ DIST + SMRPB + TRASP,
                                     data = pitchpine[, -c(1:5, 7:9)],
                                     family = binomial)

# Legacy:non-legacy
ecospat.varpart(glm_pitchpine_occr_final_leg,
                glm_pitchpine_occr_final_nonleg,
                glm_pitchpine_occr_final_both)


# Binned residual plot for logistic regression because the data are discrete and so are the residuals
library(arm)
binnedplot(fitted(glm_pitchpine_occr_final), resid(glm_pitchpine_occr_final, type = "response"))


# Logistic regression for abundance
# Logistic regression
glm_pitchpine_abun_ba <- glm(pitchpine_ba ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SOILSP + TPI + TRASP,
                            data = pitchpine,
                            family = binomial)
stepAIC(glm_pitchpine_abun_ba, direction = "both", k = 3.8415)

glm_pitchpine_abun_ba_final <- glm(pitchpine_ba ~ DIST + SMRPB + TRASP,
                                  family = quasibinomial,
                                  data = pitchpine)
summary(glm_pitchpine_abun_ba_final)
anova(glm_pitchpine_abun_ba_final, test = "Chisq")

###########################################################################################################################################################
# Red maple
# Parametric models
# Logistic regression for presence/absence
# With legacy
glm_redmaple_occr <- glm(redmaple_occr ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SOILSP + TPI + TRASP,
                         data = redmaple[ -c(1:5, 7:9)], family = binomial)
stepAIC(glm_redmaple_occr, direction = "both",  k = 3.8415)

# Change the unit of distance from m to km
# Change the scale of EROD from 0,1 to 0,100
redmaple$DIST <- redmaple$DIST/1000
redmaple$EROD <- redmaple$EROD*100

# Change on 10/23/2020
# Test plot type
redmaple <- merge(redmaple, plot_type, by = "PlotID")
redmaple$TYPE <- as.factor(redmaple$TYPE)

glm_redmaple_occr_final <- glm(redmaple_occr ~ DIST + EROD + TPI + DIST:EST + TYPE,
                               data = redmaple[, -c(1:5, 7:9)],
                               family = binomial)
summary(glm_redmaple_occr_final)
pR2(glm_redmaple_occr_final)
vif(glm_redmaple_occr_final)
anova(glm_redmaple_occr_final, test = "Chisq")

###########################################################################################################################################################
# Change on 02/19/2020
# Test dist < 20km
summary(glm(redmaple_occr ~ DIST + EROD + TPI + DIST:EST + TYPE,
                               data = redmaple[redmaple$DIST< 10, -c(1:5, 7:9)],
                               family = binomial))


# Cross-validation
set.seed(1234)
glm_redmaple_occr_final_cv <- ecospat.cv.glm(glm_redmaple_occr_final, K = 5)
glm_redmaple_occr_final_cv$obs <- as.factor(glm_redmaple_occr_final_cv$obs)
auc(glm_redmaple_occr_final_cv$obs, glm_redmaple_occr_final_cv$predictions)

# Without legacy
glm_redmaple_occr_final_wo <- glm(redmaple_occr ~ EROD + TPI,
                                  data = redmaple[, -c(1:5, 7:9)],
                                  family = binomial)
summary(glm_redmaple_occr_final_wo)
pR2(glm_redmaple_occr_final_wo)
vif(glm_redmaple_occr_final_wo)
anova(glm_redmaple_occr_final_wo, test = "Chisq")

# Cross-validation
set.seed(1234)
glm_redmaple_occr_final_wo_cv <- ecospat.cv.glm(glm_redmaple_occr_final_wo, K = 5)
glm_redmaple_occr_final_wo_cv$obs <- as.factor(glm_redmaple_occr_final_wo_cv$obs)
auc(glm_redmaple_occr_final_wo_cv$obs, glm_redmaple_occr_final_wo_cv$predictions)

# Variation partitioning
glm_redmaple_occr_final_leg <- glm(redmaple_occr ~ DIST + DIST:EST,
                                   data = redmaple[, -c(1:5, 7:9)],
                                   family = binomial)

glm_redmaple_occr_final_nonleg <- glm(redmaple_occr ~ EROD + TPI,
                                      data = redmaple[, -c(1:5, 7:9)],
                                      family = binomial)

glm_redmaple_occr_final_both <- glm(redmaple_occr ~ DIST + EROD + TPI + DIST:EST,
                                    data = redmaple[, -c(1:5, 7:9)],
                                    family = binomial)

# Legacy:non-legacy
ecospat.varpart(glm_redmaple_occr_final_leg,
                glm_redmaple_occr_final_nonleg,
                glm_redmaple_occr_final_both)


# Binned residual plot for logistic regression because the data are discrete and so are the residuals
library(arm)
binnedplot(fitted(glm_redmaple_occr_final), resid(glm_redmaple_occr_final, type = "response"))

# Logistic regression for abundance
# Logistic regression
glm_redmaple_abun_ba <- glm(redmaple_ba ~ DIST*EST + COUNT*EST + FDAY + SMRPB + BULK + DEPTH + EROD + OM + SOILSP + TPI + TRASP,
                            data = redmaple,
                            family = binomial)
stepAIC(glm_redmaple_abun_ba, direction = "both", k = 3.8415)

glm_redmaple_abun_ba_final <- glm(redmaple_ba ~ DIST + EROD + TPI + DIST:EST,
                                  family = quasibinomial,
                                  data = redmaple)
summary(glm_redmaple_abun_ba_final)
anova(glm_redmaple_abun_ba_final, test = "Chisq")

#########################################################################################################################################
#########################################################################################################################################



# Boxplot of two variables regarding charcoal hearth
library(ggplot2)
library(data.table)

box_chestnutoak <- chestnutoak[redmaple$redmaple_ba > 0 & redmaple$Nearest_Dist <5000]
box_chestnutoak$SP <- "Chestnut oak"

box_redoak <- redoak
box_redoak$SP <- "Northern red oak"

box_scarletoak <- scarletoak
box_scarletoak$SP <- "Scarlet oak"

box_whiteoak <- whiteoak
box_whiteoak$SP <- "White oak"

box_redmaple <- redmaple[redmaple$redmaple_ba > 0 & redmaple$Nearest_Dist <5000]
box_redmaple$SP <- "Red maple"

box_pitchpine <- pitchpine
box_pitchpine$SP <- "Pitch pine"

box_all_species <- rbindlist(list(box_chestnutoak, box_redoak, box_scarletoak, box_whiteoak, box_redmaple, box_pitchpine))

ggplot(box_all_species, aes(x= OCCR, y = Hearth_Count)) + 
  geom_boxplot() +
  facet_wrap(~SP, scales = "free")

ggplot(box_all_species, aes(x= OCCR, y = Nearest_Dist)) + 
  geom_boxplot() +
  facet_wrap(~SP, scales = "free")


########################################################################################################################################
# PLotting models
# Coefficients of SDMs for each species
library(cowplot)
chestnutoak_coef <- plot_model(glm_chestnutoak_occr_final,
                               transform = NULL,
                               show.values = TRUE,
                               axis.labels = "",
                               value.offset = .4, value.size = 3) +
  ggtitle("Chestnut oak") + geom_point(shape = 21, color = "black", size = 3, stroke = 1.5) +
  ylab("Estimate") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(size = 9))
           
redoak_coef <- plot_model(glm_redoak_occr_final,
                      transform = NULL,
                      show.values = TRUE,
                      axis.labels = "",
                      value.offset = .4, value.size = 3) +
  ggtitle("Northern red oak") + geom_point(shape = 21, color = "black", size = 3, stroke = 1.5) +
  ylab("Estimate") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(size = 9))
           

scarletoak_coef <- plot_model(glm_scarletoak_occr_final,
                              transform = NULL,
                              show.values = TRUE,
                              axis.labels = "",
                              value.offset = .4, value.size = 3) +
  ggtitle("Scarlet oak") + geom_point(shape = 21, color = "black", size = 3, stroke = 1.5) +
  ylab("Estimate") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(size = 9))
           

whiteoak_coef <- plot_model(glm_whiteoak_occr_final,
                            transform = NULL,
                            show.values = TRUE,
                            axis.labels = "",
                            value.offset = .4, value.size = 3) +
  ggtitle("White oak") + geom_point(shape = 21, color = "black", size = 3, stroke = 1.5) +
  ylab("Estimate") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(size = 9))
          

pitchpine_coef <- plot_model(glm_pitchpine_occr_final,
                             transform = NULL,
                             show.values = TRUE,
                             axis.labels = "",
                             value.offset = .4, value.size = 3) +
  ggtitle("Pitch pine") + geom_point(shape = 21, color = "black", size = 3, stroke = 1.5) +
  ylab("Estimate") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(size = 9))
           

redmaple_coef <- plot_model(glm_redmaple_occr_final,
                            transform = NULL,
                            show.values = TRUE,
                            axis.labels = "",
                            value.offset = .4, value.size = 3) +
  ggtitle("Red maple") + geom_point(shape = 21, color = "black", size = 3, stroke = 1.5) +
  ylab("Estimate") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(size = 9))

coef_all <- plot_grid(chestnutoak_coef, redoak_coef, scarletoak_coef,
                      whiteoak_coef, pitchpine_coef, redmaple_coef,
                      nrow = 3)
coef_all

# Marginal effects of charcoal making on species presence/absence
library(grid)
chestnutoak_grid <- grobTree(#rectGrob(gp = gpar(fill = "white")),
                             textGrob("Chestnut oak", x = 0.5, hjust = 0.5, gp = gpar(col = "black", fontface = "bold", fontsize = 10)))

chestnutoak_marginal <- visreg(glm_chestnutoak_occr_final, "DIST", scale = "response", gg = TRUE, line = list(col = "black", size = 1.3)) + 
  scale_x_continuous(limit = c(0, 24000), breaks = c(0, 5000, 10000, 15000, 20000), labels = c(0, 5, 10, 15, 20)) +
  scale_y_continuous(limit = c(0, 1), breaks = c(0, 0.5, 1)) + 
  ggtitle(element_blank()) + 
  ggtitle(element_blank()) + 
  ylab("Probability of presence") + 
  xlab("DIST (km)") +
  theme(axis.title = element_text(size = 9),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none")

chestnutoak_marginal_grid <- grid.arrange(chestnutoak_grid, chestnutoak_marginal, heights = c(1, 12))

redoak_grid <- grobTree(rectGrob(gp = gpar(fill = "#009E73")),
                        textGrob("Northern red oak", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

redoak_marginal <- visreg(glm_redoak_occr_final, "COUNT", scale = "response", gg = TRUE, line = list(col = "black", size = 1.3)) + 
  scale_x_continuous(limit = c(0, 20), breaks = c(0, 4, 8, 12, 16)) +
  scale_y_continuous(limit = c(0, 1), breaks = c(0, 0.5, 1)) +
  ggtitle(element_blank()) + 
  ggtitle(element_blank()) + 
  ylab("Probability of presence") + 
  theme(axis.title = element_text(size = 9),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none")


redoak_marginal_grid <- grid.arrange(redoak_grid, redoak_marginal, heights = c(1, 12))

scarletoak_grid <- grobTree(rectGrob(gp = gpar(fill = "#009E73")),
                            textGrob("Scarlet oak", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

scarletoak_marginal <- visreg(glm_scarletoak_occr_final, "DIST", scale = "response", gg = TRUE, line = list(col = "black", size = 1.3)) + 
  scale_x_continuous(limit = c(0, 24000), breaks = c(0, 5000, 10000, 15000, 20000), labels = c(0, 5, 10, 15, 20)) +
  scale_y_continuous(limit = c(0, 1), breaks = c(0, 0.5, 1)) + 
  ggtitle(element_blank()) + 
  ggtitle(element_blank()) + 
  ylab("Probability of presence") + 
  xlab("DIST (km)") +
  theme(axis.title = element_text(size = 9),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none")


scarletoak_marginal_grid <- grid.arrange(scarletoak_grid, scarletoak_marginal, heights = c(1, 12))

whiteoak_grid <- grobTree(rectGrob(gp = gpar(fill = "#009E73")),
                          textGrob("White oak", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

whiteoak_marginal <- visreg(glm_whiteoak_occr_final, "DIST", scale = "response", gg = TRUE, line = list(col = "black", size = 1.3)) + 
  scale_x_continuous(limit = c(0, 24000), breaks = c(0, 5000, 10000, 15000, 20000), labels = c(0, 5, 10, 15, 20)) +
  scale_y_continuous(limit = c(0, 1), breaks = c(0, 0.5, 1)) + 
  ggtitle(element_blank()) + 
  ggtitle(element_blank()) + 
  ylab("Probability of presence") + 
  xlab("DIST (km)") +
  theme(axis.title = element_text(size = 9),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none")


whiteoak_marginal_grid <- grid.arrange(whiteoak_grid, whiteoak_marginal, heights = c(1, 12))

pitchpine_grid <- grobTree(rectGrob(gp = gpar(fill = "#009E73")),
                           textGrob("Pitch pine", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

pitchpine_marginal <- visreg(glm_pitchpine_occr_final, "DIST", scale = "response", gg = TRUE, line = list(col = "black", size = 1.3)) + 
  scale_x_continuous(limit = c(0, 24000), breaks = c(0, 5000, 10000, 15000, 20000), labels = c(0, 5, 10, 15, 20)) +
  scale_y_continuous(limit = c(0, 1), breaks = c(0, 0.5, 1)) + 
  ggtitle(element_blank()) + 
  ggtitle(element_blank()) + 
  ylab("Probability of presence") + 
  xlab("DIST (km)") +
  theme(axis.title = element_text(size = 9),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none")


pitchpine_marginal_grid <- grid.arrange(pitchpine_grid, pitchpine_marginal, heights = c(1, 12))

redmaple_grid <- grobTree(rectGrob(gp = gpar(fill = "#009E73")),
                          textGrob("Red maple", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

redmaple_marginal <- visreg(glm_redmaple_occr_final, "DIST", by = "EST", scale = "response", gg = TRUE, line = list(col = "black", size = 1.3)) + 
  scale_x_continuous(limit = c(0, 24000), breaks = c(0, 10000, 20000), labels = c(0, 10, 20)) +
  scale_y_continuous(limit = c(0, 1), breaks = c(0, 0.5, 1)) + 
  ggtitle(element_blank()) + 
  ggtitle(element_blank()) + 
  ylab("Probability of presence") + 
  xlab("DIST (mk)") +
  theme(axis.title = element_text(size = 9),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none")

redmaple_marginal_grid <- grid.arrange(redmaple_grid, redmaple_marginal, heights = c(1, 12))


marginal_grid_all <- plot_grid(chestnutoak_marginal_grid, redoak_marginal_grid, scarletoak_marginal_grid,
                               whiteoak_marginal_grid, pitchpine_marginal_grid, redmaple_marginal_grid,
                               nrow = 3)

marginal_grid_all

# Model fit and predictive power
library(readxl)
Model_fit_and_predictive_power <- read_excel("E:/LiDAR/manuscript/tables and figures/Model fit and predictive power.xlsx")


chestnutoak_grid_fit <- grobTree(rectGrob(gp = gpar(fill = "grey")),
                             textGrob("Chestnut oak", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

chestnutoak_rsq <- ggplot(Model_fit_and_predictive_power[1:2, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

chestnutoak_AUC <- ggplot(Model_fit_and_predictive_power[1:2, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("5-fold CV") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

chestnutoak_fit_pred <- grid.arrange(chestnutoak_rsq, chestnutoak_AUC, ncol = 2)
chestnutoak_fit_pred <- grid.arrange(chestnutoak_grid_fit, chestnutoak_fit_pred, heights = c(1, 12))

redoak_grid_fit <- grobTree(rectGrob(gp = gpar(fill = "grey")),
                            textGrob("Northern red oak", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

redoak_rsq <- ggplot(Model_fit_and_predictive_power[3:4, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

redoak_AUC <- ggplot(Model_fit_and_predictive_power[3:4, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("5-fold CV") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

redoak_fit_pred <- grid.arrange(redoak_rsq, redoak_AUC, ncol = 2)
redoak_fit_pred <- grid.arrange(redoak_grid_fit, redoak_fit_pred, heights = c(1, 12))

scarletoak_grid_fit <- grobTree(rectGrob(gp = gpar(fill = "grey")),
                                textGrob("Scarlet oak", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

scarletoak_rsq <- ggplot(Model_fit_and_predictive_power[5:6, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

scarletoak_AUC <- ggplot(Model_fit_and_predictive_power[5:6, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("5-fold CV") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

scarletoak_fit_pred <- grid.arrange(scarletoak_rsq, scarletoak_AUC, ncol = 2)
scarletoak_fit_pred <- grid.arrange(scarletoak_grid_fit, scarletoak_fit_pred, heights = c(1, 12))

whiteoak_grid_fit <- grobTree(rectGrob(gp = gpar(fill = "grey")),
                              textGrob("White oak", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

whiteoak_rsq <- ggplot(Model_fit_and_predictive_power[7:8, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

whiteoak_AUC <- ggplot(Model_fit_and_predictive_power[7:8, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("5-fold CV") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

whiteoak_fit_pred <- grid.arrange(whiteoak_rsq, whiteoak_AUC, ncol = 2)
whiteoak_fit_pred <- grid.arrange(whiteoak_grid_fit, whiteoak_fit_pred, heights = c(1, 12))

pitchpine_grid_fit <- grobTree(rectGrob(gp = gpar(fill = "grey")),
                               textGrob("Pitch pine", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

pitchpine_rsq <- ggplot(Model_fit_and_predictive_power[9:10, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

pitchpine_AUC <- ggplot(Model_fit_and_predictive_power[9:10, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("5-fold CV") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

pitchpine_fit_pred <- grid.arrange(pitchpine_rsq, pitchpine_AUC, ncol = 2)
pitchpine_fit_pred <- grid.arrange(pitchpine_grid_fit, pitchpine_fit_pred, heights = c(1, 12))

redmaple_grid_fit <- grobTree(rectGrob(gp = gpar(fill = "grey")),
                              textGrob("Red maple", x = 0.5, hjust = 0.5, gp = gpar(col = "white", fontface = "bold", fontsize = 10)))

redmaple_rsq <- ggplot(Model_fit_and_predictive_power[11:12, c(2,4)], aes(x=as.factor(model), y = rsq, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("Nagelkerke's R"^"2")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("Training") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

redmaple_AUC <- ggplot(Model_fit_and_predictive_power[11:12, c(3,4)], aes(x=as.factor(model), y = AUC, fill=as.factor(model) )) +
  scale_y_continuous(name = expression(paste("AUC")) , limits = c(0,1)) +
  xlab("SDM") + 
  geom_bar(colour="black", stat="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ggtitle("5-fold CV") +
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        legend.position="none")

redmaple_fit_pred <- grid.arrange(redmaple_rsq, redmaple_AUC, ncol = 2)
redmaple_fit_pred <- grid.arrange(redmaple_grid_fit, redmaple_fit_pred, heights = c(1, 12))

fit_pred_all <- plot_grid(chestnutoak_fit_pred, redoak_fit_pred, scarletoak_fit_pred,
                          whiteoak_fit_pred, pitchpine_fit_pred, redmaple_fit_pred,
                          nrow = 2)

fit_pred_all

############################################################################################################################
# Plotting results of variation partitioning
# Date: 06/01/2020
############################################################################################################################
# Load data
variation_partitioning_plot <- read_excel("E:/Chapter2/manuscript/tables and figures/variation partitioning_plot.xlsx")

# Create bar chart
vp <- ggplot(variation_partitioning_plot, aes(fill =  OrderSDM, x = OrderSpecies, y = Deviance)) +
  geom_bar(position = "stack", stat = "identity", color = "black", width = 0.7) +
  scale_y_continuous(name = "Partial deviance explained (%)", 
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
                     labels = c(0, "10%", "20%", "30%", "40%", "50%", "60%", "70%")) +
  scale_x_discrete(name = "",
    breaks = c("A", "B", "C", "D", "E", "F"), 
    labels = c("Chestnut\noak", "Northern\nred oak", "Scarlet\noak",
                              "White\noak", "Pitch\npine", "Red\nmaple")) +
  scale_fill_manual(name = "", values = c("#00AFBB",  "grey", "#E69F00"), labels = c(expression(V[BF]), expression(V[JF]), expression(V[HF]))) +
  theme(panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
        legend.position = c(.85, 1.1),
        legend.direction = "horizontal",
        legend.box.just = "right",
        legend.box.margin = margin(t = 4, unit = "cm"))

vp



