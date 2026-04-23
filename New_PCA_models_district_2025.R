library(lmtest) 
library(car) 
Neospora_with_pcs$Calfnof <- as.factor(Neospora_with_pcs$Calfnof)
levels(Neospora_with_pcs$Calfnof)
levels(Neospora_with_pcs$Calfnof) <- c("0-1", "1-3", "3-6", ">6")
str(Neospora_with_pcs$Calfnof)

Neospora_with_pcs$cat_gpsalt2 <- as.factor(Neospora_with_pcs$cat_gpsalt2)
levels(Neospora_with_pcs$cat_gpsalt2)
levels(Neospora_with_pcs$cat_gpsalt2) <- c("lessthan 500", "500-1000", "1000-1500", "greaterthan 1500")
str(Neospora_with_pcs$cat_gpsalt2)

str(Neospora_with_pcs)

Neospora_with_pcs$rescaled_pop_density <- scale(Neospora_with_pcs$pop_density)

str(Neospora_with_pcs)

lapply(Neospora_with_pcs[, c("management", "water", "calfnumber", 
                             "breed", "cattlerole", "animalsex", 
                             "pregnancystatus")], function(x) levels(as.factor(x)))


HGLMM1 <- glmer(as.factor(neospora_elisa_pn) ~ as.factor(management) + as.factor(water) + 
                  pc1_subset + pc3_subset + pc2_subset + cattlerole + herdsize +
                  as.factor(Calfnof) + placenta + newanimals + pregnancystatus + 
                  as.factor(land_cover_factor) +
                  (1 | district_name), 
                data = Neospora_with_pcs, 
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(HGLMM1)

HGLMM2 <- glmer(as.factor(neospora_elisa_pn) ~ as.factor(management) + as.factor(water) + 
                  pc1_subset + pc3_subset + pc2_subset + herdsize +
                  as.factor(Calfnof) + placenta + newanimals + pregnancystatus + 
                  as.factor(land_cover_factor) +
                  (1 | district_name), 
                data = Neospora_with_pcs, 
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(HGLMM2)

HGLMM3 <- glmer(as.factor(neospora_elisa_pn) ~ as.factor(management) + as.factor(water) + 
                  pc1_subset + pc3_subset + pc2_subset + herdsize +
                  as.factor(Calfnof) + placenta +  
                  as.factor(land_cover_factor) +
                  (1 | district_name), 
                data = Neospora_with_pcs, 
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(HGLMM3)

HGLMM4 <- glmer(as.factor(neospora_elisa_pn) ~ as.factor(management) + as.factor(water) + 
                  pc1_subset + pc3_subset + pc2_subset + herdsize +
                  as.factor(Calfnof) + placenta +
                  (1 | district_name), 
                data = Neospora_with_pcs, 
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(HGLMM4)
vif(HGLMM4)

HGLMM5 <- glmer(as.factor(neospora_elisa_pn) ~ as.factor(management) + as.factor(water) + 
                  pc1_subset + pc3_subset + pc2_subset +
                  as.factor(Calfnof) + placenta +
                  (1 | district_name), 
                data = Neospora_with_pcs, 
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(HGLMM5)

HGLMM6 <- glmer(as.factor(neospora_elisa_pn) ~ as.factor(management) + as.factor(water) + 
                  pc1_subset + pc3_subset + 
                  as.factor(Calfnof) + placenta +
                  (1 | district_name), 
                data = Neospora_with_pcs, 
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(HGLMM6)

HGLMM7 <- glmer(as.factor(neospora_elisa_pn) ~ as.factor(management) +  
                +  pc2_subset  + pc3_subset  + pc1_subset +
                  as.factor(Calfnof) + placenta +
                  (1 | district_name), 
                data = Neospora_with_pcs, 
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(HGLMM7)

HGLMM8 <- glmer(as.factor(neospora_elisa_pn) ~ as.factor(management) +  
                  +  pc3_subset  + pc1_subset +
                  as.factor(Calfnof) + placenta +
                  (1 | district_name), 
                data = Neospora_with_pcs, 
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(HGLMM8)

HGLMM9 <- glmer(as.factor(neospora_elisa_pn) ~ as.factor(management) +  
                  +  pc3_subset  + 
                  as.factor(Calfnof) + placenta +
                  (1 | region/district_name), 
                data = Neospora_with_pcs, 
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(HGLMM9)

HGLMM10 <- glmer(as.factor(neospora_elisa_pn) ~  
                  +  pc3_subset  + 
                  as.factor(Calfnof) + placenta +
                  (1 | district_name), 
                data = Neospora_with_pcs, 
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(HGLMM10)




