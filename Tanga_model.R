# Tanga model

library(epitools)

tanga_df <- subset(Neosporaclim, region == "Tanga")
predictors <- setdiff(names(tanga_df), c("region", "neospora_elisa_pn")) 

TM1 <- glmer(neospora_elisa_pn ~ breedsfixed1 + Calfnof 
             + placenta + management + water + cat_gpsalt2 + 
               distance + land_cover_factor + cat_bio3 + cat_bio8 + 
               cat_bio10 + cat_bio12 + cat_bio16 + 
               (1 | district_name), data = tanga_df, family = binomial(),
             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(TM1)


TM2 <- glmer(neospora_elisa_pn ~ breedsfixed1 + Calfnof 
             + placenta + management + water + cat_gpsalt2 + 
               distance + land_cover_factor + cat_bio3 + cat_bio8 + 
               cat_bio12 + cat_bio16 + 
               (1 | district_name), data = tanga_df, family = binomial(),
             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(TM2)

TM3<- glmer(neospora_elisa_pn ~ breedsfixed1 + Calfnof 
             + placenta + management + water + cat_gpsalt2 + 
               distance + land_cover_factor + cat_bio3 + cat_bio8 + 
               cat_bio12 + 
               (1 | district_name), data = tanga_df, family = binomial(),
             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(TM3)

TM4<- glmer(neospora_elisa_pn ~ breedsfixed1 + Calfnof 
            + placenta + management + water + cat_gpsalt2 + 
              distance +  cat_bio3 + cat_bio8 + 
              cat_bio12 + 
              (1 | district_name), data = tanga_df, family = binomial(),
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(TM4)

TM5<- glmer(neospora_elisa_pn ~ breedsfixed1 + Calfnof 
            + placenta + management + water + cat_gpsalt2 + 
               cat_bio3 + cat_bio8 + 
              cat_bio12 + 
              (1 | district_name), data = tanga_df, family = binomial(),
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(TM5)

TM6<- glmer(neospora_elisa_pn ~ breedsfixed1 + Calfnof 
            + placenta + management +  cat_gpsalt2 + 
              cat_bio3 + cat_bio8 + 
              cat_bio12 + 
              (1 | district_name), data = tanga_df, family = binomial(),
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(TM6)

TM7<- glmer(neospora_elisa_pn ~ breedsfixed1 + Calfnof 
            +  management +  cat_gpsalt2 + 
              cat_bio3 + cat_bio8 + 
              cat_bio12 + 
              (1 | district_name), data = tanga_df, family = binomial(),
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(TM7)

TM8<- glmer(neospora_elisa_pn ~ Calfnof 
            +  management +  cat_gpsalt2 + 
              cat_bio3 + cat_bio8 + 
              cat_bio12 + 
              (1 | district_name), data = tanga_df, family = binomial(),
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(TM8)

TM9<- glmer(neospora_elisa_pn ~ Calfnof 
            +  management +  cat_gpsalt2 + 
              cat_bio3 + cat_bio8 + 
              (1 | district_name), data = tanga_df, family = binomial(),
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(TM9)
+ feeding

TM10<- glmer(neospora_elisa_pn ~ Calfnof 
            +  management +  cat_gpsalt2 + 
              cat_bio3 + 
              (1 | district_name), data = tanga_df, family = binomial(),
            control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(TM10)

levels(tanga_df$cat_gpsalt2)

OR  <- exp(fixef(TM10))
CI  <- exp(confint(TM10, parm = "beta_", method = "Wald"))

cbind(OR, CI)

