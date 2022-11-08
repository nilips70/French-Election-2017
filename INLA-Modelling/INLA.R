library(tidyverse)
library(corrplot)
library(purrr)
library(sf)
library(INLA)
library(spdep)  #adjacency matrix
rm(list = ls())

# ========================================================================================================
# ========================================= Using updated dataset ========================================
# ========================================================================================================
#reading the dataset
df_merged <- readRDS("depart_mostComplete.rds")

#Extracting adjacency matrix
g <- poly2nb(df_merged) 

#Visualising Adjacency matrix based on the row's number
image(inla.graph2matrix(g),xlab=NULL,ylab=NULL, main= "Adjacency Matrix")  

#adding a column for row's number
df_merged <- df_merged %>% mutate(code = seq(1,nrow(.),1)) %>% select(-nom) 

#data preparation for converting multi-nomial into poisson dist.
df_final <- df_merged %>%       
  st_drop_geometry() %>% 
  select(-total_imm) %>% 
  pivot_longer(names_to = "candidate", values_to = "vote",
               -c(code, SNHM14:agri_rate))

#preparing a dictionary for recognizing the candidates
cand_code <- unique(df_final$candidate)
candidate_code <- data.frame(candidate = cand_code, id = seq(1,11,1))

df_final <- left_join(df_final, candidate_code)

#adding spatial effects to the dataset
df_final$sp_effect <- NA        #Lepen
df_final$sp_effect2 <- NA       #macron
df_final$sp_effect3 <- NA       #fillon
df_final$sp_effect4 <- NA       #melenchon

df_final <- df_final %>%
  mutate(sp_effect = ifelse(id == 8, code,sp_effect),
         sp_effect2 = ifelse(id == 9, code,sp_effect2),
         sp_effect3 = ifelse(id == 5, code, sp_effect3),
         sp_effect4 = ifelse(id == 10, code, sp_effect4))

#Checking correlations 
df_final1 <- df_final %>% rename(avg_salary = SNHM14, immigration_rate = percent_imm_dep, 
                                 education_rate = people_education)

corrr <- df_final1 %>% 
  select(immigration_rate, poverty_rate, avg_life, education_rate, avg_salary, unemployment_rate, elderly_rate, whitecol_rate, agri_rate)

corrplot(cor(corrr), order = "hclust", type = "upper")

#creating id for random effects(bcz inla works this way) 
df_final <- df_final %>% mutate(
  id_im  = id,
  id_po  = id,
  id_ex  = id,
  id_ed  = id,
  id_wa = id,
  id_un = id,
  id_el  = id,
  id_wh = id,
  id_ag = id
)


#har kudum az in f ha ye random effecte age dakhele f chizi nanvisi fek mikone iid hast
#fixed=t yani estimate nashe un hyperparameter, constr=t yani sum coef ha 0 she (sum to zero constraint for this model)
#formula.spatial = vote ~ -1 +
#  f(code, initial = -10, fixed = T) +     
#  f(id_im, percent_imm_dep, fixed = T, constr = T) +
#  f(id_po, poverty_rate, fixed = T, constr = T) +
#  f(id_ex, avg_life, fixed = T, constr = T) +
#  f(id_ed, people_education, fixed = T, constr = T)  +
#  f(id_wa, SNHM14, fixed = T, constr = T) +
#  f(id_un, unemployment_rate, fixed = T, constr = T) +
#  f(id_el, elderly_rate, fixed = T, constr = T) +
#  f(id_wh, whitecol_rate, fixed = T, constr = T) +
#  f(id_ag, agri_rate, fixed = T, constr = T) +
#  f(sp_effect, model = 'besag', graph = g) + #spatial random effect 
#  f(sp_effect2, model = 'besag', graph = g) #+
#f(sp_effect3, model = 'besag', graph = g) #+
# f(sp_effect4, model = 'besag', graph = g)
#f(id_ed, people_education, fixed = T, constr = T)  


formula.spatial = vote ~ -1 +
  f(code, initial = -10, fixed = T) +
  f(id_ed, people_education, fixed = T, constr = T)  +
  f(id_ex, avg_life, fixed = T, constr = T) +
  f(id_un, unemployment_rate, fixed = T, constr = T) +
  f(id_wh, whitecol_rate, fixed = T, constr = T) +
  f(sp_effect, model = 'besag', graph = g) + #spatial random effect 
  f(sp_effect2, model = 'besag', graph = g)


######################## Training fit ##################

# Fit the model
model = inla(formula.spatial, 
             family = "poisson", 
             data = df_final,
             control.predictor = list(link = 1 , compute = TRUE))

# Checking model fit on training data, plausibility=unility func
df_accuracy <- df_final %>% 
  mutate(plausibility = model$summary.fitted.values[1:nrow(.), "mode"]) %>% 
  group_by(code) %>% 
  mutate(total_votes = sum(vote),
         total_plausibility = sum(plausibility),
         real_probs = vote/total_votes,
         pred_probs = plausibility/total_plausibility) %>% 
  ungroup()

summary(df_accuracy$real_probs - df_accuracy$pred_probs)


# Confusion matrix on training data
df_conf_train <- df_accuracy %>% 
  group_by(code) %>% 
  summarise(winner_real = .$candidate[which(vote == max(vote))],
            winner_pred = .$candidate[which(pred_probs == max(pred_probs))])


train_conf_tabl <- table(df_conf_train$winner_real, df_conf_train$winner_pred)
train_conf_tabl
sum(diag(train_conf_tabl))/sum(train_conf_tabl) # Accuracy, sum of diagonal values / total


# =============================================TEST SET=======================================================

# Evaluating model performance on test data
set.seed(1)
samp <- sample(1:length(unique(df_final$code)), 30) # Selecting 30 departments at random

df_test <- df_final %>%   #NA bezar tuye departemanayi ke tu sample hastan
  mutate(department = code,
         vote = ifelse(code %in% samp, NA, vote),
         code = ifelse(code %in% samp, NA, code))


model.test = inla(formula.spatial, 
                  family = "poisson", 
                  data = df_test,
                  control.predictor = list(link = 1 , compute = TRUE))


df_pred <- df_test %>% 
  mutate(plausibility = model.test$summary.fitted.values[1:nrow(.), "mode"]) %>% 
  group_by(department) %>% 
  mutate(total_plausibility = sum(plausibility),
         pred_probs = round(plausibility/total_plausibility,4)) %>% 
  ungroup() %>% 
  select(department, id, pred_probs) %>% 
  filter(department %in% samp)


df_real <- df_final %>% 
  group_by(code) %>% 
  mutate(total_votes = sum(vote),
         true_probs = round(vote/total_votes,4),
         department = code) %>% 
  ungroup() %>% 
  select(department, id, true_probs) %>% 
  filter(department %in% samp)


test2 <- left_join(df_real, df_pred) %>% mutate(diff = (true_probs - pred_probs)^2)
summary(test2$diff) 
sqrt(mean(test2$diff))

# Accuracy by confusion matrix
real_winners <- df_final %>% 
  filter(code %in% samp) %>% 
  select(code, candidate, vote) %>% 
  group_by(code) %>% 
  summarise(winner = .$candidate[which(vote == max(vote))]) %>% 
  rename(department = code) %>% 
  ungroup()


test <- left_join(df_pred, candidate_code) %>% 
  group_by(department) %>% 
  summarise(winner_pred = as.character(.$candidate[which(pred_probs == max(pred_probs))])) %>% 
  ungroup()

t <- full_join(real_winners, test)

sum(diag(table(t$winner, t$winner_pred)))/sum(table(t$winner, t$winner_pred))
t1 <- table(t$winner, t$winner_pred)
t1


#############################################################################
#interpretation of results
#############################################################################
#immigration
E_LF_im = model$summary.random$id_im$`0.5quant`[8] -  model$summary.random$id_im$`0.5quant`[5]   #lepen - fillon
lower_LF_im = E_LF_im - 2*sqrt(model$summary.random$id_im$sd[8]^2 + model$summary.random$id_im$sd[5]^2)
upper_LF_im = E_LF_im + 2*sqrt(model$summary.random$id_im$sd[8]^2 + model$summary.random$id_im$sd[5]^2)

E_MF_im = model$summary.random$id_im$`0.5quant`[9] -  model$summary.random$id_im$`0.5quant`[5]   #macron - fillon
lower_MF_im = E_MF_im - 2*sqrt(model$summary.random$id_im$sd[9]^2 + model$summary.random$id_im$sd[5]^2)
upper_MF_im = E_MF_im + 2*sqrt(model$summary.random$id_im$sd[9]^2 + model$summary.random$id_im$sd[5]^2)

E_MeF_im = model$summary.random$id_im$`0.5quant`[10] - model$summary.random$id_im$`0.5quant`[5]   #melenchon - fillon
lower_MeF_im = E_MeF_im - 2*sqrt(model$summary.random$id_im$sd[10]^2 + model$summary.random$id_im$sd[5]^2)
upper_MeF_im = E_MeF_im + 2*sqrt(model$summary.random$id_im$sd[10]^2 + model$summary.random$id_im$sd[5]^2)

E_ML_im = model$summary.random$id_im$`0.5quant`[9] -  model$summary.random$id_im$`0.5quant`[8]   #macron - lepen
lower_ML_im = E_ML_im - 2*sqrt(model$summary.random$id_im$sd[9]^2 + model$summary.random$id_im$sd[8]^2)
upper_ML_im = E_ML_im + 2*sqrt(model$summary.random$id_im$sd[9]^2 + model$summary.random$id_im$sd[8]^2)

E_MeL_im = model$summary.random$id_im$`0.5quant`[10] - model$summary.random$id_im$`0.5quant`[8]   #melenchon - lepen
lower_MeL_im = E_MeL_im - 2*sqrt(model$summary.random$id_im$sd[10]^2 + model$summary.random$id_im$sd[8]^2)
upper_MeL_im = E_MeL_im + 2*sqrt(model$summary.random$id_im$sd[10]^2 + model$summary.random$id_im$sd[8]^2)

E_MeM_im = model$summary.random$id_im$`0.5quant`[10] - model$summary.random$id_im$`0.5quant`[9]   #melenchon - macron
lower_MeM_im = E_MeM_im - 2*sqrt(model$summary.random$id_im$sd[10]^2 + model$summary.random$id_im$sd[9]^2)
upper_MeM_im = E_MeM_im + 2*sqrt(model$summary.random$id_im$sd[10]^2 + model$summary.random$id_im$sd[9]^2)

##############################################################
#life expectancy
E_LF_ex = model$summary.random$id_ex$`0.5quant`[8] -  model$summary.random$id_ex$`0.5quant`[5]   #lepen - fillon
lower_LF_ex = E_LF_ex - 2*sqrt(model$summary.random$id_ex$sd[8]^2 + model$summary.random$id_ex$sd[5]^2)
upper_LF_ex = E_LF_ex + 2*sqrt(model$summary.random$id_ex$sd[8]^2 + model$summary.random$id_ex$sd[5]^2)

E_MF_ex = model$summary.random$id_ex$`0.5quant`[9] -  model$summary.random$id_ex$`0.5quant`[5]   #macron - fillon
lower_MF_ex = E_MF_ex - 2*sqrt(model$summary.random$id_ex$sd[9]^2 + model$summary.random$id_ex$sd[5]^2)
upper_MF_ex = E_MF_ex + 2*sqrt(model$summary.random$id_ex$sd[9]^2 + model$summary.random$id_ex$sd[5]^2)

E_MeF_ex = model$summary.random$id_ex$`0.5quant`[10] - model$summary.random$id_ex$`0.5quant`[5]   #melenchon - fillon
lower_MeF_ex = E_MeF_ex - 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[5]^2)
upper_MeF_ex = E_MeF_ex + 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[5]^2)

E_ML_ex = model$summary.random$id_ex$`0.5quant`[9] -  model$summary.random$id_ex$`0.5quant`[8]   #macron - lepen
lower_ML_ex = E_ML_ex - 2*sqrt(model$summary.random$id_ex$sd[9]^2 + model$summary.random$id_ex$sd[8]^2)
upper_ML_ex = E_ML_ex + 2*sqrt(model$summary.random$id_ex$sd[9]^2 + model$summary.random$id_ex$sd[8]^2)

E_MeL_ex = model$summary.random$id_ex$`0.5quant`[10] - model$summary.random$id_ex$`0.5quant`[8]   #melenchon - lepen
lower_MeL_ex = E_MeL_ex - 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[8]^2)
upper_MeL_ex = E_MeL_ex + 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[8]^2)

E_MeM_ex = model$summary.random$id_ex$`0.5quant`[10] - model$summary.random$id_ex$`0.5quant`[9]   #melenchon - macron
lower_MeM_ex = E_MeM_ex - 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[9]^2)
upper_MeM_ex = E_MeM_ex + 2*sqrt(model$summary.random$id_ex$sd[10]^2 + model$summary.random$id_ex$sd[9]^2)
##############################################################
#poverty rate
E_LF_po = model$summary.random$id_po$`0.5quant`[8] -  model$summary.random$id_po$`0.5quant`[5]   #lepen - fillon
lower_LF_po = E_LF_po - 2*sqrt(model$summary.random$id_po$sd[8]^2 + model$summary.random$id_po$sd[5]^2)
upper_LF_po = E_LF_po + 2*sqrt(model$summary.random$id_po$sd[8]^2 + model$summary.random$id_po$sd[5]^2)

E_MF_po = model$summary.random$id_po$`0.5quant`[9] -  model$summary.random$id_po$`0.5quant`[5]   #macron - fillon
lower_MF_po = E_MF_po - 2*sqrt(model$summary.random$id_po$sd[9]^2 + model$summary.random$id_po$sd[5]^2)
upper_MF_po = E_MF_po + 2*sqrt(model$summary.random$id_po$sd[9]^2 + model$summary.random$id_po$sd[5]^2)

E_MeF_po = model$summary.random$id_po$`0.5quant`[10] - model$summary.random$id_po$`0.5quant`[5]   #melenchon - fillon
lower_MeF_po = E_MeF_po - 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[5]^2)
upper_MeF_po = E_MeF_po + 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[5]^2)

E_ML_po = model$summary.random$id_po$`0.5quant`[9] -  model$summary.random$id_po$`0.5quant`[8]   #macron - lepen
lower_ML_po = E_ML_po - 2*sqrt(model$summary.random$id_po$sd[9]^2 + model$summary.random$id_po$sd[8]^2)
upper_ML_po = E_ML_po + 2*sqrt(model$summary.random$id_po$sd[9]^2 + model$summary.random$id_po$sd[8]^2)

E_MeL_po = model$summary.random$id_po$`0.5quant`[10] - model$summary.random$id_po$`0.5quant`[8]   #melenchon - lepen
lower_MeL_po = E_MeL_po - 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[8]^2)
upper_MeL_po = E_MeL_po + 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[8]^2)

E_MeM_po = model$summary.random$id_po$`0.5quant`[10] - model$summary.random$id_po$`0.5quant`[9]   #melenchon - macron
lower_MeM_po = E_MeM_po - 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[9]^2)
upper_MeM_po = E_MeM_po + 2*sqrt(model$summary.random$id_po$sd[10]^2 + model$summary.random$id_po$sd[9]^2)

##############################################################
#education
E_LF_ed = model$summary.random$id_ed$`0.5quant`[8] -  model$summary.random$id_ed$`0.5quant`[5]   #lepen - fillon
lower_LF_ed = E_LF_ed - 2*sqrt(model$summary.random$id_ed$sd[8]^2 + model$summary.random$id_ed$sd[5]^2)
upper_LF_ed = E_LF_ed + 2*sqrt(model$summary.random$id_ed$sd[8]^2 + model$summary.random$id_ed$sd[5]^2)

E_MF_ed = model$summary.random$id_ed$`0.5quant`[9] -  model$summary.random$id_ed$`0.5quant`[5]   #macron - fillon
lower_MF_ed = E_MF_ed - 2*sqrt(model$summary.random$id_ed$sd[9]^2 + model$summary.random$id_ed$sd[5]^2)
upper_MF_ed = E_MF_ed + 2*sqrt(model$summary.random$id_ed$sd[9]^2 + model$summary.random$id_ed$sd[5]^2)

E_MeF_ed = model$summary.random$id_ed$`0.5quant`[10] - model$summary.random$id_ed$`0.5quant`[5]   #melenchon - fillon
lower_MeF_ed = E_MeF_ed - 2*sqrt(model$summary.random$id_ed$sd[10]^2 + model$summary.random$id_ed$sd[5]^2)
upper_MeF_ed = E_MeF_ed + 2*sqrt(model$summary.random$id_ed$sd[10]^2 + model$summary.random$id_ed$sd[5]^2)

E_ML_ed = model$summary.random$id_ed$`0.5quant`[9] -  model$summary.random$id_ed$`0.5quant`[8]   #macron - lepen
lower_ML_ed = E_ML_ed - 2*sqrt(model$summary.random$id_ed$sd[9]^2 + model$summary.random$id_ed$sd[8]^2)
upper_ML_ed = E_ML_ed + 2*sqrt(model$summary.random$id_ed$sd[9]^2 + model$summary.random$id_ed$sd[8]^2)

E_MeL_ed = model$summary.random$id_ed$`0.5quant`[10] - model$summary.random$id_ed$`0.5quant`[8]   #melenchon - lepen
lower_MeL_ed = E_MeL_ed - 2*sqrt(model$summary.random$id_ed$sd[10]^2 + model$summary.random$id_ed$sd[8]^2)
upper_MeL_ed = E_MeL_ed + 2*sqrt(model$summary.random$id_ed$sd[10]^2 + model$summary.random$id_ed$sd[8]^2)

E_MeM_ed = model$summary.random$id_ed$`0.5quant`[10] - model$summary.random$id_ed$`0.5quant`[9]   #melenchon - macron
lower_MeM_ed = E_MeM_ed - 2*sqrt(model$summary.random$id_ed$sd[10]^2 + model$summary.random$id_ed$sd[9]^2)
upper_MeM_ed = E_MeM_ed + 2*sqrt(model$summary.random$id_ed$sd[10]^2 + model$summary.random$id_ed$sd[9]^2)
##############################################################
#average salary
E_LF_wa = model$summary.random$id_wa$`0.5quant`[8] -  model$summary.random$id_wa$`0.5quant`[5]   #lepen - fillon
lower_LF_wa = E_LF_wa - 2*sqrt(model$summary.random$id_wa$sd[8]^2 + model$summary.random$id_wa$sd[5]^2)
upper_LF_wa = E_LF_wa + 2*sqrt(model$summary.random$id_wa$sd[8]^2 + model$summary.random$id_wa$sd[5]^2)
  
E_MF_wa = model$summary.random$id_wa$`0.5quant`[9] -  model$summary.random$id_wa$`0.5quant`[5]   #macron - fillon
lower_MF_wa = E_MF_wa - 2*sqrt(model$summary.random$id_wa$sd[9]^2 + model$summary.random$id_wa$sd[5]^2)
upper_MF_wa = E_MF_wa + 2*sqrt(model$summary.random$id_wa$sd[9]^2 + model$summary.random$id_wa$sd[5]^2)

E_MeF_wa = model$summary.random$id_wa$`0.5quant`[10] - model$summary.random$id_wa$`0.5quant`[5]   #melenchon - fillon
lower_MeF_wa = E_MeF_wa - 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[5]^2)
upper_MeF_wa = E_MeF_wa + 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[5]^2)

E_ML_wa = model$summary.random$id_wa$`0.5quant`[9] -  model$summary.random$id_wa$`0.5quant`[8]   #macron - lepen
lower_ML_wa = E_ML_wa - 2*sqrt(model$summary.random$id_wa$sd[9]^2 + model$summary.random$id_wa$sd[8]^2)
upper_ML_wa = E_ML_wa + 2*sqrt(model$summary.random$id_wa$sd[9]^2 + model$summary.random$id_wa$sd[8]^2)

E_MeL_wa = model$summary.random$id_wa$`0.5quant`[10] - model$summary.random$id_wa$`0.5quant`[8]   #melenchon - lepen
lower_MeL_wa = E_MeL_wa - 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[8]^2)
upper_MeL_wa = E_MeL_wa + 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[8]^2)

E_MeM_wa = model$summary.random$id_wa$`0.5quant`[10] - model$summary.random$id_wa$`0.5quant`[9]   #melenchon - macron
lower_MeM_wa = E_MeM_wa - 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[9]^2)
upper_MeM_wa = E_MeM_wa + 2*sqrt(model$summary.random$id_wa$sd[10]^2 + model$summary.random$id_wa$sd[9]^2)

##############################################################
#unemployment rate
E_LF_un = model$summary.random$id_un$`0.5quant`[8] -  model$summary.random$id_un$`0.5quant`[5]   #lepen - fillon
lower_LF_un = E_LF_un - 2*sqrt(model$summary.random$id_un$sd[8]^2 +  model$summary.random$id_un$sd[5]^2) # x,y are assumed to be gaussian and independent, std(X-Y) = sqrt(std(X)^2 + std(Y)^2)
upper_LF_un = E_LF_un + 2*sqrt(model$summary.random$id_un$sd[8]^2 +  model$summary.random$id_un$sd[5]^2) 

E_MF_un = model$summary.random$id_un$`0.5quant`[9] -  model$summary.random$id_un$`0.5quant`[5]   #macron - fillon
lower_MF_un = E_MF_un - 2*sqrt(model$summary.random$id_un$sd[9]^2 +  model$summary.random$id_un$sd[5]^2)
upper_MF_un = E_MF_un + 2*sqrt(model$summary.random$id_un$sd[9]^2 +  model$summary.random$id_un$sd[5]^2)

E_MeF_un = model$summary.random$id_un$`0.5quant`[10] - model$summary.random$id_un$`0.5quant`[5]   #melenchon - fillon
lower_MeF_un = E_MeF_un - 2*sqrt(model$summary.random$id_un$sd[10]^2 +  model$summary.random$id_un$sd[5]^2)
upper_MeF_un = E_MeF_un + 2*sqrt(model$summary.random$id_un$sd[10]^2 +  model$summary.random$id_un$sd[5]^2)

E_ML_un = model$summary.random$id_un$`0.5quant`[9] -  model$summary.random$id_un$`0.5quant`[8]   #macron - lepen
lower_ML_un = E_ML_un - 2*sqrt(model$summary.random$id_un$sd[9]^2 +  model$summary.random$id_un$sd[8]^2)
upper_ML_un = E_ML_un + 2*sqrt(model$summary.random$id_un$sd[9]^2 +  model$summary.random$id_un$sd[8]^2)


E_MeL_un = model$summary.random$id_un$`0.5quant`[10] - model$summary.random$id_un$`0.5quant`[8]   #melenchon - lepen
lower_MeL_un = E_MeL_un - 2*sqrt(model$summary.random$id_un$sd[10]^2 +  model$summary.random$id_un$sd[8]^2)
upper_MeL_un = E_MeL_un + 2*sqrt(model$summary.random$id_un$sd[10]^2 +  model$summary.random$id_un$sd[8]^2)


E_MeM_un = model$summary.random$id_un$`0.5quant`[10] - model$summary.random$id_un$`0.5quant`[9]   #melenchon - macron
lower_MeM_un = E_MeM_un - 2*sqrt(model$summary.random$id_un$sd[10]^2 +  model$summary.random$id_un$sd[9]^2)
upper_MeM_un = E_MeM_un + 2*sqrt(model$summary.random$id_un$sd[10]^2 +  model$summary.random$id_un$sd[9]^2)

######################################################################
#elderly rate
E_LF_el = model$summary.random$id_el$`0.5quant`[8] -  model$summary.random$id_el$`0.5quant`[5]   #lepen - fillon
lower_LF_el = E_LF_el - 2*sqrt(model$summary.random$id_el$sd[8]^2 + model$summary.random$id_el$sd[5]^2)
upper_LF_el = E_LF_el + 2*sqrt(model$summary.random$id_el$sd[8]^2 + model$summary.random$id_el$sd[5]^2)

E_MF_el = model$summary.random$id_el$`0.5quant`[9] -  model$summary.random$id_el$`0.5quant`[5]   #macron - fillon
lower_MF_el = E_MF_el - 2*sqrt(model$summary.random$id_el$sd[9]^2 + model$summary.random$id_el$sd[5]^2)
upper_MF_el = E_MF_el + 2*sqrt(model$summary.random$id_el$sd[9]^2 + model$summary.random$id_el$sd[5]^2)

E_MeF_el = model$summary.random$id_el$`0.5quant`[10] - model$summary.random$id_el$`0.5quant`[5]   #melenchon - fillon
lower_MeF_el = E_MeF_el - 2*sqrt(model$summary.random$id_el$sd[10]^2 + model$summary.random$id_el$sd[5]^2)
upper_MeF_el = E_MeF_el + 2*sqrt(model$summary.random$id_el$sd[10]^2 + model$summary.random$id_el$sd[5]^2)

E_ML_el = model$summary.random$id_el$`0.5quant`[9] -  model$summary.random$id_el$`0.5quant`[8]   #macron - lepen
lower_ML_el = E_ML_el - 2*sqrt(model$summary.random$id_el$sd[9]^2 + model$summary.random$id_el$sd[8]^2)
upper_ML_el = E_ML_el + 2*sqrt(model$summary.random$id_el$sd[9]^2 + model$summary.random$id_el$sd[8]^2)

E_MeL_el = model$summary.random$id_el$`0.5quant`[10] - model$summary.random$id_el$`0.5quant`[8]   #melenchon - lepen
lower_MeL_el = E_MeL_el - 2*sqrt(model$summary.random$id_el$sd[10]^2 + model$summary.random$id_el$sd[8]^2)
upper_MeL_el = E_MeL_el + 2*sqrt(model$summary.random$id_el$sd[10]^2 + model$summary.random$id_el$sd[8]^2)

E_MeM_el = model$summary.random$id_el$`0.5quant`[10] - model$summary.random$id_el$`0.5quant`[9]   #melenchon - macron
lower_MeM_el = E_MeM_el - 2*sqrt(model$summary.random$id_el$sd[10]^2 + model$summary.random$id_el$sd[9]^2)
upper_MeM_el = E_MeM_el + 2*sqrt(model$summary.random$id_el$sd[10]^2 + model$summary.random$id_el$sd[9]^2)
######################################################################
#white collar
E_LF_wh = model$summary.random$id_wh$`0.5quant`[8] -  model$summary.random$id_wh$`0.5quant`[5]   #lepen - fillon
lower_LF_wh = E_LF_wh - 2*sqrt(model$summary.random$id_wh$sd[8]^2 + model$summary.random$id_wh$sd[5]^2)
upper_LF_wh = E_LF_wh + 2*sqrt(model$summary.random$id_wh$sd[8]^2 + model$summary.random$id_wh$sd[5]^2)

E_MF_wh = model$summary.random$id_wh$`0.5quant`[9] -  model$summary.random$id_wh$`0.5quant`[5]   #macron - fillon
lower_MF_wh = E_MF_wh - 2*sqrt(model$summary.random$id_wh$sd[9]^2 + model$summary.random$id_wh$sd[5]^2)
upper_MF_wh = E_MF_wh + 2*sqrt(model$summary.random$id_wh$sd[9]^2 + model$summary.random$id_wh$sd[5]^2)

E_MeF_wh = model$summary.random$id_wh$`0.5quant`[10] - model$summary.random$id_wh$`0.5quant`[5]   #melenchon - fillon
lower_MeF_wh = E_MeF_wh - 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[5]^2)
upper_MeF_wh = E_MeF_wh + 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[5]^2)

E_ML_wh = model$summary.random$id_wh$`0.5quant`[9] -  model$summary.random$id_wh$`0.5quant`[8]   #macron - lepen
lower_ML_wh = E_ML_wh - 2*sqrt(model$summary.random$id_wh$sd[9]^2 + model$summary.random$id_wh$sd[8]^2)
upper_ML_wh = E_ML_wh + 2*sqrt(model$summary.random$id_wh$sd[9]^2 + model$summary.random$id_wh$sd[8]^2)

E_MeL_wh = model$summary.random$id_wh$`0.5quant`[10] - model$summary.random$id_wh$`0.5quant`[8]   #melenchon - lepen
lower_MeL_wh = E_MeL_wh - 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[8]^2)
upper_MeL_wh = E_MeL_wh + 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[8]^2)

E_MeM_wh = model$summary.random$id_wh$`0.5quant`[10] - model$summary.random$id_wh$`0.5quant`[9]   #melenchon - macron
lower_MeM_wh = E_MeM_wh - 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[9]^2)
upper_MeM_wh = E_MeM_wh + 2*sqrt(model$summary.random$id_wh$sd[10]^2 + model$summary.random$id_wh$sd[9]^2)

######################################################################
#agriculture rate
E_LF_ag = model$summary.random$id_ag$`0.5quant`[8] -  model$summary.random$id_ag$`0.5quant`[5]   #lepen - fillon
lower_LF_ag = E_LF_ag - 2*sqrt(model$summary.random$id_ag$sd[8]^2 + model$summary.random$id_ag$sd[5]^2)
upper_LF_ag = E_LF_ag + 2*sqrt(model$summary.random$id_ag$sd[8]^2 + model$summary.random$id_ag$sd[5]^2)

E_MF_ag =model$summary.random$id_ag$`0.5quant`[9] -  model$summary.random$id_ag$`0.5quant`[5]   #macron - fillon
lower_MF_ag = E_MF_ag - 2*sqrt(model$summary.random$id_ag$sd[9]^2 + model$summary.random$id_ag$sd[5]^2)
upper_MF_ag = E_MF_ag + 2*sqrt(model$summary.random$id_ag$sd[9]^2 + model$summary.random$id_ag$sd[5]^2)

E_MeF_ag =model$summary.random$id_ag$`0.5quant`[10] - model$summary.random$id_ag$`0.5quant`[5]   #melenchon - fillon
lower_MeF_ag = E_MeF_ag - 2*sqrt(model$summary.random$id_ag$sd[10]^2 + model$summary.random$id_ag$sd[5]^2)
upper_MeF_ag = E_MeF_ag + 2*sqrt(model$summary.random$id_ag$sd[10]^2 + model$summary.random$id_ag$sd[5]^2)

E_ML_ag =model$summary.random$id_ag$`0.5quant`[9] -  model$summary.random$id_ag$`0.5quant`[8]   #macron - lepen
lower_ML_ag = E_ML_ag - 2*sqrt(model$summary.random$id_ag$sd[9]^2 + model$summary.random$id_ag$sd[8]^2)
upper_ML_ag = E_ML_ag + 2*sqrt(model$summary.random$id_ag$sd[9]^2 + model$summary.random$id_ag$sd[8]^2)

E_MeL_ag =model$summary.random$id_ag$`0.5quant`[10] - model$summary.random$id_ag$`0.5quant`[8]   #melenchon - lepen
lower_MeL_ag = E_MeL_ag - 2*sqrt(model$summary.random$id_ag$sd[10]^2 + model$summary.random$id_ag$sd[8]^2)
upper_MeL_ag = E_MeL_ag + 2*sqrt(model$summary.random$id_ag$sd[10]^2 + model$summary.random$id_ag$sd[8]^2)

E_MeM_ag =model$summary.random$id_ag$`0.5quant`[10] - model$summary.random$id_ag$`0.5quant`[9]   #melenchon - macron
lower_MeM_ag = E_MeM_ag - 2*sqrt(model$summary.random$id_ag$sd[10]^2 + model$summary.random$id_ag$sd[9]^2)
upper_MeM_ag = E_MeM_ag + 2*sqrt(model$summary.random$id_ag$sd[10]^2 + model$summary.random$id_ag$sd[9]^2)

####################################################
#95% higher edu
library(readxl)
library(plotly)
library(ggplot2)
library(ggpubr)

higher_edu <- read_excel("higher edu.xlsx")
#names(higher_edu) = c("candidate", "mean", "low", "high", "odds")

plot1 <- higher_edu %>% 
  mutate(across(is.numeric, ~. -1)) %>% 
   ggplot(aes(Candidates, odss_new, label = round(odss_new, 2))) + 
   geom_linerange(aes(ymin = `exp low`, ymax = `exp high`), color = "cyan4", alpha = 0.7, size = 1) +
   geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
   geom_point(color = "orange", size = 1.5) +
  scale_y_continuous(limits = c(-0.11, 0.11)) + 
   labs( x = NULL, y = "Higher Education Rate") +
   theme_bw()  +geom_text(hjust= 1.2) + 
  theme(axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 60, margin = margin(t = 34), face = 'bold'))
ggsave("highereduplot.png", width = 10)

#unemployment
unemployement_rate <- read_excel("unemployement rate.xlsx")

plot2 <- unemployement_rate%>% mutate(across(is.numeric, ~. -1)) %>% na.omit() %>%
  ggplot(aes(Candidates, odds, label = round(odds, 2))) + 
  geom_linerange(aes(ymin = `exp low`, ymax = `exp high`), color = "cyan4", alpha = 0.7, size = 1) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(color = "orange", size = 1.5) +
  scale_y_continuous(limits = c(-0.119, 0.11)) + 
  labs( x = NULL, y = "Unemployment Rate") +
  theme_bw() +geom_text(hjust= 1.2) + 
  theme(axis.title.y = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 60, margin = margin(t = 34), face = 'bold'))
ggsave("unemployplot.png", width = 10)


#whitecollar
white_collar <- read_excel("white collar.xlsx")

plot3 <- white_collar %>% mutate(across(is.numeric, ~. -1)) %>% na.omit() %>%
  ggplot(aes(Candidates, odds, label = round(odds, 2))) + 
  geom_linerange(aes(ymin = `exp low`, ymax = `exp high`), color = "cyan4", alpha = 0.7, size = 1) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(color = "orange", size = 1.5) +
  scale_y_continuous(limits = c(-0.025, 0.045)) + 
  labs( x = NULL, y = "White Collar Rate") +
  theme_bw() + geom_text(hjust= 1.2) +
  theme(axis.text.x = element_text(angle = 60, margin = margin(t = 34), face = 'bold'),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
ggsave("whitecollarplot.png", width = 10)

#average life
average_life <- read_excel("average life.xlsx")

plot4 <- average_life%>% mutate(across(is.numeric, ~. -1)) %>% na.omit() %>%
  ggplot(aes(Candidates, odds,  label = round(odds, 2))) + 
  geom_linerange(aes(ymin = `exp low`, ymax = `exp high`), color = "cyan4", alpha = 0.7, size = 1) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(color = "orange", size = 1.5) +
  scale_y_continuous(limits = c(-0.025, 0.045)) +
  labs( x = NULL, y = "Average Life Expectancy") +
  theme_bw() +geom_text(hjust= 1.2) +
  theme(axis.text.x = element_text(angle = 60, margin = margin(t = 34), face = 'bold'),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
ggsave("averagelifeplot.png", width = 10)

totalplot<-ggarrange(plot1 , plot2 , plot3, plot4, nrow = 2, ncol = 2, common.legend = FALSE)



ggsave("plottotal.png", height = 8, width =10)

