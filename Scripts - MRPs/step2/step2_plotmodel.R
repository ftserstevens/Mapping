# Packages ----------------------------------------------------------------

library(rstan)
library(parallel)
library(mltools)
library(ggplot2)
library(bayesplot)
library(ggridges)
library(xtable)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}


colMedians = function(X) {
  dims = dim(X)[2]
  medians = 1:dims
  for (i in 1:dims) {
    medians[i] = median(X[,i])
  }
  return(medians) }

# load data ---------------------------------------------------------------


models = c("raw_average_onlydem","raw_average_onlyrep",
           "raw_average","raw_average_1for1","raked_average",
           "model_dem_discestimates","model_rep_discestiamtes",
           "model_woc_discestimates","model_1for1","model_raw")




accuracy = "model_raw"

step2 = readRDS(paste0("./stan trained models step2/step2_",accuracy,".rds"))
df = readRDS(paste0("./stan trained models step2/step2df_",accuracy,".rds"))




# make a latex table ------------------------------------------------------


a = summary(step2, probs=c(.1,.5,.9))
rownames(a$summary) = c("intercept","Sex:Female","sigma Age" , 
                        "Age:-18", "Age:19-29" ,"Age:30-39" , "Age:40+" ,"sigma State",
                        paste0( rep("State:", 51), levels(df$state_short)), "sigma Party",
                        "Party: Democrat", "Party: Neutral", "Party: Republican",
                        "State WhitePopulation", "State UndergradPopulation", "State PopulationDensity",
                        "lp__"
                        )


latex_code <- xtable(a$summary, caption = paste0("Summary of the posterior distribution of likelihood of spreading fake news online using the aggregated ratings of Democratic survey participants"), label = paste0("tab:model_summary"))
latex_code


step2_params = rstan::extract(step2)
alpha = step2_params$alpha
beta_sex = step2_params$beta
gamma_age = step2_params$gamma
delta_state = step2_params$delta
zeta_party = step2_params$zeta
eta_statecov = step2_params$eta
sex_coef = step2_params$sex_coef


# plot coefs --------------------------------------------------------------



mcmc_areas(as.data.frame(
  rstan::extract(step2, pars= c('zeta'))), prob = 0.5, prob_outer = 0.9) +
  scale_y_discrete(labels = c("zeta.1" = "Democrat",
                              "zeta.2" = "Neutral",
                              "zeta.3" = "Republican")) + theme_minimal()

mcmc_intervals(as.data.frame(
  rstan::extract(step2, pars= c('eta')))) +  
  scale_y_discrete(labels = c("eta.1" = "White ‰",
                              "eta.2" = "Undergrad Percentage",
                              "eta.3" = "Population Density"))

mcmc_intervals(as.data.frame(
  rstan::extract(step2, pars= c('alpha')))) +
  scale_y_discrete(labels ="Intercept")

mcmc_intervals(as.data.frame(
  rstan::extract(step2, pars= c('sex_coef')))) +
  scale_y_discrete(labels ="Female")

mcmc_intervals(as.data.frame(
  rstan::extract(step2, pars= c('gamma')))) + theme_minimal() +
  scale_y_discrete(labels =c("-18","19-29","30-39","40+"))


mcmc_intervals(as.data.frame(
  rstan::extract(step2, pars= c("delta")))[,order(colMedians(as.data.frame(rstan::extract(step2, pars= c("delta")))))]) +
  scale_y_discrete(labels =levels(df$state_short)[order(colMedians(as.data.frame(rstan::extract(step2, pars= c("delta")))))])

 

# compare Net & Balanced or Rep & Dem --------------------------------------------------

model_raw = readRDS(paste0("./stan trained models step2/step2_raw_average.rds"))
model_bal = readRDS(paste0("./stan trained models step2/step2_raw_average_1for1.rds"))
model_woc = readRDS(paste0("./stan trained models step2/step2_model_woc_discestimates.rds"))
raw_dem = readRDS(paste0("./stan trained models step2/step2_raw_average_onlydem.rds"))
raw_rep = readRDS(paste0("./stan trained models step2/step2_raw_average_onlyrep.rds"))
model_dem = readRDS(paste0("./stan trained models step2/step2_model_dem_discestimates.rds"))
model_rep = readRDS(paste0("./stan trained models step2/step2_model_rep_discestimates.rds"))


compare_parameter = c("zeta")
post_raw = as.data.frame(rstan::extract(model_raw, pars =compare_parameter))
post_bal = as.data.frame(rstan::extract(model_bal, pars =compare_parameter))
post_woc = as.data.frame(rstan::extract(model_woc, pars =compare_parameter))
postr_dem = as.data.frame(rstan::extract(raw_dem, pars = compare_parameter))
postr_rep = as.data.frame(rstan::extract(raw_rep, pars = compare_parameter))
postm_dem = as.data.frame(rstan::extract(model_dem, pars = compare_parameter))
postm_rep = as.data.frame(rstan::extract(model_rep, pars = compare_parameter))



post_raw$model = "Naive Sample"
post_bal$model = "Naive Balanced"
post_woc$model = "Model Wisdom"
postr_dem$model = "Naive Democrat"
postr_rep$model = "Naive Republican"
postm_dem$model = "Model Democrat"
postm_rep$model = "Model Republican"

combined_areas <- rbind(
  #post_bal, post_woc, post_raw
  postr_dem, postr_rep,
  postm_dem, postm_rep
)


df_long <- combined_areas %>%
  tidyr::pivot_longer(cols = names(combined_areas)[1:(dim(combined_areas)[2]-1)], names_to = "name", values_to = "value")

limits = df_long %>% 
  group_by(name, model) %>% 
  summarise(quantile_h = quantile(value, c(0.975)),
            quantile_l = quantile(value, c(0.025)))

df_long = merge(df_long, limits,
                by = c("name" ,"model"))
df_long = df_long[df_long$value >= df_long$quantile_l & df_long$value <= df_long$quantile_h,]


ggplot(df_long, aes(x = value, y = name, group = interaction(name, model), fill = interaction(model))) +
  geom_density_ridges(alpha = .5, scale = 1) + theme_minimal() +
  #scale_fill_manual(values = c("#08519c", "white", "darkgrey")) + 
  scale_fill_manual(values = c("skyblue", "orange", "#08519c", "red")) + 
  geom_vline(xintercept = 0, color = "red") +
  scale_y_discrete(labels = c("zeta.1" = "Democrat","zeta.2" = "Neutral","zeta.3" = "Republican")) + 
  xlab("logged odds") +ylab("") +
  theme(legend.position = "top",     # Move legend to the bottom
        legend.title = element_blank(),  # Remove legend title
        legend.spacing.x = unit(0.2, "cm"))  # Adjust spacing for better appearance
