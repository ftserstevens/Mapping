# Packages ----------------------------------------------------------------
library(ipumsr)
library(terra)
library(zipcodeR)
library(rstan)
library(fastDummies)
library(dplyr)
library(parallel)
library(usmap)
library(usmapdata)
library(mltools)
library(maps)

library(readr)
library(stringr)

library(stringi)
library(survey)
library(cdlTools)


library(mltools)

library(ggplot2)
library(bayesplot)
library(ggthemes)
library(ggridges)



rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}




# load data ---------------------------------------------------------------
accuracy = "raw_average_onlydem"
#accuracy = "model_woc_discestimates"


step2 = readRDS(paste0("./stan trained models step2/step2_",accuracy,".rds"))
df = readRDS(paste0("./stan trained models step2/df/step2df_",accuracy,".rds"))

mean(as.numeric(df$count_1for1)[df$count_1for1 >0])
table(df$count_1for1 >4)

step2_params = rstan::extract(step2)
alpha = step2_params$alpha
beta_sex = step2_params$beta
gamma_age = step2_params$gamma
delta_state = step2_params$delta
zeta_party = step2_params$zeta
eta_statecov = step2_params$eta
sex_coef = step2_params$sex_coef



# digitize census ---------------------------------------------------------

census =readRDS("./census data/census.rds")

levels(census$STATE) == levels(df$state_short)

census = census %>% 
  mutate(STATE = as.numeric(STATE),
         AGE = as.numeric(AGE),
         SEX = as.numeric(SEX),
         PARTY = as.numeric(PARTY)) %>%
  group_by(STATE) %>%
  mutate(weight = N / sum(N)) %>%
  ungroup()





# make predictions --------------------------------------------------------



preds= array(NA, c(nrow(census), nrow(alpha)))
pi_preds = array(NA, c(nrow(census), nrow(alpha)))
ilogit = function(x) {exp(x) /(1+exp(x))}


n_census <- nrow(census)
n_alpha <- nrow(alpha)
fixed_eff =  c("white_perc", "undergrad_perc", "pop")


pb <- txtProgressBar(min = 0, max = n_census, style = 3)


for(i in 1:n_census) { #for evey row in census
  census_sex.i = as.numeric(census[i,"SEX"])
  census_age.i = as.numeric(census[i,"AGE"])
  census_state.i = as.numeric(census[i,"STATE"])
  census_party.i = as.numeric(census[i,"PARTY"])
  census_fe.i = as.numeric(census[i,fixed_eff])
  
  for (j in 1:n_alpha) { # for every iteration of estimated coef
    
    ## calculate fe
    sum_fe = sum(eta_statecov[j,] * census_fe.i)
      
    
    preds[i,j] =  as.numeric(
      alpha[j] + 
        sex_coef[j]* (census_sex.i-1) + 
        gamma_age[j, census_age.i] + 
        delta_state[j, census_state.i] +
        zeta_party[j,census_party.i] +
        sum_fe)
    
    
    
    pi_preds[i,j] = ilogit(preds[i,j])
  }
  setTxtProgressBar(pb, i)
  
}


# post-stratify to state --------------------------------------------------


#preds are preds[census, iteration]
state_PS = as.data.frame(array(NA, c(51,nrow(alpha)))) # [nstate, niterations]
rownames(state_PS) = levels(df$state_short)
w_estimate =  census$weight * pi_preds

pb <- txtProgressBar(min = 0, max = nrow(alpha), style = 3)

for (i in 1:nrow(alpha)) {
  state_PS[,i] = as.data.frame(cbind(state = census$STATE, estim = w_estimate[,i])) %>%
    dplyr::group_by(state) %>% 
    dplyr::summarise(estim = sum(estim))  %>% 
    dplyr::select(estim)
  setTxtProgressBar(pb, i)
}




state_PS.median = 1:nrow(state_PS)
pb <- txtProgressBar(min = 0, max = nrow(state_PS), style = 3)

for (i in 1:nrow(state_PS)){
  state_PS.median[i] = median(as.numeric(state_PS[i,]))
  setTxtProgressBar(pb, i)
  
}

state_PS.median = as.data.frame(cbind(rownames(state_PS),state_PS.median))
names(state_PS.median) = c('state','median')
state_PS.median$median = as.numeric(state_PS.median$median)
state_PS.median$med_order = as.numeric(rank(state_PS.median$median))

saveRDS(object = state_PS.median, file =
          paste0('./Scripts - MRPs/step2/state_order/df_',accuracy,'.rds'))




# plot predictions --------------------------------------------------------

us_map <- map_data("state")
unique(us_map$state)

us_map = usmap::us_map()
us_map$state = us_map$abbr

df_coord = as.data.frame(cbind(df$lon, df$lat))
names(df_coord) = c("lon",'lat')
df_coord = usmap::usmap_transform(df_coord)
merged_data <- dplyr::left_join(us_map, state_PS.median, by = "state")



#eff_10thpercentile = as.data.frame(table(df$alpha <= dependent)/nrow(df))[2,2]
states_us = mcmc_intervals(as.data.frame(t(state_PS[order(state_PS.median[,2]),]))) + 
  #geom_vline(xintercept=  eff_10thpercentile  , colour = "red") +
  theme_minimal()

ggsave(filename = paste0('./Scripts - MRPs/step2/plots_poststrat/poststates_',accuracy,'.pdf'),
  plot = states_us,
  width = 8, height = 10, units = 'inch')

names(state_PS.median) =c("state","median")


n_colors <- 9
custom_colors <- scales::col_numeric(palette = c(
  "#f7fbff",
           "#deebf7",
           "#c6dbef",
           "#9ecae1",
           "#6baed6",
           "#4292c6",
           "#2171b5",
           "#08519c",
           "#08306b"
),
#"#662506"), 
domain = NULL)(seq(0, 1, length.out = n_colors))



# Plot the state polygons
map_us = ggplot(merged_data, aes(x = x, y = y)) + 
  #ggtitle(paste(f_alpha, if(context_filter) {context})) + 
  geom_polygon(aes(fill = med_order, group = group)) +
  #limits=c(0.08,0.125)) +
  scale_fill_gradientn(colors = custom_colors, na.value = "grey50") + guides(fill="none") +
  geom_path(data = merged_data, aes(x = x, y = y, group = group), color = "black", size = 0.3) +
  #geom_jitter(data = df_coord, aes(x = x, y = y),   color = "white", size = 1.2, alpha = .25) + 
  #geom_point(data = df_coord, aes(x = x, y = y), color = "black", size = 1.4, shape = 1, alpha = .75)  +
  
  
  ggthemes::theme_map() 

map_us
ggsave(paste0(
  filename = './Scripts - MRPs/step2/plots_poststrat/USAmap_',accuracy,'.pdf'),
  plot = map_us,
  width = 10, height = 8, units = 'inch')

