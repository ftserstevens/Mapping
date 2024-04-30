# Packages ----------------------------------------------------------------

library(rstan)
library(stringr)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

colMedians = function(X) {
  dims = dim(X)[2]
  medians = 1:dims
  for (i in 1:dims) {
    medians[i] = median(X[,i])
  }
  return(medians) }

# load df ----------------------------------------------------------------

df = readRDS("./merged_step2.rds")
df_og = df ##og dataset


# create select vec ----------------------------------------------------------
input_acuracy = names(df)[c(grep('raw_average',names(df)),
                            grep('raked',names(df)),
                            which(names(df) %in% str_subset(names(df), "(?<!_)model_")))]

input_count = c(gsub("raw_average","count", x= names(df)[grep('raw_average',names(df))]),
                rep('count',length(grep('raked',names(df)))),
                rep('count',length(which(names(df) %in% str_subset(names(df), "(?<!_)model_")))))


# select df ---------------------------------------------------------------

#### vecotr of all possible accuracy metrics
input_acuracy


i = 1 ### select the dependent var with i index. i = 1 is the raw average - naive sample.

accuracy = input_acuracy[i] ## or write the correct strn
f_count_ty = input_count[i]

f_count = c(0) #min count per tweet

mean(as.numeric(df$count_1for1),na.rm = T)
table(is.na(df$count_1for1))

# select correct alphas & counts ------------------------------------------
##remove NA & make num  
df$alpha = as.numeric(df_og[[accuracy]])
df$count = as.numeric(df_og[[f_count_ty]])
df = df[!is.na(df$alpha),]
df = df[!is.na(df$count),]

#mincount at least = to n
df = df[df$count >= f_count,]

#dependent to at least 10%
dependent =  quantile(df$alpha, .1, na.rm = T)


### fixed effects at state level
X_statecov = as.matrix(df[c(
  "white_perc", 
  "undergrad_perc", 
  "pop")])




# run stan ----------------------------------------------------------------
stan_list = list(
  age = as.numeric(factor(df$age)), #age
  female = ifelse(df$sex =="female",1,0), ##female dummy
  sex = ifelse(df$sex =="female",2,1), ##female dummy
  state = as.numeric(df$state),
  party = as.numeric(df$party),
  misc = ifelse(is.na(df$misinfo_exposure_score) == T, mean(df$misinfo_exposure_score, na.rm = T),df$misinfo_exposure_score),
  N = nrow(df),
  y = ifelse(df$alpha <= dependent, 1,0),
  n_S = 51, #total amount of states
  n_P =length(unique(df$party)),
 
  X_statecov = X_statecov,
  D_statecov = dim(X_statecov)[2]
)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())




run = T
  if (run) {
    print(accuracy)
    print(paste(nrow(df), "rows in the data"))
    print(paste(mean(df$count), "reviews per tweet"))
            step2 <-stan(
            file = "./stan models step2/step2.stan",
            #file = "./stan models step2/step2a includemisc.stan",
            data = stan_list, 
            iter = 2500,  
            warmup = 1250,  
            thin = 5,
            seed = 111,
            chains = 10,
            #control= list(max_treedepth=10),
            init = 'random',
            verbose = F,open_progress =F) 
          saveRDS(step2, paste0("./stan trained models step2/step2_",accuracy,".rds"))
          saveRDS(df,paste0("./stan trained models step2/df/step2df_",accuracy,".rds"))
  }



        