
# Loading ----------------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(ggtext)
library(corrplot)
library(tidyr)
library(dplyr)

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
ilogit = function(x) {exp(x)/(1+ exp(x)) }




# metric plotting ----------------------------------------------------------
df = readRDS("~/Documents/tweet_woc/merged_step2.rds")



# corr matrix -------------------------------------------------------------


mat = as.data.frame(df[df$count >= (0), c("raw_average",
                                          "raw_average_1for1",
                                          "raked_average",
                                          "raw_average_onlydem",
                                          "raw_average_onlyrep",
                                          "model_raw",
                                          "model_1for1",
                                          #"model_dem_discestimates",
                                          #"model_neu_discestimates",
                                          #"model_rep_discestimates",
                                          "model_woc_discestimates"
)])


names(mat) = c("Sample","Balanced", "Population" ,"OnlyDem","OnlyRep",
               "Model Sample","Model Balanced", 
               #"model_dem","model_neu","model_rep",
               "Model Population")
mat = mat[complete.cases(mat),]


corrplot::corrplot(cor(mat), method = "color", type = "full", 
                   #order = 'hclust', 
                   diag = T, 
                   cl.pos = "n",
                   addCoef.col = 'black', outline = T,   tl.col = "black")


corrplot::corrplot(cor.mtest(mat)$p, method = "color", type = "lower", 
                   #order = 'hclust', 
                   add = T, 
                   
                   diag = F,
                   tl.pos = "n",
                   cl.pos = "n",
                   addCoef.col = 'black', outline = T,   tl.col = "black",
                   col =c(rep('white' , 11),rep('#ee6b6e', 9))
)



View(df[df$count >=5,c("text", "raw_average", "model_woc_discestimates", 'raw_average_onlydem', 'raw_average_onlyrep')])
