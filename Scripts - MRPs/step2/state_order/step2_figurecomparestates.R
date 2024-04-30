library(ggplot2)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

if (!grepl("tweet_woc", getwd())) {setwd("./tweet_woc/")}


files = list.files("./Scripts - MRPs/step2/state_order", pattern = ".rds")
f_names = substr(files, 3, nchar(files)-4)



for (i in seq_along(files)) {
  if(i == 1) {
    df = readRDS(paste0("./Scripts - MRPs/step2/state_order/",files[i]))[,-2]
    names(df)[2] = paste0(names(df)[2], f_names[i])
    next
    }
  df.i = readRDS(paste0("./Scripts - MRPs/step2/state_order/",files[i]))[,-2]
  names(df.i)[2] = paste0(names(df.i)[2], f_names[i])
  df = merge(df, df.i, by = 'state')
    
}

View(df)

df$avg = rowMeans(df[,-c(1,7,8)])
df$avg_nb = rowMeans(df[,-c(1,6,7,8)])

df$sd <- apply(df[,-c(1,7,8)], 1, sd)
df$sd_nb <- apply(df[,-c(1,6,7,8)], 1, sd)

df$state = factor(df$state)
df$state <- factor(df$state, levels = df$state[order(df$avg)])

df$max_value_nb <- apply(df[, c(2:5,9)], 1, max)
df$min_value_nb <- apply(df[, c(2:5,9)], 1, min)
df$range_nb = df$max_value_nb - df$min_value_nb


df$state = factor(df$state)
df$state <- factor(df$state, levels = df$state[order(df$avg)])



df$state <- factor(df$state, levels = df$state[order(df$avg_nb)])
ggplot(data = df, mapping = aes(x = avg_nb, y = state)) +
  geom_point()  +theme_minimal() +
  #geom_errorbar(aes(xmin = ifelse(avg_nb - range_nb/2 < 1,1, avg_nb - range_nb/2), 
  #                  xmax = ifelse(avg_nb + range_nb/2 > 51,51, avg_nb + range_nb/2) , 
  #                  width = 0.2), alpha = .6) + 
  geom_point(mapping = aes(x=df[,2], y = state), shape =1, alpha = .25) +
  geom_point(mapping = aes(x=df[,3], y = state), shape =1 ,alpha = .25)  +
  geom_point(mapping = aes(x=df[,4], y = state), shape =1 ,alpha = .25)  +
  geom_point(mapping = aes(x=df[,5], y = state), shape =1 ,alpha = .25)  +
  geom_point(mapping = aes(x=df[,9], y = state), shape =1 ,alpha = .25) +
  geom_point(mapping = aes(x=df[,7], y = state), shape =3 ,alpha = 1, size = 2, color = "blue") +
  geom_point(mapping = aes(x=df[,8], y = state), shape =3 ,alpha = 1, size = 2, color = "red") +
  geom_errorbar(aes(xmin = pmin(df[,7], df[,8]), 
                    xmax = pmax(df[,7], df[,8]), 
                    width = 0.1), alpha = .4 ) + 

  xlab("Average Rank of State") + ylab("State")


mean(df$avg_nb - df$med_order_raw_average_onlyrep)
mean(df$avg_nb - df$med_order_raw_average_onlydem)
mean((df$avg_nb - df$med_order_raw_average_onlyrep)**2)
mean((df$avg_nb - df$med_order_raw_average_onlydem)**2)

  
