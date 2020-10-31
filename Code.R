library(haven)
library(tidyverse)
library(broom)
library(Metrics) 

biltrade <- read_dta("/Users/tancredepolge/Documents/M2/M2S1/International/Trade HW/biltrade.dta")
#View(biltrade)

## Question a
biltrade2016 <- filter(biltrade, year == 2016)
#View(biltrade2016)

# Estimating coefs
biltrade2016$flow[which(biltrade2016$flow==0)] = NA #Removes 0 before taking the log
biltrade2016$log_flow <- log(biltrade2016$flow)
biltrade2016$log_dist <- log(biltrade2016$distw) 
reg_1 <- lm(log_flow ~ log_dist + contig + iso_o + iso_d, data = biltrade2016, na.action = na.exclude)

table_reg_1 <- tidy(reg_1)
table_reg_1 

# Extracting destination specific effects estimates (leaving aside standard errors here !!)

d_effect <- table_reg_1 %>% 
  .[substr(.$term,1,5)=="iso_d",] %>% 
  transmute(
    iso_d = substr(.$term,6,8),
    d_effect = estimate
  )

#rmse(biltrade2016$log_flow,predict(reg_1), ) #à chercher

# Adding importer specific effects in biltrade 2016

biltrade2016_extended <- merge(biltrade2016, d_effect, by.x = "iso_d")
biltrade2016_extended <- arrange(biltrade2016_extended, iso_o)
biltrade2016_extended$dist_effect <- table_reg_1$estimate[which(table_reg_1$term=="log_dist")]
biltrade2016_extended$contig_effect <- table_reg_1$estimate[which(table_reg_1$term=="contig")]
#View(biltrade2016_extended)

#Calculating FMP

biltrade2016_FMP<- biltrade2016_extended %>% 
  group_by(iso_o) %>% 
  summarise(
    FMP = sum(exp(1)^d_effect * distw^dist_effect * contig^contig_effect, na.rm = T),   # exp(1) because variable ptn_j is binary  
    GDP = mean(gdp_o / (pop_o*10^6)) # mean() allows for 'contraction' of the dataset (ie reducing from 31 000 to 199 variables) 
    )
#View(biltrade2016_FMP)

#Regression

biltrade2016_FMP$FMP[which(biltrade2016_FMP$FMP==0)] = NA #Removes 0 before taking the log
biltrade2016_FMP$GDP[which(biltrade2016_FMP$GDP==0)] = NA

biltrade2016_FMP$log_FMP <- log(biltrade2016_FMP$FMP)
biltrade2016_FMP$log_GDP <- log(biltrade2016_FMP$GDP) 
reg_2 <- lm(log_GDP ~ log_FMP, data = biltrade2016_FMP, na.action = na.exclude)

summary(reg_2)



# Question b

