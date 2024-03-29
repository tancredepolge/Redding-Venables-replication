###########################################################################
#                       INTERNATIONAL ECONOMICS : Homework 1              #                            
#                                 Exercise 2                              #
#                     Azizakhon, Tancrède, Victor, Xuan                   #
###########################################################################

library(haven)
library(tidyverse)
library(broom)
library(Metrics)
library(stargazer)

biltrade <- read_dta("./biltrade.dta")
#View(biltrade)

#### QUESTION A ####

## Calculate FMP for 2016

biltrade2016 <- filter(biltrade, year == 2016)
#View(biltrade2016)

# Estimating coefs
biltrade2016$flow[which(biltrade2016$flow==0)] = 1 #Removes 0 before taking logs. Consider that 0s correspond to rounded values rather than NAs (as in the paper)
biltrade2016$log_flow <- log(biltrade2016$flow)
biltrade2016$log_dist <- log(biltrade2016$distw) 
reg_1 <- lm(log_flow ~ log_dist + contig + iso_o + iso_d, data = biltrade2016, na.action = na.exclude)

#stargazer(reg_1, type="latex", out="/Users/tancredepolge/Documents/M2/M2S1/International/Trade HW/TeX/reg1.tex")
summary(reg_1)   ### Table 1
RMSE_reg1 <- sqrt(mean(reg_1$residuals^2)) # RMSE

# Extracting destination specific effects estimates (leaving aside standard errors here !!)

table_reg_1 <- tidy(reg_1)

d_effect <- table_reg_1 %>% 
  .[substr(.$term,1,5)=="iso_d",] %>% 
  transmute(
    iso_d = substr(.$term,6,8),
    d_effect = estimate
  )


# Adding importer specific effects in biltrade 2016

biltrade2016_extended <- merge(biltrade2016, d_effect, by.x = "iso_d")
biltrade2016_extended <- arrange(biltrade2016_extended, iso_o)
biltrade2016_extended$dist_effect <- table_reg_1$estimate[which(table_reg_1$term=="log_dist")]
biltrade2016_extended$contig_effect <- table_reg_1$estimate[which(table_reg_1$term=="contig")]
#View(biltrade2016_extended)

#Calculating FMP

biltrade2016_FMP <- biltrade2016_extended %>% 
  group_by(iso_o) %>% 
  summarise(
    FMP = sum(exp(1)^d_effect * distw^dist_effect * contig^contig_effect, na.rm = T),   # exp(1) because variable ptn_j is binary  
    GDP = mean(gdp_o / (pop_o*10^6)) # mean() allows for 'contraction' of the dataset (ie reducing from 31 000 to 199 variables) 
    )
#View(biltrade2016_FMP)

## Regression of GDP per capita on FMP

biltrade2016_FMP$FMP[which(biltrade2016_FMP$FMP==0)] = NA #Removes 0 before taking the log
biltrade2016_FMP$GDP[which(biltrade2016_FMP$GDP==0)] = NA

biltrade2016_FMP$log_FMP <- log(biltrade2016_FMP$FMP)
biltrade2016_FMP$log_GDP <- log(biltrade2016_FMP$GDP) 
reg_2 <- lm(log_GDP ~ log_FMP, data = biltrade2016_FMP, na.action = na.exclude)

summary(reg_2) # Table 2
#stargazer(reg_2, type="latex", out="/Users/tancredepolge/Documents/M2/M2S1/International/Trade HW/TeX/reg2.tex")

fig_1 <- ggplot(biltrade2016_FMP, aes(log_FMP,log_GDP, label=iso_o)) +     #Fig 1 
  geom_text(size=3) +
  xlab("Log Foreign Market Potential") + 
  ylab("Log GDP per capita") +
  ggtitle("GDP per capita and Foreign Market Potential")
  
#pdf(file = "/Users/tancredepolge/Documents/M2/M2S1/International/Trade/Trade HW/TeX/fig1.pdf", 8, 5)
#print(fig_1)
#dev.off() 





    ##### QUESTION B #####

## Calculating FMPs for the period 2004-2016

biltrade_all_FMP <- biltrade2016_FMP %>% select(-log_FMP,-log_GDP)
biltrade_all_FMP$year <- 2016

for (i in 2004:2015) {
  biltrade_i <- filter(biltrade, year == i)
  #View(biltrade_i)

# Estimating coefs
  biltrade_i$flow[which(biltrade_i$flow==0)] = 1 #Removes 0 before taking logs. Consider that 0s correspond to rounded values rather than NAs (as in the paper)
  biltrade_i$log_flow <- log(biltrade_i$flow)
  biltrade_i$log_dist <- log(biltrade_i$distw) 
  reg_1 <- lm(log_flow ~ log_dist + contig + iso_o + iso_d, data = biltrade_i, na.action = na.exclude)

  #summary(reg_1)   ### Table 1
  #sqrt(mean(reg_1$residuals^2)) # RMSE

# Extracting destination specific effects estimates

  table_reg_1 <- tidy(reg_1)

  d_effect <- table_reg_1 %>% 
    .[substr(.$term,1,5)=="iso_d",] %>% 
    transmute(
      iso_d = substr(.$term,6,8),
      d_effect = estimate
    )


# Adding importer specific effects 

  biltrade_i_extended <- merge(biltrade_i, d_effect, by.x = "iso_d")
  biltrade_i_extended <- arrange(biltrade_i_extended, iso_o)
  biltrade_i_extended$dist_effect <- table_reg_1$estimate[which(table_reg_1$term=="log_dist")]
  biltrade_i_extended$contig_effect <- table_reg_1$estimate[which(table_reg_1$term=="contig")]
  #View(biltrade_i_extended)

#Calculating FMP

  biltrade_i_FMP <- biltrade_i_extended %>% 
    group_by(iso_o) %>% 
    summarise(
      FMP = sum(exp(1)^d_effect * distw^dist_effect * contig^contig_effect, na.rm = T), # exp(1) because variable ptn_j is binary  
      GDP = mean(gdp_o / (pop_o*10^6)) # mean() allows for 'contraction' of the dataset (ie reducing from 31 000 to 199 variables) 
      )

  biltrade_i_FMP$year <- i
  biltrade_all_FMP <- rbind(biltrade_all_FMP,biltrade_i_FMP)
  cat("Year",i,"complete")
}

biltrade_all_FMP <- arrange(biltrade_all_FMP,iso_o , -year )
#View(biltrade_all_FMP)

## Computing log(GDP) per capita and log(FMP) for 2004-2016

biltrade_all_FMP$FMP[which(biltrade_all_FMP$FMP==0)] = NA #Removes 0 before taking the log
biltrade_all_FMP$GDP[which(biltrade_all_FMP$GDP==0)] = NA
biltrade_all_FMP$log_FMP <- log(biltrade_all_FMP$FMP)
biltrade_all_FMP$log_GDP <- log(biltrade_all_FMP$GDP) 


##  Adding covariates : 


# Average of Doing Business Index across 2009-2016 period
DB <- read.csv('./DB Index.csv')
biltrade_all_FMP <- merge(biltrade_all_FMP, DB, by = 'iso_o')

# Tax Haven status according to Oxfam 
tax_haven <- read.csv('./Tax Haven.csv')
biltrade_all_FMP <- merge(biltrade_all_FMP, tax_haven, by = 'iso_o')

# Yearly average Brent price
brent <- read.csv('./brent.csv')
biltrade_all_FMP <- merge(biltrade_all_FMP, brent, by = 'year')


## Regression GDP per captia on FMP with Fixed Effects

reg_3a <- lm(log_GDP ~ log_FMP, data = biltrade_all_FMP, na.action = na.exclude)    #Simple regression
summary(reg_3a)
reg_3b <- lm(log_GDP ~ log_FMP + year, data = biltrade_all_FMP, na.action = na.exclude)   #Regression with year FEs
summary(reg_3b)
reg_3c <- lm(log_GDP ~ log_FMP + iso_o , data = biltrade_all_FMP, na.action = na.exclude)    #Regression with country of origin FEs
summary(reg_3c)
reg_3d <- lm(log_GDP ~ log_FMP + iso_o + year, data = biltrade_all_FMP, na.action = na.exclude)    #Regression with country of origin and year FEs
summary(reg_3d)
#stargazer(reg_3a, reg_3b, reg_3c, reg_3d, type="latex", out="/Users/tancredepolge/Documents/M2/M2S1/International/Trade/Trade HW/TeX/reg3.tex")


## Regression GDP per captia on FMP with covariates
reg_4 <- lm(log_GDP ~ log_FMP + average_DB + tax_haven + avg_brent_spot, data = biltrade_all_FMP, na.action = na.exclude)  #Regression with covariates
summary(reg_4)
#stargazer(reg_4, type="latex", out="/Users/tancredepolge/Documents/M2/M2S1/International/Trade/Trade HW/TeX/reg4.tex")


