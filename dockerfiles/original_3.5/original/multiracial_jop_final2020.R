####################################################################################################
# DATE:           May 2020
# DESCRIPTION:    Replication file creates figures, tables, and appendices for 
#                 Davenport, Iyengar, & Franco (2020)
# R VERSION:      Requires R version 3.6.2 (2019-12-12)
####################################################################################################

# Code requires the following packages/dependencies. If necessary, please uncommend and run below

## Install old versions of Zelig and ZeligChoice
# 

#packageurl <- "http://cran.r-project.org/src/contrib/Archive/Zelig/Zelig_5.1.6.tar.gz" 
## 
#install.packages(packageurl, repos=NULL, type="source")
#
#
## packageurl2 <- "http://cran.r-project.org/src/contrib/Archive/ZeligChoice/ZeligChoice_0.9-6.tar.gz" 
## 
#install.packages(packageurl2, repos=NULL, type="source")




library("MASS")

library("dplyr")

library("tidyr")

library("ggplot2")

library("purrr")

library("broom")

library("foreign")

library("Zelig")

library("ZeligChoice")

library("nnet")

library("car")

library("texreg")

library("extrafont")

library("caret")

library("e1071")

library("scales")

library("xtable")

library("questionr")

library("ggridges")

library("reporttools")

library("stringr")




#setwd('~/Dropbox/PewMultiracial/analysis/')
source("multiracial_jop_functions_robust.R")





##########
## DATA ##
##########

df <- read.spss('multiracial.sav', to.data.frame = TRUE)

colnames(df) <- tolower(colnames(df))

# Exclude South Asians
df <- subset(df, (is.na(df$q24_2) | q24_2 == "No") &
               (is.na(df$q24_3) | q24_3 == "No"))

#############
## RECODES ##
#############

## RACIAL GROUP ##
df$race <- df$q_11rand
levels(df$race) <- c("WHITE-BLACK", "WHITE-ASIAN", "WHITE", "BLACK", "ASIAN")
df$race <- relevel(df$race, ref = "WHITE")
table(df$race)


## EDUCATION ##
df$educ3 <- recode(as.numeric(df$educ), "2=1; 3=2; 4=2; 5=3; 6=3")
df$educ3 <- factor(df$educ3, labels = c("Less than or equal HS", "Some College", "College Grad"))
table(df$educ, df$educ3)


## AGE ##
df$age <- 2015 - as.numeric(as.character(df$birthyr))
summary(df$age)
df %>%
  ggplot(aes(age, race), fill = race) +
  geom_density_ridges(alpha = 0.5)


## RELIGION ##
df$salRelig <- as.numeric(revFac(df$q6f))
table(df$salRelig, df$q6f)


## INCOME ##
df$income <- ifelse(df$faminc == "Prefer not to say", NA, as.numeric(df$faminc))
df$income <- ifelse(df$income > 11, 12, df$income)
summary(df$income)




## REGION ##
southern_states <- c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky",
            "Louisiana", "Mississippi", "North Carolina", "South Carolina",
            "Tennessee", "Texas", "Virginia", "West Virginia")
df$south <- ifelse(df$inputstate %in% southern_states, 1, 0)
table(df$south, df$inputstate %in% southern_states)


## RELATIVE SALIENCE OF RACE ##
# recode s.t. higher values = more salient
df$sal.race <- oneto0(df$q6b)
df$sal.class <- oneto0(df$q6a)
df$sal.age <- oneto0(df$q6c)
df$sal.gender <- oneto0(df$q6d)
df$sal.polit <- oneto0(df$q6e)
df$sal.relig <- oneto0(df$q6f)

# avg salience of race minus avg salience along other dimensions
df$relRace <- df$sal.race -
  (df$sal.class + df$sal.age + df$sal.gender + df$sal.polit + df$sal.relig)/5




## CLOSENESS TO RACIAL GROUPS ##
# higher values = closer
df$closeWhite <- revFac(df$q12a)
df$closeBlack <- revFac(df$q12b)
df$closeAsian <- revFac(df$q12c)

# Compute relative closeness scores by subtracting
# average closeness to out-groups from closeness to in-group (treating
# minority group as biracials' in-group)
df$closeW <- neg1to1(df$closeWhite)
df$closeA <- neg1to1(df$closeAsian)
df$closeB <- neg1to1(df$closeBlack)

df$relCloseW <- neg1to1(df$closeW - (df$closeA + df$closeB)/2)
df$relCloseA <- neg1to1(df$closeA - (df$closeW + df$closeB)/2)
df$relCloseB <- neg1to1(df$closeB - (df$closeW + df$closeA)/2)

df$relClose[df$race == "WHITE"] <- df$relCloseW[df$race == "WHITE"]
df$relClose[df$race == "BLACK"] <- df$relCloseB[df$race == "BLACK"]
df$relClose[df$race == "ASIAN"] <- df$relCloseA[df$race == "ASIAN"]
df$relClose[df$race == "WHITE-BLACK"] <- df$relCloseB[df$race == "WHITE-BLACK"]
df$relClose[df$race == "WHITE-ASIAN"] <- df$relCloseA[df$race == "WHITE-ASIAN"]


## LINKED FATE ##

# recode s.t. higher values = more linked

df$lfWhite <- revFac(df$q7a)

df$lfBlack <- revFac(df$q7b)

df$lfAsian <- revFac(df$q7c)



# Coalesce linked fate variables. For minority and biracial respondents,

# this variable measures perceptions of fate linked to minority group

df$linked <- coalesce(df$lfBlack, df$lfAsian, df$lfWhite)

df$linked <- neg1to1(df$linked)



# Compute relative lf for biracials by subtracting perceived

# linked fate to whites from linked fate to non-whites

df$lfWA <- rescale(as.numeric(df$lfAsian) - as.numeric(df$lfWhite), c(-1, 1))

df$lfWB <- rescale(as.numeric(df$lfBlack) - as.numeric(df$lfWhite), c(-1, 1))



df$relLinked <- df$linked
df$relLinked[df$race == "WHITE-ASIAN"] <- df$lfWA[df$race == "WHITE-ASIAN"]
df$relLinked[df$race == "WHITE-BLACK"] <- df$lfWB[df$race == "WHITE-BLACK"]


## IAT ##

# remove IAT scores with a high error rate (N = 548)

df$white_asian_dscore[df$error_rate > .3] <- NA

df$white_black_dscore[df$error_rate > .3] <- NA



# coalesce IAT scores.

df$iat <- coalesce(df$white_asian_dscore, df$white_black_dscore)



## PARTY ID ##
# Trichotomize, merge "Independent" and "Other"
df$pid <- recode(as.numeric(df$q21), "1=3; 2=1; 3=2; 4=2; else=NA")
df$pid <- factor(df$pid, labels = c("Republican", "Ind/Other", "Democrat"))
table(df$pid, df$q21)


## PARTY SORTING ##
df$dem <-  ifelse(
  df$pid7 == "Strong Democrat" |
    df$pid7 == "Not very strong Democrat" |
    df$pid7 == "Lean Democrat", 1, 0)

# Equals 1 if self-reported Democrats & self-reported liberals.
df$sorted_dems <- ifelse(
  (df$q22 == "Very liberal" | df$q22 == "Liberal") &
    df$dem == 1, 1, 0)


## IDEOLOGY ##
# negative values = liberal
# positive values = conservative
df$ideo <- recode(as.numeric(df$q22), "1=1; 2=1; 3=0; 4=-1; 5=-1")
df$ideo <- factor(df$ideo, labels = c("Liberal", "Moderate", "Conservative"))



## POLICE TREATMENT ##
df$police <- df$q20
levels(df$police) <- c("Excellent/Good", "Excellent/Good", "Fair", "Poor")


## CULTURAL PREFERENCES ##
# Abortion
df$abortion <- factor(recode(
  as.numeric(df$q18), "1:2=1; NA=NA; else=0"),
  labels = c("Oppose", "Support")
)

# Same-sex marriage
df$marriage <- factor(recode(
  as.numeric(df$q15), "1:2=1; NA=NA; else=0"),
  labels = c("Oppose", "Support")
)

df$cultural <- factor(
  (as.numeric(df$abortion) + as.numeric(df$marriage))/2,
  labels = c("Oppose Both", "Conflicting Views", "Support Both")
)



## SOCIAL WELFARE PREFERENCES ##
# Spending
df$spending <- factor(df$q13, labels = c("Fewer", "More"))

# ACA
df$obamacare <- revFac(df$q17)

df$welfare <- factor(
  (as.numeric(df$obamacare) + as.numeric(df$spending))/2,
  labels = c("Oppose Both", "Conflicting Views", "Support Both")
)


##################
## DESCRIPTIVES ##
##################

## TABLE 1: Partisanship and Ideology x Race

# Partisanship
t1a <- round(prop.table(wtd.table(x = df$pid, y = df$race, weights = df$psweight), 2)*100, 1)

# Ideology
t1b <- round(prop.table(wtd.table(x = df$ideo, y = df$race, weights = df$psweight), 2)*100, 1)

# Sorted Democrats
t1c <- df %>%
  filter(dem == 1) %>%
  wtd.table(
    x = .$sorted_dems,
    y = .$race,
    weights = .$psweight) %>%
  prop.table(2)



# Linked fate
df <- df %>%
  mutate(
    lfBlack2 = recode(as.numeric(df$lfBlack), "1:2='Disagree'; 3:4='Agree'"),
    lfAsian2 = recode(as.numeric(df$lfAsian), "1:2='Disagree'; 3:4='Agree'"),
    lfWhite2 = recode(as.numeric(df$lfWhite), "1:2='Disagree'; 3:4='Agree'")
  )

t1d <- round(prop.table(wtd.table(x = df$lfWhite2, y = df$race, weights = df$psweight), 2)*100, 1)
t1e <- round(prop.table(wtd.table(x = df$lfBlack2, y = df$race, weights = df$psweight), 2)*100, 1)
t1f <- round(prop.table(wtd.table(x = df$lfAsian2, y = df$race, weights = df$psweight), 2)*100, 1)



# Table 1
t1 <- rbind(
  cbind(rownames(t1a), t1a),
  cbind(rownames(t1b), t1b),
  c("Sorted Democrats", round(t1c[2,] * 100, 1)),
  c("Linked Fate to Whites", t1d[1,]),
  c("Linked Fate to Blacks", t1e[1,]),
  c("Linked Fate to Asians", t1f[1,])
)
print(xtable(
  t1, digits = 1, align = rep("r", 7),
  caption = "Partisanship and Ideology by Racial Background"),
  include.rownames = FALSE
)




# chi-sq tests of independence
weights::wtd.chi.sq(df$pid, df$race, weight = df$psweight, na.rm = TRUE)
weights::wtd.chi.sq(df$ideo, df$race, weight = df$psweight, na.rm = TRUE)
weights::wtd.chi.sq(
  df$sorted_dems[df$dem == 1],
  df$race[df$dem == 1],
  weight = df$psweight[df$dem == 1],
  na.rm = TRUE
)
weights::wtd.chi.sq(df$lfWhite2, df$race, weight = df$psweight, na.rm = TRUE)
weights::wtd.chi.sq(df$lfBlack2, df$race, weight = df$psweight, na.rm = TRUE)
weights::wtd.chi.sq(df$lfAsian2, df$race, weight = df$psweight, na.rm = TRUE)



# two-sample z test of weighted means comparison multiracial minority
z.test = function(x1,x2,n1,n2){
  numerator = (x1/n1) - (x2/n2)
  p.common = (x1+x2) / (n1+n2)
  denominator = sqrt(p.common * (1-p.common) * (1/n1 + 1/n2))
  z.test.ris = numerator / denominator
  return(z.test.ris)
}

z.test(
  wtd.table(x = df$lfBlack2, y = df$race, weights = df$psweight)["Agree", "WHITE-BLACK"],
  wtd.table(x = df$lfAsian2, y = df$race, weights = df$psweight)["Agree", "WHITE-ASIAN"],
  sum(wtd.table(x = df$lfBlack2, y = df$race, weights = df$psweight)[,"WHITE-BLACK"]),
  sum(wtd.table(x = df$lfAsian2, y = df$race, weights = df$psweight)[,"WHITE-ASIAN"])
)



2*pnorm(-abs(
  z.test(
    wtd.table(x = df$lfBlack2, y = df$race, weights = df$psweight)["Agree", "WHITE-BLACK"],
    wtd.table(x = df$lfAsian2, y = df$race, weights = df$psweight)["Agree", "WHITE-ASIAN"],
    sum(wtd.table(x = df$lfBlack2, y = df$race, weights = df$psweight)[,"WHITE-BLACK"]),
    sum(wtd.table(x = df$lfAsian2, y = df$race, weights = df$psweight)[,"WHITE-ASIAN"])
  )
))



# two-sample z test of weighted means comparison monoracial minority
z.test = function(x1,x2,n1,n2){
  numerator = (x1/n1) - (x2/n2)
  p.common = (x1+x2) / (n1+n2)
  denominator = sqrt(p.common * (1-p.common) * (1/n1 + 1/n2))
  z.test.ris = numerator / denominator
  return(z.test.ris)
}


z.test(
  wtd.table(x = df$lfBlack2, y = df$race, weights = df$psweight)["Agree", "BLACK"],
  wtd.table(x = df$lfAsian2, y = df$race, weights = df$psweight)["Agree", "ASIAN"],
  sum(wtd.table(x = df$lfBlack2, y = df$race, weights = df$psweight)[,"BLACK"]),
  sum(wtd.table(x = df$lfAsian2, y = df$race, weights = df$psweight)[,"ASIAN"])
)

2*pnorm(-abs(
  z.test(
    wtd.table(x = df$lfBlack2, y = df$race, weights = df$psweight)["Agree", "BLACK"],
    wtd.table(x = df$lfAsian2, y = df$race, weights = df$psweight)["Agree", "ASIAN"],
    sum(wtd.table(x = df$lfBlack2, y = df$race, weights = df$psweight)[,"BLACK"]),
    sum(wtd.table(x = df$lfAsian2, y = df$race, weights = df$psweight)[,"ASIAN"])
  )
))



###################
## MAIN ANALYSES ##
###################

set.seed(933301)

med <- c(0, 1/2, 1)
quantiles <- c(0.025, 0.05, 0.5, 0.95, 0.975)


## Police Mistreatment of Minorities
modelPolice <- zelig(
  as.factor(police) ~ age + educ3 + gender + income + south + race +
    relLinked + relRace + relClose + salRelig,
  data = df,
  weights = "psweight",
  model = "ologit",
  cite = FALSE
)


police <- data.frame(
  race = c("Black", "White-Black", "Asian", "White-Asian"),
  lower95 = rep(NA, 4),
  lower90 = rep(NA, 4),
  mean = rep(NA, 4),
  upper90 = rep(NA, 4),
  upper95 = rep(NA, 4))

# contrasts
police_setx_w <- set_ppparams(model = modelPolice, group = 'WHITE')
police_setx_b <- set_ppparams(model = modelPolice, group = 'BLACK')
police_setx_wb <- set_ppparams(model = modelPolice, group = 'WHITE-BLACK')
police_setx_a <- set_ppparams(model = modelPolice, group = 'ASIAN')
police_setx_wa <- set_ppparams(model = modelPolice, group = 'WHITE-ASIAN')

# predicted probabilities
police[1, 2:6] <- sim(modelPolice, x = police_setx_w, x1 = police_setx_b, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
   rowSums() %>%
   quantile(quantiles)

police[2, 2:6] <- sim(modelPolice, x = police_setx_w, x1 = police_setx_wb, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

police[3, 2:6] <- sim(modelPolice, x = police_setx_w, x1 = police_setx_a, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)


police[4, 2:6] <- sim(modelPolice, x = police_setx_w, x1 = police_setx_wa, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

police


# plot
police$race <- factor(
  police$race,
  levels = c("White-Asian", "Asian", "White-Black", "Black"))

ggplot(police, aes(y = mean, x = race)) +
  geom_errorbar(width = .3, aes(ymin = lower95, ymax = upper95)) +
  geom_linerange(size = 1.2, aes(ymin = lower90, ymax = upper90)) +
  geom_point(size = 2.5) + theme_bw() + coord_flip() +
 geom_hline(yintercept = 0, linetype = "dashed") + ylim(-.1, .3) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ylab("Difference from Monoracial Whites") + xlab("")
ggsave("multiracial_jop/police.pdf", width = 6, height = 2.5)



## Cultural Preferences
modelCultural <- zelig(
  as.factor(cultural) ~ age + educ3 + gender + income + south + race +
    relLinked + relRace + relClose + salRelig,
  data = df,
  weights = "psweight",
  model = "ologit",
  cite = FALSE
)

cultural <- data.frame(
  race = c("Black", "White-Black", "Asian", "White-Asian"),
  lower95 = rep(NA, 4),
  lower90 = rep(NA, 4),
  mean = rep(NA, 4),
  upper90 = rep(NA, 4),
  upper95 = rep(NA, 4))


# contrasts
cultural_setx_w <- set_ppparams(model = modelCultural, group = 'WHITE')
cultural_setx_b <- set_ppparams(model = modelCultural, group = 'BLACK')
cultural_setx_wb <- set_ppparams(model = modelCultural, group = 'WHITE-BLACK')
cultural_setx_a <- set_ppparams(model = modelCultural, group = 'ASIAN')
cultural_setx_wa <- set_ppparams(model = modelCultural, group = 'WHITE-ASIAN')

# predicted probabilities
cultural[1, 2:6] <- sim(modelCultural, x = cultural_setx_w, x1 = cultural_setx_b, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

cultural[2, 2:6] <- sim(modelCultural, x = cultural_setx_w, x1 = cultural_setx_wb, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

cultural[3, 2:6] <- sim(modelCultural, x = cultural_setx_w, x1 = cultural_setx_a, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)


cultural[4, 2:6] <- sim(modelCultural, x = cultural_setx_w, x1 = cultural_setx_wa, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

cultural


# plot
cultural$race <- factor(
  cultural$race,
  levels = c("White-Asian", "Asian", "White-Black", "Black"))


ggplot(cultural, aes(y = mean, x = race)) +
  geom_errorbar(width = .3, aes(ymin = lower95, ymax = upper95)) +
  geom_linerange(size = 1.2, aes(ymin = lower90, ymax = upper90)) +
  geom_point(size = 2.5) + theme_bw() + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") + ylim(-.25, .1) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ylab("Difference from Monoracial Whites") + xlab("")
ggsave("multiracial_jop/cultural.pdf", width = 6, height = 2.5)



## Social Welfare Preferences
modelWelfare <- zelig(
  as.factor(welfare) ~ age + educ3 + gender + income + south + race +
    relLinked + relRace + relClose + salRelig,
  data = df,
  weights = "psweight",
  model = "ologit",
  cite = FALSE
)



welfare <- data.frame(
  race = c("Black", "White-Black", "Asian", "White-Asian"),
  lower95 = rep(NA, 4),
  lower90 = rep(NA, 4),
  mean = rep(NA, 4),
  upper90 = rep(NA, 4),
  upper95 = rep(NA, 4))



# contrasts
welfare_setx_w <- set_ppparams(model = modelWelfare, group = 'WHITE')
welfare_setx_b <- set_ppparams(model = modelWelfare, group = 'BLACK')
welfare_setx_wb <- set_ppparams(model = modelWelfare, group = 'WHITE-BLACK')
welfare_setx_a <- set_ppparams(model = modelWelfare, group = 'ASIAN')
welfare_setx_wa <- set_ppparams(model = modelWelfare, group = 'WHITE-ASIAN')

# predicted probabilities
welfare[1, 2:6] <- sim(modelWelfare, x = welfare_setx_w, x1 = welfare_setx_b, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

welfare[2, 2:6] <- sim(modelWelfare, x = welfare_setx_w, x1 = welfare_setx_wb, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

welfare[3, 2:6] <- sim(modelWelfare, x = welfare_setx_w, x1 = welfare_setx_a, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

welfare[4, 2:6] <- sim(modelWelfare, x = welfare_setx_w, x1 = welfare_setx_wa, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

welfare


# plot
welfare$race <- factor(
  welfare$race,
  levels = c("White-Asian", "Asian", "White-Black", "Black"))

ggplot(welfare, aes(y = mean, x = race)) +
  geom_errorbar(width = .3, aes(ymin = lower95, ymax = upper95)) +
  geom_linerange(size = 1.2, aes(ymin = lower90, ymax = upper90)) +
  geom_point(size = 2.5) + theme_bw() + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") + ylim(-.2, .2) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ylab("Difference from Monoracial Whites") + xlab("")
ggsave("multiracial_jop/welfare.pdf", width = 6, height = 2.5)




## MULTIRACIAL TO MINORITY COMPARISON


m2m <- data.frame(
  race = rep(c("White-Black", "White-Asian"), 3),
  model = rep(c('Police\nPerceptions', 'Cultural\nLiberalism', 'Social\nWelfare'), each = 2),
  lower95 = rep(NA, 6),
  lower90 = rep(NA, 6),
  mean = rep(NA, 6),
  upper90 = rep(NA, 6),
  upper95 = rep(NA, 6))

m2m[1, 3:7] <- sim(modelPolice, x = police_setx_b, x1 = police_setx_wb, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

m2m[2, 3:7] <- sim(modelPolice, x = police_setx_a, x1 = police_setx_wa, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

m2m[3, 3:7] <- sim(modelCultural, x = cultural_setx_b, x1 = cultural_setx_wb, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

m2m[4, 3:7] <- sim(modelCultural, x = cultural_setx_a, x1 = cultural_setx_wa, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)


m2m[5, 3:7] <- sim(modelWelfare, x = welfare_setx_b, x1 = welfare_setx_wb, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

m2m[6, 3:7] <- sim(modelWelfare, x = welfare_setx_a, x1 = welfare_setx_wa, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

m2m


# plot
m2m$race <- factor(
  m2m$race,
  levels = c("White-Asian", "Asian", "White-Black", "Black"))

m2m$model <- factor(
  m2m$model,
  levels = c('Police\nPerceptions', 'Social\nWelfare', 'Cultural\nLiberalism')
)


ggplot(m2m, aes(y = mean, x = model)) +
  geom_errorbar(width = .3, aes(ymin = lower95, ymax = upper95)) +
  geom_linerange(size = 1.2, aes(ymin = lower90, ymax = upper90)) +
  geom_point(size = 2.5) + theme_bw() + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") + ylim(-.2, .2) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  facet_wrap(~ race) +
  ylab("Difference from Monoracial Minority Group") + xlab("")
ggsave("multiracial_jop/m2m.pdf", width = 7, height = 2.5)



## MULTIRACIAL TO WHITE COMPARISON



m2w <- data.frame(
  race = rep(c("White-Black", "White-Asian"), 3),
  model = rep(c('Police\nPerceptions', 'Cultural\nLiberalism', 'Social\nWelfare'), each = 2),
  lower95 = rep(NA, 6),
  lower90 = rep(NA, 6),
  mean = rep(NA, 6),
  upper90 = rep(NA, 6),
  upper95 = rep(NA, 6))

m2w[1, 3:7] <- sim(modelPolice, x = police_setx_w, x1 = police_setx_wb, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)


m2w[2, 3:7] <- sim(modelPolice, x = police_setx_w, x1 = police_setx_wa, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

m2w[3, 3:7] <- sim(modelCultural, x = cultural_setx_w, x1 = cultural_setx_wb, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

m2w[4, 3:7] <- sim(modelCultural, x = cultural_setx_w, x1 = cultural_setx_wa, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

m2w[5, 3:7] <- sim(modelWelfare, x = welfare_setx_w, x1 = welfare_setx_wb, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)

m2w[6, 3:7] <- sim(modelWelfare, x = welfare_setx_w, x1 = welfare_setx_wa, num = 10000) %>%
  get_qi(qi = 'fd', xvalue = 'x1') %*%
  diag(med) %>%
  rowSums() %>%
  quantile(quantiles)



m2w


# plot
m2w$race <- factor(
  m2w$race,
  levels = c("White-Asian", "Asian", "White-Black", "Black"))

m2w$model <- factor(
  m2m$model,
  levels = c('Police\nPerceptions', 'Social\nWelfare', 'Cultural\nLiberalism')
)



ggplot(m2w, aes(y = mean, x = model)) +
  geom_errorbar(width = .3, aes(ymin = lower95, ymax = upper95)) +
  geom_linerange(size = 1.2, aes(ymin = lower90, ymax = upper90)) +
  geom_point(size = 2.5) + theme_bw() + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") + ylim(-.2, .25) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  facet_wrap(~ race) +
  ylab("Difference from Monoracial Whites") + xlab("")
ggsave("multiracial_jop/m2w.pdf", width = 7, height = 2.5)


##################
## APPENDIX ##
##################

## TABLE A-1: Political Views x Race
t2a <- round(prop.table(wtd.table(x = df$cultural, y = df$race, weights = df$psweight), 2)*100, 1)
t2b <- round(prop.table(wtd.table(x = df$welfare, y = df$race, weights = df$psweight), 2)*100, 1)
t2c <- round(prop.table(wtd.table(x = df$police, y = df$race, weights = df$psweight), 2)*100, 1)
t2 <- rbind(cbind(paste('Cultural -', rownames(t2a)), t2a),
            cbind(paste('Welfare -', rownames(t2b)), t2b),
            cbind(paste('Police Mistreatment - ', rownames(t2c)), t2c))
print(xtable(
  t2, digits = 1, align = rep("r", 7),
  caption = "Political Views by Racial Background."),
  include.rownames = FALSE
)

# chi-sq tests

weights::wtd.chi.sq(df$cultural, df$race, weight = df$psweight, na.rm = TRUE)

weights::wtd.chi.sq(df$welfare, df$race, weight = df$psweight, na.rm = TRUE)

weights::wtd.chi.sq(df$police, df$race, weight = df$psweight, na.rm = TRUE)





## REGRESSION TABLES


## Pooled sample
## TABLE A-2
oLogitTable(
  modelCultural$zelig.out$z.out[[1]],
  names = c("More Liberal"),
  caption = "Cultural Preferences Among Monoracial and Biracial Whites, Blacks, and Asians. \\it{Note}: Values shown are multinomial logistic regression estimates with 95 percent confidence intervals.",
  tablenum = 2,
  filename = "multiracial_jop/cultural"
)


## TABLE A-3
oLogitTable(
  modelWelfare$zelig.out$z.out[[1]],
  names = c("More Liberal"),
  caption = "Social Welfare Preferences Among Monoracial and Biracial Whites, Blacks, and Asians. \\it{Note}: Values shown are ordered logistic regression estimates with 95 percent confidence intervals.",
  tablenum = 3,
  filename = "multiracial_jop/welfare"
)


## TABLE A-4
oLogitTable(
  modelPolice$zelig.out$z.out[[1]],
  filename = "multiracial_jop/police",
  names = c("More Negative"),
  caption = "Perceptions of Police Treatment of Minorities Among Monoracial and Biracial Whites, Blacks, and Asians. \\it{Note}: Values shown are ordered logistic regression estimates with 95 percent confidence intervals.",
  tablenum = 4
)


## By subgroup
## TABLE A-5
modelCulturalByRace <- df %>%
  nest(-race) %>%
  mutate(
    model = map(data, ~ polr(
      as.factor(cultural) ~ age + educ3 + gender + income + south +
        relLinked + relRace + relClose + salRelig,
      data = .,
      weights = psweight,
      method = "logistic",
      model = FALSE,
      Hess = TRUE
    ))
  ) %>%
  dplyr::select(race, model)


oLogitTableByRace(
  modelCulturalByRace$model,
  filename = "multiracial_jop/cultural_byrace",
  names = as.vector(modelCulturalByRace$race),
  caption = "Cultural Preferences Among Monoracial and Biracial Whites, Blacks, and Asians. \\it{Note}: Values shown are multinomial logistic regression estimates with 95 percent confidence intervals.",
  tablenum = 5
)

## TABLE A-6
modelWelfareByRace <- df %>%
  nest(-race) %>%
  mutate(
    model = map(data, ~ polr(
      as.factor(welfare) ~ age + educ3 + gender + income + south +
        relLinked + relRace + relClose + salRelig,
      data = .,
      weights = psweight,
      method = "logistic",
      model = FALSE,
      Hess = TRUE
    ))
  ) %>%
  dplyr::select(race, model)



oLogitTableByRace(
  modelWelfareByRace$model,
  filename = "multiracial_jop/welfare_byrace",
  names = as.vector(modelWelfareByRace$race),
  caption = "Social Welfare Preferences Among Monoracial and Biracial Whites, Blacks, and Asians. \\it{Note}: Values shown are ordered logistic regression estimates with 95 percent confidence intervals.",
  tablenum = 6
)

## TABLE A-7
modelPoliceByRace <- df %>%
  nest(-race) %>%
  mutate(
    model = map(data, ~ polr(
    as.factor(police) ~ age + educ3 + gender + income + south +
      relLinked + relRace + relClose + salRelig,
    data = .,
    weights = psweight,
    method = "logistic",
    model = FALSE,
    Hess = TRUE
  ))
  ) %>%
  dplyr::select(race, model)


oLogitTableByRace(
  modelPoliceByRace$model,
  filename = "multiracial_jop/police_byrace",
  names = as.vector(modelPoliceByRace$race),
  caption = "Perceptions of Police Treatment of Minorities Among Monoracial and Biracial Whites, Blacks, and Asians. \\it{Note}: Values shown are ordered logistic regression estimates with 95 percent confidence intervals.",
  tablenum = 7
)




## DESCRIPTIVES

##TABLE A-8
reporttools::tableContinuous(
  vars = continuous,
  group = group,
  stats = c("n", "min", "q1", "median", "mean", "q3", "max", "s", "iqr"),
  cap = "Demographic Characteristics by Racial Group (Continuous Variables)",
  lab = "tab:age",
  longtable = FALSE,
  file = "multiracial_jop/cont.tex"
  )


## TABLE A-9
# gender, age, education, income, region
factors <- data.frame(
  "Gender" = df$gender,
  "Education" = df$educ3,
  "Income" = str_replace_all(as.character(df$faminc), '\\$', '\\\\$'),
  "South" = ifelse(df$south == 1, 'South', 'Non-South')
)

group <- df$race
continuous <- data.frame("Age" = df$age)

reporttools::tableNominal(
  vars = factors,
  group = group,
  vertical = TRUE,
  cap = "Demographic Characteristics by Racial Group (Nominal Variables)",
  lab = "tab:nominal",
  longtable = TRUE,
  cumsum = FALSE,
  file = "multiracial_jop/nominal.tex",
  rotate.colnames = TRUE
)
