####################
## DATA WRANGLING ##
####################

# coalesce variables
coalesce <- function(...) {
  ans <- ..1
  for (elt in list(...)[-1]) {
    i <- is.na(ans)
    ans[i] <- elt[i]
  }
  ans
}

# reverse the levels of a factor
revFac <- function(x){factor(x, levels=rev(levels(x)))}

# rescale numeric vars
neg1to1 <- function(x){scales::rescale(as.numeric(x), c(-1, 1))}
onetoneg1 <- function(x){scales::rescale(as.numeric(x), c(1, -1))}
zeroto1 <- function(x){scales::rescale(as.numeric(x), c(0, 1))}
oneto0 <- function(x){scales::rescale(as.numeric(x), c(1, 0))}

##########################
## PREDICTED PROB PLOTS ##
##########################

set_ppparams <- function(model, group){
  setx(model,
     race = group,
     age = mean(df$age, na.rm = T),
     gender = "Female",
     income = mean(df$income, na.rm = T),
     educ3 = "Some College",
     south = 0,
     relLinked = mean(df$linked, na.rm = T),
     relClose = mean(df$relClose, na.rm = T),
     relRace = mean(df$relRace, na.rm = T),
     salRelig = mean(df$salRelig, na.rm = T))
}

############
## TABLES ##
############

oLogitTable <- function(models, tablenum, caption, names, filename, ...){
  texreg(models, file = paste0(filename, ".tex"),
         custom.coef.names = c(
           "Age", "Some College", "College Grad", "Female",
           "Income", "South", "White-Black", "White-Asian",
           "Black", "Asian", "Linked Fate", "Salience of Race",
           "Racial Closeness",  "Salience of Religion"),
         groups = list("Demographics" = 1:6,
                       "Racial Group" = 7:10,
                       "Identity and Affect" = 11:14),
         custom.model.names = names,
         caption = caption,
         label = paste0("tab:", tablenum),
         booktabs = TRUE,
         dcolumn = TRUE,
         # bold = 0.05,
         stars = 0,
         ci.force = TRUE,
         ci.test = 0,
         ci.force.level = 0.95,
         use.packages = FALSE,
         single.row = TRUE,
         fontsize = "scriptsize"
  )
}

oLogitTableByRace <- function(models, tablenum, caption, names, filename, ...){
  texreg(models, file = paste0(filename, ".tex"),
         custom.coef.names = c(
           "Age", "Some College", "College Grad", "Female",
           "Income", "South", "Linked Fate", "Salience of Race",
           "Racial Closeness",  "Salience of Religion"),
         groups = list("Demographics" = 1:6,
                       "Identity and Affect" = 7:10),
         custom.model.names = names,
         caption = caption,
         label = paste0("tab:", tablenum),
         booktabs = TRUE,
         dcolumn = TRUE,
         # bold = 0.05,
         stars = 0,
         ci.force = TRUE,
         ci.test = 0,
         ci.force.level = 0.95,
         use.packages = FALSE,
         single.row = TRUE,
         fontsize = "scriptsize",
         sideways = TRUE
  )
}
