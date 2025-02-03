basic_clean_1 <- function(dataset){
  dataset %>%
      clean_names() %>%
      # Converts all SPSS labels to
      # native R factors
      mutate(across(where(is.labelled),
                    as_factor)) %>%
      # Exclude South Asians
      filter((is.na(q24_2) | q24_2 == "No") &
             (is.na(q24_3) | q24_3 == "No")) %>%
      mutate(educ3 = case_when(
               educ %in% c("No HS", "High school graduate") ~
                 "Less than or equal HS",
  
               educ == "Some college" ~
                 "Some college",
  
               educ %in% c("2-year", "4-year", "Post-grad")  ~
                 "College Grad",
               TRUE ~ NA_character_
             )) %>%
    mutate(age = 2015 - as.numeric(as.character(birthyr))) %>%
    # notice the use of dplyr::if_else to keep
    # the factor class. ifelse() would coerce
    # the result to an integer
    # should I stay close to the original paper? depends on zelig()
    mutate(income = if_else(faminc == "Prefer not to say", NA, faminc),
           income = fct_lump_min(income, 100,
                                 other_level = ">150,000")) %>%
    mutate(south = ifelse(
             inputstate %in% c("Alabama", "Arkansas", "Florida",
                               "Georgia", "Kentucky", "Louisiana",
                               "Mississippi", "North Carolina",
                               "South Carolina", "Tennessee", "Texas",
                               "Virginia", "West Virginia"),
             1, 0
           ))
}

closeness_2 <- function(dataset){

  # Closeness to racial Groups
  dataset %>%  
    mutate(race = q_11rand,
           salRelig = as.numeric(revFac(q6f))
           ) %>%
    ## RELATIVE SALIENCE OF RACE ##
    # recode s.t. higher values = more salient
    mutate(sal.race = oneto0(q6b),
           sal.class = oneto0(q6a),
           sal.age = oneto0(q6c),
           sal.gender = oneto0(q6d),
           sal.polit = oneto0(q6e),
           sal.relig = oneto0(q6f)) %>%
    # avg salience of race minus avg salience along other dimensions
    mutate(relRace = sal.race - (sal.class +
                                 sal.age +
                                 sal.gender +
                                 sal.polit +
                                 sal.relig)/5) %>%

    ## CLOSENESS TO RACIAL GROUPS ##
    # higher values = closer
    mutate(closeWhite = revFac(q12a),
           closeBlack = revFac(q12b),
           closeAsian = revFac(q12c),
           closeW = neg1to1(closeWhite),
           closeA = neg1to1(closeAsian),
           closeB = neg1to1(closeBlack)) %>%
    mutate(
      relCloseW = neg1to1(closeW - (closeA + closeB)/2),
      relCloseA = neg1to1(closeA - (closeW + closeB)/2),
      relCloseB = neg1to1(closeB - (closeW + closeA)/2)
    ) %>%
    mutate(
      relClose = NA,
      relClose = ifelse(race == "WHITE", relCloseW, relClose),
      relClose = ifelse(race == "BLACK", relCloseB, relClose),
      relClose = ifelse(race == "ASIAN", relCloseA, relClose),
      relClose = ifelse(race == "WHITE-BLACK", relCloseB, relClose),
      relClose = ifelse(race == "WHITE-ASIAN", relCloseA, relClose),
    )
}

linked_fate_3 <- function(dataset){

  ## LINKED FATE ##
  # recode s.t. higher values = more linked
    dataset %>%  
    mutate(
      lfWhite = revFac(q7a),
      lfBlack = revFac(q7b),
      lfAsian = revFac(q7c),
    ) %>%  
    mutate(
      # Coalesce linked fate variables. For minority and biracial respondents,
      # this variable measures perceptions of fate linked to minority group
      linked = coalesce(lfBlack, lfAsian, lfWhite),
      linked = neg1to1(linked)
    ) %>%  
    mutate(
      # Compute relative lf for biracials by subtracting perceived
      # linked fate to whites from linked fate to non-whites
      lfWA = rescale(as.numeric(lfAsian) - as.numeric(lfWhite), c(-1, 1)),
      lfWB = rescale(as.numeric(lfBlack) - as.numeric(lfWhite), c(-1, 1))
    ) %>%  
    mutate(
      relLinked = linked,
      relLinked = ifelse(race == "WHITE-ASIAN", lfWA, relLinked),
      relLinked = ifelse(race == "WHITE-BLACK", lfWB, relLinked)

    ) %>%  
    mutate(
     lfBlack2 = if_else(grepl("disagree", lfBlack), "Disagree", "Agree",
                        NA_character_),
     lfAsian2 = if_else(grepl("disagree", lfAsian), "Disagree", "Agree",
                        NA_character_),
     lfWhite2 = if_else(grepl("disagree", lfWhite), "Disagree", "Agree",
                        NA_character_)
    )
}

remove_iat_4 <- function(dataset){
  dataset %>%  
    mutate(
      ## IAT ##
      # remove IAT scores with a high error rate (N = 548)
      white_asian_dscore = ifelse(error_rate > .3, NA, white_asian_dscore), 
      white_black_dscore = ifelse(error_rate > .3, NA, white_black_dscore),
    
      # coalesce IAT scores.
    
      iat = coalesce(white_asian_dscore, white_black_dscore)
    
    ) 
}

party_id_5 <- function(dataset){
  dataset %>%  
    mutate(
      ## PARTY ID ##
      # Trichotomize, merge "Independent" and "Other"
      pid = case_when(
             q21 == "Democrat" ~ "Democrat",
             q21 == "Republican" ~ "Republican",
             q21 %in% c("Independent", "Other") ~ "Ind/Other",
             TRUE ~ NA_character_
             )
      #pid = recode(as.numeric(q21), "1=3; 2=1; 3=2; 4=2; else=NA"),
      #pid = factor(pid, labels = c("Republican", "Ind/Other", "Democrat"))
    ) %>%  
    mutate(
      ## PARTY SORTING ##
      dem = ifelse(
              pid7 == "Strong Democrat" |
              pid7 == "Not very strong Democrat" |
              pid7 == "Lean Democrat",
              1,
              0),
  
      # Equals 1 if self-reported Democrats & self-reported liberals.
      sorted_dems = ifelse(
                      (q22 == "Very liberal" | q22 == "Liberal") & dem == 1,
                      1,
                      0)
    )
}

ideology_6 <- function(dataset){
  dataset %>%  
    mutate(
      ## IDEOLOGY ##
      # negative values = liberal
      # positive values = conservative
      ideo = case_when(
             q22 %in% c("Very conservative", "Conservative") ~ "Conservative",
             q22 %in% c("Very liberal", "Liberal") ~ "Liberal",
             q22 == "Moderate" ~ "Moderate",
             TRUE ~ NA_character_
             )
      #ideo = recode(as.numeric(q22), "1=1; 2=1; 3=0; 4=-1; 5=-1"),
      #ideo = factor(ideo, labels = c("Liberal", "Moderate", "Conservative"))
    ) 
}

police_7 <- function(dataset){
  dataset %>%  
    mutate(
      ## POLICE TREATMENT ##
      police = case_when(
             q20 %in% c("Excellent job", "Good job") ~ "Excellent/Good",
             q20 == "Only a fair job" ~ "Fair",
             q20 == "Poor job" ~ "Poor",
             TRUE ~ NA_character_
             )
      #police = factor(q20,
      # this was wrong in the original code, that label was duplicated
      #           levels = c("Excellent/Good", "Fair", "Poor"))
    )

}

cultural_8 <- function(dataset){
  dataset %>%  
    mutate(
      ## CULTURAL PREFERENCES ##
      # Abortion
      abortion = case_when(
          q18 %in% c("Legal in ALL cases", "Legal in MOST cases") ~ "Support",
          q18 %in% c("Illegal in ALL cases", "Illegal in MOST cases") ~ "Oppose",
          TRUE ~ NA_character_
          ),
      
      #abortion = factor(recode(
      #  as.numeric(q18), "1:2=1; NA=NA; else=0"),
      #labels = c("Oppose", "Support")
      #),

    # Same-sex marriage
      marriage = case_when(
          q15 %in% c("Strongly favor", "Favor") ~ "Support",
          q15 %in% c("Strongly oppose", "Oppose") ~ "Oppose",
          TRUE ~ NA_character_
          ),
    #marriage = factor(recode(
    #  as.numeric(q15), "1:2=1; NA=NA; else=0"),
      #labels = c("Oppose", "Support")
    #  ),

      cultural = case_when(
          marriage == "Support" &   abortion == "Support" ~ "Support Both",
          marriage == "Oppose" & abortion == "Oppose" ~ "Oppose Both",
          marriage == "Support" & abortion == "Oppose" ~ "Conflicting Views",
          marriage == "Oppose" & abortion == "Support" ~ "Conflicting Views",
          TRUE ~ NA_character_
          )
    #cultural = factor(
    #  (as.numeric(abortion) + as.numeric(marriage))/2,
    #  #labels = c("Oppose Both", "Conflicting Views", "Support Both")
    #  )
    ) 
}

social_9 <- function(dataset){
  dataset %>%  
    mutate(
      ## SOCIAL WELFARE PREFERENCES ##
      # Spending
      #spending = factor(q13, labels = c("Fewer", "More")),
      spending = case_when(
          grepl("more services", q13) ~ "More",
          grepl("fewer services", q13) ~ "Fewer",
          TRUE ~ NA_character_
          ),
      
      # ACA
      obamacare = q17,
      
      welfare = case_when(
          spending == "More" & obamacare == "Approve" ~ "Support Both",
          spending == "Oppose" & obamacare == "Oppose" ~ "Oppose Both",
          spending == "Support" & obamacare == "Oppose" ~ "Conflicting Views",
          spending == "Oppose" & obamacare == "Support" ~ "Conflicting Views",
          TRUE ~ NA_character_
          )
    )
}

make_table_1 <- function(dataset){

  t1a <- round(prop.table(wtd.table(x = dataset$pid,
                                    y = dataset$race,
                                    weights = dataset$psweight), 2)*100, 1)
  
  # Ideology
  t1b <- round(prop.table(wtd.table(x = dataset$ideo,
                                    y = dataset$race,
                                    weights = dataset$psweight), 2)*100, 1)
  
  # Sorted Democrats
  t1c <- dataset %>%
    filter(dem == 1) %>%
    wtd.table(
      x = .$sorted_dems,
      y = .$race,
      weights = .$psweight) %>%
    prop.table(2)


  t1d <- round(prop.table(wtd.table(x = dataset$lfWhite2,
                                    y = dataset$race,
                                    weights = dataset$psweight), 2)*100, 1)

  t1e <- round(prop.table(wtd.table(x = dataset$lfBlack2,
                                    y = dataset$race,
                                    weights = dataset$psweight), 2)*100, 1)

  t1f <- round(prop.table(wtd.table(x = dataset$lfAsian2,
                                    y = dataset$race,
                                    weights = dataset$psweight), 2)*100, 1)

  t1 <- rbind(
    cbind(rownames(t1a), t1a),
    cbind(rownames(t1b), t1b),
    c("Sorted Democrats", round(t1c[2,] * 100, 1)),
    c("Linked Fate to Whites", t1d[1,]),
    c("Linked Fate to Blacks", t1e[1,]),
    c("Linked Fate to Asians", t1f[1,])
  )

  t1

#  xtable(
#    t1, digits = 1, align = rep("r", 7),
#    caption = "Partisanship and Ideology by Racial Background")

}


revFac <- function(x){
  factor(x, levels=rev(levels(x)))
}

neg1to1 <- function(x){
  scales::rescale(as.numeric(x), c(-1, 1))
}

onetoneg1 <- function(x){
  scales::rescale(as.numeric(x), c(1, -1))
}

zeroto1 <- function(x){
  scales::rescale(as.numeric(x), c(0, 1))
}

oneto0 <- function(x){
  scales::rescale(as.numeric(x), c(1, 0))
}


coalesce <- function(...) {
  ans <- ..1
  for (elt in list(...)[-1]) {
    i <- is.na(ans)
    ans[i] <- elt[i]
  }
  ans
}
