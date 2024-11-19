

# Interpretations -- YAY!!
#collect beta vector
beta <- coef(m11)
beta

# model assumptions!!!! add them to the presentation 
# random forest and glm
# more concrete about Y variable
# linear regression, assuming the effect of income....

25 - 86.24869 # = -61.24869
# (1) std_poverty_level -- B1 * -61.24869 (25 level increase in poverty level )
# B1 = 1.503e-03

exp(1.503e-03*-61.24869) # = 0.9120534
# Holding all other characteristics constant, the odds of a family pantry goer
# repeat visiting in a month decrease by 9% for every 25 level increase in poverty level.

# ---------- probability ------------- 
# mid pov compared to low pov
110 - 86.24869 # = 23.75131
mid_pov <- visit_count_rf_data[visit_count_rf_data$std_poverty_level >= 24 & visit_count_rf_data$std_poverty_level <= 26 & visit_count_rf_data$fam_size >= 4, ]
poor <- visit_count_rf_data[visit_count_rf_data$std_poverty_level >= -61 & visit_count_rf_data$fam_size >= 4, ]

fam_mid = mid_pov[18, ] # 110 pov level
fam_poor = poor[94, ] # 25 pov level

predict(m11, fam_mid, type = "response") # = 0.01577187 
predict(m11, fam_poor, type = "response") # = 0.01797098

# if we have a family who sits within the 25 poverty level, and another family who sits in 
# the 110 poverty level range, we would expect 1.5% probability of repeat visiting in a given month for a family 
# in the middle of the poverty scale, and a 1.8% if they are on the lower end of the poverty scale. 

# thus, the family who sit on the lower end of the poverty scale are more likely to repeat 
# visit in a given month than those within the mid range

# high pov compared to mid pov
300 - 86.24869 # = 213.7513
150 - 86.24869 # = 63.75131

rich <- visit_count_rf_data[visit_count_rf_data$std_poverty_level >= 214 & visit_count_rf_data$fam_size >= 4, ]
mid_pov2 <- visit_count_rf_data[visit_count_rf_data$std_poverty_level >= 63 & visit_count_rf_data$std_poverty_level <= 65 & visit_count_rf_data$fam_size >= 4, ]

fam_mid2 = mid_pov2[23, ] # 150 pov level
fam_rich = rich[22, ] # 300 pov level

predict(m11, fam_mid2, type = "response") # = 0.01462837
predict(m11, fam_rich, type = "response") # = 0.0184271

# fam in 150 level and 300 level, we'd expect 1.5% probability of repeat visiting in a month for a fam in 
# the mid (150) range, and a 1.8% if they are on the higher end of the poverty scale.


# (2) Family Size 
# B2 = -5.880e-02, so the larger a family, their odds of repeat visiting decrease

# Holding all other characteristics constant, the odds of a family repeat visiting decrease by 
# a factor of exp(-5.880e-02) = 0.9428953 for every 1 person increase in family size. 

# So, as the family size increases by one person, their odds of repeat visiting decrease by 9.4%
# That is, if we compare a 4 person family to a 3 person family, the odds of the 4 person family repeat visiting MORE THAT ONCE in a month
# are 9.4% less than the odds of a 3-person family. 

# Family of 8
exp(-5.880e-02*8) # = 0.6247523
0.6247523 - 1 # = -0.3752477
# If there is family of 8, the odds of them repeat visiting in a given month decrease by 37.5%. 

exp(-5.880e-02*2) # = 0.8890516
0.8890516-1 # = -0.1109484
# If there is family of 2, the odds of them repeat visiting in a given month decrease by 11%.

# Thus, a smaller family's odds of repeat visiting are 26% higher than a larger family, which implies
# more affluent families are taking advantage of the Food Bank Rule change.

# This confirms our hypothesis, that those who are more affluent are taking advantage of the Food Bank 
# Law change. 
# bigger families would require more food and mouths to feed, but those with smaller families are the ones taking 
# advantage of going multiple times.

# --- family size probability ---
big_fam <- visit_count_rf_data[visit_count_rf_data$fam_size >= 6 & visit_count_rf_data$fam_size <= 8, ]
small_fam <- visit_count_rf_data[visit_count_rf_data$fam_size == 3, ]

big_fam1 <- big_fam[64, ]
small_fam1 <- small_fam[21, ]

predict(m11, big_fam1, type = "response") # = 0.01188904
predict(m11, small_fam1, type = "response") # = 0.01589771

# fam of 6 and fam of 3
# 1.2% probability of repeat visiting in a month for a family of 6, and 1.6% for family of 3
big_fam2 <- visit_count_rf_data[visit_count_rf_data$fam_size >= 5 & visit_count_rf_data$fam_size <= 7, ]
small_fam2 <- visit_count_rf_data[visit_count_rf_data$fam_size == 2, ]
# so, smaller families have 0.4% higher probability of repeat visiting than larger families

big_fam3 <- big_fam2[22,] # fam of 5
small_fam3 <- small_fam2[50, ] # fam of 2

predict(m11, big_fam3, type = "response") # = 0.01741049
predict(m11, small_fam3, type = "response") # = 0.01842929


# ----------- poverty level and snap status probabilities ---------------------
mean(visit_count_rf_data$poverty_level) # = 86.24869

# family in the 400 poverty level, who DOES NOT receive snap 
400 - 86.24869 = 313.7513 # this is the standardized federal poverty level

high_pov_level <- visit_count_rf_data[visit_count_rf_data$std_poverty_level >= 313 & visit_count_rf_data$fam_size >= 4, ]

family_no_snap = high_pov_level[18, ]

# family with similar status, who DOES RECIEVE snap ("YES")
test = family_no_snap
test$snap_household_cat <- "Y"

predict(m11, family_no_snap, type = "response") # = 0.02624456
predict(m11, test, type = "response") # = 0.02022228

# If we have 2 white family in the 400 poverty range, one family does receive snap and the other who doesn't
# We would expect a 2.6% probability of repeat visiting in a given month for a family who doesn't receive snap and
# 2% if they did receive snap.

# -----------
# 2 fams in the 10 or 20 poverty level, and compare snap status
30 - 86.24869 #= -56.24869
low_pov_level <- visit_count_rf_data[visit_count_rf_data$std_poverty_level < -56.24869 & visit_count_rf_data$fam_size ==4, ]
View(low_pov_level)

# fam in low pov level, who DOES NOT RECIEVE snap ("NO")
low_fam_no = low_pov_level[26, ]

# fam in low pov level, who DOES RECIEVE snap ("YES")
test2 = low_fam_no
test2$snap_household_cat <- "Y"

predict(m11, low_fam_no, type = "response") # = 0.01321025
predict(m11, test2, type = "response") # = 0.01617682 

# fam with no snap in low pov range (16) and similar fam with snap, 1.3% probability of repeat visiting in a month
# for fam with no SNAP, and a 1.6% probability for a fam that does receive snap 


# -----------
# fam of 3 in 0-30 range and fam of 6 in 0-20 range compared to mid poverty range

small_fam <- visit_count_rf_data[visit_count_rf_data$poverty_level < 15 & visit_count_rf_data$fam_size == 3, ]
View(small_fam)

# poverty level of 12
smallz = small_fam[38, ]
predict(m11, smallz, type = 'response') # = 0.02394816

big_fam <- visit_count_rf_data[visit_count_rf_data$poverty_level < 15 & visit_count_rf_data$fam_size == 6, ]
View(big_fam)

# poverty level of 2
bigz = big_fam[36, ]
predict(m11, bigz, type = 'response') # = 0.01343509

mid_fam <- visit_count_rf_data[visit_count_rf_data$poverty_level >= 100 & visit_count_rf_data$poverty_level <= 110 & 
                                 visit_count_rf_data$fam_size == 4, ]
View(mid_fam)

# poverty level of 107
midz = mid_fam[23, ]
predict(m11, midz, type = 'response') # = 0.01784896



