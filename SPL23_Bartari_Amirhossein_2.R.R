################################################################################ 
#################### Statistical Programming Languages 2023 ####################
###################             Take-home Exam              ####################
################################################################################

#-------------------------------------------------------------------------------
# Surname: Bartari
# Name: Amirhossein
# Student ID (Matrikelnummer): 629940
#-------------------------------------------------------------------------------




# change WD
#setwd("Z:/Humboldt/SPL/Code")

fastfood <- read.csv("fastfood.csv")

# ------------------------ part a ------------------------
# Keep columns we want
keep <- c("restaurant", "item", "calories", "trans_fat", "cholesterol", "sodium", "total_carb", "fiber", "sugar", "protein")
fastfood <- fastfood[keep]

# Rename columns
colnames(fastfood)[colnames(fastfood) == 'item'] <- "product"
colnames(fastfood)[colnames(fastfood) == 'total_carb'] <- "carbohydrates"

# Change restaurant column to factor
fastfood$restaurant <- as.factor(fastfood$restaurant)

# Class of other variables
sapply(fastfood, class)

# ------------------------ part b ------------------------
# Remove duplicates
fastfood <- fastfood[!duplicated(fastfood), ]

# ------------------------ part c ------------------------
# Identify NAs in the data frame
is.na(fastfood$protein)
apply(is.na(fastfood), 2, which)

#------------------------ part d ------------------------
# Calculate the maximum calories grouped by restaurant
maxcalories <- aggregate(fastfood$calories, by = list(fastfood$restaurant), max)
maxcalories <- maxcalories[order(maxcalories$x, decreasing = FALSE), ]
print(maxcalories)

#------------------------ part e ------------------------
# Number of observations
nrow(fastfood[fastfood$trans_fat >= 2 | fastfood$cholesterol >= 300, ])

healthy_Res <- fastfood[fastfood$trans_fat < 2 & fastfood$cholesterol < 300, ]
unique(healthy_Res$restaurant)

# Convert 'protein' and 'calories' columns to numeric
fastfood$protein <- as.numeric(fastfood$protein)
fastfood$calories <- as.numeric(fastfood$calories)

fastfood$prot_cal_ratio <- fastfood$protein / fastfood$calories

fastfood$prot_cal_indicator <- factor(
  cut(fastfood$prot_cal_ratio, breaks = c(-Inf, 0.05, 0.1, 0.15, Inf),
      labels = c("very bad", "bad", "good", "very good"),
      na.value = NA)
)

#---------------------------------------
very_good_prods <- subset(fastfood, prot_cal_indicator == "very good")
very_good_products <- very_good_prods$product

#-----------------------------------------------
# Create a table of the two variables
tbl <- table(fastfood$prot_cal_indicator, fastfood$restaurant)

# Create a mosaic plot for the table using base R
mosaicplot(tbl, color = TRUE,
           main = "Prot_Cal_Indicator and Restaurant Mosaic Plot",
           xlab = "Prot_Cal_Indicator", ylab = "Restaurant")

#-----------------------------------------------------------
chi_sq <- chisq.test(tbl)
chi_sq
