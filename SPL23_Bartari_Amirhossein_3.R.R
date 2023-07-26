################################################################################ 
#################### Statistical Programming Languages 2023 ####################
###################             Take-home Exam              ####################
################################################################################

#-------------------------------------------------------------------------------
# Surname: Bartari
# Name: Amirhossein
# Student ID (Matrikelnummer): 629940
#-------------------------------------------------------------------------------

### Exercise 3 -----------------------------------------------------------------



# Load the trees dataset
data(trees)

pdf("Tree_graphics.pdf", width = 7, height = 7)




# Load the trees dataset
data(trees)

# Set up the layout for three plots in a single frame

layout(matrix(c(1, 1, 0, 2, 2, 3, 2, 2, 3), nrow = 3, ncol = 3, byrow = TRUE))


# Plot 1: Histogram of Girth
par(pin = c(3, 0.9))
# Create a histogram with height on the x-axis, relative density on the y-axis, 13 bars, and filled bars with "burlywood3" color
hist(trees$Height, breaks = 13, freq = FALSE, xlab = "Height (in ft)", ylab = "Density", main = " ", col = "burlywood3")

# Add kernel density estimate line
lines(density(trees$Height), lty = "dotdash", col = "royalblue")




# Plot 3: Scatter plot of Girth vs. Height
par(pin = c(3, 2.8))
lm_model <- lm(Girth ~ Height, data = trees)

# Plot the regression model with triangles as data points in forestgreen color
plot(trees$Height, trees$Girth, xlab = "Height (in ft)", ylab = "Diameter (in inches)", main = " ", pch = 2, col = "forestgreen")
abline(lm_model, col = "royalblue")



# Plot 2: Box plot of Girth
par(pin = c(1.3, 2.8))
boxplot(trees$Girth, main = " ", ylab = "Diameter (in inches)", col = "burlywood3")

dev.off()


# Reset the layout to default (1 plot per frame)
par(mfrow = c(1, 1))
