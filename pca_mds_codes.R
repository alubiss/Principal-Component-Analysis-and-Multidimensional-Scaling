# # DATA
# In the data set, each observation is characterized by 4 quantitative variables describing the features of the apartments sold:
#   **floor_area_sqm**, 
# **resale_price**, 
# **age**, 
# **room_numb**, 
# **town**.


knitr::opts_chunk$set(fig.align = 'center', out.width = '80%', echo = TRUE)

library(foreign)
library(dplyr)
library(GGally)
library(factoextra)
library(ggbiplot)
library(knitr)
library(kableExtra)

setwd("/Users/alubis/Desktop/OneDrive/DS")
dane=read.csv("flats.csv", sep=",")
dane$age= 2020-dane$construction_date
dane = dane %>% filter(flat_type== c("1 ROOM", "2 ROOM","3 ROOM", "4 ROOM", "5 ROOM"))
dane$room_numb = as.integer(substring(dane$flat_type, 1, 1))

kable(dane[1:10,], align = "cc", caption = "Table 1.1 The first 10 rows of the database.") %>%   kable_styling() %>% scroll_box(width = "100%", height = "500px")

# PCA

# My first analysis is Principal Components Analysis, which aims to reduce the number of variables by finding components that are a linear combination of these variables.Indication of variables that have a large impact on the appearance of individual principal components allows for the indication of a group representative.
# Other components are mutually uncorrelated and they have to maximize the variability that was not explained by the previous component.The parameters which possess the highest degree of variation will be the most optimal predictor of house prices.

dane_PCA = dane %>% filter(town== c("CENTRAL AREA", "TOA PAYOH", "PASIR RIS", "BEDOK"))
dane_PCA = dane_PCA %>% select(floor_area_sqm, resale_price, age, room_numb, town)
ggpairs(data = dane_PCA, columns = 1:4)


# If the correlation did not occur, it would not be possible to determine the principal components because each component would be as good as the other.
# 
# The data contains mixed units, so we need to mean-center the data and divide it by standard deviation and then apply a correlation matrix.


normalize <- function(x)
{
  return((x - mean(x))/sd(x))
}
dane_PCA[,1:4] <- dane_PCA[,1:4] %>% apply(2, normalize) %>% as.data.frame()

# correlation matrix
cor <- cor(dane_PCA[,1:4])
eigenvalues <- eigen(cor)

rownames(eigenvalues$vectors) <- c("floor_area_sqm", "resale_price", "age", "room_numb")
colnames(eigenvalues$vectors) <- c("PC1", "PC2", "PC3", "PC4")
eigenvalues2=as.data.frame(eigenvalues$vectors)


# Matrix of eigenvectors:

kable(eigenvalues2, align = "cc", caption = "Table 1.2 The matrix of eigenvectors.")

pc1.var <- 100*round(eigenvalues$values[1]/sum(eigenvalues$values), digits = 3)
pc2.var <- 100*round(eigenvalues$values[2]/sum(eigenvalues$values), digits = 3)
pc3.var <- 100*round(eigenvalues$values[3]/sum(eigenvalues$values), digits = 3)
pc4.var <- 100*round(eigenvalues$values[4]/sum(eigenvalues$values), digits = 3)

pc <- data.frame(PC = c("PC1", "PC2", "PC3", "PC4"), Percentage = c(pc1.var, pc2.var, pc3.var, pc4.var))
pc= as.data.frame(pc)


# Main component variance:

kable(pc, align = "cc", caption = "Table 1.3 The variance of main component.")

# We can see that the major component describes 76% of the variance.
# The first two components describe 93% of the variance.

PCA <- prcomp(dane_PCA[,1:4], center = TRUE, scale. = TRUE)
pca_summary <- summary(PCA)$importance %>% as.data.frame()
kable(pca_summary, align = "cc", caption = "Table 1.3 Summary.")


ggpairs(data = as.data.frame(PCA$x) , columns = 1:4)


# Components are orthogonal vectors.
# The analysis will reduce the set of correlated variables to components that are not correlated with each other with the least possible loss of information. 

screeplot(PCA, type = "line")


# The scree plot can also be used to select the main components.
# The factors are placed on the horizontal axis, and their eigenvalues are placed on the vertical axis.
# We are looking for the break points at which the curve's refraction angle changes.

ggbiplot(PCA, obs.scale=1, var.scale=1, varname.size=5)


# Arrows with the names of the variables show in which directions and how much they affect the individual main components.
# 
# Observations belonging to different species are marked accordingly in the graph below:

pca.species <- dane_PCA[,5]
ggbiplot(PCA, obs.scale=1, var.scale=1, ellipse=TRUE, groups = pca.species)+theme(legend.position="bottom")


# Individual observations form distinct groups depending on the city.
# 
# The analysis made it possible to reduce the number of variables to two without a significant loss of the information contained.Principal component analysis enables the description of consumer behavior in the market. It allows you to determine the position of the product in relation to competitive products.

# Multidimensional scaling – MDS

# In a situation where we would like to find cities where apartments have similar features, we can use the mds method.In the analysis, we take into account the largest apartments in a given city.
# The analysis could be repeated for the average values for a given city.


dane_MDS = dane %>% arrange(desc(town), desc(floor_area_sqm))
dane_MDS = dane_MDS %>% select(floor_area_sqm, resale_price, age, room_numb, town)
dane_MDS= dane_MDS[!duplicated(dane_MDS$town), ]
row.names(dane_MDS) <- dane_MDS$town
dane_MDS=dane_MDS[,1:4]

dane_MDS$floor_area_sqm <- scale(sqrt(dane_MDS$floor_area_sqm))
dane_MDS$resale_price <- scale(sqrt(dane_MDS$resale_price))
dane_MDS$age <- scale(sqrt(dane_MDS$age))
dane_MDS$room_numb <- scale(sqrt(dane_MDS$room_numb))

odl <- dist(dane_MDS[,1:4], method = "manhattan")
mds <- as.matrix((odl))
mds=as.data.frame(mds)

kable(mds, align = "cc", caption = "Table 2.1 Manhattan distance between variables.")

# Configuration of points representing cities:

fit<-cmdscale(mds, eig=TRUE, k=2)
x <- 0 - fit$points[, 1]
y <- 0 -  fit$points[, 2]

plot(x, y, xlab = 'Wymiar 1', ylab='Wymiar 2' )
text(x, y, pos = 4, labels = rownames(dane_MDS))


# The cities where the apartments differ the most are Central Area and Kallang. 
# There are apartments with similar characteristics in other cities.

# # Conclusions and summary
# 
# In this study, PCA was used to find the component that is the main factor influencing the purchase of an apartment and explains the highest percentage of variance. First component explains 75% of the variance. It consists of variables with the following weights: **-0.5123898 x floor_area_sqm + -0.5074730 x resale_price + 0.4474903 x age + -0.5288480 x room_numb' **, therefore the least explained variance among the variables is the age of the apartment. This allows us to reduce the number of variables and determine the factors that guide us in choosing an apartment. Knowing what kind of apartments we are interested in, we can check in which city we can look for an apartment with similar parameters. MDS analysis showed that apartments in the Central Area and Kallang are less similar to the others. The analysis allowed to determine where to look for apartments most similar to each other.
# Both analyses allow to create a picture of the residential market.
# 
# ------------------------------------------------------------------------------------
#   **Sources:**
#   1. *Statystyczna Analiza Danych z wykorzystaniem programu R, Marek Walesiak, 2012, PWN*
#   2. *PRINCIPAL COMPONENTS ANALYSIS (PCA), Steven Holand, Department of Geology, University of Georgia, Athens, 2019*
#   3. *Metric Multidimensional Scaling (MDS):Analyzing Distance Matrices, Hervé Abdi, Encyclopedia of Measurement and Statistics, 2007*
#   