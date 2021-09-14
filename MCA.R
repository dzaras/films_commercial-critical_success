# Dimitrios Zaras 
# 5/29/2019
# MCA - Multiple Correspondence Analysis
setwd("C:/Users/dima/OneDrive - Emory University/CASPER Fall 17")
# install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR);library(factoextra)
moviedata= read.csv("C17_MCA_big_dataset_2.csv", header=TRUE, sep=",")#, na.strings = "NA")
moviedata <- na.omit(moviedata)
str(moviedata)
table(moviedata$pg_13)
colnames(moviedata)<-c("Major",
                       "Box Office Top 100 Films",
                       "Opening Weekend Theaters Top 100 Films",
                       "Summer",
                       "Fall",
                       "Winter", 
                       "Movie Star Presence", 
                       "PG-13",
                       "Middle Critics Average Rating 3.4-6.6",
                       "Middle Critics Average Rating 6.7-10.0",
                       "Top Critics Average Rating 3.4-6.6",
                       "Top Critics Average Rating 6.7-10.0",
                       "Audience Average Rating 1.6-3.3",
                       "Audience Average Rating 3.4-5.0",
                       "Number of Audience Reviews - 33rd pctl",
                       "Number of Audience Reviews - 66th pctl",
                       "Number of Audience Reviews - 99th pctl", 
                       "Number of Middle Critics Reviews - 33rd pctl", 
                       "Number of Middle Critics Reviews - 66th pctl",
                       "Number of Middle Critics Reviews - 99th pctl",
                       "Number of Top Critics Reviews - 33rd pctl", 
                       "Number of Top Critics Reviews - 66th pctl", 
                       "Number of Top Critics Reviews - 99th pctl", 
                       "Hybrid",
                       "Comedy",
                       "Drama",
                       "Documentary",
                       "Animation",
                       "Kids & Family",
                       "Action",
                       "Mystery",
                       "Sci-Fi", 
                       "romance", "musical", "horror",
                       "western",	"arthouse_intl", "special_interest",	
                       "faith_spirituality",	"sports_fitness",
                       "genres_num"
                       
                      
)
## data=moviedata
moviedata2 <- moviedata[c(-1)]
attach(moviedata2)


moviedata.active <- moviedata2[, 1:25]

attach(moviedata.active)
summary("PG-13")

# Summary of the 4 first variables
summary(moviedata.active)[, 1:4]

moviedata.active[sapply(moviedata.active, is.numeric)] <- lapply(moviedata.active[sapply(moviedata.active, is.numeric)], as.factor)
str(moviedata.active)
# It's also possible to plot the frequency of variable categories. 
# The R code below, plots the first 4 columns:
  for (i in 1:26) {
    plot(moviedata.active[,i], main=colnames(moviedata.active)[i],
         ylab = "Count", col="steelblue", las = 2)
  }


res.mca <- MCA(moviedata.active, graph = FALSE)
print(res.mca)

eig.val <- get_eigenvalue(res.mca)
head(eig.val)

# visualize the percentages of inertia explained by each MCA dimensions
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

# The function get_mca_var() [in factoextra] is used to extract the results for variable categories.
# This function returns a list containing the coordinates, the cos2 and the contribution of variable categories:
var <- get_mca_var(res.mca)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

# To visualize the correlation between variables and MCA principal dimensions, type this:
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# coordinates of each variable categories in each dimension (1, 2 and 3):
head(round(var$coord, 2), 4)

# Use the function fviz_mca_var() [in factoextra] to visualize only variable categories:
fviz_mca_var(res.mca, 
               repel = TRUE, # Avoid text overlapping (slow)
               ggtheme = theme_minimal())

# The quality of the representation is called the squared cosine (cos2),
# which measures the degree of association between variable categories and a particular axis. 
# The cos2 of variable categories can be extracted as follow:
head(var$cos2, 4)

# Color by cos2 values: quality on the factor map
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())
# You can visualize the cos2 of row categories on all the dimensions using the corrplot package:
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Cos2 of variable categories on Dim.1 and Dim.2
fviz_cos2(res.mca, choice = "var", axes = 1:2)

# The contribution of the variable categories (in %) to the definition of the dimensions can be extracted as follow:
head(round(var$contrib,2), 4)

# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)

# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)

#The most important (or, contributing) variable categories can be highlighted on the scatter plot as follow:
fviz_mca_var(res.mca, col.var = "contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
               repel = TRUE, # avoid text overlapping (slow)
               ggtheme = theme_minimal()
  )

# Graph of individuals
# This function returns a list containing the coordinates,
# the cos2 and the contributions of individuals:
ind <- get_mca_ind(res.mca)
ind
# The result for individuals gives the same information as described for variable categories.

# Color individuals by groups

# The R code below colors the individuals by groups using the levels of the variable bo_top100
# The argument habillage is used to specify the factor variable for coloring the individuals by groups.
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "top100_theaters_opn_wkd", # color by groups 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

# If you want to color individuals using multiple categorical variables at the same time, 
# use the function fviz_ellipses() [in factoextra] as follow:
fviz_ellipses(res.mca, c("Box Office Top 100 Films", "Top Critics Average Rating 6.7-10.0", "Middle Critics Average Rating 6.7-10.0", "Audience Average Rating 3.4-5.0"),
              geom = "point")

fviz_ellipses(res.mca, c("Box Office Top 100 Films", "Number of Top Critics Reviews - 99th pctl", "Number of Middle Critics Reviews - 99th pctl", "Number of Audience Reviews - 99th pctl"),
              geom = "point")

# Alternatively, you can specify categorical variable indices:
fviz_ellipses(res.mca, 18:23, geom = "point")

## Dimension Description

# The function dimdesc() [in FactoMineR] can be used to identify the most correlated variables
# with a given dimension:
res.desc <- dimdesc(res.mca, axes = c(1,2))
# Description of dimension 1
res.desc[[1]]
# Description of dimension 2
res.desc[[2]]

# To specify supplementary individuals and variables, 
# the function MCA() can be used as follow :
res.mca <- MCA(moviedata, ind.sup = 53:55, 
               quanti.sup = 1:2, quali.sup = 3:4,  graph=FALSE)

# The predicted results for supplementary individuals/variables can be extracted as follow:
# Supplementary qualitative variable categories
res.mca$quali.sup
# Supplementary quantitative variables
res.mca$quanti
# Supplementary individuals
res.mca$ind.sup

# To make a biplot of individuals and variable categories (including supplementary 
# individuals and variables), type this:
  
# Biplot of individuals and variable categories
fviz_mca_biplot(res.mca, repel = TRUE,
                  ggtheme = theme_minimal())

# If you want to highlight the correlation between variables (active & supplementary)
# and dimensions, use the function fviz_mca_var() with the argument choice = "mca.cor":
fviz_mca_var(res.mca, choice = "mca.cor",
             repel = TRUE)

# For supplementary quantitative variables, type this:
fviz_mca_var(res.mca, choice = "quanti.sup",
             ggtheme = theme_minimal())





























