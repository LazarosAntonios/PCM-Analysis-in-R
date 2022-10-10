#########################################################################################
################################ Project in Statistics 1 ################################
######################################################################################### 

#Install the packages we need
library(tidyverse)
library(GGally)
library(dplyr)
library(factoextra)
library(rworldmap)
library(countrycode)


# First Step: Import databse downloaded form the World Bank Database
dat = read.csv("WBD 1.csv", header = TRUE)
View(dat)
# We want to see how many rows our dataset has
nrow(dat)

# Given the fact that we are using the whole world as datasample we cannot hace more than 217 rows as there are 217 countries in WB database.
## For example we can see below that on row 218, all columns included, the country is "Arab World"
dat.218 <- dat[218, ]
dat.218
## A second example is the row 250, all columns included, where it corresponds to the "North America" region
dat.250 <- dat[250, ]
dat.250

# Since we have regions apart from countries we have to eliminate rows 218 until 269 #

## First we create a subset of the last 52 rows and sum them to see what are the regions that are not single countries that have to be deleted.
dat.last52 <- dat[218:269, ]
str(dat.last52)
### Now we can remove the separated dataframe from the original dataset and obtain our desired dataframe to work with
dat.remove.last52 <- dat[-c(218:269), ]

# Rename the new dataframe. This is now the dataset we will work with after clearing the missing values
WBD <- dat.remove.last52[1:217, ]

# Since our mising values are denoted as ".." we have to rename them as "NA" in order to eliminate them.
WBD[WBD==".."] = NA

# We test to see if we have any missing values, now that we have changed the notation properly.
is.na(WBD)

# It is useful to see how many NA's we have in our dataframe. 
sum(is.na(WBD[]))

# We can observe that all of the rows contain at least on missing "NA" value. Thus the option complete.cases is not useful as we will have 0 observations after 
# the cleaning process.
# With colSums we can see how many missing variables we have on each column and decide how to eliminate them according to the weight of our variables.
colSums(is.na(WBD))

####################################################################################################
################################ Missing Values Elimination Process ################################
####################################################################################################

#First we see how many rows we have now so to compare how much our dataset will shrink
nrow(WBD)

# We create a dataframe with all the rows and transfer across columns that have less than 100 missing values
WBD.clean1 <- WBD[ ,colSums(is.na(WBD))<100]
# Then we strip out the NA's and summarize the final sample we will work with
WBD.clean.final <- na.omit(WBD.clean1)
# Our dataframe slightly drops in observations but this is completely rational given the fact that the development indexes we chose are not available for the African Continent.
nrow(WBD.clean.final)

# We test to see if there are still any missing data in our dataframe. It turns out a logical var (TRUE or FALSE) to indicate the presence of "NA's".
any(is.na(WBD.clean.final[]))

str(WBD.clean.final)

####################################################################################################
####################################################################################################


# We rename our variables to plot easier and provie more clear information
WBD.final <-WBD.clean.final %>% rename("Year" = Time, "Country" = Country.Name , "Female Employers (% of female employment)" = Employers..female....of.female.employment...modeled.ILO.estimate...SL.EMP.MPYR.FE.ZS. ,  "Male Employers (% of male employment)" = Employers..male....of.male.employment...modeled.ILO.estimate...SL.EMP.MPYR.MA.ZS. , "Employment in industry, male (% of male employment)" = Employment.in.industry..male....of.male.employment...modeled.ILO.estimate...SL.IND.EMPL.MA.ZS. , "Employment in industry, female (% of female employment)" = Employment.in.industry..female....of.female.employment...modeled.ILO.estimate...SL.IND.EMPL.FE.ZS. , "GDP (current US$)" = GDP..current.US....NY.GDP.MKTP.CD. , "GDP per capita (current US$)" = GDP.per.capita..current.US....NY.GDP.PCAP.CD. , "GDP per capita growth (annual %)" = GDP.per.capita.growth..annual.....NY.GDP.PCAP.KD.ZG. , "Population ages 65 and above (total)" = Population.ages.65.and.above..total..SP.POP.65UP.TO. , "Population ages 65 and above (% of total population)" = Population.ages.65.and.above....of.total.population...SP.POP.65UP.TO.ZS. , "Population ages 65 and above, female (% of female population)" = Population.ages.65.and.above..female....of.female.population...SP.POP.65UP.FE.ZS. , "Population ages 65 and above, male (% of male population)" = Population.ages.65.and.above..male....of.male.population...SP.POP.65UP.MA.ZS. , "Population growth (annual %)" = Population.growth..annual.....SP.POP.GROW. , "Start-up procedures to register a business (number)" = Start.up.procedures.to.register.a.business..number...IC.REG.PROC. , "Urban population growth (annual %)" = Urban.population.growth..annual.....SP.URB.GROW. , "Urban population (% of total population)" = Urban.population....of.total.population...SP.URB.TOTL.IN.ZS.) 

# Basic Descriptive statistics
dim(WBD.final)
str(WBD.final)

# A more detailed summary option 
glimpse(WBD.final)

# Our variables are depicted as factors, thus we convert them all into numericals.
WBD.final$Year <- as.numeric(levels(WBD.final$Year))[WBD.final$Year]
WBD.final$`Male Employers (% of male employment)` <- as.numeric(levels(WBD.final$`Male Employers (% of male employment)`))[WBD.final$`Male Employers (% of male employment)`]
WBD.final$`Employment in industry, male (% of male employment)` <- as.numeric(levels(WBD.final$`Employment in industry, male (% of male employment)`))[WBD.final$`Employment in industry, male (% of male employment)`]
WBD.final$`Employment in industry, female (% of female employment)` <- as.numeric(levels(WBD.final$`Employment in industry, female (% of female employment)`))[WBD.final$`Employment in industry, female (% of female employment)`]
WBD.final$Country <- as.character(WBD.final$Country)
WBD.final$`Female Employers (% of female employment)` <- as.numeric(levels(WBD.final$`Female Employers (% of female employment)`))[WBD.final$`Female Employers (% of female employment)`]
WBD.final$`GDP (current US$)` <- as.numeric(levels(WBD.final$`GDP (current US$)`))[WBD.final$`GDP (current US$)`]
WBD.final$`GDP per capita (current US$)` <- as.numeric(levels(WBD.final$`GDP per capita (current US$)`))[WBD.final$`GDP per capita (current US$)`]
WBD.final$`GDP per capita growth (annual %)` <- as.numeric(levels(WBD.final$`GDP per capita growth (annual %)`))[WBD.final$`GDP per capita growth (annual %)`]
WBD.final$`Population ages 65 and above (% of total population)` <- as.numeric(levels(WBD.final$`Population ages 65 and above (% of total population)`))[WBD.final$`Population ages 65 and above (% of total population)`]
WBD.final$`Population ages 65 and above, female (% of female population)` <- as.numeric(levels(WBD.final$`Population ages 65 and above, female (% of female population)`))[WBD.final$`Population ages 65 and above, female (% of female population)`]
WBD.final$`Population ages 65 and above, male (% of male population)` <- as.numeric(levels(WBD.final$`Population ages 65 and above, male (% of male population)`))[WBD.final$`Population ages 65 and above, male (% of male population)`]
WBD.final$`Population ages 65 and above (total)` <- as.numeric(levels(WBD.final$`Population ages 65 and above (total)`))[WBD.final$`Population ages 65 and above (total)`]
WBD.final$`Population growth (annual %)` <- as.numeric(levels(WBD.final$`Population growth (annual %)`))[WBD.final$`Population growth (annual %)`]
WBD.final$`Start-up procedures to register a business (number)` <- as.numeric(levels(WBD.final$`Start-up procedures to register a business (number)`))[WBD.final$`Start-up procedures to register a business (number)`]
WBD.final$`Urban population growth (annual %)` <- as.numeric(levels(WBD.final$`Urban population growth (annual %)`))[WBD.final$`Urban population growth (annual %)`]
WBD.final$`Urban population (% of total population)` <- as.numeric(levels(WBD.final$`Urban population (% of total population)`))[WBD.final$`Urban population (% of total population)`]

str(WBD.final)

# Construct logs gor large numbers such as GDP and Pop over 65
WBD.finalm = WBD.final
WBD.finalm[,10] = log(WBD.final[,10]) #GDP {log}
WBD.finalm[,11] = log(WBD.final[,11]) #GDP {log}
WBD.finalm[,13] = log(WBD.final[,13]) #Pop over 65 {log}


####################################################################################################
###################################### Plots, Pies and Graphs ######################################
####################################################################################################

#Basic Histogram

plot(WBD.final$`Female Employers (% of female employment)`, WBD.final$`GDP per capita (current US$)`, xlab = "Female Empl", ylab = "GDPPC", frame.plot = FALSE, pch = 19, col = "blue"  )
plot(WBD.finalm$`GDP (current US$)`, WBD.finalm$`Start-up procedures to register a business (number)`, xlab = "Female Employers", ylab = "Start-up procedures to register a business (number)", frame.plot = FALSE, pch = 19, col = "blue"  )

# Basic Boxplot
boxplot(WBD.finalm[,6:20], las=2, col=rainbow(20))

# We plot GDPPC with urban population as a % of the total population of a country to see the correlation between the 2 variables
ggplot(WBD.finalm, aes(x=`GDP per capita (current US$)`, y= `Urban population (% of total population)`)) + geom_point() + labs(title="Countries", x="GDP per capita (current US$)", y="Urban population (% of total population)") + theme(legend.position="bottom")

# Plot with GDPPC and Urban Population:
ggplot(WBD.finalm, aes(x = `GDP per capita (current US$)`, y = `Urban population (% of total population)`, color = Region, size=`GDP (current US$)`/1e5)) + geom_point() + 
  scale_color_brewer(palette="Dark2")+ xlab("GDP per capita (current US$)") + ylab("Urban population (% of total population)")+theme_minimal()+ theme(legend.position="bottom")

# Other Plot with Female Employers (% of female employment) and Start-up procedures to register a business (number):
ggplot(WBD.finalm, aes(x = `Female Employers (% of female employment)`, y = `Start-up procedures to register a business (number)`, color = Region, size=`GDP (current US$)`/1e5)) + geom_point() + 
  scale_color_brewer(palette="BrBG")+ xlab("GDP per capita (current US$)") + ylab("Urban population (% of total population)")+theme_minimal()+ theme(legend.position="bottom")

# GDPPC and Urban Pop plots:
ggplot(WBD.finalm, aes(`GDP per capita (current US$)`)) + geom_density(aes(group=Region, colour=Region, fill=Region), alpha=0.1) +xlab("GDP per capita (current US$)")
ggplot(WBD.finalm, aes(`Urban population (% of total population)`)) + geom_density(aes(group=Region, colour=Region, fill=Region), alpha=0.1) +xlab("Urban population (% of total population)")

# Pop>65 plot:
ggplot(WBD.finalm, aes(`Population ages 65 and above (total)`)) + geom_density(aes(group=Region, colour=Region, fill=Region), alpha=0.1) +xlab("Population ages 65 and above (total)")

# Male Empl plot:
ggplot(WBD.finalm, aes(`Male Employers (% of male employment)`)) + geom_density(aes(group=Region, colour=Region, fill=Region), alpha=0.1) +xlab("Male Employers (% of male employment)") 


# Other relationship: Pop over 65 and GDPPC
ggplot(WBD.finalm, aes(x = `Population ages 65 and above (total)`, y = `GDP per capita (current US$)`, color=Region, label=Country)) + geom_text(size=2, check_overlap = TRUE) +
  theme_bw() + theme(legend.position="bottom")

# Other relationship: Pop over 65 and GDP
ggplot(WBD.finalm, aes(x = `Population ages 65 and above (total)`, y = `GDP (current US$)`, color=Region, label=Country)) + geom_text(size=2, check_overlap = TRUE) +
  theme_bw() + theme(legend.position="bottom")


# Other relationship: Male and Female Employers
ggplot(WBD.finalm, aes(x = `Male Employers (% of male employment)`, y = `Female Employers (% of female employment)`, color=Region, label=Country)) + geom_text(size=2, check_overlap = TRUE) +
  theme_bw() + theme(legend.position="bottom")

# Plot separated according the regions
ggplot(WBD.finalm, aes(x = `GDP per capita (current US$)`, y = `Urban population (% of total population)`, color=`Population growth (annual %)`,size=`GDP (current US$)`)) +
  geom_point() + facet_wrap(~ Region) + scale_color_gradient(low="red", high="blue")+labs(title="Countries by region", x="GDP per Capita", y="Urban Population", colour="Annual Pop Growth (%)")+theme_minimal()+ theme(legend.position="bottom")

# Relations in dimension 2: bivariate analysis using scatter plots; multiple scatter plot: partial and all relations in dimension 2
pairs(WBD.finalm[,6:13],pch=19,col=WBD.finalm$Region)

pairs(WBD.finalm[,13:20],pch=19,col=WBD.finalm$Region)

pairs(WBD.finalm[,6:20],pch=19,col=WBD.finalm$Region)

# Correlation Graph
ggcorr(WBD.finalm, label = T, palette = "BrBG")+labs(title = "Correlations")

####################################################################################################
####################################################################################################


####################################################################################################
################################### Principal Component Analysis ###################################
####################################################################################################

# Remove possible outliers (not mandatory, but useful in practice):
Outliers = subset(WBD.finalm, `GDP per capita (current US$)` < 10 & `GDP per capita growth (annual %)` < 0) 
nrow(Outliers)
Outliers[c("Country","GDP per capita (current US$)","GDP per capita growth (annual %)")]


idx = which(WBD.finalm$`GDP per capita (current US$)` >= 10 | WBD.finalm$`GDP per capita growth (annual %)` >= 0)           
X = WBD.finalm[idx,6:20]
dim(X)
Region.Filtered = WBD.finalm$Region[idx]
Country.Filtered = WBD.finalm$Country[idx]

# Remove missing values for PCA
id.complete = complete.cases(X)


pca = prcomp(X[id.complete,])
pca
pca$sdev
summary(pca)
screeplot(pca,main="Screeplot",col="blue",type="barplot",pch=19)

fviz_screeplot(pca, addlabels = TRUE)

fviz_pca_biplot(pca, repel = TRUE)

# Plot the first two pca scores:
qplot(pca$x[,1],pca$x[,2],col=Region.Filtered[id.complete],label=Country.Filtered[id.complete],size=I(.1)) + 
  labs(title="First two principal components", x="PC1", y="PC2",color="Region") +
  theme_bw() + theme(legend.position="bottom") + geom_text(size=2, hjust=.4, vjust=-.4)

biplot(pca)

# Map our PCA index in a map:
map = data.frame(country=Country.Filtered[id.complete], value=pca$x[,1])
#Convert the country code into iso3c using the function countrycode()
map$country = countrycode(map$country, 'country.name', 'iso3c')
#Create data object supporting the map
matched <- joinCountryData2Map(map, joinCode = "ISO3",
                               nameJoinColumn = "country")
#Draw the map
mapCountryData(matched,nameColumnToPlot="value",missingCountryCol = "gray94",
               addLegend = T, borderCol = "gray0",
               catMethod = "pretty", colourPalette = "topo",
               mapTitle = c("PCA1 by Country"), lwd=1)

# zoom in Europe
mapCountryData(matched, nameColumnToPlot="value",missingCountryCol = "white",
               addLegend = F, borderCol = "gray99",
               mapRegion = "Europe", mapTitle = c("PCA1 by Country"), lwd=1, 
               catMethod = "quantile", colourPalette = "topo")

# zoom in Asia
mapCountryData(matched, nameColumnToPlot="value",missingCountryCol = "white",
               addLegend = F, borderCol = "gray0",
               mapRegion = "Asia", mapTitle = c("PCA1 by Country"), lwd=1, 
               catMethod = "quantile", colourPalette = "topo")

####################################################################################################
####################################################################################################

####################################################################################################
######################################### Factor Analysis ##########################################
####################################################################################################

library(factoextra)
library(quantmod)
install.packages('psych')
install.packages('GPArotation')
Dat <- WBD.finalm[6:20]
x.f <- factanal(Dat, factors = 3, rotation="none", scores="regression")
x.f
cbind(x.f$loadings, x.f$uniquenesses)

# var explained by first three factors is around 98%. 
# The model explains very well the comovements between different maturities.

par(mfrow=c(3,1))
barplot(x.f$loadings[,1], names=F, las=2, col="darkblue", ylim = c(-1, 1))
barplot(x.f$loadings[,2], names=F, las=2, col="darkblue", ylim = c(-1, 1))
barplot(x.f$loadings[,3], las=2, col="darkblue", ylim = c(-1, 1))

# Now estimate the model with only two factors, rotation varimax (sparser representation), and Barlett estimatin for scores (WLS)

x.f <- factanal(Dat, factors = 2, rotation="varimax", scores="Bartlett", lower = 0.001)
x.f
cbind(x.f$loadings, x.f$uniquenesses)

# var explained by the two factors is 97%. 
# The model explains very well the comovements between different maturities, except the 30yr one
# That means the different products "live" in dimension 2

par(mfrow=c(2,1))
barplot(x.f$loadings[,1], names=F, las=2, col="darkblue", ylim = c(-1, 1))
barplot(x.f$loadings[,2], las=2, col="darkblue", ylim = c(-1, 1))

# the factors can be interpreted as two different levels: one with more weights to short-term maturities, 
# and the other one with more weights to long-term maturities

factor.df = data.frame(date=date, x.f$scores) %>% gather("factor", "score", -date)

factor.df %>%
  ggplot(aes(x=date,y=score)) + geom_line(size=1) + 
  theme_bw()  + scale_color_brewer(palette="Dark2") +
  facet_wrap(~factor, ncol=1) +
  labs(title="2-factor model", x="", y="scores", col="") 


####################################################################################################
####################################################################################################

































### Clustering: kmeans

X = scale(nba)  # or  X = nba (no scale)

# kmeans with 5 clusters
fit = kmeans(X, centers=5, nstart=100)
groups = fit$cluster
groups
barplot(table(groups), col="blue")

centers=fit$centers
centers

# clusplot
fviz_cluster(fit, data = X, geom = c("point"),ellipse.type = 'norm', pointsize=1)+
  theme_minimal()+geom_text(label=names,hjust=0, vjust=0,size=2,check_overlap = T)+scale_fill_brewer(palette="Paired")

# Silhouette plot
# The silhouette value in [-1,1] measures the similarity (cohesion) of a data point to its cluster relative to other clusters (separation). 
# Silhouette plots rely on a distance metric and suggest that the data matches its own cluster well.
# The larger the silhouette widths, the better.
d <- dist(X, method="euclidean")  
sil = silhouette(groups, d)
plot(sil, col=1:5, main="", border=NA)
summary(sil)

# kmeans with 2 clusters
fit = kmeans(X, centers=2, nstart=100)
groups = fit$cluster
groups
barplot(table(groups), col="blue")

centers=fit$centers
centers

# clusplot
fviz_cluster(fit, data = X, geom = c("point"),ellipse.type = 'norm', pointsize=1)+
  theme_minimal()+geom_text(label=names,hjust=0, vjust=0,size=2,check_overlap = T)+scale_fill_brewer(palette="Paired")

# Silhouette plot
d <- dist(X, method="euclidean")  
sil = silhouette(groups,d)
plot(sil, col=1:2, main="", border=NA)
summary(sil)

# in terms of Silhouette plot, it seems 2 clusters is better than 5.

# Hence, how many clusters?

fviz_nbclust(X, kmeans, method = 'wss')

fviz_nbclust(X, kmeans, method = 'silhouette')

# difficult to see, but maybe 2 or 3 is ok

# any insight about the clusters by looking at the names?


### Clustering: interpretation


i=1  # plottinng the centers in cluster 1
barplot(centers[i,], las=2, col="darkblue", main=paste("Cluster", i))
# insights?

i=2  # plottinng the centers in cluster 2
barplot(centers[i,], las=2, col="darkblue", main=paste("Cluster", i))
# insights?

# Clusters by profiling variables
as.data.frame(X) %>% mutate(cluster=factor(groups), names=names, min=min, gp=gp) %>%
  ggplot(aes(x = cluster, y = min)) + 
  geom_boxplot(fill="lightblue") +
  labs(title = "Minutes played by cluster", x = "", y = "", col = "") 
# seems players in one cluster play more minutes per game

as.data.frame(X) %>% mutate(cluster=factor(groups), names=names, min=min, gp=gp) %>%
  ggplot(aes(x = cluster, y = gp)) + 
  geom_boxplot(fill="lightblue") +
  labs(title = "Games played by cluster", x = "", y = "", col = "") 
# seems players in one cluster have more experience

# maybe first cluster contains best overall players with more experience

as.data.frame(X) %>% mutate(cluster=factor(groups), names=names, min=min, gp=gp, pc1=-pca$x[,1]) %>%
  ggplot(aes(x = cluster, y = pc1)) + 
  geom_boxplot(fill="lightblue") +
  labs(title = "PC1-scores by cluster", x = "", y = "", col = "") 
# seems players in first cluster are clearly better in terms of overall performance (PC1)

# k-means with Mahalanobis distance

S_x <- cov(nba)
iS <- solve(S_x)
e <- eigen(iS)
V <- e$vectors
B <- V %*% diag(sqrt(e$values)) %*% t(V)
Xtil <- scale(nba,scale = FALSE)
nbaS <- Xtil %*% B

# kmeans with 2 clusters
fit = kmeans(nbaS, centers=2, nstart=100)
groups = fit$cluster
groups
barplot(table(groups), col="blue")

centers=fit$centers
colnames(centers)=colnames(X)
centers

# clusplot
fviz_cluster(fit, data = X, geom = c("point"),ellipse.type = 'norm', pointsize=1)+
  theme_minimal()+geom_text(label=names,hjust=0, vjust=0,size=2,check_overlap = T)+scale_fill_brewer(palette="Paired")

i=1  # plottinng the centers in cluster 1
barplot(centers[i,], las=2, col="darkblue", main=paste("Cluster", i))
# insights?

i=2  # plottinng the centers in cluster 2
barplot(centers[i,], las=2, col="darkblue", main=paste("Cluster", i))
# insights?


### Clustering: hierarchical clustering

# The input of hierarchical clustering is a distance matrix:
d <- dist(X, method = "euclidean") # distance matrix using euclidean distance
# we can choose any of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski" 

# With the input, we select the linkage for the cluster joining (simple, complete, average, Ward)
fit.h <- hclust(d, method="ward.D2") # cluster using Ward linkage (based on minimizing variance)

# Then, we can visualize the dendogram
plot(fit.h) 

# And we can also cut the tree in our desired number of groups
rect.hclust(fit.h, k=4, border="red")

# Nicer but slower
# fviz_dend(fit.h, k=4, rect = TRUE) # dendrogam

# Instead of visualizing, we can create the groups by choosing our convenient number of groups 
groups.h <- cutree(fit.h, k=2) 


# Clusters by profiling variables
as.data.frame(X) %>% mutate(cluster=factor(groups.h), names=names, min=min, gp=gp) %>%
  ggplot(aes(x = cluster, y = min)) + 
  geom_boxplot(fill="lightblue") +
  labs(title = "Minutes played by cluster", x = "", y = "", col = "") 

as.data.frame(X) %>% mutate(cluster=factor(groups.h), names=names, min=min, gp=gp) %>%
  ggplot(aes(x = cluster, y = gp)) + 
  geom_boxplot(fill="lightblue") +
  labs(title = "Games played by cluster", x = "", y = "", col = "") 