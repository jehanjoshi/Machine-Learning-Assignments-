# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
ScaledWine <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(ScaledWine)

# Exercise 2:
#   * How many clusters does this method suggest?
# we can say number of clusters that can be chosen can be 6 -7 as after that the WSS doesn't have massive improvement 
#   * Why does this method work? What's the intuition behind it?
 # this shows us how homogenous the clusters are a good clustering algorithm should have 
 # less WSS that indicates more homogenity 


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# this method suggests 3 as the best clusters 
# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(ScaledWine, 3, nstart=25)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster,wine$Type)
#yes if I view the resutls of the clustering I can see that the high degree 
#of homogenity that is each cluster has one type of wine so the wines are clustered based on their type
# except cluster 2 has 6 observations from different wine types so overall performance is good 

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(pam(ScaledWine,3), main = "Clustering")
#you can see distinctive clusters so the algorithm is good 

table.km <- table(wine$Type, fit.km$cluster)
randIndex(table.km)
#shows the value ranging from 0 to 1 where value closer to 1 signifies that the partitions are good 
# signifies agreement between the wine variety types and clustering solution