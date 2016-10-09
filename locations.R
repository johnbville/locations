# locations.r
#
# by John Bettonville
# 2016-10-08
#
# This script uses k-means clustering to find an area centrally located among
# various points in a geographic area. Using pamk() to find an optimal value for k
# given the input, it runs kmeans() multiple times to find unique centroid sets,
# then saves the mean of each set.
# 
# It then creates a density plot of the means to give an approximate area near the
# center of all input points, which can be used as a starting point to find the 
# optimal center of all input points.
# 
# To create density plot, stop at line 85.
# To find top ranking, continue and follow instructions.
#
# Input: .csv file containing the following column headers:
# 
# Name - Identifier of location
# Lat - Latitude of location
# Lng - Longitude of location

library(grid)
library(gridExtra)
library(ggmap)
library(ggplot2)
library(mapproj)
library(fpc)
library(cluster)
library(wesanderson) # Wes Anderson color palletes just for fun

# Change working directory to wherever your data is
setwd("~/_WORKING_DIRECTORY_")
# Read your input file
mydata <- read.csv("_INPUT_FILE_.csv", header = TRUE)

# Get the optimal number of clusters using pam for kmeans
numclusters <- pamk(mydata[,2:3], krange=2:min((nrow(mydata)-1), 10))$nc

universe <- as.data.frame(matrix(nrow = 0, ncol = 2)) # going to be a dataframe of coordinates
colnames(universe) <- c("Lat", "Lng")
running <- universe # create a running total from universe of centroids
keptseeds <- vector() # useful seed numbers for the purpose of running kmeans

for (q in 1:10000) { # default is 10,000 iterations, adjust if desired
  # pull from Lat & Lng & run kmeans
  clusterLoc <- kmeans(mydata[,2:3], numclusters)
  
  # Store coordinates of cluster centers in a data frame
  mycenters <- data.frame(clusterLoc$centers)
  mycentmean <- as.data.frame(t(c(mean(mycenters$Lat), mean(mycenters$Lng))))
  colnames(mycentmean) <- c("Lat", "Lng")
  # Add the current mean of centroids to the running list of means of centroids
  running <- rbind(running, mycentmean)
  # If current mean of centroids is a duplicate, remove it
  if (identical(running, unique(running))) keptseeds <- c(keptseeds, q) else running <- unique(running)
  # Save the current center of mean of centroids in a data frame of everything found in this loop
  universe[q,] <- mycentmean
}
# Take unique lat/lng pairs of means of clusters
running <- na.omit(unique(rbind(running, universe)))

# Create a data frame containing the center of the cloud of means of centroids
mymeanmean <- as.data.frame(t(c(mean(running$Lat), mean(running$Lng))))
colnames(mymeanmean) <- c("Lat", "Lng")
# Create a vector for creating a map
mymean <- c(mymeanmean[,2], mymeanmean[,1])

# Zoom for map is set automatically, adjust manually as needed
zoomlevel <- calc_zoom(extendrange(mydata$Lng), extendrange(mydata$Lat))
mymap <- get_map(location = mymean, zoom = zoomlevel, maptype = "roadmap")

# Create a density plot to show the range of cities to examine
overlay <- stat_density2d(data = running, aes(x = Lng, y = Lat, fill = ..level..,
      alpha = ..level..),
  bins = 16, geom = "polygon")

ggmap(mymap) + overlay + scale_fill_gradient(low="#000000", high="#005596", guide=FALSE) +
  scale_alpha(range = c(0,0.2), guide=FALSE) +
  geom_point(data = running, aes(x = Lng, y = Lat, color='Means of centroids'), size = 1) +
  geom_point(data = mydata, aes(x = Lng, y = Lat, color='Locations'), size = 3) + 
  geom_point(data = mymeanmean, aes(x = Lng, y = Lat, color='Center of Market'), size = 3) + 
  scale_color_manual(guide = guide_legend(title = NULL), values = wes_palette("Darjeeling")) +
  theme(legend.position = "bottom")

# STOP after creating density plot, or continue with user adjustments to find ranking of
# distances from locations in input files to other locations.
# When entering city names, use city and state so that gmapsdistance can properly geocode.

# Create a new data frame from existing dataset, concatenate lat & lng into single field
distmatrix <- cbind.data.frame(mydata$Name, paste(mydata$Lat, mydata$Lng, sep="+"))

# Use some domain knowledge to add in the names of the cities around the region we want

numcity <- _INTEGER_ ### Change this based on the number of cities you want to examine!!

# Initialize the columns for as many cities as you plan to use
distmatrix[,3:(numcity+2)] <- NA

### Include city and state of all locations you want to try here!!
# The number of arguments must be numcity+2
colnames(distmatrix) <- c("Name", "Coord", "CITY, STATE", "CITY, STATE", ...)
numrecords <- nrow(distmatrix)
###

# This loop meansures the distance from each store to each city using gmapsdistance
# NOTE: Occasionally this throws errors when dealing with large datasets.
# If the script stops before completion, adjust starting value of b in line 111
# to account for which city the script stopped at.
for (b in 1:numcity) { 
  for (a in 1:numrecords) {
    distmatrix[a,b+2] <- NISTmeterTOmile(
      gmapsdistance(distmatrix[a,2], gsub(" ", "+", colnames(distmatrix)[b+2]), mode = "driving")$Distance)
  }
}

# Get the sums of distances to each city
for (a in 3:(numcity+2)) distmatrix[(numrecords+1),a] <- sum(distmatrix[1:numrecords,a])

newlocs <- data.frame()
newlocs[1:numcity,1] <- colnames(distmatrix)[3:(numcity+2)]

# Geocode each city and calculate distance statistics
ab <- geocode(newlocs[1:numcity,1], output = "latlon")
newlocs <- cbind(newlocs, ab)
newlocs[,4] <- newlocs[,2]
newlocs$lon <- NULL
newlocs[,4] <- t(distmatrix[(numrecords+1),3:(numcity+2)])
for (a in 1:numcity) newlocs[a,5] <- mean(distmatrix[1:numrecords,a+2]) # Average distance
for (a in 1:numcity) newlocs[a,6] <- max(distmatrix[1:numrecords,a+2]) # Max distance
for (a in 1:numcity) newlocs[a,7] <- min(distmatrix[1:numrecords,a+2]) # Min distance
for (a in 1:numcity) newlocs[a,8] <- newlocs[a,6] - newlocs[a,7] # Volatility between max & min
colnames(newlocs) <- c("Name", "Lat", "Lng", "TotDist", "AvgDist", "MaxDist", "MinDist", "Diff")

# Create data frames that order cities by avg, max, min dist, and volatility
avgdistdf <- newlocs[order(newlocs$AvgDist),]
mindistdf <- newlocs[order(newlocs$MinDist),]
maxdistdf <- newlocs[order(newlocs$MaxDist),]
diffdf <- newlocs[order(newlocs$Diff),]

# Assign rankings, lowest number is optimal
avgdistdf$rownumber <- 1:nrow(avgdistdf)
mindistdf$rownumber <- 1:nrow(mindistdf)
maxdistdf$rownumber <- 1:nrow(maxdistdf)
diffdf$rownumber <- 1:nrow(diffdf)

# Order for the purposes of plotting
adf <- avgdistdf[order(as.numeric(row.names(avgdistdf))),]
midf <- mindistdf[order(as.numeric(row.names(mindistdf))),]
madf <- maxdistdf[order(as.numeric(row.names(maxdistdf))),]
ddf <- diffdf[order(as.numeric(row.names(diffdf))),]

# Another data frame to calculate overall rankings
thebest <- data.frame()
thebest <- adf
thebest$Score <- adf$rownumber + midf$rownumber + madf$rownumber + ddf$rownumber

# Use ggplot2 to create plots showing how each city ranks.
# Your miles may vary with respect to colors

p1 <- ggplot(data = avgdistdf, aes(reorder(Name, AvgDist), -AvgDist)) +
  geom_bar(fill="#005596", color="#005596", aes(reorder(Name, -AvgDist), AvgDist), stat = "identity") +
  scale_y_continuous("Average Distance to Store (mi)") +
  scale_x_discrete("") +
  scale_fill_manual("", values = c('#00483A', '#FB5B1F')) +
  coord_flip() +
  ggtitle("")

p2 <- ggplot(data = mindistdf, aes(reorder(Name, MinDist), -MinDist)) +
  geom_bar(fill="#005596", color="#005596", aes(reorder(Name, -MinDist), MinDist), stat = "identity") +
  scale_y_continuous("Shortest Distance to Theatre") +
  scale_x_discrete("") +
  scale_fill_manual("", values = c('#00483A', '#FB5B1F')) +
  coord_flip() +
  ggtitle("")

p3 <- ggplot(data = maxdistdf, aes(reorder(Name, MaxDist), -MaxDist)) +
  geom_bar(fill="#005596", color="#005596", aes(reorder(Name, -MaxDist), MaxDist), stat = "identity") +
  scale_y_continuous("Farthest Distance to Theatre") +
  scale_x_discrete("") +
  scale_fill_manual("", values = c('#00483A', '#FB5B1F')) +
  coord_flip() +
  ggtitle("")

p4 <- ggplot(data = diffdf, aes(reorder(Name, Diff), -Diff)) +
  geom_bar(fill="#005596", color="#005596", aes(reorder(Name, -Diff), Diff), stat = "identity") +
  scale_y_continuous("Difference Between Max & Min Distances") +
  scale_x_discrete("") +
  scale_fill_manual("", values = c('#00483A', '#FB5B1F')) +
  coord_flip() +
  ggtitle("")

# Plot a grid of each statistic
grid.arrange(p1, p2, p3, p4)

# Create and plot overall ranking
p5 <- ggplot(data = thebest, aes(reorder(Name, Score), -Score, fill=NULL)) +
  geom_bar(fill="#005596", color="#005596", aes(reorder(Name, -Score), Score), stat = "identity") +
  scale_y_continuous("Sum of Rankings") +
  scale_x_discrete("") +
  scale_fill_manual("", values = c('#00483A', '#FB5B1F')) +
  coord_flip() +
  ggtitle("")

p5 # Location at top of grid is optimal


# Start looking for places to live!
