# locations.r
An R script to find the approximate center of a list of locations

by John Bettonville

2016-10-09

This script takes a comma-delimited list of locations and uses k-means clustering to find
the approximate geographic center of the locations.

The input is a .csv file containing the following column headers:
* "Name" - A unique identifier for each location
* "Lat" - The latitude of each location in decimal format
* "Lng" - The longitude of each location in decimal format

The script uses pamk() to determine the optimum value of k for k-means clustering. Once
the value of k is found, kmeans() is called multiple times (10,000 by default) and the
mean value of the latitude and longitude of each set of k centroids is calculated. The
unique means of centroids are plotted on a density map centered on the average
latitude and longitude of the collection of means of centroids.

The final part of the script allows a user to calculate the distance from another set of
locations to each of the locations in the input file. It creates plots ranking each of
the new locations based on their average distance, minimum distance, and maximum distance
from locations found in the input file, as well as the difference between the minimum and
maximum distances. From these calculations, it also creates a plot showing which of the
new locations has the lowest overall rankings in these categories.

For the purposes of testing the script, please use any of the .csv files included in this
repository.
