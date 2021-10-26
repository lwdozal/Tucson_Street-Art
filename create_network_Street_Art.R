#############################
# Laura W. Dozal            #
# ASSI&T Conference Poster  #
# 10/12/2021                #
# Tucson Street Art         #
#############################

rm(list = ls())

# load libraries
library("here")
library("readxl")
library("dplyr")
library("tidyr")
library("tidyverse")
library("ggplot")
library("network")
library("statnet")


#read in data
sa <- read_xlsx(here("murals_tucson.xlsm"))


# create matrix of dataframe
mtx_sa <- sa %>% select(-c('artist website', 'comment', "Image"))
mtx_sa <- data.matrix(mtx_sa)

mycoord <- sa %>% select(c("Latitude", "Longitude")) %>% as.matrix("Latitude", "Longitude")


# create binary matrix with area with rows and style as columns
# contingency table?
artist_style <-sa[, c("Style", "Artist")] %>% drop_na()
artist_style_mtx <- as.data.frame.matrix(table(artist_style))
bin_artist_s <- as.matrix((artist_style_mtx >0) + 0) # keep 0s 0s, and everything else 1
sa_net_artist_s <- network::network(bin_artist_s) # turn binary matrix into network
style <- dimnames(artist_style_mtx)[[1]]
artist <- dimnames(artist_style_mtx)[[2]][1:10]
my.colors <-rep("blue")
gplot(sa_net_artist_s, gmode="twomode", 
      label=c(style, artist),
      vertex.cex=2,
      vertex.col = my.colors,
      usearrows=FALSE,
      label.col = "black",
      main = "Street Art Style Network \n Artist = square, Style = circle",
)
summary <- summary(sa_net_artist_s)
summary

## Density
# density = 0.04309764

d.cent <- degree(sa_net_artist_s) 
d.cent
table(d.cent)
names(d.cent) <- sa_net_artist_s %v% "vertex.names"
sort.d <- sort(d.cent)
# Draw the graph with node sizes proportional to betweenness centralitiy scores:
gplot(sa_net_artist_s, vertex.col = my.colors, 
      usearrows=TRUE, vertex.cex = .05 * d.cent, label=names(d.cent[1:20])) + #label=names(d.cent),
title(main = "Degree Centrality:Artist and Style Network")

## Closeness Centrality
c.cent <- closeness(sa_net_artist_s, gmode = "digraph")
c.cent
names(c.cent) <- sa_net_artist_s %v% "vertex.names"
# sort(c.cent)
sort.c <- sort(c.cent)
top.c <- names(sort(c.cent))#[155:163]
# note there are all 0s for closeness centrality
# Draw the graph with node sizes proportional to closeness centralitiy scores:
gplot(sa_net_artist_s,  vertex.col = my.colors, usearrows=TRUE, vertex.cex = 5 * c.cent, label=names(c.cent[1:20])) +
  title(main = "Closeness Centrality:Artist and Style Network")

## Betweeness Centrality
b.cent <- betweenness(sa_net_artist_s, gmode = "digraph")
table(b.cent)
names(b.cent) <- sa_net_artist_s %v% "vertex.names"
sort.b <- sort(b.cent)
# kable(sort.b[153:163])
# top.b <- names(sort(b.cent))[155:163]
# Draw the graph with node sizes proportional to closeness centralitiy scores:
gplot(sa_net_artist_s, vertex.col = my.colors, usearrows=TRUE, vertex.cex = .005 * b.cent, label=names(b.cent[1:20])) +
  title(main = "Betweenness Centrality:Artist and Style Network")


#### Artist and Area ####
artist_area <- sa[, c("Area", "Artist")] %>% drop_na()
artist_area_mtx <- as.data.frame.matrix(table(artist_area))
bin_artist_area <- as.matrix((artist_area_mtx >0) + 0) # keep 0s 0s, and everything else 1
sa_net_artist_area <- network::network(bin_artist_area) # turn binary matrix into network
area <- dimnames(artist_area_mtx)[[1]]
artist <- dimnames(artist_area_mtx)[[2]][1:15]
my.colors <- rep("green")
gplot(sa_net_artist_area, gmode="twomode", 
      label = c(area, artist),
      vertex.cex=2,
      vertex.col = my.colors,
      usearrows=FALSE,
      label.col = "black",
      main = "Street Art Style Network \n Artist = square, Area = circle",
)
summary <- summary(sa_net_artist_area)
summary

## Degree Centrality
#  density = 0.04025974

d.cent <- degree(sa_net_artist_area) 
d.cent
table(d.cent)
names(d.cent) <- sa_net_artist_area %v% "vertex.names"
sort.d <- sort(d.cent)
# Draw the graph with node sizes proportional to betweenness centralitiy scores:
gplot(sa_net_area_style, vertex.col = my.colors, 
      usearrows=TRUE, vertex.cex = .05 * d.cent, label=names(d.cent[1:15])) + #label=names(d.cent),
title(main = "Degree Centrality:Artist and Area")

## Closeness Centrality
c.cent <- closeness(sa_net_artist_area, gmode = "digraph")
c.cent
names(c.cent) <- sa_net_artist_area %v% "vertex.names"
# sort(c.cent)
sort.c <- sort(c.cent)
top.c <- names(sort(c.cent))#[155:163]
# note there are all 0s for closeness centrality
# Draw the graph with node sizes proportional to closeness centralitiy scores:
gplot(sa_net_artist_area,  vertex.col = my.colors, usearrows=TRUE, vertex.cex = 5 * c.cent, label=names(c.cent[1:20])) +
  title(main = "Closeness Centrality:Artist and Area Network")

## Betweeness Centrality
b.cent <- betweenness(sa_net_artist_area, gmode = "digraph")
table(b.cent)
names(b.cent) <- sa_net_artist_area %v% "vertex.names"
sort.b <- sort(b.cent)
# kable(sort.b[153:163])
# top.b <- names(sort(b.cent))[155:163]
# Draw the graph with node sizes proportional to closeness centralitiy scores:
gplot(sa_net_artist_area, vertex.col = my.colors, usearrows=TRUE, vertex.cex = .005 * b.cent, label=names(b.cent[1:20])) +
  title(main = "Betweenness Centrality:Artist and Area Network")


#### Area and Syle ####
area_style <-sa[, c("Area", "Style")] %>% drop_na()
area_style_mtx <- as.data.frame.matrix(table(area_style))
bin_area_style <- as.matrix((area_style_mtx > 0) + 0) # keep 0s 0s, and everything else 1
sa_net_area_style <- network::network(bin_area_style) # turn binary matrix into network
area <- dimnames(area_style_mtx)[[1]]
style <- dimnames(area_style_mtx)[[2]]
my.colors <- rep("orange")
gplot(sa_net_area_style, gmode="twomode", 
      label=c(area, style),
      vertex.cex=2,
      vertex.col = my.colors,
      usearrows=FALSE,
      label.col = "black",
      main = "Street Art Style Network \n Area = circle, Style = square",
)
#centrality measures
require(sna)

summary <- summary(sa_net_area_style)
summary

## Degree Centrality
# density = 0.4363636

d.cent <- degree(sa_net_area_style) 
d.cent
table(d.cent)
names(d.cent) <- sa_net_area_style %v% "vertex.names"
sort.d <- sort(d.cent)
# Draw the graph with node sizes proportional to betweenness centralitiy scores:
gplot(sa_net_area_style, vertex.col = my.colors, 
      usearrows=TRUE, vertex.cex = .25 * d.cent, label=names(d.cent)) #label=names(d.cent),
title(main = "Degree Centrality:Style and Area")

## Closeness Centrality
c.cent <- closeness(sa_net_area_style, gmode = "digraph")
c.cent
names(c.cent) <- sa_net_area_style %v% "vertex.names"
# sort(c.cent)
sort.c <- sort(c.cent)
top.c <- names(sort(c.cent))#[155:163]
# note there are all 0s for closeness centrality
# Draw the graph with node sizes proportional to closeness centralitiy scores:
gplot(sa_net_area_style,  vertex.col = my.colors, usearrows=TRUE, vertex.cex = 3 * c.cent, label=names(c.cent[1:20])) +
  title(main = "Closeness Centrality:Style and Area Network")

## Betweeness Centrality
b.cent <- betweenness(sa_net_area_style, gmode = "digraph")
table(b.cent)
names(b.cent) <- sa_net_area_style %v% "vertex.names"
sort.b <- sort(b.cent)
# kable(sort.b[153:163])
# top.b <- names(sort(b.cent))[155:163]
# Draw the graph with node sizes proportional to closeness centralitiy scores:
gplot(sa_net_area_style, vertex.col = my.colors, usearrows=TRUE, vertex.cex = .5 * b.cent, label=names(b.cent[1:20])) +
  title(main = "Betweenness Centrality:Style and Area Network")


# To build this network both area and style columns are joined together 
# to create one long list of nodes. A weights column is also created in a 
# separate grouping of the area and style columns to show their connecting 
# measures and are labeled "to" and "from". The network is then created by 
# implementing the nodes as rows and columns, and the edge list as the matrix meat. 

