# (1)
# 'for' loops
# a simple example
# determine sd for each column of a matrix
matrix1 <- matrix(1:25, nrow = 5)
# always define a variable for the output before running a loop
colsd <- vector(length = dim(matrix1)[2])
# fill up each position in the vector,
# one by one with one iteration of the code within the loop
for(i in 1:length(colsd)) {
  colsd[i] <- sd(matrix1[,i])
}
colsd

# the same job with apply()
apply(matrix1, MARGIN = 2, FUN = sd)


# simple example 2
# determine the difference btw adjacent columns for each row
# define a vector first
diff1 <- matrix(NA, nrow = dim(matrix1)[1], ncol = dim(matrix1)[2] - 1)
# fill up each position in the vector
for(i in 1:dim(diff1)[2]) {
  diff1[,i] <- matrix1[,i+1] - matrix1[,i]
}


# (2)
# Creating multipanel figures using 'for' loops
# Given: a complex list with several dates and samples/date
library(ggplot2)
setwd("")
load("test_workspace.RData")
summary(cyto.list)

# This is how one plot (sample) looks like
ggplot(data = cyto.list[[10]][[5]], mapping = aes(FSC,FL3)) +
  geom_hex(bins = 200) +
  theme_bw() +
  theme(panel.grid = element_blank(), aspect.ratio = 1,
        axis.title = element_blank(), plot.title = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"mm"),
        legend.position = "none") +
  scale_x_continuous(breaks = c(1,2,3)) +
  scale_y_continuous(breaks = c(1,2,3))

# This is how one date with 12 plots (samples) looks like
# make an empty list
scatterplot <- vector(mode = "list", length = length(cyto.list[[10]]))

# fill up the list with 12 plots (graphical objects)
for(j in 1:length(cyto.list[[10]])) {
  scatterplot[[j]] <- ggplot(data = cyto.list[[10]][[j]], mapping = aes(FSC,FL3)) +
    geom_hex(bins = 200) +
    theme_bw() +
    theme(panel.grid = element_blank(), aspect.ratio = 1,
          axis.title = element_blank(), plot.title = element_blank(),
          plot.margin = unit(c(0.5,0.5,0.5,0.5),"mm"),
          legend.position = "none") +
    scale_x_continuous(breaks = c(1,2,3)) +
    scale_y_continuous(breaks = c(1,2,3))
}

# Plot all the grobs in the list
multipanel <- arrangeGrob(grobs = scatterplot, nrow = 4,
                          top = textGrob(names(cyto.list)[[10]],
                                         gp = gpar(fontface = "bold", fontsize = 18)),
                          bottom = textGrob("FSC", gp = gpar(fontface = "bold", fontsize = 15)),
                          left = textGrob("FL3", gp = gpar(fontface = "bold", fontsize = 15), rot = 90))
grid.draw(multipanel)


# This is how plotting all the dates looks like
# (using a nested loop)
# Plus info: 6 samples were heated
hightemp <- c(1,3,5,8,9,10)

# make an empty list
plots <- vector(mode = "list", length = length(cyto.list))

# fill up the list with graphical objects
# using an if statement to differentiate between heated and control samples
for(i in 1:length(plots)) {
  # make a temporary empty list
  scatterplots <- vector(mode = "list", length = length(cyto.list[[i]]))
  # fill up the temporary list
  for(j in 1:length(cyto.list[[i]])) {
    if(j %in% hightemp == TRUE) {
      scatterplots[[j]] <- ggplot(data = cyto.list[[i]][[j]], mapping = aes(FSC,FL3)) +
        geom_hex(bins = 200) +
        theme_bw() +
        theme(panel.grid = element_blank(), aspect.ratio = 1,
              panel.background = element_rect(fill = "lightblue2"),
              axis.title = element_blank(), plot.title = element_blank(),
              plot.margin = unit(c(0.5,0.5,0.5,0.5),"mm"),
              legend.position = "none") +
        scale_x_continuous(breaks = c(1,2,3)) +
        scale_y_continuous(breaks = c(1,2,3)) +
        annotate(geom = "text", x=0.3, y=2.9,
                 label = "+3°C", size = 4, fontface = "bold" )

    } else {
      scatterplots[[j]] <- ggplot(data = cyto.list[[i]][[j]], mapping = aes(FSC,FL3)) +
        geom_hex(bins = 200) +
        theme_bw() +
        theme(panel.grid = element_blank(), aspect.ratio = 1,
              panel.background = element_rect(fill = "white"),
              axis.title = element_blank(), plot.title = element_blank(),
              plot.margin = unit(c(0.5,0.5,0.5,0.5),"mm"),
              legend.position = "none") +
        scale_x_continuous(breaks = c(1,2,3)) +
        scale_y_continuous(breaks = c(1,2,3))
    }
  }
  
  plots[[i]] <- arrangeGrob(grobs = scatterplots, nrow = 4,
                                     top = textGrob(names(cyto.list)[[i]],
                                                    gp = gpar(fontface = "bold", fontsize = 18)),
                                     bottom = textGrob("FSC", gp = gpar(fontface = "bold", fontsize = 15)),
                                     left = textGrob("FL3", gp = gpar(fontface = "bold", fontsize = 15), rot = 90))
  rm(scatterplots)
}

grid.newpage()
grid.draw(plots[[10]])


# Saving plots for each date into separate files
# using a loop
setwd("")
for(i in 1:length(plots[[i]])) {
  jpeg(filename = paste("Meso", names(cyto.list)[[i]], ".jpg", sep=""),
       width = 1400, height = 1400, res = 144)
  grid.newpage()
  grid.draw(plots[[i]])
  dev.off()
}


# Opening multiple files with a loop
dates.taxon <- c("2019-04-20","2019-04-26","2019-05-02","2019-05-08","2019-05-14","2019-05-20")
community <- list()
setwd("")
for(i in 1:6) {
  community[[i]] <- read.csv(file = paste("Phytoplankton_",dates.taxon[i],".csv", sep = ""),
                             header=TRUE, row.names=1, sep=",")
  community[[i]] <- t(community[[i]])
}
names(community) <- dates.taxon

library(stringr)
# Open all files in a directory full of csv files
data.list <- vector(mode = "list", length = length(dir()))
for(i in 1:length(dir())) {
  data.list[i] <- read.csv(file = dir()[i], header = TRUE, sep = ",")
  names(data.list)[i] <- str_sub(dir()[i], start = 1, end = -5)
}


# (3)
# 'while' loops
# Silly and simple example
# At what number does the factorial reach 10^9?
counter <- 0
factorial <- 1
while(factorial < 10^9) {
  counter <- counter + 1
  factorial <- factorial * counter
}
counter

# Do the same with a 'for' loop and a 'break'
counter <- 1:100
factorial <- 1
for(position in counter) {
  factorial <- factorial * counter[position]
  if(factorial > 10^9) {
    break
  }
}
position


# (4)
# FUNCTIONS
# Define a function for coefficient of variation
cv <- function(x) {
  coeff <- sd(x)/mean(x)
  return(coeff)
  }
apply(matrix1, MARGIN = 2, FUN = cv)

# Define a function for Shannon evenness
library(vegan)
even <- function(x) {
  div <- diversity(x, index = "shannon")
  species <- specnumber(x)
  evenness <- div/log(species)
  return(evenness)
}

# A simple debugging step
# What happens if x is not a numeric vector?
even <- function(x) {
  if(!is.numeric(x)) {
    stop("What the hell are you doing?")
  }
  div <- diversity(x, index = "shannon")
  species <- specnumber(x)
  evenness <- div/log(species)
  return(evenness)
}

# Saving/Loading your own functions
setwd("c:/Users/Karesz/Documents/R/Loops_Functions_Course")
source("my_own_functions.R")



