
library(networkD3)

nodes <- data.frame("name" = c("cluster analysis",
                               "base", "factoextra",
                               "hierarchical clustering", "k-means clustering",
                               "3", "11"))
links <- as.data.frame(matrix(c(
  0,1,50, 0,2,50,
  1,3,50, 1,4,50,
  2,4,50,
  3,5,50,
  4,6,50),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

nodes <- data.frame("name" = c("significance testing",
                               "base", "ggpubr", "gtools", "vcd",
                               "chi-square test", "correlation test", 
                               "f-test", "t-test",
                               "7", "9", "10", "12", "13", "14"))
links <- as.data.frame(matrix(c(
  0,1,50, 0,2,50, 0,3,50, 0,4,50,
  1,5,50, 1,6,50, 1,7,50, 1,8,50, 
  2,8,50, 
  3,5,50,
  4,5,50,
  5,10,50,
  6,11,50, 6,14,50,
  7,13,50,
  8,9,50, 8,12,50, 8,13,50),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

nodes <- data.frame("name" = c("effect size testing",
                               "effectsize", "effsize", "questionr", "rcompanion",
                               "cohen's d", "cramer's v", "glass's delta", "hedges' g",
                               "7", "9", "12", "13"))
links <- as.data.frame(matrix(c(
  0,1,50, 0,2,50, 0,3,50, 0,4,50, 
  1,5,50, 1,7,50, 1,8,50,
  2,5,50, 
  3,6,50, 
  4,6,50,
  5,9,50, 5,11,50, 5,12,50,
  6,10,50,
  7,12,50,
  8,12,50),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

nodes <- data.frame("name" = c("modeling",
                               "base", "broom", "car", "caret", "InformationValue", "pscl", "questionr",
                               "SciViews", "tree", 
                               "analysis of variance", "linear regression", 
                               "logistic regression", "regression tree",
                               "5", "14"))
links <- as.data.frame(matrix(c(
  0,1,50, 0,2,50, 0,3,50, 0,4,50, 0,5,50, 0,6,60, 0,7,50, 0,8,50, 0,9,50, 
  1,10,50, 1,11,50, 1,12,50, 
  2,11,50,
  3,11,50,
  4,12,50,
  5,12,50,
  6,12,50,
  7,12,50,
  8,12,50,
  9,13,50,
  10,15,50,
  11,14,50,
  12,15,50,
  13,14,50),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

nodes <- data.frame("name" = c("operations research",
                               "base", "ggQC", "gtools", "lpSolve", "janitor", "qcc", "tidyverse",
                               "80-20 rule", "constrained optimization",
                               "optimal stopping", "permutations",
                               "4", "8", "9", "15"))
links <- as.data.frame(matrix(c(
  0,1,50, 0,2,50, 0,3,50, 0,4,50, 0,5,50, 0,6,50, 0,7,50,
  1,9,50,
  2,8,50,
  3,11,50,
  4,9,50,
  5,10,50,
  6,8,50, 
  7,8,50, 7,9,50, 7,10,50,
  8,15,50,
  9,12,50,
  10,13,50,
  11,14,50),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

nodes <- data.frame("name" = c("probability",
                               "base", "runner", "tidyverse",
                               "expected value analysis", "laplace's rule", 
                               "3", "16"))
links <- as.data.frame(matrix(c(
  0,1,50, 0,2,50, 0,3,50, 
  1,4,50, 1,5,50,
  2,5,50, 
  3,4,50,
  4,6,50,
  5,7,50),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

nodes <- data.frame("name" = c("statistical dispersion",
                               "base", "ineq",
                               "gini coefficients",
                               "mean absolute deviation method", "median absolute deviation method",
                               "range method", "standard deviation method", "variance method",
                               "12", "13", "18"))
links <- as.data.frame(matrix(c(
  0,1,50, 0,2,50,
  1,4,50, 1,5,50, 1,6,50, 1,7,50, 1,8,50,
  2,3,50,
  3,9,50, 3,10,50,
  4,11,50,
  5,11,50,
  6,11,50,
  7,11,50,
  8,11,50),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

nodes <- data.frame("name" = c("standardization",
                               "base",
                               "centering method", "range method", 
                               "standard deviation method", "z-score method",
                               "11", "14", "19"))
links <- as.data.frame(matrix(c(
  0,1,50, 
  1,2,50, 1,3,50, 1,4,50, 1,5,50, 
  2,8,50,
  3,8,50,
  4,8,50,
  5,6,50, 5,7,50, 5,8,50),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)

nodes <- data.frame("name" = c("summary statistics / visualization",
                               "base", "DataExplorer", "GGally", "reshape2","SmartEDA", "sqldf",
                               "tableone", "tidyverse",
                               "automated eda", "manual eda",
                               "2", "5", "6", "10", "14", "17"))
links <- as.data.frame(matrix(c(
  0,1,50, 0,2,50, 0,3,50, 0,4,50, 0,5,50, 0,6,50, 0,7,50, 0,8,50,
  1,10,50,
  2,9,50,
  3,10,50,
  4,10,50,
  5,9,50,
  6,10,50,
  7,9,50,
  8,10,50,
  9,16,50, 
  10,11,50, 10,12,150, 10,13,50, 10,14,50, 10,15,50),
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)
