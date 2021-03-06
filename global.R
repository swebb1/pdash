library(shiny)
library(shinyFiles)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(d3heatmap)
library(ggthemes)
library(colorRamps)
library(scales)
library(ggExtra)
library(gdata)

#set maximum file size
options(shiny.maxRequestSize=500*1024^2) #500Mb

#setup a color pallete choice on main dashboard or per tool?
cols<-brewer.pal(9,"Set1")

#test table
test<-data.frame(A=seq(1,10),B=seq(1,40,4),C=seq(1,100,10))