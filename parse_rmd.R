# Helper file to prepare Rmd and HTML

setwd("/home/hannarud/Documents/ReproducibleResearch/RepData_PeerAssessment1")

library(knitr)

knit2html("PA1_template.Rmd")

browseURL("PA1_template.html")
