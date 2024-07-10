install.packages(c("dplyr","shiny","shinylive","httpuv", "usethis"))
library(dplyr) 
library (shiny)
library (shinylive) 
library (httpuv)
library(usethis)

# Find instructions here: https://github.com/RamiKrispin/shinylive-r/blob/main/README.md

# Rendering the Shiny App

shinylive::export(appdir = "myapp", destdir = "docs")

# Checking if rendering process was succesful

httpuv::runStaticServer("docs", port=8008)

# Adding first commit 

usethis::use_git()

# Pushing to remote repository

usethis::use_github()
