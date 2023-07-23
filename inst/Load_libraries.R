#library(shiny)
#library(shinydashboard)
#library(shinydashboardPlus)
#library(dashboardthemes)
#library(fontawesome)
#library(shinycssloaders)
#library(shinyWidgets)
#library(shinymeta)
#library(DT)
#library(tidyverse)
#library(cBioPortalData)
library(operator.tools)
library(survival)
library(survminer)

if (!requireNamespace("reshape2", quietly = TRUE))
    BiocManager::install("reshape2")
library(reshape2)

library(RColorBrewer)

# Association/Stat test
if (!requireNamespace("rstatix", quietly = TRUE))
    BiocManager::install("rstatix")
if (!requireNamespace("DescTools", quietly = TRUE))
    BiocManager::install("DescTools")
if (!requireNamespace("compareGroups", quietly = TRUE))
    BiocManager::install("compareGroups")

library(stats)
library(rstatix)
library(DescTools) # Dunns Test
library(compareGroups)
library(car) # leveneTest
