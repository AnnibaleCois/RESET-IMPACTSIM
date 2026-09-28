################################################################################################################################################################################
# RESET ALCOHOL PROJECT                                                                                                                                                        #
# IMPACT SIMULATOR - APP INITIALISATION                                                                                                                                        #
# v.1, 2026/09/24                                                                                                                                                              #
# annibale.cois@uct.ac.za                                                                                                                                                      #
################################################################################################################################################################################

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

################################################################################################################################################################################
# SETTINGS                                                                                                                                                                     # 
################################################################################################################################################################################

IDIR <- "./data/"          # DATA FOLDER  
LB <- 0.03 ; UB <- 150     # INTEGRATION LIMITS [g/day]
RRCAP <- 150               # CAP FOR RR FUNCTIONS
BDM <- 60; BDF <- 48       # BINGE DRINKING CUTOFF [g/day] (MALES/FEMALES)
MAXMULT <- 1               # ALLOWED MAXIMUM MULTIPLIER 
INTNODES <- 64             # NUMBER OF INTEGRATION NODES FOR THE GAUSS-LEGENDRE INTEGRATOR 

# LIST AND VARIABLES FOR INTERFACE 

genders  <- c("Male", "Female")                       
agecats  <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65+")              
measures <- c(Deaths = "DEATH", YLL = "YLL", YLD = "YLD", DALY = "DALY")
mult_cols <- c("curr", "bing", "cons")
plot_cat <- c("Individual causes" = "cause", "Level 1 groups" = "cat1", "Level 2 groups" = "cat2", "Gender" = "gender", "Age" = "agecat")
plot_group <- c("None" = "none", "Gender" = "gender", "Age" = "agecat", "Individual causes" = "cause", "Level 1 groups" = "cat1", "Level 2 groups" = "cat2")
plot_var <- c("Total burden" = "bod", "Attributable burden" = "abod", "Avoidable burden" = "vbod")

template <- expand.grid(agecat = agecats, gender = genders, stringsAsFactors = FALSE)[, c("gender", "agecat")]
template[mult_cols] <- 1   

footercontent <- "Footer dassd"

# THEME 

theme_vs <- bs_theme_update(
  bs_theme(version = 5, bootswatch = "flatly"),
  "nav-link-font-weight" = "bold",       # Weight of the tab labels
  `nav-link-color` = "#2c3e50",          # Color for inactive tabs
  `nav-link-hover-color` = "#18bc9c",    # Color when hovering over tabs
  primary = "#18bc9c"                    # Color for the active tab
) %>% 
  bs_add_rules("
    .tab-footer {
      text-align: right;
      color: #6c757d;
      font-size: 0.8rem;
      margin-top: 0.75rem;
    }
    .tab-footer-left {
      text-align: left;
      color: #6c757d;
      font-size: 0.8rem;
      margin-top: 0.75rem;
    }
    .header-icon-btn {
      color: #ffffff;        
      background-color: #E8A33D;
      border: none;
    }
    .header-icon-btn:hover {
      color: #000000;
      background-color: #E8A33D;
    }
  ")

defaultfill <- "#18bc9c"
valueboxcolors <- c("#2C3E50","#18A999","#E8A33D","#5B4B8A")
names(valueboxcolors) <- c("DEATH","YLL","YLD","DALY")

################################################################################################################################################################################
# APP INITIALISATION                                                                                                                                                           # 
################################################################################################################################################################################

source("./functions.R")                 # LOAD CUSTOM FUNCTIONS FOR PIF CALCULATIONS AND DATA MANAGEMENT
load(paste0(IDIR, "APP_DATA.RData"))    # LOAD BASE DATA (EXPOSURE, BOD, PRE-CALCULATED PAF AND ABOD)

# SPLIT DATA PER METRIC

DATA1 <- DATA %>% filter(metric == "DEATH") %>% arrange(metric, gender, agecat)   # DEATHS 
DATA2 <- DATA %>% filter(metric == "YLD") %>% arrange(metric, gender, agecat)   # YLD 
DATA3 <- DATA %>% filter(metric == "YLL") %>% arrange(metric, gender, agecat)   # YLL 

# SUBSTITUTE BASE PAFs WITH APPROXIMATED CALCULATED WITH THE SAME METHOD AS IN THE APP (APPROXIMATE INTEGRATION)

load(paste0(IDIR, "PAFBASE_",INTNODES,".RData"))

DATA1$paf <- pafbase[1:nrow(DATA1)]
DATA2$paf <- pafbase[(nrow(DATA1) + 1):(2*nrow(DATA1))]
DATA3$paf <- pafbase[1:nrow(DATA1)]

# GENERATE INTEGRATION NODES 

GL <- gl_nodes(INTNODES)

# CLEAN WORKSPACE

rm(DATA)
