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

IDIR <- "./data/CRA2/"     # DATA FOLDER (CRA2 = USES CRA2 RR FUNCTIONS; UPDATED = USES UPDATED RR FUNCTIONS)
LB <- 0.03 ; UB <- 150     # INTEGRATION LIMITS [g/day]
RRCAP <- 150               # CAP FOR RR FUNCTIONS
BDM <- 60; BDF <- 48       # BINGE DRINKING CUTOFF [g/day] (MALES/FEMALES)
MAXMULT <- 1               # ALLOWED MAXIMUM MULTIPLIER 
INTNODES <- 64             # NUMBER OF INTEGRATION NODES FOR THE GAUSS-LEGENDRE INTEGRATOR 
TOTAA <- 8.4*43590000      # TOTAL CONSUMPTION OF ABSOLUTE ALCOHOL IN POPULATION 15+ (WHO ALCOHOL PER CAPITA ESTIMATE 2022 * STATSSA MID-YEAR POPULATION 15+ ESTIAMATE)

# LIST AND VARIABLES FOR INTERFACE 

genders  <- c("Male", "Female")                       
agecats  <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65+")              
measures <- c(Deaths = "DEATH", YLL = "YLL", YLD = "YLD", DALY = "DALY")
plot_cat <- c("Individual causes" = "cause", "Level 1 groups" = "cat1", "Level 2 groups" = "cat2", "Gender" = "gender", "Age" = "agecat")
plot_group <- c("None" = "none", "Gender" = "gender", "Age" = "agecat", "Individual causes" = "cause", "Level 1 groups" = "cat1", "Level 2 groups" = "cat2")
#plot_var <- c("Total burden" = "bod", "Attributable burden" = "abod", "Avoidable burden" = "vbod")
plot_var <- c("Attributable burden" = "abod", "Avoidable burden" = "vbod")


RBREAKS <- data.frame(Female = c(0,20,40,60,Inf), Male = c(0,40,60,100,Inf))

mult_cols <- c("curr", "bing", "Low_risk", "Moderate_risk", "High_risk", "Very_high_risk") 
risk_cols <- mult_cols[3:6]
names(mult_cols) <- c("PREV", "HED", "LR", "MR", "HR", "VR") 
template <- expand.grid(agecat = agecats, gender = genders, stringsAsFactors = FALSE)[, c("gender", "agecat")]
template[c(mult_cols)] <- 1   # 1 = no change (baseline) for all seven columns

footercontent <- ""

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
names(valueboxcolors) <- c("AVOIDABLE DEATHS","AVOIDABLE YLLS","AVOIDABLE YLDS","AVOIDABLE DALYS")

################################################################################################################################################################################
# APP INITIALISATION                                                                                                                                                           # 
################################################################################################################################################################################

source("./functions.R")                 # LOAD CUSTOM FUNCTIONS FOR PIF CALCULATIONS AND DATA MANAGEMENT
load(paste0(IDIR, "APP_DATA.RData"))    # LOAD BASE DATA (EXPOSURE, BOD, PRE-CALCULATED PAF AND ABOD)

# SPLIT DATA PER METRIC

DATA1 <- DATA %>% filter(metric == "DEATH") %>% arrange(metric, gender, agecat)   # DEATHS 
DATA2 <- DATA %>% filter(metric == "YLD") %>% arrange(metric, gender, agecat)   # YLD 
DATA3 <- DATA %>% filter(metric == "YLL") %>% arrange(metric, gender, agecat)   # YLL 

BASELINE_EXPOSURE <- DATA1[, c("gender","agecat","alpha","beta","pdrink","pbing","mu")]
BASELINE_EXPOSURE <- BASELINE_EXPOSURE [!duplicated(BASELINE_EXPOSURE),]

# SUBSTITUTE BASE PAFs WITH APPROXIMATED CALCULATED WITH THE SAME METHOD AS IN THE APP (APPROXIMATE INTEGRATION)

load(paste0(IDIR, "PAFBASE_",INTNODES,".RData"))

DATA1$paf <- pafbase[1:nrow(DATA1)]
DATA2$paf <- pafbase[(nrow(DATA1) + 1):(2*nrow(DATA1))]
DATA3$paf <- pafbase[1:nrow(DATA1)]

# GENERATE INTEGRATION NODES 

GL <- gl_nodes(INTNODES)

# CLEAN WORKSPACE

rm(DATA)
