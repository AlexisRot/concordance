#/* ======================================================================
#  PROPERTY OF SANOFI-AVENTIS
#
#Program          : .alloc_install.R
#Description      : File to define the package need to install.
#Revision History :
#  No :    Date     :    Author    :   Description of the change
#-- + ----------- + ------------ + ------------------------------------
#  01 : 27-JUN-2018 :  WISE Team   :  Creation
#====================================================================== */

options(download.file.method = "libcurl")

#############################################
# Chnage the date of the CRAN base on your
# Requirement of your DEV
#############################################

repos <- structure(c(CRAN="http://mran.pharma.aventis.com/snapshots/2018-06-17/"))

############################################
# END SECTION TO UPDATE
############################################
options(repos=repos)
getOption("repos")


WORKSPACE <- Sys.getenv(c("WORKSPACE"))
W_WISE <- paste(WORKSPACE)
.libPaths( paste(W_WISE, "/.R/LIB", sep = ""))
R_LIBS=paste(W_WISE, "/.R/LIB", sep = "")
R_PATH=R_LIBS
.libPaths( c( .libPaths(), R_LIBS))
getwd()
print (R_LIBS)
print (R_PATH)

install.packages("usethis", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("httr", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("/cm/shared/apps/R-Package/contrib/stringi/stringi_1.4.3.tar.gz", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("devtools", lib=R_LIBS, quiet = FALSE, verbose = TRUE)

library("devtools")
install.packages("shiny", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("shinytest", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("shinyjs", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("shinydashboard", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("httpuv", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("Rcpp", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("later", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("promises", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("rlang", lib=R_LIBS, quiet = FALSE, verbose = TRUE)

##################################################
# ADD THE PACKAGE YOU NEED FOR YOUR APP SHINY
##################################################

install.packages("DT", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("knitr", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("plotly", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("dplyr", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
# install.packages("shinycssloaders", lib=R_LIBS, quiet = FALSE, verbose = TRUE) ##déjà installé avec TMtools


#####################################################
# END SECTION OF INSTALLATION PACKAGE FOR YOUR APP
####################################################
