#/* ======================================================================
#  PROPERTY OF SANOFI-AVENTIS
#
#Program          : .alloc_install_rsconnect.R
#Description      : File to define the package need to install.
#Revision History :
#  No :    Date     :    Author    :   Description of the change
#-- + ----------- + ------------ + ------------------------------------
#  01 : 27-JUN-2018 :  WISE Team   :  Creation
#====================================================================== */


#/* ======= Beginning of Users Parmeters ================================= *
# Need to be commented for Jenkins deployment
# unlink(list.files(".R/LIB", full.names = TRUE), recursive= TRUE)
# unlink(list.files(".R/LIB/affy", full.names = TRUE), recursive= TRUE)
# remove.packages(list.files(".R/LIB"), lib = paste0(W_WISE,"/.R/LIB"))
# JJM options(download.file.method = "wget")

repos <- structure(c(CRAN="https://rstudio-pm.dev.p488440267176.aws-emea.sanofi.com/prod-cran/__linux__/centos7/2021-12-10",
                     RSPM="https://rstudio-pm.dev.p488440267176.aws-emea.sanofi.com/prod-cran/__linux__/centos7/2021-12-10",
                     ART_GIT="https://rstudio-pm.dev.p488440267176.aws-emea.sanofi.com/art-git/latest"))
options(repos=repos)

options(internet.info = 0)
options(download.file.method="wget")

WORKSPACE <- Sys.getenv(c("WORKSPACE"))
W_WISE <- paste(WORKSPACE)
.libPaths( paste(W_WISE, "/.R/LIB", sep = ""))
R_LIBS=paste(W_WISE, "/.R/LIB", sep = "")
R_PATH=R_LIBS
.libPaths( c( .libPaths(), R_LIBS))
getwd()
print (R_LIBS)
print (R_PATH)

unlink(list.files(".R/LIB", full.names = TRUE), recursive= TRUE)
unlink(list.files(".R/LIB/affy", full.names = TRUE), recursive= TRUE)

###
# REQUIREMENT PACKAGE

# install.packages("remotes")
# library(remotes)
# remotes::install_git("http://gitwise.pharma.aventis.com/R-PACKAGE/stringi.1.4.6.git", ref='master', verbose=TRUE)



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
# install.packages("shinycssloaders", lib=R_LIBS, quiet = FALSE, verbose = TRUE)

##################################################
# ADD THE PACKAGE YOU NEED FOR YOUR APP SHINY
##################################################

install.packages("DT", lib=R_LIBS, quiet = FALSE, verbose = TRUE) ##déjà installé avec TMtools

#####################################################
# END SECTION OF INSTALLATION PACKAGE FOR YOUR APP
####################################################

# ##################################################
# # ADD RSCONNECT PACKAHE AND GENERATE MANIFEST.json
# ##################################################

setwd(WORKSPACE)
install.packages('knitr', lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("rmarkdown", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("tinytex", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("xfun", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
install.packages("rsconnect", lib=R_LIBS, quiet = FALSE, verbose = TRUE)
library(rsconnect)
rsconnect::writeManifest()

# ##################################################
# # END ADD RSCONNECT PACKAHE AND GENERATE MANIFEST.json
# ##################################################

