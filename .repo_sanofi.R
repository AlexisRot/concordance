#/* ======================================================================
#  PROPERTY OF SANOFI-AVENTIS
#
#Program          : .repo_sanofi.R
#Description      : File to define the repo CRAN of sanofi
#Revision History :
#  No :    Date     :    Author    :   Description of the change
#-- + ----------- + ------------ + ------------------------------------
#  01 : 27-JUN-2018 :  WISE Team   :  Creation
#====================================================================== */

# Definition CRAN mirrors 

options(download.file.method = "libcurl")


repos <- structure(c(CRAN="http://mran.pharma.aventis.com/snapshots/2018-06-17/"))
options(repos=repos)
getOption("repos")
