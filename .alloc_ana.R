#/* ======================================================================
#  PROPERTY OF SANOFI-AVENTIS
#
#Program          : .alloc_ana.R
#Description      : File allocate the LIB
#Revision History :
#  No :    Date     :    Author    :   Description of the change
#-- + ----------- + ------------ + ------------------------------------
#  01 : 27-JUN-2018 :  WISE Team   :  Creation
#====================================================================== */

source("[WORKSPACE]/.repo_sanofi.R")
#====================================================================== */
W_WISE <- paste("[WORKSPACE]")
.libPaths( paste(W_WISE, "/.R/LIB", sep = ""))
R_LIBS=paste(W_WISE, "/.R/LIB", sep = "")
R_PATH=R_LIBS
.libPaths( c( .libPaths(), R_LIBS))
getwd()
print (R_LIBS)
print (R_PATH)
#library("devtools")
