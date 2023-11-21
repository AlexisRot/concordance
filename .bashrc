#/* ======================================================================
#                           PROPERTY OF SANOFI-AVENTIS
#
#   Description      : Users Install for allocation
#   Revision History :
#   No :    Date     :    Author    :   Description of the change
#   -- + ----------- + ------------ + ------------------------------------
#   01 : 23-MAR-2021 :  WISE Team   :  Creation
#   ====================================================================== */
#/* ======= Beginning of Users Parmeters ================================= *
export MANPATH="/usr/local/share/man:/usr/share/man/overrides:/usr/share/man:/cm/local/apps/environment-modules/current/share/man"
export MODULEPATH="/cm/local/modulefiles:/cm/shared/modulefiles"
export MODULESHOME="/cm/local/apps/environment-modules/3.2.10/Modules/3.2.10"
export MODULE_VERSION="3.2.10"
export MODULE_VERSION_STACK="3.2.10"
module ()
{
  eval `/cm/local/apps/environment-modules/3.2.10/Modules/$MODULE_VERSION/bin/modulecmd sh $*`
}
export EASYBUILD_PREFIX=/cm/easybuild
module use $EASYBUILD_PREFIX/modules/wise-lang/
  module use $EASYBUILD_PREFIX/modules/all
module load EasyBuild
export EASYBUILD_MODULES_TOOL=EnvironmentModulesC
export EASYBUILD_MODULE_SYNTAX=Tcl
export EASYBUILD_ROBOT_PATHS=/cm/easybuild/sanofi_repo:${EASYBUILD_ROBOT_PATHS}

module load R/4.0.4-foss-2020bREL-001