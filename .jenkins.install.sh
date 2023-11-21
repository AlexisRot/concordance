#!/usr/bin/bash -x
echo $APP_NAME
echo $USER_APP_UNIX
echo $BRANCH_NAME

#sudo /usr/bin/echo "source('/SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.alloc_ana.R')" > /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.Rprofile
rstring="/SHINY_WEB/web/$APP_NAME"
rstringtmp=`echo $rstring | sed 's/\//\\\\\//g'`
rstringsed="s/\[WORKSPACE\]/$rstringtmp/g"
sudo /usr/bin/rm -rf /tmp/$APP_NAME
mkdir /tmp/$APP_NAME
sudo /usr/bin/sed ${rstringsed} /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.alloc_ana.R > /tmp/$APP_NAME/.alloc_ana.R.tmp
sudo /usr/bin/mkdir /home-$BRANCH_NAME/$APP_NAME
sudo /usr/bin/mv /tmp/$APP_NAME/.alloc_ana.R.tmp /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.alloc_ana.R
echo "source(\"/SHINY_WEB/web/$APP_NAME/.alloc_ana.R\")" > /tmp/$APP_NAME/.Rprofile 
sudo /usr/bin/mv /tmp/$APP_NAME/.Rprofile /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.Rprofile
sudo /usr/bin/cp /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.Rprofile /home-$BRANCH_NAME/$APP_NAME/.Rprofile
sudo /usr/bin/chown $USER_APP_UNIX:$USER_APP_UNIX /home-$BRANCH_NAME/$APP_NAME/.Rprofile
sudo /usr/bin/chown $USER_APP_UNIX:$USER_APP_UNIX /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.Rprofile
sudo /usr/bin/chown $USER_APP_UNIX:$USER_APP_UNIX /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.alloc_ana.R
sudo /usr/bin/chown -R $USER_APP_UNIX:$USER_APP_UNIX /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME

#####################
# Create LOG FOLDER
#####################
sudo /usr/bin/mkdir -p /SHINY_WEB-${BRANCH_NAME}-log/log/$APP_NAME
sudo /usr/bin/chown $USER_APP_UNIX:$USER_APP_UNIX -R /SHINY_WEB-${BRANCH_NAME}-log/log/$APP_NAME
sudo /usr/bin/chmod 775 -R /SHINY_WEB-${BRANCH_NAME}-log/log/$APP_NAME


#####################
# DATA PERSISTANT
#####################
#sudo mkdir -p /SHINY_DATA/$APP_NAME
#cd /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME
#sudo chown $USER_APP_UNIX:$USER_APP_UNIX /SHINY_DATA/$APP_NAME
#sudo mkdir -p /SHINY_DATA-$BRANCH_NAME/$APP_NAME
#sudo chown $USER_APP_UNIX:$USER_APP_UNIX /SHINY_DATA-$BRANCH_NAME/$APP_NAME
#sudo mv /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/data /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.data
#sudo /usr/bin/ln -fs /SHINY_DATA/$APP_NAME data

exit 0
