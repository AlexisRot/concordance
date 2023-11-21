pipeline {
  agent  any
  parameters {
        choice(
            choices: ['Rebuild' , 'Build'],
            description: '',
            name: 'BUILD_ACTION')
        string(defaultValue: "ITS-CHGXXXXX", description: 'Change Number?', name: 'CHANGE_NUMBER')
  }
  environment {
    GIT_COMMITTER_NAME = "jenkins"
    GIT_COMMITTER_EMAIL = "jenkins.wise@sanofi.com"
    // Please upate this value with your project
    APP_NAME = "[APP_NAME]"
    USER_APP_UNIX= "[APP_NAME]"
    // END SECTION TO UPDATE
  }
  stages {
  
    stage("Build - R LIB") {
    when {
                // Install packages only if a "Rebuild" is requested
                expression { params.BUILD_ACTION == 'Rebuild' }
            }
      steps {
        sh '/usr/bin/pwd'
        sh '. $WORKSPACE/.bashrc;/cm/easybuild/software/R/4.0.4-foss-2020bREL-001/bin/R CMD BATCH --vanilla --slaves ./.alloc_install.R $WORKSPACE/alloc_ana.R.log'
        sh 'cat $WORKSPACE/alloc_ana.R.log'
        sh 'cp /home/jenkins/bin/phantomjs $WORKSPACE'
	    
      }
      }
    stage("Build - Package") {
      steps {
        sh 'export'
        sh 'env'
        sh 'sudo /usr/bin/mkdir -p /SHINY_WEB_PROJET/$JOB_NAME'
        sh 'sudo /usr/bin/chown jenkins:jenkins -R /SHINY_WEB_PROJET/$JOB_NAME'
        sh 'cd $WORKSPACE; tar cvf /SHINY_WEB_PROJET/$JOB_NAME/$BRANCH_NAME.$BUILD_ID.$GIT_COMMIT.tar --exclude ".git" .'
      }
    }
    
    stage("Build - PreDeploy"){
    when {
                // Delete application folder only if a "Rebuild" is requested
                expression { params.BUILD_ACTION == 'Rebuild' }
            }
            steps {
            sh 'sudo /usr/bin/rm -rf /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME'
            sh 'sudo /usr/bin/mkdir -p /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME'
            }
    
    }
    
    
    stage("Build - Deploy") {
      steps {
        
        sh 'cd /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME;sudo /usr/bin/tar xvf /SHINY_WEB_PROJET/$JOB_NAME/$BRANCH_NAME.$BUILD_ID.$GIT_COMMIT.tar '
        sh 'sudo /usr/bin/chown $USER_APP_UNIX:$USER_APP_UNIX -R /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME'
        sh 'sudo /usr/bin/chown $USER_APP_UNIX:$USER_APP_UNIX /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.*'
        sh 'sudo /usr/bin/chmod 755 /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.jenkins.install.sh'
        sh 'sudo /usr/bin/rm /SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/Jenkinsfile'
        sh 'mkdir -p /SHINY_WEB-$BRANCH_NAME-log/log/$APP_NAME;exit 0'
        sh 'sudo /usr/bin/chown $USER_APP_UNIX:$USER_APP_UNIX /SHINY_WEB-$BRANCH_NAME-log/log/$APP_NAME'
        sh 'sudo /usr/bin/chmod 755 /SHINY_WEB-$BRANCH_NAME-log/log/$APP_NAME'
      }
    }
    stage("Config - SHINY") {
      steps {
        sh '/SHINY_WEB-$BRANCH_NAME/web/$APP_NAME/.jenkins.install.sh'
      }
    }
  }
  post {
    always {
      sh 'pwd'
    }
    success {
      deleteDir()
      mail to:"[EMAIL]", subject:"SUCCESS: ${currentBuild.fullDisplayName}", body: "Yay, we passed."
    }
    failure {
      sh 'cat $WORKSPACE/alloc_ana.R.log'
      mail to:"[EMAIL]", subject:"FAILURE: ${currentBuild.fullDisplayName}", body: "Boo, we failed."
    }
  }
}
