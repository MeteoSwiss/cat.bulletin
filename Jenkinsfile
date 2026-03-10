class Globals {
    // Pin mchbuild to stable version to avoid breaking changes
    static String mchbuildVersion = ">=0.9.0"

    // This parameters will be adapted during preparation stage
    // package name and version (from DESCRIPTION file)
    static String package_name = 'cat.bulletin'
    static String package_version = ''
    static String package_file = ''

    // deployment stages setup (check vs deploy vs abort)
    static String deploy_stages = 'check'

    // publish documentation
    static boolean documentation_publish = false
}

pipeline {
    agent { label 'podman' }

    environment {
	NEXUS_REPO = 'https://nexus.meteoswiss.ch/nexus/repository/r-mch/src/contrib'
	DOCKER_REPO = 'dockerhub.apps.cp.meteoswiss.ch'
	PATH = "${WORKSPACE}/.venv-mchbuild/bin:${HOME}/tools/openshift-client-tools:${PATH}"
    }

    options {
	gitLabConnection('CollabGitLab')

	// do not checkout the git-repo by default, get only Jenkinsfile
	// checkout is done in 'Checkout' stage into source directory
	skipDefaultCheckout(true)

        // New jobs should wait until older jobs are finished
        disableConcurrentBuilds()

        // Discard old builds - keep 1 for 2 days
        buildDiscarder(logRotator(artifactDaysToKeepStr: '2',
				  artifactNumToKeepStr: '1',
                                  daysToKeepStr: '2',
				  numToKeepStr: '1'))

        // Timeout the pipeline build after 1 hour
        timeout(time: 2, unit: 'HOURS')
    }

    stages {

	stage('Init') {
	    steps {

                updateGitlabCommitStatus name: 'Build', state: 'running'

		script{
		    echo 'Install mchbuild'
		    sh """
                    python -m venv .venv-mchbuild
                    PIP_INDEX_URL=https://hub.meteoswiss.ch/nexus/repository/python-all/simple \
                        .venv-mchbuild/bin/pip install 'mchbuild${Globals.mchbuildVersion}'
                    """
		}
	    }
	}

        stage('Checkout') {
            steps {
		updateGitlabCommitStatus name: 'Build', state: 'running'

		// checkout into source directory
		dir('source'){
                    checkout scm
		}
            }
        }

	stage('Preparation') {
	    steps {
		script {
		    // get Global variables from R-package DESCRIPTION
		    Globals.package_version = sh( script: 'grep "Version:" source/DESCRIPTION | cut -d":" -f2', returnStdout: true).trim()

		    // set package name
		    Globals.package_file = "${Globals.package_name}_${Globals.package_version}.tar.gz"

		    echo "Package specs: ${Globals.package_file}"

		    // get env variables from git
		    echo "GIT specs: ${env.BRANCH_NAME}"

		    // decide what to do in next stages
		    if (env.BRANCH_NAME != "main" && env.BRANCH_NAME != "master"){
			echo "Do only package check for ${Globals.package_name} as branch is ${env.BRANCH_NAME} (--> neither main nor master)."
			Globals.deploy_stages = "check"
			Globals.documentation_publish = false
		    } else {
			echo "Do package check, deploy and docu for ${Globals.package_name} as branch is ${env.BRANCH_NAME}."
			Globals.deploy_stages = "deploy"
			Globals.documentation_publish = true
		    }
		}

		dir('build-lib'){
		    sh 'pwd -P'
		}
		dir('docs'){
		    sh 'pwd -P'
		}
	    }
	}

        stage('Check') {
	    when {
		expression { return Globals.deploy_stages != "abort" }
	    }
		environment {
			MCHDWH_CLIENTSECRET = credentials("cats_dwhqueryservice_prod")
			GRIDGET_CLIENTSECRET = credentials("cats_rasterdata_prod")
			PROVIDER_CLIENTSECRET = credentials("cats_productprovider_prod")
		}
        steps {
		script {
		    echo "task: build.check_rpkg"

		    def docker_env = '["--env", "MCHDWH_CLIENTSECRET=\'${MCHDWH_CLIENTSECRET}\'", "--env", "GRIDGET_CLIENTSECRET=\'${GRIDGET_CLIENTSECRET}\'", "--env", "PRODUCT_PROVIDER_CLIENTSECRET=\'${PROVIDER_CLIENTSECRET}\'", "--env", "CI=\'TRUE\'"]'

		    sh "mchbuild -c ${WORKSPACE}/source/Jenkinsfile_config.yml \
                                 -s extraArgs='${docker_env}' \
                                 jenkins.build.check_rpkg"

		    if ( ! fileExists ("source/${Globals.package_file}")){
			error (message: "Package build was not successful. Package file (source/${Globals.package_file}) not found. Aborting.")
		    } else {
			echo "Package build was successful."
		    }
		}
            }
	    post{
		always {
		    echo "Check complete"
		    junit (allowEmptyResults: true,
			   keepLongStdio: true,
			   testResults: "source/${Globals.package_name}.Rcheck/tests/testthat/junit_result.xml")
		    archiveArtifacts (allowEmptyArchive: true,
				      artifacts: "source/${Globals.package_name}.Rcheck/*.log",
				      followSymlinks: false)
		}
	    }
	}

	stage('Compare versions') {
	    when {
		expression { return Globals.deploy_stages != "abort" }
	    }
            steps {
		echo "Check package version on Nexus"
		script{
		    def status = sh( script: "curl -o /dev/null --silent -Iw '%{http_code}' ${NEXUS_REPO}/${Globals.package_file}",
				    returnStdout: true).trim()
		    if ( status == '200' ) {
			if ( Globals.deploy_stages == "deploy" ){
			    error (message: "${Globals.package_file} already exists on Nexus.")
			} else {
			    unstable (message: "${Globals.package_file} already exists on Nexus.")
			}
		    }
		}
	    }
	}

	stage('Deploy Package') {
	    when {
		expression { return Globals.deploy_stages == "deploy" }
	    }
            steps {
		echo "Upload Package to Nexus"
		script{
		    withCredentials([usernamePassword(credentialsId: 'r-nexus',
						      passwordVariable: 'NXPASS',
						      usernameVariable: 'NXUSER')]) {

			def status = sh( script: "curl --user " + NXUSER + ":" + NXPASS + " -w '%{http_code}' --upload-file source/${Globals.package_file} ${NEXUS_REPO}/${Globals.package_file}",
					returnStdout: true).trim()
			echo status

			if ( status != '200' ){
			    error (message: "Upload of ${Globals.package_file} to Nexus FAILED (with status ${status}).")
			} else {
			    echo "Upload of ${Globals.package_file} to Nexus SUCCESSFUL."
			}
		    }
		}
	    }
	}

	stage("Publish documentation") {
	    when {
		expression { return Globals.documentation_publish == true }
	    }
	    steps {
		script {
		    withCredentials([string(credentialsId: 'documentation-main-prod-token',
                                            variable: 'DOC_TOKEN')]) {
			sh "mchbuild -c ${WORKSPACE}/source/Jenkinsfile_config.yml \
                                     -s project=${Globals.package_name} \
                                     -s docSrc='${WORKSPACE}/docs/' \
                                     jenkins.deploy.publish_docu"
                    }
		}
	    }
	}
    }

    post {
	always {
            echo "Build stage complete"
	}
	cleanup {
	    echo "Cleanup workspace"
	    cleanWs(deleteDirs: true,
		    patterns: [[pattern: '*@tmp', type: 'INCLUDE']])
	    echo "Monitor workspace size"
	    script {
	        sh '''#!/bin/bash -l
                   shopt -s dotglob; du -sh * | sort -h
                   '''
	    }
	}
	failure {
            echo "Build failed"
            updateGitlabCommitStatus name: 'Build', state: 'failed'
            emailext(subject: "${currentBuild.fullDisplayName}: ${currentBuild.currentResult}",
		     body: "Job ${currentBuild.currentResult}: ${JOB_NAME} #${BUILD_NUMBER}\n\n"+
		     "Check console output at ${BUILD_URL} to view the results. \n\n",
                     recipientProviders: [[$class: 'DevelopersRecipientProvider'], [$class: 'RequesterRecipientProvider']],
		     attachLog: true)
	}
	aborted {
            echo "Build aborted"
            updateGitlabCommitStatus name: 'Build', state: 'canceled'
            emailext(subject: "${currentBuild.fullDisplayName}: ${currentBuild.currentResult}",
		     body: "Job ${currentBuild.currentResult}: ${JOB_NAME} #${BUILD_NUMBER}\n\n"+
		     "Check console output at ${BUILD_URL} to view the results. \n\n",
                     recipientProviders: [[$class: 'DevelopersRecipientProvider'], [$class: 'RequesterRecipientProvider']],
		     attachLog: true)
	}
	success {
            echo "Build succeeded"
            updateGitlabCommitStatus name: 'Build', state: 'success'
	}
    }
}
