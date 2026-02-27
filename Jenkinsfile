class Globals {
    // docker: builder image
    static String docker_base = 'climateanalysis/cat-build/ubuntu-noble/r-4.4'
    static String docker_version = 'latest'
    static String docker_image = ''

    // This parameters will be adapted during preparation stage
    // package name and version (from DESCRIPTION file)
    static String package_name = ''
    static String package_version = ''
    static String package_file = ''

    // deployment stages setup (check vs deploy vs abort)
    static String deploy_stages = 'check'

    // documentation tag (develop vs main)
    static String documentation_tag = 'develop'

    // publish documentation
    static boolean documentation_publish = false
}

@Library('dev_tools@main') _
pipeline {
    agent { label 'docker' }

    environment {
	NEXUS_REPO = "https://nexus.meteoswiss.ch/nexus/repository/r-mch/src/contrib"
	DOCKER_REPO = "dockerhub.apps.cp.meteoswiss.ch"
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
		    Globals.package_name = sh( script: 'grep "Package:" source/DESCRIPTION | cut -d":" -f2', returnStdout: true).trim()
		    Globals.package_version = sh( script: 'grep "Version:" source/DESCRIPTION | cut -d":" -f2', returnStdout: true).trim()

		    // set package name
		    Globals.package_file = "${Globals.package_name}_${Globals.package_version}.tar.gz"

		    echo "Package specs: ${Globals.package_file}"

		    // get env variables from git
		    echo "GIT specs: ${env.BRANCH_NAME}"

		    // set docker image name
		    Globals.docker_image = "${DOCKER_REPO}/${Globals.docker_base}:${Globals.docker_version}"

		    echo "Docker specs: ${Globals.docker_image}"

		    // decide what to do in next stages
		    if (env.BRANCH_NAME != "main" && env.BRANCH_NAME != "master"){
			echo "Do only package check for ${Globals.package_name} as branch is ${env.BRANCH_NAME} (--> not main or master)."
			Globals.deploy_stages = "check"
			Globals.documentation_publish = false
		    } else {
			echo "Do package check, deploy and docu for ${Globals.package_name} as branch is ${env.BRANCH_NAME}."
			Globals.deploy_stages = "deploy"
			Globals.documentation_publish = true
			Globals.documentation_tag = env.BRANCH_NAME
		    }

		    echo "Deploy specs: ${Globals.deploy_stages}"
		    echo "Docu specs: ${Globals.documentation_publish} with docu_tag ${Globals.documentation_tag}"
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
            withCredentials([usernamePassword(credentialsId: 'openshift-nexus',
			 			      passwordVariable: 'NXPASS',
						      usernameVariable: 'NXUSER')]) {
			runWithDocker "${Globals.docker_image}", "/src/scripts/start_r_builder.bash docu_check", "--env MCHDWH_CLIENTSECRET=\$MCHDWH_CLIENTSECRET --env GRIDGET_CLIENTSECRET=\$GRIDGET_CLIENTSECRET --env PRODUCT_PROVIDER_CLIENTSECRET=\$PROVIDER_CLIENTSECRET --env CI=TRUE", true
                    }
		}
            }
	    post{
		always {
		    echo "Check complete"
		    junit allowEmptyResults: true, keepLongStdio: true, testResults: "source/${Globals.package_name}.Rcheck/tests/testthat/junit_result.xml"
		    archiveArtifacts allowEmptyArchive: true, artifacts: "source/${Globals.package_name}.Rcheck/*.log", followSymlinks: false
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
		    try {
			withCredentials([usernamePassword(credentialsId: 'r-nexus',
							  passwordVariable: 'NXPASS',
							  usernameVariable: 'NXUSER')]) {
            		    sh """#!/bin/bash
                            cd source
                            if [ -f ${Globals.package_file} ]; then
                               status=\$(curl --user ${NXUSER}:${NXPASS} -o /dev/null --silent -Iw "%{http_code}" ${NEXUS_REPO}/${Globals.package_file})
                               if [ "\$status" -eq 200 ]; then
                                  echo "PROBLEM: "${Globals.package_file}" already exists on Nexus."
                                  echo "--> Please check your package version (DESCRIPTION)"
                                  exit 1
                               fi
                            else
                               echo "${Globals.package_file} does not exist"
                               exit 1
                            fi
                            cd ${WORKSPACE}
                            """
			}
		    } catch (err) {
			if ( Globals.deploy_stages == "deploy" ){
			    error(message: "ERROR: ${Globals.package_file} already exists on Nexus.")
			} else {
			    unstable(message: "WARNING: ${Globals.package_file} already exists on Nexus.")
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
            		sh """#!/bin/bash
                        cd source
                        if [ -f ${Globals.package_file} ]; then
                            status=\$(curl --user ${NXUSER}:${NXPASS} -w "%{http_code}" --upload-file ${Globals.package_file} ${NEXUS_REPO}/${Globals.package_file})
                            if [ "\$status" -ne 200 ]; then
                                echo "Error: curl upload failed due to server return code - \$status"
                                exit 1
                            fi
                        else
                            echo "${Globals.package_file} does not exist"
                        fi
                        cd ${WORKSPACE}
                    """
		    }
		}
	    }
	}

	stage('Publish Documentation') {
	    when {
		expression { return Globals.documentation_publish == true }
	    }
            environment {
		PATH = "${HOME}/tools/openshift-client-tools:$PATH"
		KUBECONFIG = "${WORKSPACE}/.kube/config"
            }
            steps {
		withCredentials([string(credentialsId: "documentation-main-prod-token",
					variable: 'TOKEN')]) {
                    sh "oc login https://api.prod.cp1.meteoswiss.ch:6443/ --token \$TOKEN"
                    publishDoc "${WORKSPACE}/docs/", Globals.package_name, Globals.package_version, 'R', Globals.documentation_tag
		}
            }
            post {
		cleanup {
                    sh 'oc logout || true'
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
