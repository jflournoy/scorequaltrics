# Score Qualtrics R Package

The `scorequaltrics` R package provides functions to interact with the [Qualtrics](http://www.qualtrics.com) online survey tool. It requires that your account have API access. The latest development version can be installed from Github with the `devtools` package.

	> require(devtools)
	> install_github('jflournoy/scorequaltrics')
	
	> ls('package:qualtrics')
	 [1] "addRecipient"           "createPanel"            "getPanel"              
	 [4] "getPanelMemberCount"    "getPanels"              "getRecipient"          
	 [7] "getSurvey"              "getSurveyName"          "getSurveyResults"      
	[10] "getSurveys"             "sendReminder"           "sendSurveyToIndividual"
	[13] "varEntryDialog" 

This version of the package now uses the [qualtRics](https://github.com/ropensci/qualtRics) package, which is installed as a dependency to this pacakge, for api access to your data. As such, please follow the link to that package to learn how to set up your credentials.

After you have set up the qualtRics package, you can continue to follow this guide.

You can retrieve the list of survey in your account with the `getSurveys` function (note output has been truncated).

	> getSurveys(login$Username, login$Password)
    	responses SurveyType               SurveyID                            SurveyName
               1         117  SS SV_39RKJNe1htAdR4g  Faculty Survey of Student 	Readiness
               2         400  SS SV_3rQruV2eBPG7uFS      Staff Satisfaction Survey - 2011

