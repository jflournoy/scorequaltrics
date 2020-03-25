# Score Qualtrics R Package

The `scorequaltrics` R package provides functions to interact with the [Qualtrics](http://www.qualtrics.com) online survey tool. It requires that your account have API access. The latest development version can be installed from Github with the `devtools` package.

	> require(devtools)
	> install_github('jflournoy/scorequaltrics')

This version of the package now uses the [qualtRics](https://github.com/ropensci/qualtRics) package, which is installed as a dependency to this pacakge, for api access to your data. As such, please follow the link to that package to learn how to set up your credentials.

There is an example credentials file in `credentials.yaml.DEFAULT`. Copy this file to a secure location and replace the example values with your values. See Qualtrics help for finding these values. Here is a brief example of usage:

```r
library(scorequaltrics)

loaded <- scorequaltrics::creds_from_file('credentials.yaml.DEFAULT')

if(loaded){
  survey_names_all <- scorequaltrics::get_surveys()
  survey_data <- scorequaltrics::get_survey_data(survey_names_all, 
                                                 pid_col = 'ID')
}
```

