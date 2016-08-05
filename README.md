#README for survey-project

##Directories
* code : scripts for running analyses and producing graphics
* data : raw data from survey
* output : product of code, such as formatted data and pdf figures; contents not 
under version control

--------------------------------------------------------------------------------
##code
* analyses-bioinfo-interest.R : general assessment of interest in 
bioinformatics
* analyses-demographics.R : basic demographics of respondents
* analyses-format-prefs.R : test for significant preference in training 
format
* analyses-library-resource-use.R : test for differential use of library 
resources
* analyses-response-through-time.R : responses through time, in relation 
to e-mail reminders
* analyses-scripting-dispersion.R : distribution of preferences for 
scripting training
* analyses-topic-preference-sums.R : sum of preference scores (Likert 
scale)
* analyses-topic-preferences.R : test for significant preference 
difference among training topics
* data-processing.R : data processing of raw results into format for all
downstream analyses
* graphs-format-preferences.R : graphing training format preferences
* graphs-response-through-time.R : graphing responses through time
* graphs-topic-correlations.R : graphing correlation among training 
topics
* graphs-topic-interests.R : graphing topic interest levels

--------------------------------------------------------------------------------
##data
* survey-results-processed.txt : tab-delimited text file of survey results, some 
cleaning of Qualtrics output occurred (e.g. standardizing college names). There 
is some additional processing by code/data-processing.R, which produces 
a data frame called `results`, stored in output/results-processed.RData. That 
file should be loaded for all downstream analyses (rather than using the 
original, raw data).