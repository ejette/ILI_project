#Building Influenza Surveillance Systems in the United States
The goal of this project is to optimize the healthcare provider reporting system the Centers for Disease and Control use to track influenza in the US population. We have a 'true measure' of flu in the population, and we build  build linear regression models using forward selection using outpatient healthcare providers as possible predictor variables to predict the 'true neasure' of flu.

##Scraping and Preparing the Influenza and Pneumonia Mortality Data

###Scraping and Formatting
Every week, 122 cities across the United States report to the Centers for Disease Control (CDC) the number of deaths that have occurred in their cities, and they also specifiy the number of deaths for which pneunomia or influenza is a contributing cause. The CDC then publishes these data online. We use this data as the 'true measure' of flu in the United States. 

The python program `scraper/scrape_weekly_flu_deaths.py` scrapes the weekly deaths from the <a href="http://wonder.cdc.gov/mmwr/mmwrmort.asp">Mortality and Morbidity weekly report</a>. It scrapes all the data, and then `scraper/format_weekly_flu_deaths.py` extracts pneumonia and influenza deaths and replaces '-' characters with '0' (this means no deaths occurred that week) and 'U' characters with 'NA' ('U' means that the data were not available that week). The formatted death data are stored as a comma separated file and located in `data/regression/mmwr.csv`.

###Aggregating
The R script `data_prep/regression/deaths_data_prep.R` aggregates all the 122 cities death data to the national-level and to the <a href="http://www.hhs.gov/about/regionmap.html">HHS regional level</a>.

We are only interested in optimizing surveillance systems for the flu season, which means we remove observations greater than week 20 and less than week 40 of each year in the model building phase.

##Preparing the ILINet Data

Every week, outpatient health care providers voluntarily report to the CDC the number of patients they have treated and the number of patients they treated with Influenza-Like Illness symptoms. The entire network of these providers is called ILINet. We have ILINet data for the years of 2008 - 2012. Unfortunately, these data are not for public use and were acquired through a defined partnership with the CDC.
 
The `data_prep/regression/flu_data_prep.R` prepares the ILINet data for use in our analysis. Due to a lot of missing data, we aggregated all the data to the county level. This required processing the county variable in order to make all county names uniform. We replaced the remaining of the missing data with zeros, but a future extension of this project would be to impute the missing data.

As above, we are only interested in optimizing ILINet for the flu season, so we exclude observations for which the week was greater than 20 and less than 40 of each year.

##Linear Regression using Forward Selection
We build linear regression models using forward selection. The number of weekly pneumonia and flu deaths is the dependent variable, and the pool of candidate independent variables consists of the ILINet provider data aggregated to the county level.

###Model-Building with Mode Lag
For each region, we build a model using provider data that is lagged the mode optimal lag for that region. By that, I mean for each provider we ran six separate univariate regressions with a week lag of 0 through 5 weeks and record the R<sup>2</sup> value for each regression. We then record the lag that resulted in the highest R<sup>2</sup> for that provider. For example, for provider i, region j, and a lag of 1 week, we slide provider j's data down by one week, run the regression with region j's deaths and record the R^{2}. We do this for weeks 0, 2, 3, 4, and 5 as well. All of this analysis is performed in `data_prep/regression/lag_analysis.R`.

To decide which lag to use for that region, we tallied up the optimal lags for all the providers, and chose the mode lag for the model-building for that region (e.g., if the mode lag for the national-level death data was 2 weeks, we lagged all the provider data by 2 weeks and and then built the model for the nation using the 2-week-lagged provider data). 

The model-building is implemented in `model/regression/optimization_mode_lag.R.`

###Optimization with Dynamic Lag
For each region we build a model that can dynamically choose the lag and provider for each region. This essentially means that there are 6 near-identical copies of each provider, differing only in that they are lagged 0, 1, 2, 3, 4, or 5 weeks. At each step the model chooses the lag and provider combination that boosts the R<sup>2</sup> the most.

The model-building for all regions is implemented in `model/regression/optimization_dynamic_lag.R.`

##Logistic Analysis
In order to analyze the key factors in the diversity of the providers chosen by during the model-building process, we built a logistic model for each of the HHS regions. The response variable was whether a county was picked during the model-building process (1 if chosen, 0 otherwise).
###Preparing the Response Variable
For each region, we build a response variable. There are as many rows are there are counties in the ILINet data. A row is equal to 1 for a region if that provider county was picked for that region and 0 if it was not picked for that region (`data_prep/logistic/make_response_variable_dynamic_lag.R` and `data_prep/logistic/make_response_variable_mode_lag.R`)

###Preparing Predictors
For each county that had contained at least one ILINet provider, we built the following predictor variables (the script that prepares the variales is listed in parethesis following the description):
<li> Geographic information -- which region a provider county is located (national level) or whether a provider is in the region, adjacent to the region, or neither (regional level) (`data_prep/logistic/make_geo_predictor.R`)
<li> Percentage of patients over 65 (`data_prep/logistic/make_age_reporting_predictors.R`)
<li> Reporting rate of providers (both during flu season and overall) (`data_prep/logistic/make_age_reporting_predictors.R`)
<li> Absolute Humidity of county (`data_prep/logistic/make_absolute_humidity_predictor.R`)
<li> Population of county (`data_prep/logistic/make_pop_predictors.R`)
<li> Average weekly number of patients treated (`data_prep/regression/ILI_data_prep.R`)
<li> Number of providers per county (`data_prep/regression/ILI_data_prep.R`)

###Running the Analysis
The logistic analysis using the providers picked using the dynamic lag model is in `model/logistic/logistic_analysis_dynamic_lag.R`, and the logistic analysis using the providers picked using the mode lag model is in `model/logistic/logistic_analysis_mode_lag.R`


