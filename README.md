# Bayesian path analysis of life-history traits in a capital breeder

## Project description
We have developed a mechanistic path model to disentangle the direct and indirect effects of density dependence and environmental variability on body mass and reproductive success in a capital breeder. All models and visualisations are run in R.
This study focuses on one extensively monitored herd of semi-domestic reindeer in northern Norway. We included individuals of known age and older than 1 year, with at least one complete set of observations on three consecutive occasions: in autumn, the subsequent spring, and the following autumn. This resulted in a sample size of 814 observations from 235 females in 18 years.
To assess the contribution of direct, indirect, and total effects of environmental variables and population density on seasonal body mass and reproductive success, we built a mechanistic path model. The path model consisted of three sub-models, one for female spring body mass, one for reproductive success, and one for female autumn body mass. The model allowed for the mediating effect of spring body mass on reproductive success and the mediating effect of spring body mass and reproductive success on autumn body mass.
The path model was first fitted in piecewiseSEM to test the conditional independence claims stated by the path model. Then the path coefficients were estimated in a Bayesian framework in STAN through the brms-package.

## Data
The data is in the file "data.txt".

## Scripts
To run through the scripts you first need to run script 01 to load and formate the data. Script 02 runs the models.

Script 03 shows the diagnostics of the models, using both ShinyStan and by doing diagnostic plots..

Script 04 to 11 generates the figures S2 to S13, each script loads the necessary model run to generate the figures. The packages need to loaded from script 01.

## Output
All the output is in the repository as png or pdf.

## Acknowledgments
I would like to express my gratitude to my supervisors Torkild Tveraa, Sandra Hamel, John-André Henden, Audun Stien and Nigel Gilles Yoccoz or their guidance and support throughout this project.

## Contact
* Mikaela Simmonds
* mikaela.tilman@nina.no


