# Big Data / Visualization Shiny Application

   ## Group Members

| Name                      | Github username           |
|---------------------------|---------------------------|
| ANGULO MONTES LUIS EDUARDO| LuisEduardoAngulo         |
| BORRERO GARCIA FRANCISCO  | Macvayne                  |
| TAHIRI ALAOUI OUSSAMA     | oussama-talaoui           |

[Practical Work Github repository](https://github.com/oussama-talaoui/visualization)

## About the Shiny application
The objective of this practical work is to put into practice the concepts learnt
during the theory classes and to get proficiency in the use of some tools that will allow us to
interactively analyze a our dataset. Allowing us to develop our own application that implements interactive visualizations, to solve some questions about our chosen dataset.
to conduct this kind of analysis.

The practical work consists in the following exercises:
## First exercise: Shiny Tutorial
[Link to repository.](https://github.com/oussama-talaoui/Big-Data-Visualization)

## Second exercise: Design of a new interactive data analysis tool
[Link to repository.](https://github.com/oussama-talaoui/visualization)
- [X] Proposing our own [dataset](https://www.kaggle.com/noahgift/social-power-nba#nba_2017_br.csv).
- [x] Using of 3+ different types of plots.
- [X] Filtering and aggregation is possible in order for the user to be able to focus on the information he finds more interesting.
- [x] Using multiple (synchronized) views.
- [x] Integration some data analysis algorithm (machine learning).
- [x] Using specific arrangements for the data under consideration.
- [X] Sharing the applications:
  - [x] Sharing the Shiny app as R scripts.
  - [x] Sharing the Shiny app as a web page, to run it we use the fellowing commands:
```R 
packages = c("shiny", "ggplot2", "readr", "dplyr", "plyr")
install.packages(packages, repos = "https://cran.rstudio.com/")
library(shiny)
runGitHub("visualization", "oussama-talaoui")
```
