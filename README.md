## Summary

A NFL team comparison tool based on a statistical model that utilizes 100 key features to develop strategic insights. Users select a team, and the tool generates the 10 most similar teams from the past 18 seasons

## Getting Started
A step-by-step guide to running the Shiny App in R

1. If you do not have R installed already on your local machine, follow this [guide](https://rstudio-education.github.io/hopr/starting.html) (you do not need to install RStudio)
2. From the R command line, install the [shiny](https://cran.r-project.org/web/packages/shiny/index.html) library to R: 
```
> install.packages("shiny")
```

3. Frome the R command line, load `shiny`: 
```
> library(shiny)
```
4. From the R command line, run the app: 
```
> runGitHub("NFL-Team-Similarity-Tool", "callancapitolo17")
```
It takes 3-4 minutes for the Shiny app to open as all the data since the 2006 NFL season needs to be downloaded as well as some additional packages. You will know it has completed when a new window opens with the app, or you see on the command line `Listening on http://127.0.0.1:5384`

## Description
The app looks at 100 different features mainly focused on offensive and defensive efficiency.  The app then uses these features and nearest neighbor search using a k-d tree to determine the 10 most similar teams. It then outputs the reference team and the 10 most similar teams along with their similarity score and summary statistics surrounding efficiency on both sides of the ball.
* TODO: add a link for k-d tree, as well a brief description of what it is


## Team Similarity Tool Preview

https://github.com/callancapitolo17/NFL-Team-Similarity-Tool/assets/155870379/9b0ec0a0-fda9-439d-ac02-bba90a6fe67f

## Future Work

Having a section like this is nice as it allows you to reference this when the interviewer inevitably asks "What would you do next?"
