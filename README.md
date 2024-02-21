## Summary

A NFL team comparison tool based on a statistical model that utilizes 100 key features to develop strategic insights. Users select a team, and the tool generates the 10 most similar teams from the past 18 seasons

## Getting Started
A step-by-step guide to running the Shiny App in R

1. Make sure that you have R or RStudio [successfully downloaded](https://rstudio-education.github.io/hopr/starting.html) on your location machine
1. Install the `shiny` library to R: 
```
> install.packages("shiny")
```
3. Load the shiny library: 
```
> library(shiny)
```
4. Run the shiny app: 
```
> runGitHub("NFL-Team-Similarity-Tool", "callancapitolo17")
```
**Note**: It may take 3-5 minutes for the Shiny app to open as all the data since the 2006 NFL season needs to be downloaded and some packages may need to be downloaded as well.

## Description
The app looks at 100 different features mainly focused on offensive and defensive efficiency.  The app then uses these features and nearest neighbor search using a k-d tree to determine the 10 most similar teams. It then outputs the reference team and the 10 most similar teams along with their similarity score and summary statistics surrounding efficiency on both sides of the ball.



## Team Similarity Tool Preview

https://github.com/callancapitolo17/NFL-Team-Similarity-Tool/assets/155870379/9b0ec0a0-fda9-439d-ac02-bba90a6fe67f
