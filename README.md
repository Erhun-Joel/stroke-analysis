# Stroke Prediction Project
This project is a simulation of how one would go about identifying the individuals that are likely to have stroke, taking two assumptions into account:
- The data allowed to be used in initial modeling is sparse and general for the sake of patient privacy.
- There is insufficient resources that can be spent in passing every patient through highly accurate prediction methodology

These assumptions would aid in making the decision to increase sensitivity at the expense of specificity, with those testing positive to go a second more intensive testing method.
## Data Source
The data used is gotten from Coursera and Kaggle. To see the specific Kaggel web location, click [here](https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset).
## Methodology
The methodology used is quite standard, involving:
- Data sourcing and cleaning
- Exploration
- Logistic model development
- Random Forest model development
- Shiny App development and deployment

The Reciever Operating Characteristics or roc was given preference above other metrics due to its ability to assess a models differentiating ability.

## Shiny Deployment
The Shiny Application is used to predict the probability of having a stroke as well as providing recommendations using gemini-1.5-flash. This deployed tool can be viewed and used by clicking this [link](https://qcidhj-erhun-igbinnosa.shinyapps.io/shiny_deployment/).

## Model Development and Computation
Most models maxed out around an roc score of 0.83. A major cause of this was imbalanced stroke occurances. While techniques such as SMOTE was used, they produced rather disappointing results.
Therefore a special model training technique was tried involving selective resampling.
- Special folds where created which included every stroke occurance in the train data and bootstraped proportions of no-stoke occurances.
- The Random Forest spec was trained on the folds to produce biased models.
- Every of such bias models was then used to give individual predictions
- The mean of each individual prediction became the overall output.

This system worked quite well on training data, but more poorly in testing, suggesting overfitting. It also proved computationally expensive as compared to the scope of this project. Later work on this method will be done and updates given.

## Disclaimer
This project is not to be used to diagnose medical conditions. It is merely a sample case study. If having stroke related issues, please visit the nearest medical professional.
