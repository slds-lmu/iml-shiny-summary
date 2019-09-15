# iml--summary

This dashboard provides an iml-summary for your data.

## File Structure

The general structure of this repo does have the following tree :
```
├───app 
├───pdPlots
├───example
│   └───PrediObj
│   └───test_model
├───www
 |   └─── Cover
 |
└───libraries
```

### How to use

You can use this app for your predictor object (R6Class object), which should hold any machine learning model (mlr, caret, randomForest, ...) 
and the data to be used of analysing the model. The package iml and mlr can help you.

There is a right-click menu. Click Global Effects, save your predictor object as .RDS file and upload it. Then, 
you get a table in few minutes, which includes variables' names,corresponding value ranges, partial dependence 
plots(PDPs) and their feature importance. Click Local Interpretation, you will see the data set on the top. Then, if you select a certain row, the corresponding
shapley values and its plot will be shown automatically as follows.


## Example

You can use `PrediObj.RDS` in the file example, which is created from code in ` test_model.R`. The Boston Housing data is fitted with Gradient Boosting Machine(Regression).

