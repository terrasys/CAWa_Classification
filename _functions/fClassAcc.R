#######################################################################################################
#######################################################################################################
#######################################################################################################
##Function for the evaluation of classifications 
##source: 
#http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html 
#https://github.com/saidbleik/Evaluation/blob/master/eval.R
#######################################################################################################
#######################################################################################################
#######################################################################################################
#input actual & predicted vectors or actual vs predicted confusion matrix 
fClassAcc = function(actual=NULL, 
                    predicted=NULL, 
                    cm=NULL){
  if(is.null(cm)) {
    naVals = union(which(is.na(actual)), which(is.na(predicted)))
    if(length(naVals) > 0) {
      actual = actual[-naVals]
      predicted = predicted[-naVals]
    }
    f = factor(union(unique(actual), unique(predicted)))
    actual = factor(actual, levels = levels(f))
    predicted = factor(predicted, levels = levels(f))
    cm = as.matrix(table(Actual=actual, Predicted=predicted))
  }
  
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the classes
  q = colsums / n # distribution of instances over the predicted classes
  
  #accuracy
  accuracy = sum(diag) / n
  
  #per class prf
  recall = diag / rowsums
  precision = diag / colsums
  f1 = 2 * precision * recall / (precision + recall)
  
  #random/expected accuracy
  expAccuracy = sum(p*q)
  #kappa
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)


  classNames = names(diag)
  if(is.null(classNames)) classNames = paste("C",(1:nc),sep="")
  
  AccMetrics = rbind(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    Kappa = kappa)
  colnames(AccMetrics) = classNames
  return(list(ConfusionMatrix = cm, AccMetrics = AccMetrics))
}
