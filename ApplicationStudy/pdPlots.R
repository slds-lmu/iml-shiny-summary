pdPlot = function(mod, x, target){
  
  # IML basis model
  p = PrediObj
  
  dat.type = as.data.frame(sapply(x, class))
  feat.name = as.data.frame(names(x))
  
  feat.name$dat.type = dat.type
  
  # reorder variables -- name
  feat.name = feat.name[order(feat.name$name), ]
  
  names(feat.name) = c("feat", "dat.type")
  feat.name.pure = as.vector(feat.name$feat)
  
  # create PDPsr
  pd.pl = lapply(1:length(feat.name.pure), function(i){
    pd = FeatureEffect$new(p, feat.name.pure[[i]], method = "pdp")
    pd = pd$results
    feat = colnames(pd)[1]
    colnames(pd) = c("org.val", "yhat", "type")
    
    
    dat.type = feat.name[feat.name$feat == feat, ]$dat.type # type of this i-th variable
    if (dat.type == "factor"){
      if (nrow(pd) > 4) {
        plot = ggplot(pd, aes(reorder(org.val, yhat), yhat)) + 
          geom_bar(stat="identity", fill="#337ab7") + 
          theme_classic() +
          ylab("Rent / sqm") + xlab(feat.name$feat[i]) +
          ggtitle("Partial Dependence Plot") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(hjust = 0.5)) +
          geom_text(aes(label = round(yhat, 2), y = yhat + 0.02),
                    color = "white", vjust = 2)  
      } else {
        plot = ggplot(pd, aes(reorder(org.val, yhat), yhat)) + 
          geom_bar(stat="identity", fill="#337ab7") + 
          theme_classic() +
          ylab("Rent / sqm") + xlab(feat.name$feat[i]) +
          ggtitle("Partial Dependence Plot") +
          theme(plot.title = element_text(hjust = 0.5)) +
          geom_text(aes(label = round(yhat, 2), y = yhat + 0.02),
                    color = "white", vjust = 2)  
      }
    } else {  ## not "numeric"
      plot = ggplot(pd, aes(org.val, yhat)) + 
        geom_line(colour="#337ab7") + 
        theme_classic() +
        ylab("target") + xlab(feat.name$feat[i]) +
        ggtitle("Partial Dependence Plot") +
        theme(plot.title = element_text(hjust = 0.5))  
    }
    plot 
  })
  return(pd.pl)
}
