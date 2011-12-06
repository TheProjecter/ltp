
#################UTILITIES

##################################
 ltp.GetModels <- function(what=NULL) {
	model=data.frame(id=c("lm", "arima","es", "trend", "mean","naive"),
			name=c("Linear", "Arima", "ExpSmooth", "Trend", "Mean","Naive"),
			color = c("green", "red", "blue", "gray", "black","yellow"),
			legend= c("Linear","Arima" , "Exp.Smooth" , "Trend" ,"Mean", "Naive"))
	rownames(model)=model$id
	
	if(!is.null(what)) { 
		names=model$id
		model=as.character(model[,what])
		names(model)=names
		}
  
  model
}
#es: ltp.GetModels("es")

#########################

getMaxJump <- function(product, period.freq, pred.mod)  {
  res=exp(abs(log(
    mean(product[max(1,(nrow(product)-period.freq+1)):nrow(product),1],na.rm=TRUE)/
	mean(pred.mod[1:min(period.freq,nrow(product))],na.rm=TRUE))
	))
  if(is.null(res)) {
	res = NA 
  } else {
	if(length(res)!=1) res=1
	if(any(is.na(res))) res=1
  }
  res
}

getVarCoeff <- function(product, pred.mod){
   sd(c(as.vector(pred.mod),unlist(product)),na.rm=TRUE)/mean(c(as.vector(pred.mod),unlist(product)),na.rm=TRUE)
}

getModelsStatistics <- function(object,whichStats=c("AIC","IC.width","R2","VarCoeff","maxJump")){
	res=c()
	for (i in names(object))
		res=rbind(res,object[[i]][whichStats])
	rownames(res)=names(object)
	return(res)
}

getRetainedModelsByRule <- function(models,ruleSetting){
	retain=ifelse(getModelsStatistics(models,"VarCoeff")<ruleSetting$rule.noMaxCVOver,TRUE,FALSE)*
	      ifelse(getModelsStatistics(models,"maxJump") <ruleSetting$rule.noMaxJumpOver,TRUE,FALSE)
	retain
}

getBestModel <- function(models,rule,ruleSetting){
	stats=getModelsStatistics(models,switch(rule, BestIC="IC.width",BestAIC="AIC"))
	retain=getRetainedModelsByRule (models,ruleSetting)
  ID.model <- which.min(stats[retain]) ;
  ID.model <- ltp.GetModels("name")[ID.model]
  if((is.null(ID.model)||(length(ID.model)==0))||is.na(ID.model))  ID.model <-  "Naive"
  ID.model
  }