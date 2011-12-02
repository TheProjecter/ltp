#==========================================================
# CLASS DEFINITION *** CLASS DEFINITION *** CLASS DEFINITION
#==========================================================

#setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))

setClass("ltp.object", 
  representation(
    values = "data.frameOrNULL",
    models = "listOrNULL", 
	BestModel="characterOrNULL",
	rule="characterOrNULL",
	ruleSetting="listOrNULL"
  ),
  prototype = list(
	values = NULL,
    models = NULL, 
	BestModel=NULL,
	rule="BestAIC",
	ruleSetting=NULL
  )
)

#==========================================================
# Function "show" prints a "ltp.object" object
#==========================================================
setMethod("show", "ltp.object", function(object)
{
  cat("ltp-object\n values:\n")
  print(rbind(head(object@values),etc="..."))
  cat(" Models: ") 
  cat(paste(names(object@models),collapse=", "))
  cat(paste("\n Rule for Suggested Model: ",object@rule,sep=""))
  cat("\n Rule's setting: ")
  cat(paste(names(object @ruleSetting),object @ruleSetting,sep="=",collapse=";  "))
  cat("\n")
})

setGeneric("summary")
setMethod("summary", "ltp.object", function(object, ...)
{
print(getModelsSummary(object@models)) 
})


#==========================================================
# Functions to extract relevant information from 
# a ltp.object object
#==========================================================
setGeneric("result", function(object, ...) standardGeneric("result"))
setMethod("result", "ltp.object",
  function(object) { 
    show(object)
})

