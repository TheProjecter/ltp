
ltp.normalizeData <- function(product, range, NA2value=0,period.start,period.freq,increment,period.end) {
  #increment non lo sto usando. a cosa serviva?
  #i dati che arrivano gia' con un NA vengono rimpiazzati subito. se is.na(NA2value) non cambia nulla
  product[is.na(product),]=NA2value
  #trova l'effettiva data di partenza: minima data su product e minore di period.end 
  period.start = Period.FromNumber(
                    min(Period.ToNumber(period.end,period.freq), max(Period.ToNumber(period.start,period.freq),min(Period.ToNumber(rownames(product),period.freq)))),period.freq)
  n <- Period.ToNumber(period.end,period.freq)-Period.ToNumber(period.start,period.freq) +1
  
  times <- Period.BuildRange(period.start, period.freq, n, shift=0)
  productnew=data.frame( rep(NA2value, len = length(times) ))
  rownames(productnew)=times
  colnames(productnew)=colnames(product)
  if(n==0) { #se non ci sono dati ne creo uno fittizio
	productnew[1,]=NA2value
	rownames(productnew)=Period.ToString(period.end)
    }
  
  
  #mette i dati originali dove ci sono.
  ValidValues=intersect(rownames(product),rownames(productnew)) #se ci sono prezzi con data successiva li toglie
  if (length(ValidValues)>0)   productnew[ValidValues,]=product[ValidValues,]
  
  #se NA2value==NA ci mette la media delle stagioni presenti nel dataset.
  if(any(is.na(productnew))){
	for(i in 1:period.freq) {
		if(length(grep(paste("-",i,sep=""),rownames(productnew)))>0)
		productnew[grep(paste("-",i,sep=""),rownames(productnew)),][is.na(productnew[grep(paste("-",i,sep=""),rownames(productnew)),])] = 
		mean(productnew[grep(paste("-",i,sep=""),rownames(productnew)),],na.rm=TRUE)	
	}
  }
  #se proprio non c'e' nulla da fare ci sbatte uno 0
  productnew[is.na(productnew)]=0
  	
    productnew[productnew < range[1], ] = range[1]
    productnew[productnew > range[2], ] = range[2]
    return(list(product=productnew,start=period.start))
	
}
