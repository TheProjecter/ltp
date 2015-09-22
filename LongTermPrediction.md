## Models ##

  * Arima
  * Exponential smoothing
  * Linear
  * Mean
  * Trend

![http://strategico.googlecode.com/svn/trunk/doc/ltp/ltp_bestfit.png](http://strategico.googlecode.com/svn/trunk/doc/ltp/ltp_bestfit.png)

![http://strategico.googlecode.com/svn/trunk/doc/ltp/ltp_all.png](http://strategico.googlecode.com/svn/trunk/doc/ltp/ltp_all.png)

## R 2 ##
L'[R2](https://code.google.com/p/ltp/source/detail?r=2) è un indice statistico che rappresenta il rapporto tra varianza spiegata dal modello e varianza totale dei dati.L'indice è compreso tra 0 e 1.
  * Se l'indice è prossimo a 1 il modello riesce a cogliere gran parte della variabilità dai dati;
  * in caso contrario (indice prossimo a 0) modello il modello potrebbe non essere adeguato.


## AIC ##
L'AIC è un indice statistico utilizzato per valutare la bonta di un modello. L'indice prevede una penalizzazione crescente all'aumentare del numero di parametri utilizzati.
  * In generale il modello che presenta AIC inferiore è preferibile.


## IC.width ##
IC.width rappresenta la media delle ampiezze degli intervalli di confidenza di ogni previsione fatta.
Una previsione caratterizzata da piccolo IC.whith risulterà soggetta a minore variabilità rispetto ad una previsione con IC.whith maggiore; in generale quindi la stima
risulterà più precisa nel caso di basso IC.whith.

## MaxJump ##
Rappresenta la massima proporzione di scostamento (valore assoluto) tra le osservazioni dei periodi dell'ultimo anno e le corrispondenti previsioni dell'anno successivo.
Esempio:
  * Osservati 2010-1 = 10 e 2010-2 = 20
  * Previsti 2011-1 = 11 e 2011-2 = 19
  * Proporzione degli scostamenti: |"Previsti 2010-1"/ "Osservati 2010-1" - 1 | (|x| è il valore assoluto di x)
  * semestre 1: |11/10 - 1|= +.10 (cioè +10%)
  * semestre 2: |19/20 - 1|= -.05 (cioè -5%)
  * MaxJump: masimo tra |+.10| e |-.05| = .10

## MaxPredRatio ##
Max among predicted values over max of values (ie observed data)

## MeanPedicted ##
Mean of predicted values

## MeanValues ##
Mean of observed values (i.e. the data)

## MeanPedictedRatioMeanValues ##
Mean of predicted over mean of values (ie observed data)

## SdPedictedRatioSdValues ##
Standard deviation of predicted values over Standard deviation of observed ones.

## BestAICNoOutRangeExclude ##
Model with minimum AIC  (exclusion criterion not used)

## BestICNoOutRangeExclude ##
Model with minimum I.C. width  (exclusion criterion not used)

## LastNotEqualValues ##
Length of last not-constant consecutives serie of values (e.g. not constant 0 values, it gives a row idea of most recent informative serie)

## Residui ##
Buoni residui presentano un grafico con una nuvola di punti distribuiti casualmente intorno allo 0; non devono essere presenti evidenti relazioni lineari, paraboliche o
"pattern" riconoscibili in generale, la nuvola di punti deve cioè apparire "casuale".

Un esempio di buoni residui è il seguente:

![http://strategico.googlecode.com/svn/trunk/doc/ltp/residuals1.jpg](http://strategico.googlecode.com/svn/trunk/doc/ltp/residuals1.jpg)

Un esempio di residui non soddisfacenti è invece il seguente:

![http://strategico.googlecode.com/svn/trunk/doc/ltp/residuals2.jpg](http://strategico.googlecode.com/svn/trunk/doc/ltp/residuals2.jpg)

## Linear Model ##
I principali indicatori per valutare la qualità di un modello lineare sono [R2](https://code.google.com/p/ltp/source/detail?r=2), il test F e la tabella dei "Coefficients".
  * [R2](https://code.google.com/p/ltp/source/detail?r=2) è la proporzione di varianza spiegata dai predittori; è compreso tra 0 e 1 e "più alto è meglio". Vedi anche commenti sopra riportati.
  * La statistica F verifica se il fenomeno è in relazione con i predittori del modello; valuta cioè se l'indice [R2](https://code.google.com/p/ltp/source/detail?r=2) è significativamente diverso da 0 o se questo valore possa essere attribuita al caso. Affinchè la relazione tra predittori e fenomeno studiato sia statisticamente significativa, il valore di P-value deve essere quanto più basso possibile (almeno minore di .01).
  * (SOLO) se il test F risulta significativo può essere utile valutare l'influenza dei singoli predittori. La colonna "Estimate" indica l'effetto stimato sulla y al crescere
di una unità del predittore; il P-value per la verifica che questo effetto sia significativo (cioè non diverso da 0 solo per caso) è riportato nella colonna "Pr(>|t|)"
(basso è meglio)

## Naive Model ##
permette di inserire dati decisi dall'utente. se "naive"%in%try.models ltp usa i valori di naive.values per la predizione.
il modello risulterà essere il bestModel solo se tutti gli altri non sono inclusi.
naive.values può contenere numeri o "last" o "lastPeriod". Se naive.values="last" predirrà con l'ultimo valore della serie storica,
se naive.values="lastPeriod" prederà gli ultimi n.freq e replicherà quelli. se naive.values è un numero di valori minore del numero di forcast richiesti, replicherà i valori fino a raggiungere la lunghezza di predizione richiesta

## Riferimenti ##

TODO: aggiungere qui' link a documentazioni utilizzate/inerenti al long term prediction