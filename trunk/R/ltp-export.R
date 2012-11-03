## This program is fre esoftware: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## project name: ltp
## project website: http://code.google.com/p/ltp/
## created: 2012

library(XML)

.dfToXML <- function(df,name) {
  xml <- xmlTree("ltp")
  xml$addNode(name, close=FALSE)
  for (i in 1:nrow(df)) {
    xml$addNode("row", close=FALSE)
    for (j in names(df)) {
         xml$addNode(j, df[i, j])
       }
    xml$closeTag()
  }
  xml$closeTag()
  return(xml)
}

.dfToXMLfile <- function(df,name, file=NULL) {
  if (is.null(file)) {
    file=sprintf("%s.xml", name)
  }
  tr <- dfToXML(df,name)
  cat(saveXML(tr$value(), file=file))
}


modelToListOfDF <- function(model, period.freq, param, id=1) {
  model.df <- list()

  if ( nrow(model@values) > 0) {
    normalized.periods <- rownames(model@values)
    normalized.data <- data.frame(item_id=id, PERIOD=normalized.periods, V=model@values$V)

    model.df$normalized.data <- normalized.data
  }

  summary <- ltp.BuildOneRowSummary(id=id, model=model, param=param)
  model.df$summary <- summary

  if (!is.null(model@BestModel)) {
    summary.models <- data.frame(ltp.GetModelsComparisonTable(model))
    summary.models = cbind(item_id=id, model=rownames(summary.models), summary.models)
    model.df$summary.models <- summary.models
  }

  if (!is.null(model@BestModel)) {
    all.results <- NULL
    all.residuals <- NULL

    tot.hist.points <- length(normalized.periods)
    n.ahead <- param$n.ahead
    period.end <- Period.FromString(tail(normalized.periods, 1))
    predicted.periods <-Period.BuildRange(period.start=period.end,
                                          period.freq=period.freq,
                                          n.ahead, shift=1)

    for (m in names(model@models)) {
      predictions <- as.vector(model@models[[m]]$prediction)
      residuals <- as.vector(model@models[[m]]$Residuals)

      if (length(predictions) == n.ahead) {
        model.results <- data.frame(
           item_id=rep(id, n.ahead),
           model=rep(m, n.ahead),
           PERIOD=predicted.periods,
           V=predictions)
        all.results <- rbind(all.results, model.results)
      }

      if (length(residuals) == tot.hist.points) {
        model.residuals <- data.frame(
           item_id=rep(id, tot.hist.points),
           model=rep(m, tot.hist.points),
           PERIOD=normalized.periods,
           V=trunc(residuals, 1))
        all.residuals <- rbind(all.residuals, model.residuals)
      }
    } ## end for model


    if (!is.null(all.results)) {
      colnames(all.results) <- c("item_id", "model", "PERIOD", "V")
      model.df$results <- all.results
    }

    if (!is.null(all.residuals)) {
      colnames(all.residuals) <- c("item_id", "model", "PERIOD", "V")
      model.df$residuals <- all.residuals
    }


  } ## end if
  return(model.df)
}

modelToHtml5 <- function(model, param, id=1, img.options=list(width=850, height=500, legend="bottom", gvis.editor="Editor")) {
  model.df <- modelToListOfDF(model, param, id=1) 
}
 
