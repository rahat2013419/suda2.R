suda2 <- function(obj, ...) {
  suda2X(obj=obj, ...)
}

setGeneric("suda2X", function(obj, ...) {
  standardGeneric("suda2X")
})

setMethod(f="suda2X", signature=c("sdcMicroObj"), definition=function(obj, ...) {
  manipData <- get.sdcMicroObj(obj, type = "manipKeyVars")
  keyVars <- colnames(manipData)

  risk <- get.sdcMicroObj(obj, type = "risk")
  risk$suda2 <- suda2WORK(manipData, variables = keyVars, ...)

  obj <- set.sdcMicroObj(obj, type = "risk", input = list(risk))
  if(length(keyVars)<=2){
    warn_s <- "This version of Suda2 can find MSUs only in Dataset with more than 2 variables."
    warn_s <- paste0(warn_s,"\nDummy variables have been added and the result might be wrong!")
    obj <- addWarning(obj, warnMsg=warn_s, method="suda2", variable=NA)
  }
  return(obj)
})

setMethod(f="suda2X", signature=c("data.frame"), definition = function(obj, ...) {
  suda2WORK(data = obj, ...)
})

suda2WORK <- function(data, variables = NULL, missing = -999, DisFraction = 0.01, original_scores=TRUE) {
  stopifnot(is.logical(original_scores))
  stopifnot(length(original_scores) == 1)

  if (is.null(variables)) {
    variables <- colnames(data)
  }
  dataX <- data[, variables, drop = FALSE]
  if (length(variables) == 2) {
    dataX <- cbind(dataX, rep(1, nrow(dataX)))
  } else if (length(variables) == 1) {
    dataX <- cbind(dataX, rep(1, nrow(dataX)), rep(1, nrow(dataX)))
  }
  for (i in seq_len(ncol(dataX))) {
    if (!is.numeric(dataX[, i])) {
      dataX[, i] <- as.numeric(dataX[, i])
    }
  }
  dataX <- as.matrix(dataX)
  dataX[is.na(dataX)] <- missing
  dat <- Suda2(dataX, missing, ncol(dataX), DisFraction, original_scores)$Res
  if (length(variables) == 2) {
    dat <- dat[, -3]
  } else if (length(variables) == 1) {
    dat <- dat[, c(-2, -3)]
  }
  colnames(dat) <- c(paste0(variables, "_contribution"), "suda_score", "dis_suda_score")
  res <- list(
    contributionPercent = dat[, 1:length(variables)],
    score = dat[, "suda_score"],
    disScore = dat[, "dis_suda_score"]
  )
contribs <- res$contributionPercent * res$score
  df <- data.frame(
    variable = variables,
    contribution = 100 * (colSums(contribs) / sum(res$score)),
    stringsAsFactors = FALSE
  )

  rownames(df) <- NULL
  res$attribute_contributions <- df

  # attribute level contributions
  tmp <- cbind(data[, variables, drop = FALSE], contribs)
  tots <- apply(contribs, 2, sum)
  df <- NULL
  #browser()
  for (vv in variables) {
    levs <- sort(unique(data[[vv]]))
    val <- sapply(levs, function(x) {
      100*(sum(tmp[[paste0(vv, "_contribution")]][tmp[[vv]] == x], na.rm = TRUE))
    }) / tots[[paste0(vv, "_contribution")]]
    df <- rbind(df, data.frame(
      variable = vv,
      attribute = levs,
      contribution = val,
      stringsAsFactors = FALSE
    ))
  }
  res$attribute_level_contributions <- df

  class(res) <- "suda2"
  if (length(variables) <= 2) {
    warn_s <- "This version of Suda2 can find MSUs only in Dataset with more than 2 variables."
    warn_s <- paste0(warn_s,"\nDummy variables have been added and the result might be wrong!")
    warning(warn_s)

  }
  invisible(res)
}
