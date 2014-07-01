# qdm.R
#
# last mod: Jun/17/2014, FW


## QDM function
qdmfun <- function(x, y, p, response = c("logistic", "guessing", "dlogistic",
                   "shepardA", "shepardB", "shepardE", "shepardF"),
                   log = "") {
  if (grepl("x", log)) x <- log(x)
  if (grepl("y", log)) y <- log(y)

  s <- p[1] + p[2]*x + p[3]*x^2 + 2*p[4]*abs(x - y) + p[5]*y + p[6]*y^2

  response <- match.arg(response)
  switch(EXPR = response,
     logistic = 1/(1 + exp(-p[7]*s - p[8])),
     guessing = p[9] + (1 - p[9])*1/(1 + exp(-p[7]*s - p[8])),
    dlogistic = 1 - exp(-p[7]*s - p[8])/((1 + exp(-p[7]*s - p[8]))^2),
     shepardA = 1 - (1 - s/p[7] + (s/p[7])*log(s/p[7])),
     shepardB = 1 - (1 - (s/p[7])^2 + p[8]*(s/p[7])*log(s/p[7])),
     shepardE = 1 - (1 - p[8]*s/p[7] + p[8]*(s/p[7])^2 - (s/p[7])^3),
     shepardF = 1 - exp(-p[7]*s - p[8])
  )
}


## QDM objective function
objfun <- function(p, psi, estimfun = c("minchi2", "ols", "wls"), ...){
  yhat <- outer(psi$x, psi$y, function(x, y) qdmfun(x, y, p, ...))

  estimfun <- match.arg(estimfun)
  out <- switch(EXPR = estimfun,
    minchi2 =
      sum(na.omit(as.vector((psi$freq - psi$ntrials*yhat)^2 /
                            (psi$ntrials*yhat * (1 - yhat))))),
    ols =
      sum((psi$prob - yhat)^2),
    wls = {
      weights <- 1 / (psi$prob*(1 - (psi$prob - 0.01)))
      sum(na.omit(as.vector(weights*((psi$freq - psi$ntrials*yhat)^2))))
    }
  )
  if (any(yhat < 0)) Inf else out
}


## QDM user interface
qdm <- function(psi, start, respfun = c("logistic", "guessing", "dlogistic",
                "shepardA", "shepardB", "shepardE", "shepardF"),
                estimfun = c("minchi2", "ols", "wls"),
                optimizer = c("optim", "nlm"), optimargs = list(), ...){

  if (class(psi) == "psi"){

  respfun <- match.arg(respfun)
  esitmfun <- match.arg(estimfun)

  ## Optimize
  optimizer <- match.arg(optimizer)
  if (optimizer == "nlm") {
    optArgs <- list(f=objfun, p=start, psi=psi, estimfun=estimfun,
                    response=respfun)
    optArgs <- c(optArgs, as.list(optimargs), list(...))
    optimout <- do.call(nlm, optArgs)
    coefficients <- optimout$estimate
  } else {  # optim()
    optArgs <- list(par=start, fn=objfun, psi=psi, estimfun=estimfun,
                    response=respfun)
    optArgs <- c(optArgs, as.list(optimargs), list(...))
    optimout <- do.call(optim, optArgs)
    coefficients <- optimout$par
  }

  retval = list(optimout=optimout, coefficients=coefficients, psi=psi)
  class(retval) <- "qdm"
  retval
  } else {
  stop("Object given to argument psi has to be of class psi.")
}
}
# TODO Give warning if optimizer does not give new parameters?


print.qdm <- function(x, digits = max(3L, getOption("digits") - 3L),
                      ...){
  cat("\nQuadrilateral dissimilarity models\n\n")
  cat("Coefficients:\n")
  print.default(format(coef(x), digits = digits), print.gap = 2L, 
                quote = FALSE)
  cat("\n")
  invisible(x)
}


## FIX ME: args to qdmfun
predict.qdm <- function(object, x = object$psi$x, y = object$psi$y, ...){
  yhat <- outer(x, y, function(x, y) qdmfun(x, y, coef(object)))
  dimnames(yhat) <- list(x, y)
  yhat
}

