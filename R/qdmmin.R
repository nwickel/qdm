# qdmmin.R
#
# Last mod: Jun/16/2014, FW

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






## Minimum chi square
minchi2 <- function(p, psi, ...){
  pre_d <- outer(psi$x, psi$y, function(x, y) qdmfun(x, y, p, ...))
  chi <- sum(na.omit(as.vector((freq.ob - n*pre_d)^2 /
                               (n*pre_d * (1 - pre_d)))))
  #if (chi < 0) Inf else chi
  if (any(pre_d < 0)) Inf else chi
}

## Ordinary least squares
ols <- function(p, psi, ...){
  sum((prob.ob - outer(psi$x, psi$y, function(x, y) qdmfun(x, y, p, ...)))^2)
}

## Weighted least squares
wls <- function(p, psi, ...){
  #prob.ob[prob.ob == 1] <- 0.9
  pre_d <- outer(psi$x, psi$y, function(x, y) qdmfun(x, y, p, ...))
  weights <- 1 / (prob.ob*(1 - (prob.ob - 0.01)))
  # sum(na.omit(as.vector(weights*((freq.ob - n*pre_d)^2 /
  #     (n*pre_d*(1-pre_d))))))
  sum(na.omit(as.vector(weights*((freq.ob - n*pre_d)^2))))
}


## Minimum chi square
minchi2 <- function(freq.ob, n, ...){
  pre_d <- outer(x, y, function(x, y) qdmfun(x, y, p, ...))
  chi <- sum(na.omit(as.vector((freq.ob - n*pre_d)^2 /
                               (n*pre_d * (1 - pre_d)))))
  #if (chi < 0) Inf else chi
  if (any(pre_d < 0)) Inf else chi
}

## Ordinary least squares
ols <- function(prob.ob, ...){
  sum((prob.ob - outer(x, y, function(x, y) qdmfun(x, y, p, ...)))^2)
}

## Weighted least squares
wls <- function(prob.ob, freq.ob, n, ...){
  #prob.ob[prob.ob == 1] <- 0.9
  pre_d <- outer(x, y, function(x, y) qdmfun(x, y, p, ...))
  weights <- 1 / (prob.ob*(1 - (prob.ob - 0.01)))
  # sum(na.omit(as.vector(weights*((freq.ob - n*pre_d)^2 /
  #     (n*pre_d*(1-pre_d))))))
  sum(na.omit(as.vector(weights*((freq.ob - n*pre_d)^2))))
}

