# qdm.R
#
# Last mod: May/13/2014, FW

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

