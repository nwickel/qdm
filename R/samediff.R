# samediff.R
#
# last mod: 16/Jun/2014, FW

# Calculate discrimination probabilities, P("different"), from same-different
# judgments
psi <- function(subj, dat, oa1="s1", oa2="s2"){
    names(dat)[names(dat) == oa1] <- "s1"
    names(dat)[names(dat) == oa2] <- "s2"
    freq <- as.matrix(unclass(xtabs(~ s1 + s2, dat[dat$resp == "d" & dat$id == subj,])))
    attr(freq, "call") <- NULL
    # nstim <- length(unique(dat$s1))  FIX ME: delete?
    # probability different
    n <- as.matrix(unclass(xtabs(~ s1 + s2, dat[dat$id == subj,])))
    attr(n, "call") <- NULL
    prob <- freq/n
    prob[which(is.na(prob))] <- 1
    x <- as.numeric(rownames(freq))
    y <- as.numeric(rownames(freq))
    list(prob=prob, ntrials=n, freq=freq, x=x, y=y)
}

