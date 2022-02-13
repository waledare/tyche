#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

filname <- args[1]

#filname <- "/tmp/temp26753-0.txt"
df <- read.csv(filname)

#df[!is.na(df$GDPC1), ]
colnames(df)[2] <- "y"

inflation <- diff(log(df$CPIAUCSL))
rate <- (df$FEDFUNDS)/100
unemployment <- (((df$UNRATE))[-1])/100
port <- diff(log(df$y))
mkt <- diff(log(df$X.SP500TR))

#gdp <- diff(log(df$GDPC1))

x <- colnames(df)[3:ncol(df)] 

formula <- "y ~ "
for (i  in 1:length(x)){
  sep <- " + "
  if (i == 1){
    sep <- " "
  }
  formula <- paste(formula, x[i], sep=sep)
}

ddf <- data.frame(port,inflation, "rate"=rate[-1], unemployment, mkt)
fit <-lm(port ~ inflation + rate + unemployment + mkt,  data=ddf)

library(tools)

vs <- apply(ddf, c(0,2), function(x) var(x, na.rm=T))
cs <- fit$coefficients

capm <- summary(lm(port ~ mkt, data=ddf))

out <- list("betas"=as.vector(cs), "sigmas"=as.vector(vs), "capmRsquare"= capm$r.squared, "model"=as.character(fit$call) )
write.csv(data.frame(rbind(cs, vs), row.names = NULL), paste(file_path_sans_ext(filname), "coef", sep="."))
library(rjson)
write(toJSON(out), paste(file_path_sans_ext(filname), "json", sep="."))

mlog <- function(x){
  if( x > 0) {
    res <- -log(x)
  }
  if (x == 0){
    res <- 15
  }
  if ( x < 0){
    res <- 15 + abs(log(abs(x)))
  }
  res
}


