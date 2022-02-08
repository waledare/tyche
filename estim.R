#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

filname <- args[1]

#filname <- "/tmp/temp4450-0.txt"
df <- read.csv(filname)

#df[!is.na(df$GDPC1), ]
colnames(df)[2] <- "y"

inflation <- diff(log(df$CPIAUCSL))
rate <- df$FEDFUNDS
unemployment <- ((df$UNRATE))[-1]
port <- diff(log(df$y))

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

ddf <- data.frame(port,inflation, "rate"=rate[-1], unemployment)
fit <-lm(port~ inflation + rate + unemployment, data=ddf)

library(tools)
 
write.csv(fit$coefficients, paste(file_path_sans_ext(filname), "coef", sep="."))
