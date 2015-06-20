library(vegsoup)

load("~/Documents/vegsoup-data/steyr und ennstal dta/se.rda")
x <- se

x <- layers(x, collapse = c("tl", "tl", "sl", "hl", NA))
p <- VegsoupPartition(x, 7, method = "FCM")

f <- fidelity(p)
layers(f) <- rev(layers(f))

X <- f

rm(list = ls()[-grep("X", ls())])