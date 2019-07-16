library(ROCR)
new_tab = cbind(c(5800, 10000-5800),
                c(3800, 10000-3800))
make_vec = function(cat_tab) {
  rownames(cat_tab) = 1:nrow(cat_tab)
  colnames(cat_tab) = c(0, 1)  
  xx <- rep(rep(rownames(cat_tab), ncol(cat_tab)), c(cat_tab))
  xx = as.numeric(xx)
  yy <- rep(colnames(cat_tab), colSums(cat_tab))
  yy = as.numeric(yy)
  ttab = table(xx, yy)
  stopifnot(all(ttab == cat_tab))
  return(list(x = xx, y = yy))
}
rownames(new_tab) = c(0, 1)
colnames(new_tab) = c(0, 1)

ndiv = 20
tt = new_tab/ndiv
long_mat = rbind(
  matrix(rep(tt[1,], ndiv), nrow = ndiv, byrow = TRUE),
  matrix(rep(tt[2,], ndiv), nrow = ndiv, byrow = TRUE)
)
u = unique(sort(round(runif(ndiv * 2*5), 4)))
rownames(long_mat) = sort(sample(u, size = ndiv * 2))
colnames(long_mat) = c(0, 1)

bin = make_vec(new_tab)
lbin = make_vec(long_mat)

pred = prediction(cbind(bin$x, lbin$x), cbind(lbin$y, lbin$y))
perf = performance(pred, "tpr", "fpr")

plot(perf@x.values[[1]], perf@y.values[[1]], type = 'l',
     xlab = "False positive rate", ylab = "True positive rate", lwd = 2.5)
lines(perf@x.values[[2]], perf@y.values[[2]], col = "deepskyblue3", type = "s", lwd = 2)
lines(perf@x.values[[1]], perf@y.values[[1]], col = "red", type = "s", lwd = 2.5)
points(perf@x.values[[2]], perf@y.values[[2]], col = "blue", pch = 16)
