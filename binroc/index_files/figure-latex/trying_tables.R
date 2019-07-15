x = c(rep(0, 52), rep(1, 32),
      rep(0, 35), rep(1, 50))
y = c(rep(0, 84), rep(1, 85))
tab = table(x, y)
n = length(y)
n_y = sum(y)
sens = tab[2,2] / sum(tab[,2])
spec = tab[1,1] / sum(tab[,1])
auc.defn = sens * spec
##################################
# Making a 3 cat table
##################################
fpr = seq(0.01, 1 - spec, by = 0.001)
b = sens/(1-spec)
tpr = fpr * b
y1 = sum(y==1)
y0 = sum(y == 0)

pred = prediction(x, y)
perf = performance(pred, "tpr", "fpr")
plot(perf)
points(fpr, tpr)

x2 = fpr * y0
x1 = 32-x2
x4 = tpr * y1
x3 = 50-x4

ind = 241
new_tab = cbind(c(52, x1[ind], x2[ind]),
            c(35, x3[ind], x4[ind]))
new_tab = round(new_tab)
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

vec = make_vec(new_tab)

pred2 = prediction(vec$x, vec$y)
perf2 = performance(pred2, "tpr", "fpr")
plot(perf)
points(perf2@x.values[[1]], perf2@y.values[[1]])
new_tab_try = new_tab
















fpr = seq(1 - spec, 1, by = 0.001)
b = (1 - sens)/(spec)
tpr = fpr * b + (sens - (1 - spec)*b)
y1 = sum(y==1)
y0 = sum(y == 0)

pred = prediction(x, y)
perf = performance(pred, "tpr", "fpr")
plot(perf)
points(fpr, tpr)

x2 = (fpr * y0 ) - 32
x1 = 52 - x2
x4 = tpr * y1 - 50
x3 = 35 - x4

ind = 251
new_tab = cbind(c(x1[ind], x2[ind], 32),
                c(x3[ind], x4[ind], 50))
new_tab = round(new_tab)

vec = make_vec(new_tab)

pred2 = prediction(vec$x, vec$y)
perf2 = performance(pred2, "tpr", "fpr")
plot(perf)
points(perf2@x.values[[1]], perf2@y.values[[1]])


new_tab
