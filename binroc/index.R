## ----setup, include = FALSE----------------------------------------------
# devtools::install_github("yihui/knitr@0da648bff63d3c4234ea4a19bd9cd62e45efa42a")
library(knitr)
library(kableExtra)
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE, warning = FALSE, 
  comment = "")
knitr::opts_chunk$set(fig.pos = "H")
run_python = FALSE
library(reticulate)
library(tidyverse)
py_version = "3.5"
python = file.path("/Library/Frameworks/Python.framework",
                   "/Versions", py_version, "bin/python3")
use_python(python)
# use_virtualenv("r-tensorflow")
knitr::knit_engines$set(python = reticulate::eng_python)
# devtools::install_github("rstudio/rticles", ref = "jss")

## ---- message=FALSE, echo = FALSE----------------------------------------
library(cranlogs)
library(ggplot2)
library(dplyr)

## ----make_data, echo = FALSE , results = "asis", out.extra = ''----------
x = c(rep(0, 52), rep(1, 32),
      rep(0, 35), rep(1, 50))
y = c(rep(0, 84), rep(1, 85))
tab = table(x, y)

## ----create_tab, echo = FALSE--------------------------------------------
knitr::kable(tab, 
             caption = "A simple 2x2 table of a binary predictor (rows) versus a binary outcome (columns)", valign = "ht") %>% 
  kableExtra::kable_styling(position = "center")

## ---- echo = FALSE-------------------------------------------------------
sens = tab[2,2] / sum(tab[,2])
spec = tab[1,1] / sum(tab[,1])
auc.defn = sens * spec
# print(auc.defn)

## ---- echo = FALSE-------------------------------------------------------
flip.auc = (1 - sens) * (1 - spec)
# print(flip.auc)

## ---- echo = FALSE-------------------------------------------------------
fpr = 1 - spec
area_of_tri = 1/2 * sens 
area_of_tri = area_of_tri * fpr
area_of_quad = sens * spec 
area_of_upper_tri = 1/2 * spec 
area_of_upper_tri = area_of_upper_tri * (1 - sens)
area_of_quad = sens * spec + area_of_upper_tri
auc = area_of_tri + area_of_quad
# print(auc)

## ---- echo = FALSE-------------------------------------------------------
n = 1000000

## ------------------------------------------------------------------------
est.auc = function(x, y, n = 1000000) {
  x1 = x[y == 1] # x | y = 1
  x0 = x[y == 0] # x | y = 0
  c1 = sample(x1, size = n, replace = TRUE)
  c0 = sample(x0, size = n, replace = TRUE)
  auc.defn = mean(c1 > c0) # strictly greater
  auc.wties = auc.defn + 1/2 * mean(c1 == c0) # half for ties
  return(c(auc.definition = auc.defn,
           auc.wties = auc.wties))
}
sample.estauc = est.auc(x, y)
sample.estauc

## ----main, echo = FALSE, fig.width=10, fig.height=5, fig.cap = "ROC curve of the data in the simple concrete example.  Here we present a standard ROC curve, with the false positive rate or $1 - \\text{specificity}$ on the x-axis and true positive rate or sensitivity on the y-axis.  The dotted line represents the identity. The shaded area in panel represents the AUC for the strict definition.  The additional shaded areas on panel B represent the AUC when accounting for ties.  "----
library(ROCR)
yvalues = function(perf) {
  unlist(slot(perf, "y.values"))
}
xvalues = function(perf) {
  unlist(slot(perf, "x.values"))
}
pred = prediction(x, y)
perf = performance(pred, "tpr", "fpr")
par(mfrow = c(1, 2), oma = c(0, 0, 0, 0), mar = c(5, 4.1, 0, 0))
plot(xvalues(perf), yvalues(perf), type = "s", 
     ylab = "True positive rate", 
     xlab = "False positive rate", cex = 2)
rect(xleft = 1 - spec, xright = 1, ybottom = 0, ytop = sens, col = "deepskyblue3", border = FALSE)
lines(xvalues(perf), yvalues(perf), lwd = 2, type = "s")
abline(a = 0, b = 1, col = "gray", lty = "dashed")
text(x = 0.15, y = 0.8, labels = c("A"), cex = 12)
plot(perf, ylab = "")
verts = cbind(x = c(0, 1 - spec, 1 - spec), y = c(0, sens, 0))
polygon(verts, col = "orange", border = FALSE)
verts = cbind(x = c(1 - spec, 1, 1), y = c(sens, 1, sens))
polygon(verts, col = "firebrick", border = FALSE)
rect(xleft = 1 - spec, xright = 1, ybottom = 0, ytop = sens, col = "deepskyblue3", border = FALSE)
lines(xvalues(perf), yvalues(perf), lwd = 2)
text(x = 0.15, y = 0.8, labels = c("B"), cex = 12)
# rect(xleft = 0, ybottom =0, xright= 1-spec, ytop = sens, col = "red")
# rect(xleft = 1 - spec, ybottom = sens, xright= 1, ytop = 1, col = "blue")
abline(a = 0, b = 1,  col = "gray",lty = "dashed")

## ----main2, echo = FALSE, fig.width=5, fig.height=5, include = FALSE-----
library(ROCR)
plot.new()
verts = cbind(x = c(0, 1 - spec, 1 - spec), y = c(0, sens, 0))
polygon(verts, col = "orange", border = FALSE)
verts = cbind(x = c(1 - spec, 1, 1), y = c(sens, 1, sens))
polygon(verts, col = "firebrick", border = FALSE)
rect(xleft = 1 - spec, xright = 1, ybottom = 0, ytop = sens, col = "deepskyblue3", border = FALSE)

## ---- echo = FALSE, message = FALSE, include=FALSE-----------------------
dl = cranlogs::cran_downloads(
  # when = "last-month", 
  from = "2017-09-08",
  to =  Sys.Date(),
  packages = c("pROC", "ROCR", "fbroc", "AUC",
               "PKNCA", "auRoc", "caTools", "npROCRegression", 
               "roccv", "ROC632", "correctedAUC", "cvAUC",
               "optAUC", "tpAUC"))
dl = dl %>% 
  arrange(package, date) %>% 
  group_by(package) %>%
  mutate(count = cumsum(count),
         label = last(count),
         last_date = last(date)) %>% 
  ungroup
r = range(dl[["date"]])
r[2] = r[2] + 3
dl_text = dl %>% 
  select(package, label, last_date) %>% 
  distinct %>% 
  mutate(date = last_date + 3,
         count = label) %>% 
  arrange(desc(count), package)
dl_text = dl_text %>% head(5)
dl = dl %>% 
  filter(package %in% dl_text[["package"]])
dl_text2 = dl_text %>% 
  mutate(count = ifelse(package == "AUC", count + 2000, count),
         count = ifelse(package == "cvAUC", count - 2000, count))
dl %>% 
  ggplot(aes(x = date, y = count, group = package)) + 
  xlim(r) +
  geom_line() + 
  geom_text(aes(label = package), data = dl_text2,
                size = 3)

dl %>% 
  filter(!package %in% c("caTools")) %>% 
  ggplot(aes(x = date, y = count, group = package)) + 
  geom_line() + 
  geom_text(
    aes(label = package),
    data = dl_text %>% 
      filter(!package %in% c("caTools")))

## ------------------------------------------------------------------------
library(caTools)
colAUC(x, y)

## ------------------------------------------------------------------------
library(ROCR)
pred = prediction(x, y)
auc.est = performance(pred, "auc")
auc.est@y.values[[1]]

## ------------------------------------------------------------------------
library(pROC)
pROC.roc = pROC::roc(predictor = x, response = y)
pROC.roc[["auc"]]

## ---- include = FALSE, echo = FALSE, fig.width=10, fig.height=10---------
ggroc(pROC.roc, colour = "firebrick")

## ---- include = FALSE, echo = FALSE, fig.width=10, fig.height=10---------
par(mfrow = c(2, 2), oma = c(0, 0, 0, 0), mar = c(5, 4.1, 0, 0))
plot.roc(pROC.roc)
plot.roc(pROC.roc, type = "s")
plot(y = pROC.roc[["sensitivities"]],
     x = 1 - pROC.roc[["specificities"]], type = "l")
plot(y = pROC.roc[["sensitivities"]],
     x = 1 - pROC.roc[["specificities"]], type = "s")

## ---- include = FALSE----------------------------------------------------
fpr = 1 - spec
left.tri = 1/2 * sens * fpr
right.tri = 1/2 * spec * (1 - sens)
false.auc = left.tri + auc.defn + right.tri
false.auc

## ------------------------------------------------------------------------
library(fbroc)
fbroc.default = boot.roc(x, as.logical(y), 
                         n.boot = 1000, tie.strategy = 2)
auc.def = perf(fbroc.default, "auc")
auc.def[["Observed.Performance"]]
fbroc.alternative = boot.roc(x, as.logical(y), 
                             n.boot = 1000, tie.strategy = 1)
auc.alt = perf(fbroc.alternative, "auc")
auc.alt[["Observed.Performance"]]

## ---- include = FALSE----------------------------------------------------
graph2 <- plot(fbroc.default, main = "Tie Strategy 2")
graph1 <- plot(fbroc.alternative, add = TRUE, main = "Tie Strategy 1")

## ---- include = FALSE----------------------------------------------------
gridExtra::grid.arrange(graph1, graph2, ncol = 2)

## ---- include = FALSE, eval = TRUE---------------------------------------
pred = prediction(x, y)
perf = performance(pred, "tpr", "fpr")
pnger = function(filename){
  png(filename, height = 7, width = 7, units = "in", res = 300)
}
pnger("ROCR.png")
plot(perf, main = "ROCR", lwd = 5, cex.axis = 2, cex.lab = 2, cex.main
= 3, cex =3 )
dev.off()
pnger("pROC.png")
plot(pROC.roc, main = "pROC", lwd = 5, cex.axis = 2, cex.lab = 2, cex.main
= 3)
dev.off()
pnger("fbroc2.png")
plot(fbroc.default) + ggtitle("fbroc, Strategy 2") +
  theme(axis.text=element_text(size = 20),
        axis.title=element_text(size = 20,face="bold"))
dev.off()
pnger("fbroc1.png")
plot(fbroc.alternative) + ggtitle("fbroc, Strategy 1") +
  theme(axis.text=element_text(size = 20),
        axis.title=element_text(size = 20,face="bold"))
dev.off()

## ---- echo = FALSE-------------------------------------------------------
python_figure = "python_roc.png"

## ----python_plot_show, eval = TRUE---------------------------------------
# Adapted from https://qiita.com/bmj0114/items/460424c110a8ce22d945
library(reticulate)
sk = import("sklearn.metrics")
py.roc.curve = sk$roc_curve(y_score = x, y_true = y)
names(py.roc.curve) = c("fpr", "tpr", "thresholds")
py.roc.auc = sk$auc(py.roc.curve$fpr, py.roc.curve$tpr)
py.roc.auc

## ----python_plot, eval = run_python, message=FALSE, results='hide', include = FALSE----
# Adapted from
# https://qiita.com/bmj0114/items/460424c110a8ce22d945
sk = import("sklearn.metrics")
mpl = import("matplotlib")
mpl$use('TkAgg')
plt = import("matplotlib.pyplot")
py.roc.curve = sk$roc_curve(y_score = x, y_true = y)
names(py.roc.curve) = c("fpr", "tpr", "thresholds")
py.roc.auc = sk$auc(py.roc.curve$fpr, py.roc.curve$tpr)

plt$figure()
plt$plot(
  py.roc.curve$fpr,
  py.roc.curve$tpr,
  color = 'darkorange',
  lw = 1,
  label = sprintf('ROC curve (area = %0.3f)', py.roc.auc))
plt$plot(c(0, 1),
         c(0, 1),
         color = 'navy',
         lw = 1,
         linestyle = '--')
plt$xlim(c(0.0, 1.0))
plt$ylim(c(0.0, 1.05))
plt$xlabel('False Positive Rate')
plt$ylabel('True Positive Rate')
plt$title('Receiver operating characteristic')
plt$legend(loc = "lower right")

## ---- echo = FALSE, include=FALSE, eval = run_python---------------------
if (!file.exists(python_figure)) {
  plt$savefig(python_figure)
}

## ---- echo = FALSE, out.width="100%", include = FALSE--------------------
knitr::include_graphics(python_figure)

##   proc logistic data=roc;

## ----global-options, include=FALSE---------------------------------------
library(haven)
df = data.frame(x,y)
haven::write_dta(data = df, path = "sample_data.dta", version = 13)

library(statamd)
statapath = statamd::stata_engine_path()
profile_do(dataset = "sample_data.dta")

## roctab x y

## rocreg y x, nodots auc

## rocreg y x, auc

## ---- echo = FALSE, fig.width=10, fig.height=5, fig.cap = "hey", results="hide"----
library(ROCR)
png("ROCR_plot.png", height = 7, width = 7, units = "in", res = 300)
pred = prediction(x, y)
perf = performance(pred, "tpr", "fpr")
par(mfrow = c(1, 2), oma = c(0, 0, 0, 0), mar = c(5, 4.1, 0, 0))
plot(xvalues(perf), yvalues(perf), type = "s", 
     ylab = "True positive rate", 
     xlab = "False positive rate", cex = 2)
rect(xleft = 1 - spec, xright = 1, ybottom = 0, ytop = sens, col = "deepskyblue3", border = FALSE)
lines(xvalues(perf), yvalues(perf), lwd = 2, type = "s")
abline(a = 0, b = 1, col = "gray", lty = "dashed")
plot(perf, ylab = "", lwd = 2)
verts = cbind(x = c(0, 1 - spec, 1 - spec), y = c(0, sens, 0))
polygon(verts, col = "orange", border = FALSE)
verts = cbind(x = c(1 - spec, 1, 1), y = c(sens, 1, sens))
polygon(verts, col = "firebrick", border = FALSE)
rect(xleft = 1 - spec, xright = 1, ybottom = 0, ytop = sens, col = "deepskyblue3", border = FALSE)
lines(xvalues(perf), yvalues(perf), lwd = 2)
# rect(xleft = 0, ybottom =0, xright= 1-spec, ytop = sens, col = "red")
# rect(xleft = 1 - spec, ybottom = sens, xright= 1, ytop = 1, col = "blue")
abline(a = 0, b = 1,  col = "gray",lty = "dashed")
dev.off()

## ---- include=FALSE------------------------------------------------------
nboot = 1000
n = length(x)
samps = matrix(sample(n, size = n * nboot, replace = TRUE), 
               ncol = nboot)
ssens = sspec = rep(NA, nboot)
for (iind in seq(nboot)) {
  ind = samps[, iind]
  xx = factor(x[ind], levels = c(0, 1))
  yy = factor(y[ind], levels = c(0, 1))
  tab = table(xx, yy)
  sens = tab[2,2] / sum(tab[,2])
  spec = tab[1,1] / sum(tab[,1])
  ssens[iind] = sens
  sspec[iind] = spec
}
plot(c(0, 1), c(0, 1), type = "n")
df = cbind(sspec, ssens)
apply(df, 1, function(x) {
  d = rbind(
    c(0, 0),
    x,
    c(1,1))
  lines(d, type = "s")
})
spec_quant = quantile(sspec, probs = c(0.025, 0.975))
sens_quant = quantile(ssens, probs = c(0.025, 0.975))
plot(c(0, 1), c(0, 1), type = "n")
df = cbind(spec_quant, sens_quant)
apply(df, 1, function(x) {
  d = rbind(
    c(0, 0),
    x,
    c(1,1))
  lines(d, type = "s")
})

## ----fawcett, include = FALSE--------------------------------------------
faw = data.frame(y = c(rep(TRUE, 6), rep(FALSE, 4)),
                 x = c(0.99999, 0.99999, 0.99993, 
                       0.99986, 0.99964, 0.99955, 
                       0.68139, 0.50961, 0.48880, 0.44951))
faw = faw %>% mutate(hyp = x > 0.5)
pred = prediction(predictions = faw[, "x"], labels = faw[, "y"])
auc.estimated = performance(pred, "auc")
auc.estimated@y.values[[1]]
est.auc(x = faw[, "x"], y = faw[, "y"])

## ----fawplot, include = FALSE--------------------------------------------
par(mfrow = c(1, 2))
perf = performance(pred, "tpr", "fpr")
plot(perf)
abline(a = 0, b = 1)
plot(perf, type = "s")
abline(a = 0, b = 1)

## ---- echo = FALSE-------------------------------------------------------
library(dplyr)
fawcett = function(df) {
  L_sorted = df %>% 
    arrange(desc(x), y)
  n_sample = nrow(L_sorted)
  P = sum(L_sorted[["y"]])
  N = n_sample - P
  FP = TP = 0
  R = NULL
  f_prev = -Inf
  i = 1
  for (i in seq(n_sample)) {
    f_i = L_sorted[["x"]][i]
    if (f_i != f_prev) {
      fpr = FP/N
      tpr = TP / P
      R = rbind(R, c(fpr = fpr, tpr = tpr))
      f_prev = f_i
    }
    if (L_sorted[["y"]][i]) {
      TP = TP + 1
    }
    if (!L_sorted[["y"]][i]) {
      FP = FP + 1
    }  
  }
  fpr = FP/N
  tpr = TP / P
  R = rbind(R, c(fpr = fpr, tpr = tpr))
  return(R) 
}
fawcett_roc = fawcett(faw)
df = data_frame(x =x, y = y)
fawcett_roc = fawcett(df)
seq_range = function(x, ...) {
  rx = range(x)
  seq(rx[1], rx[2], ...)
}

## ---- engine='R', include = FALSE, eval = TRUE---------------------------
unlink("profile.do")
file.remove("sample_data.dta")

