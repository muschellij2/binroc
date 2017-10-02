x = c(rep(0, 52), rep(1, 32),
      rep(0, 35), rep(1, 50))
y = c(rep(0, 84), rep(1, 85))
df = data.frame(x,y)
write.csv(df, "sample_data.csv", row.names = FALSE)
tab = table(x, y)
sens = tab[2,2] / sum(tab[,2])
spec = tab[1,1] / sum(tab[,1])
fpr = 1-spec
area_of_tri = 1/2 * sens * fpr
area_of_quad = sens * spec + 1/2 * spec * (1-sens)
auc = area_of_tri + area_of_quad

step_auc = sens * spec




est_auc = function(x, y) {
  x1 = x[y==1]
  x0 = x[y==0]
  n = 1000000
  c1 = sample(x1, size = n, replace = TRUE)
  c0 = sample(x0, size = n, replace = TRUE)
  mean(c1 > c0)
}
true_auc = est_auc(x, y)
library(ROCR)

pred = prediction(x, y)
perf = performance(pred, "tpr", "fpr")
plot(perf)
abline(a = 0, b = 1)
plot(perf, type = "s")
abline(a = 0, b = 1)
auc_est = performance(pred, "auc")
auc_est@y.values[[1]]

df = data.frame(x = x, y = y)
mod = glm(y ~ x, data = df, family = binomial())
unique(predict(mod, type = "response"))

pred = prediction(x, y)
perf = performance(pred, "tpr", "fpr")
plot(perf)

library(pROC)
myroc = pROC::roc(response = y, predictor = x)
plot(myroc)
plot(y = myroc$sensitivities,
     x = 1 - myroc$specificities, type = "l")
plot(y = myroc$sensitivities,
     x = 1 - myroc$specificities, type = "s")
plot(myroc, type = "s")


# library(AUC) # NA
AUC::auc(AUC::sensitivity(x, y)) #NA
