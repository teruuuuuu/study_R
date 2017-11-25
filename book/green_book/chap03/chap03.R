# GLM(一般化線形モデル)を扱う
# 体サイズと種子数が施肥処理にどう影響を与えるか確認する
# csvファイルの読み込み
d <- read.csv("data3a.csv")
print(d) 

# x列(体サイズ)を表示
print(d$x)
# print(head(d, n=10)$x)

# y列(種子数)を表示
print(d$y)

# f列(施肥処理)を表示
print(d$f)

# データオブジェクトの型表示
print(class(d))
print(class(d$x))
print(class(d$y))
print(class(d$f))

# summaryでが意表を表示
print(summary(d))

# まずはデータを図示する
plot(d$x, d$y, pch = c(21,19)[d$f])
legend("topleft", legend=c("C", "T"), pch = c(21, 19))
plot(d$f, d$y)

# 最尤推定
fit <- glm(y ~ x, data = d, family = poisson) # y ~ xの最尤推定
print(fit)
print(summary(fit))
# 最大対数尤度
print(logLik(fit)) # パラメータ数が2子であることがわかる

# ポアソン回帰モデルによる体サイズxにおける平均種子数λの予測
plot(d$x, d$y, pch = c(21, 19)[d$f])
xx <- seq(min(d$x), max(d$y), length = 100)
lines(xx, exp(1.29 + 0.0757 * xx), lwd = 2) # 最尤推定のパラメータを渡している
# predictを使っても同じ表示が行える
yy <- predict(fit, newdata = data.frame(x = xx), type="response")
lines(xx, yy, lwd=2)

# 種子数と施肥処理の最尤推定
fit.f <- glm(y ~ f, data = d, family = poisson)
print(fit.f)
# 肥料なしの水準: λi = exp(2.05 + 0) = exp(2.05) = 7.77
# 肥料ありの水準: λi = exp(2.05 + 0.0128) = exp(2.0628) = 7.87
# 　→ 肥料有りの方が平均種子数が若干大きい
print(summary(fit.f)) 
print(logLik(fit.f))

# 体サイズ+施肥処理と種子数の最尤推定
fit.all <- glm(y ~ x + f, data = d, family = poisson)
print(summary(fit.all))
# 種子数と施肥処理の最大対数尤度よりもちょっとよくなっている
print(logLik(fit.all))
