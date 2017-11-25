# データの読み込み
# 右下の画面で事前の作業ディレクトリをセットしておく
# getwd()
load("data.RData")
print(data)

print("length:")
print(length(data))

print("summary:")
print(summary(data))

# 度数分布を取得
table(data)

# ,ヒストグラム表示
hist(data, breaks = seq(-0.5, 9.5, 1))


# 標本分散
print(var(data))

# 標準偏差
print(sd(data))
print(sqrt(var(data)))

# とりあえずポアソン分布
# 平均3.56のポアソン分布に従って"種子数がyであると観察される確率"を生成
y <- 0:9 # 種子数9個
prob <- dpois(y, lambda = 3.56)
plot(y, prob, type = "b", lty = 2)
print(prob)


# λの値毎の当てはまりをみてみる
logL <- function(m) sum(dpois(data, m, log = TRUE))

plot.poisson <- function(lambda) {
  y <- 0:9
  prob <- dpois(y, lambda = lambda)
  
  hist(data, breaks = seq(-0.5, 9.5, 1), ylim = c(0, 15),
       main = "", xlab = "", ylab = "")
  points(y, prob * 50)
  lines(y,  prob * 50, lty = 2)
  
  title(sprintf("lambda= %.1f\n logL= %.1f", lambda, logL(lambda)))
}

layout(matrix(1:9, byrow = T, ncol = 3))
junk <- sapply(seq(2, 5.2, 0.4), plot.poisson)


# 対数尤度とlog L(λ)の関係を確認
logL <- function(m) sum(dpois(data, m, log = TRUE))
lambda <- seq(2, 5, 0.1)
plot(lambda, sapply(lambda, logL), type = "b")

