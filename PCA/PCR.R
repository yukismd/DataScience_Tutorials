

########## サンプルデータ ##########

N <- 20

set.seed(123)
x1 <- rnorm(N)
x2 <- x1 + 0.4*rnorm(N)
x3 <- -1*x1 + 0.6*rnorm(N)
x4 <- rnorm(N)

X <- data.frame(x1, x2, x3, x4)
X <- data.frame(scale(X))   # 標準化

apply(X, 2, mean)
apply(X, 2, sd)

write.csv(X, file='../data/pca_sample.csv', row.names=FALSE)

plot(X)   # 散布図
cor(X)    # 相関係数行列


########## 主成分分析 ##########

# scale=TRUEは相関係数行列から実行の意味
# 通常、相関係数行列から実行（元データを標準化すると同義）
#res_pca <- prcomp(X, scale=TRUE)
res_pca <- prcomp(X)   # 標準化済みなのでscaleの指定なし

## 各主成分の寄与率（固有値の平方根）
summary(res_pca)

# 固有値の平方根。要するに主成分の標準偏差
res_pca$sdev
# 以下に一致
apply(res_pca$x, 2, sd)

## 主成分
res_pca$x
# 元データと一緒に表示
data.frame(X, res_pca$x)

## 固有ベクトル
res_pca$rotation

## 負荷量行列（元データと主成分の相関）
cor(X, res_pca$x)



########## 固有値分解 ##########

res_evd <- eigen(cor(X))    # 相関係数行列に対する固有値分解

## 固有値
res_evd$values
# 主成分分析閣下の以下に一致
res_pca$sdev^2

# 固有ベクトル
res_evd$vectors
# 主成分分析結果の以下に一致
res_pca$rotation



########## 特異値分解 ##########

res_svd <- svd(X)
# 以下の分解を実施した
# X = U D V'

U <- res_svd$u

res_svd$d
D <- diag(res_svd$d)

V <- res_svd$v

# 元データ
U %*% D %*% t(V)
# 元データに一致
X

# 以下が一致
U %*% D
res_pca$x   # 主成分

# 以下が一致（固有ベクトル）
V
res_evd$vectors
res_pca$rotation


