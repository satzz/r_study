### R Program

### 1. サイズ100の正規乱数標本の生成

A <- rnorm(100, mean=0,   sd=1)
B <- rnorm(100, mean=0.5, sd=1)
C <- rnorm(100, mean=5,   sd=1)


### 2. 母平均の95%信頼区間の推定

tval <- qt(0.025, 100) # -1.983972, 両側検定
coeff <-tval /sqrt(99)

meanA <- mean(A) # 0.1782382
sdA <- sd(A) # 1.128631
widthA <- sdA * coeff
print (meanA-widthA)
print (meanA+widthA) 
## 標本Aの95%信頼区間は[-0.046807,0.4032834]

meanB <- mean(B) # 0.4068589
sdB <- sd(B) # 1.161505
widthB <- sdB * coeff
print (meanB-widthB)
print (meanB+widthB)
## 標本Bの95%信頼区間は[0.1752587,0.638459]

meanC <- mean(C) # 4.983492
sdC <- sd(C) # 1.037820
widthC <- sdC * coeff
print (meanC-widthC)
print (meanC+widthC)
## 標本Cの95%信頼区間は[4.776554,5.19043]


### 3. 対応のない等分散二標本間の95%t検定

## 帰無仮説 : 二標本の母平均間に差がない

t.test(A, B, var.equal=TRUE)
# p-value = 0.1596 > 0.05 なので帰無仮説が95%信頼区間で棄却されない(二標本の母平均間に差があるとはいえない)
t.test(A, C, var.equal=TRUE)
# p-value < 2.2e-16 なので帰無仮説は棄却される(二標本の母平均間に差がある)
t.test(B, C, var.equal=TRUE)
# p-value < 2.2e-16 なので帰無仮説は棄却される(二標本の母平均間に差がある)


### 4. 母数の95%片側検定

## 帰無仮説 : 母平均 mu = 1200 (カタログは正しい)
## 対立仮説 : 母平均 mu < 1200 (カタログは正しくない)
t_sample <- sqrt(9) * (1100-1200) / 150 # 標本から算出した t_sample = -2
t_limit <- qt(0.05, 10) # t分布における切断点 t_limit = -1.812461
## |t_sample| > |t_limit| なので帰無仮説は棄却される(カタログは正しくない)
