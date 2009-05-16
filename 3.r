### R Program

### Problem1: 2水準直交表による実験計画法
## 2水準直交表を用いて水準の組み合わせを決定
d <- data.frame(
                A <- c('1', '1', '1', '1', '2', '2', '2', '2'),
                B <- c('1', '1', '2', '2', '1', '1', '2', '2'),
                C <- c('1', '2', '1', '2', '1', '2', '1', '2'),
                )
## 乱獲法により第１日と第２日の実験の順番を決定
o <- data.frame(
                rand1 <- c(rnorm(8)),
                rand2 <- c(rnorm(8))
                )
o_after <- data.frame(
                      day1 = order(o$rand1),
                      day2 = order(o$rand2)
                   )
##   day1 day2
## 1    4    3
## 2    5    1
## 3    8    8
## 4    2    7
## 5    1    5
## 6    6    2
## 7    7    4
## 8    3    6



### Problem 2: 2水準直交表による実験計画法(擬水準法)
d2 <- data.frame(
                 A     = c('1', '1', '2', '2', '3', '3', '1', '1'),
                 B     = c('1', '2', '1', '2', '1', '2', '1', '2'),
                 C     = c('1', '2', '2', '1', '2', '1', '1', '2'),
                 data1 = c( 46,  61,  55,  73,  68,  97,  47,  61),
                 data2 = c( 50,  62,  56,  69,  67,  96,  49,  59)
                )
d2$sum <- d2$data1 + d2$data2
# 実験回数
N <- length(d2$data1) + length(d2$data2)
# 平均
m <- mean(c(d2$data1, d2$data2))

# 各水準によってデータを分割
A_split <- split(d2, d2$A)
## $`1`
##   A B C data1 data2 sum
## 1 1 1 1    46    50  96
## 2 1 2 2    61    62 123
## 7 1 1 1    47    49  96
## 8 1 2 2    61    59 120

## $`2`
##   A B C data1 data2 sum
## 3 2 1 2    55    56 111
## 4 2 2 1    73    69 142

## $`3`
##   A B C data1 data2 sum
## 5 3 1 2    68    67 135
## 6 3 2 1    97    96 193

B_split <- split(d2, d2$B)
C_split <- split(d2, d2$C)

#分散分析表を作成
#変動
T <- sum(d2$sum) ^ 2 / (length(d2$data1) + length(d2$data2))
S <- sum(c(d2$data1,d2$data2)^2) - T
sqmean <- function(a) sum(a)^2/2/length(a)
func <- function(a) sqmean(a$sum)
S_A <- sum(sapply (A_split, func)) - T # 2035.375
S_B <- sum(sapply (B_split, func)) - T # 1225
S_C <- sum(sapply (C_split, func)) - T # 90.25
S_total <- sum(d2$sum^2/2) -T
S_E2 <- S - S_total # 22
S_E1 <- S - (S_A + S_B + S_C + S_E2) # 93.375

# 自由度
f_total <- 15
f_A <- 2
f_B <- 1
f_C <- 1
f_E1 <- 2
f_E2 <- f_total-(f_A + f_B + f_C + f_E1)

# 不偏分散
v_A <- S_A / f_A
v_B <- S_B / f_B
v_C <- S_C / f_C
v_E1 <- S_E1 / f_E1
v_E2 <- S_E2 / f_E2

# 1次誤差の分散比
F_1_2 <- v_E1 / v_E2 # 19.09943
F_lim <- qf(0.95, f_E1, f_E2) # 4.256495

# 1次誤差が有意なのでプーリングせずに分散比を再計算
F_A_1 <- v_A / v_E1 # 21.79786
F_lim <- qf(0.95, f_A, f_E1) # 19
F_B_1 <- v_B / v_E1 # 26.23829
F_lim <- qf(0.95, f_B, f_E1) # 18.51282
F_C_1 <- v_C / v_E1 # 1.933066
F_lim <- qf(0.95, f_C, f_E1) # 18.51282

# 有意となった因子AとBについて水準効果の推定を行う
# 平均
func <- function(a) mean(c(a$data1, a$data2))
mean_A <- sapply(A_split, func)
##      1      2      3 
## 54.375 63.250 82.000 

mean_B <- sapply(B_split, func)
##     1     2 
## 54.75 72.25 

mean_C <- sapply(C_split, func)
##      1      2 
## 65.875 61.125
# 因子Cは有意ではないが、最適水準の組み合わせは(A3, B2, C1)


# 信頼区間の幅
f <- 3 # 自由度(平均,A,B)
rpt <- N / f # 有効反復数
func <- function(a) qt(0.95, f_E1) * sqrt(v_E1 / rpt)
width_A <- sapply(A_split, func)
##        1        2        3 
## 7.054009 9.975876 9.975876 
width_B <- sapply(B_split, func)
##        1        2 
## 7.054009 7.054009

# 特性値
A3 <- A_split$`3`
B2 <- B_split$`2`
spec <- m + (mean(c(A3$data1, A3$data2)) - m) + (mean(c(B2$data1, B2$data2)) - m) # 90.75
# 信頼区間幅
width <- width_A[3] + width_B[2] # 17.27872 
# 信頼区間
range <- c(
           min = spec - width,
           max = spec + width
           )
##     min.3     max.3 
##  73.47128 108.02872



### Problem 3: 二元配置法
b <- rep(1:4, each=6)
a <- rep(1:3)
x <- c(
       173.5, 172.5, 173.0,
       173.1, 172.5, 172.5,

       172.5, 171.5, 172.5,
       173.2, 172.6, 173.0,

       171.0, 170.5, 172.5,
       172.3, 171.7, 172.5,

       173.0, 170.5, 172.0,
       173.4, 171.8, 172.5
       )
df <- data.frame(a,b,x=x)
result <- summary(aov(x ~ a+b, df))
##             Df Sum Sq Mean Sq F value   Pr(>F)   
## f1           2 5.0175  2.5087  7.0476 0.005489 **
## f2           3 4.0312  1.3437  3.7749 0.029110 * 
## Residuals   18 6.4075  0.3560                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 因子A(分析技術者)も因子B(温度計)も棄却域にあり有意

m_total <- mean(x) # 全平均
N <- length(x) # 24
correction <- sum(x)^2/N # 712805.1
S <- sum(x^2) - correction # 15.45625
S_A <- sum(sapply(split(df, df$a), function(chunk) sum(chunk$x)^2 / length(chunk$x))) - correction # 5.0175
S_B <- sum(sapply(split(df, df$b), function(chunk) sum(chunk$x)^2 / length(chunk$x))) - correction # 4.03125
f_A = 3-1
f_B = 4-1
f_E = N-1 - f_A - f_B # 18
S_E <- S - S_A - S_B # 6.4075
V_E <- S_E / f_E # 0.3559722

lsd_A <- qt(0.95, f_E) * sqrt((1/8 + 1/8) * V_E) # 0.5173007
mean_A <- sapply(A_split, function(a) mean(a$x))
diff_A <- matrix(0,3,3)
for (i in 1:2) {
  for (j in i:3) {
    diff_A[i,j] = abs(mean_A[i] - mean_A[j])
    diff_A[j,i] = diff_A[i,j]
  }
}
##        [,1]   [,2]   [,3]
## [1,] 0.0000 1.0500 0.1875
## [2,] 1.0500 0.0000 0.8625
## [3,] 0.1875 0.8625 0.0000
# A1/A2, A2/A3が有意

lsd_B <- qt(0.95, f_E) * sqrt((1/6 + 1/6) * V_E) # 0.5973274
mean_B <- sapply(B_split, function(a) mean(a$x))
diff_B <- matrix(0,4,4)
for (i in 1:3) {
  for (j in i:4) {
    diff_B[i,j] = abs(mean_B[i] - mean_B[j])
    diff_B[j,i] = diff_B[i,j]
  }
}
##      [,1] [,2] [,3] [,4]
## [1,] 0.00 0.30 1.10 0.65
## [2,] 0.30 0.00 0.80 0.35
## [3,] 1.10 0.80 0.00 0.45
## [4,] 0.65 0.35 0.45 0.00
# B1/B3, B1/B4, B2/B3が有意

