# R Program

# 整合度を求める関数
consistency <- function(m, n) {
  M <- c(0.00, 0.00, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49, 1.51, 1.53) # random cosistency index
  CI <- (m - n) / (n - 1) # consistency index
  CR <- CI / M[n] # consistency ratio
  list(CI=CI, CR=CR)
}

# 三角行列の成分から残りを埋める関数
complete_matrix <- function(e, f) {
  n <- length(f) 
  mat <- matrix(0,n,n)
  mat[lower.tri(mat,diag=FALSE)] <- 1/e
  mat <- mat + t(matrix(sapply(mat, function(x) ifelse (x, 1/x, 0)), n, n))
  diag(mat) <- 1
  rownames(mat) <- f
  colnames(mat) <- f
  mat
}

# 行列の重みベクトルと整合度を計算する関数
ahp <- function (A) {
  n <- nrow(A)
# 幾何平均法
  gm <- sapply(1:n, function(i) exp(mean(log(A[i,]))))
  gmsum <- sum(gm)
  gw <- gm / gmsum
  
  gv <- A %*% gw
  gr <- gv / gw
  gm <- mean(gr)
  gc <- consistency(gm, n)
  
# 簡易計算法
  s <- sapply(1:n, function(i) sum(A[,i]))
  A2 <- matrix(0,n,n)
  for (i in 1:n) A2[i,] <- A[i,] / s
  sw <- sapply(1:n, function(i) mean(A2[i,]))
  
  sv <- A %*% sw
  sr <- sv / sw
  sm <- mean(sr)
  sc <- consistency(sm, n)

# 固有ベクトル法
##   e <- eigen(A)
##   l_max <- max(e$values)
##   ec <- consistency(l_max, n)

##   list(gw=gw, gc=gc, sw=sw, sc=sc, ec=ec)
  list(gw=gw, gc=gc, sw=sw, sc=sc)
}

# 項目(fun and creativity, internationality, colleague, salary, welfare, location)と会社(A,B,C,D)の2階層を設けAHP(階層分析)を行う。
# 項目階層の一対比較行列
field <- c('fun and creativity', 'internationality', 'colleague', 'salary', 'welfare', 'location')
by_field <- complete_matrix(
                            c(
                              1,2,2,5,5,
                                2,2,4,4,
                                  1,2,2,
                                    2,2,
                                      1
                              ),
                            field)
##                    fun and creativity internationality colleague salary welfare location
## fun and creativity                1.0             1.00       2.0    5.0       2        2
## internationality                  1.0             1.00       2.0    5.0       4        2
## colleague                         0.5             0.50       1.0    2.0       4        2
## salary                            0.5             0.50       1.0    1.0       1        2
## welfare                           0.2             0.25       0.5    0.5       1        1
## location                          0.2             0.25       0.5    0.5       1        1

# 項目階層の重みベクトルと整合度の算出
field_result <- ahp(by_field)
## $gw
## [1] 0.30267726 0.28098068 0.14049034 0.14049034 0.06768069 0.06768069

## $gc
## $gc$CI
## [1] 0.001660916

## $gc$CR
## [1] 0.001339449


## $sw
## [1] 0.30298786 0.28076564 0.14038282 0.14038282 0.06774043 0.06774043

## $sc
## $sc$CI
## [1] 0.001661797

## $sc$CR
## [1] 0.001340159

## 単純計算法、幾何平均法いずれにおいてもCI,CRともに0.1よりも小さいため整合性があるとみなしてよい。

# 会社階層の一対比較行列
company <- c('companyA', 'companyB', 'companyC', 'companyD')
by_company <- list(
                complete_matrix( c(
                                   2,   1,   1,
                                      1/2, 1/3,
                                             1
                                   ),
                                company),
                complete_matrix( c(
                                   1/2, 1/2, 1/3,
                                          1,   2,
                                               2
                                   ),
                                company),
                complete_matrix( c(
                                   2, 2, 1,
                                      1, 1,
                                         1
                                   ),
                                company),
                complete_matrix( c(
                                   1, 1, 1/3,
                                      1, 1/2,
                                         1/2
                                   ),
                                company),
                complete_matrix( c(
                                   1/2, 1, 1/3,
                                        2,   2,
                                             1
                                   ),
                                company),
                complete_matrix( c(
                                   1,   2, 1,
                                      1/2, 1,
                                           2
                                   ),
                                company)
                )
## $`fun and creativity`
##          companyA companyB companyC  companyD
## companyA      1.0        2      1.0 1.0000000
## companyB      0.5        1      0.5 0.3333333
## companyC      1.0        2      1.0 1.0000000
## companyD      1.0        3      1.0 1.0000000

## $internationality
##          companyA companyB companyC  companyD
## companyA        1      0.5      0.5 0.3333333
## companyB        2      1.0      1.0 2.0000000
## companyC        2      1.0      1.0 2.0000000
## companyD        3      0.5      0.5 1.0000000

## $colleague
##          companyA companyB companyC companyD
## companyA      1.0        2        2        1
## companyB      0.5        1        1        1
## companyC      0.5        1        1        1
## companyD      1.0        1        1        1

## $salary
##          companyA companyB companyC  companyD
## companyA        1        1        1 0.3333333
## companyB        1        1        1 0.5000000
## companyC        1        1        1 0.5000000
## companyD        3        2        2 1.0000000

## $welfare
##          companyA companyB companyC  companyD
## companyA        1      0.5        1 0.3333333
## companyB        2      1.0        2 2.0000000
## companyC        1      0.5        1 1.0000000
## companyD        3      0.5        1 1.0000000

## $location
##          companyA companyB companyC companyD
## companyA      1.0        1      2.0        1
## companyB      1.0        1      0.5        1
## companyC      0.5        2      1.0        2
## companyD      1.0        1      0.5        1

names(by_company) <- field
company_result <- c()
# 会社階層の重み付け
for(i in 1:length(field)) company_result[[i]] <- ahp(by_company[[i]])
# company_result
## $`fun and creativity`
## $`fun and creativity`$gw
## [1] 0.2810186 0.1269645 0.2810186 0.3109982

## $`fun and creativity`$gc
## $`fun and creativity`$gc$CI
## [1] 0.006864762

## $`fun and creativity`$gc$CR
## [1] 0.007627513


## $`fun and creativity`$sw
## [1] 0.2803571 0.1276786 0.2803571 0.3116071

## $`fun and creativity`$sc
## $`fun and creativity`$sc$CI
## [1] 0.006880274

## $`fun and creativity`$sc$CR
## [1] 0.007644749



## $internationality
## $internationality$gw
## [1] 0.1250571 0.3291688 0.3291688 0.2166053

## $internationality$gc
## $internationality$gc$CI
## [1] 0.05108666

## $internationality$gc$CR
## [1] 0.05676295


## $internationality$sw
## [1] 0.1302083 0.3229167 0.3229167 0.2239583

## $internationality$sc
## $internationality$sc$CI
## [1] 0.05220638

## $internationality$sc$CR
## [1] 0.0580071



## $colleague
## $colleague$gw
## [1] 0.3452664 0.2052967 0.2052967 0.2441402

## $colleague$gc
## $colleague$gc$CI
## [1] 0.02014454

## $colleague$gc$CR
## [1] 0.02238282


## $colleague$sw
## [1] 0.3458333 0.2041667 0.2041667 0.2458333

## $colleague$sc
## $colleague$sc$CI
## [1] 0.02017722

## $colleague$sc$CR
## [1] 0.02241913



## $salary
## $salary$gw
## [1] 0.1765894 0.1954283 0.1954283 0.4325540

## $salary$gc
## $salary$gc$CI
## [1] 0.006864762

## $salary$gc$CR
## [1] 0.007627513


## $salary$sw
## [1] 0.1773810 0.1952381 0.1952381 0.4321429

## $salary$sc
## $salary$sc$CI
## [1] 0.006873176

## $salary$sc$CR
## [1] 0.007636863



## $welfare
## $welfare$gw
## [1] 0.1496945 0.3940180 0.1970090 0.2592785

## $welfare$gc
## $welfare$gc$CI
## [1] 0.05108666

## $welfare$gc$CR
## [1] 0.05676295


## $welfare$sw
## [1] 0.1549451 0.3868132 0.1934066 0.2648352

## $welfare$sc
## $welfare$sc$CI
## [1] 0.05190588

## $welfare$sc$CR
## [1] 0.0576732



## $location
## $location$gw
## [1] 0.2928932 0.2071068 0.2928932 0.2071068

## $location$gc
## $location$gc$CI
## [1] 0.08210678

## $location$gc$CR
## [1] 0.09122976


## $location$sw
## [1] 0.2964286 0.2026786 0.2982143 0.2026786

## $location$sc
## $location$sc$CI
## [1] 0.08263023

## $location$sc$CR
## [1] 0.09181136
## いずれの会社においてもCI,CRともに0.1よりも小さいため整合性があるとみなしてよい。

# 階層全体の重み付け
# 幾何平均法
names(company_result) <- field
elements_gw <- c(sapply(company_result, function(x) x$gw))
# 会社の重み行列
weight_matrix_gw <- matrix(elements_gw, length(company), length(field))
##          fun and creativity internationality colleague    salary   welfare  location
## companyA          0.2810186        0.1250571 0.3452664 0.1765894 0.1496945 0.2928932
## companyB          0.1269645        0.3291688 0.2052967 0.1954283 0.3940180 0.2071068
## companyC          0.2810186        0.3291688 0.2052967 0.1954283 0.1970090 0.2928932
## companyD          0.3109982        0.2166053 0.2441402 0.4325540 0.2592785 0.2071068

rownames(weight_matrix_gw) <- company
colnames(weight_matrix_gw) <- field
# 階層全体の重みベクトル
result_gw <- weight_matrix_gw %*% field_result$gw
## companyA 0.2234669
## companyB 0.2279019
## companyC 0.2670029
## companyD 0.2816283
## D > C > B > A

# 簡単計算法
names(company_result) <- field
elements_sw <- c(sapply(company_result, function(x) x$sw))
# 会社の重み行列
weight_matrix_sw <- matrix(elements_sw, length(company), length(field))
##          fun and creativity internationality colleague    salary   welfare  location
## companyA          0.2803571        0.1302083 0.3458333 0.1773810 0.1549451 0.2964286
## companyB          0.1276786        0.3229167 0.2041667 0.1952381 0.3868132 0.2026786
## companyC          0.2803571        0.3229167 0.2041667 0.1952381 0.1934066 0.2982143
## companyD          0.3116071        0.2239583 0.2458333 0.4321429 0.2648352 0.2026786

rownames(weight_matrix_sw) <- company
colnames(weight_matrix_sw) <- field
# 階層全体の重みベクトル
result_sw <- weight_matrix_sw %*% field_result$sw
## companyA 0.2255294
## companyB 0.2253510
## companyC 0.2649809
## companyD 0.2841388
## D > C > A > B
## 単純計算法、幾何平均法いずれにおいても最有力候補は会社Dである。
