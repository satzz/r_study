### R program

## http://sidc.oma.be/sunspot-data/dailyssn.phpにある太陽黒点数のデータを読み込む(後述のPerlプログラムにより後ろ2列だけを抜き出したデータ)
df <- read.table("dayssn_new.dat", header=F)
colnames(df) <- c("year", "sunspots")

## 自己相関関数の計算
a <- acf(df, type="correlation", plot=TRUE)

## スペクトル密度の計算
s <- spec.pgram(df)

## それぞれのグラフを以下に示す。
