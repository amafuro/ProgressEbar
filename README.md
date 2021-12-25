
# ProgressEbar

エヴァっぽいプログレスバーが作りたくて

## Installation

以下をコピペでインストール

``` r
# install.packages("devtools")
devtools::install_github("amafuro/ProgressEbar")
```

## Example

Progress.Ebar()の引数をいじれば色々プログレスバーをカスタマイズ出来ます．

``` r
library(ProgressEbar)

#style1,2 is prototypes.　Main style is 3.
#style4　is textProgressbar.

#Progress.Ebar(min = 0,max = 1,style = 3,
 #char = "/",char.color = "cyan",
 #bgchar = "/",bgchar.color = "red",
 #text = "逃げちゃだめだ ",text.color = "blue)
 
n<-100
pe<-Progress.Ebar(min = 1,max = n,style = 3)

for (i in seq(n)) {
  
  set.PE(pe,i)
  Sys.sleep(1 / 100)
  
}
```

