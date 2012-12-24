

amo_1 <- function(i1=Sys.Date()-7, i2=Sys.Date()-1){
  a <- read.csv('原始成交.csv', head=T)
  a <- a[, -6]
  a[, 5] <- as.Date(a[, 5])
  a <- a[(a[, 5] >= i1) & (a[, 5] <= i2), ]
  a$总价 <- a[, 3] * a[, 4]
  b <- a[order(a[, 1]), ]
  detail <- read.csv('慈溪房产项目列表.csv', head=T, stringsAsFactors=F)
  detail <- detail[, c(4, 2)]
  b$板块 <- detail[match(b[, 1],detail[, 1]), 2]
  b$属性 <- ''
  b <- b[, c(1,9,8,2,3,4,5,7,6)]
  
  files <- list.files()
  if('添加属性.csv' %in% files){
    if(winDialog('yesno', 
                 '\n《添加属性.csv》这个文件已经存在啦~~~~\n\n\n要不要覆盖掉呢?') == 'YES'){
      write.csv(b, '添加属性.csv', row.names=F, quote=F)
      winDialog('ok', '\n\n\n已经覆盖掉啦!!!\n\n\n可以去添加属性啦!!!\n\n\n')
    } else{
      winDialog('ok', '\n\n\n不愿意覆盖就算啦!!!\n\n\n能告诉我为什么不要覆盖吗?\n\n\n')
    }
  } else{
    write.csv(b, '添加属性.csv', row.names=F, quote=F)
       winDialog('ok', '\n\n\n可以去添加属性啦!!!\n\n\n')
  }
}

