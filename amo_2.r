

amo_2 <- function(){
  a <- read.csv('添加属性.csv', head=T, stringsAsFactors=F)
  a <- a[, -c(6, 7, 9)]
  a[, 1] <- gsub('号楼|号|楼|幢|及|-|#|、|南区|北区|[0-9]|储藏室|地下室', 
                 '', a[, 1])
  a_total <- a[, c(2, 4, 5, 6)]
  a_total[!a_total[, 1] %in% c('车位', '商铺', '储藏室'), 1] <- '住宅'
  a_total[, 1] <- factor(a_total[, 1], levels=c('住宅','商铺','车位','储藏室'))
  a_park <- a[a[, 2] == '车位', c(1, 4, 5, 6)]
  a_shop <- a[a[, 2] == '商铺', c(1, 4, 5, 6)]
  a_storage <- a[a[, 2] == '储藏室', c(1, 4, 5, 6)]
  a_house <- a[!a[, 2] %in% c('车位', '商铺', '储藏室'), ]
  a_house_downtown <- a_house[a_house[, 2] == '主城区', c(1, 4, 5, 6)]
  a_house_country <- a_house[a_house[, 2] != '主城区', c(1, 4, 5, 6)]
  a_house_location <- tapply(a_house[, 3], a_house[, 1], '[', 1)
  a_house_location <- data.frame(item=names(a_house_location), 
                                 location=a_house_location, 
                                 stringsAsFactors=F, 
                                 row.names=NULL)
  a_house_location <- rbind(a_house_location, 
                            data.frame(item='合计', location='合计'))
  f_sum <- function(d, title='楼盘'){
    cnt <- tapply(d[, 2], d[, 1], sum)
    size <- tapply(d[, 3], d[, 1], sum)
    money <- tapply(d[, 4], d[, 1], sum)
    cnt <- cnt[!is.na(cnt)]
    size <- size[!is.na(size)]
    money <- money[!is.na(money)]
    item=c(names(cnt), '合计')
    cnt <- c(cnt, sum(cnt))
    size <- c(size, sum(size))
    money <- c(money, sum(money))
    size_avg <- size/cnt
    price_avg <- money/size
    money_avg <- money/cnt
    d2 <- data.frame(cnt, size, size_avg, price_avg, 
                     money_avg, money, row.names=NULL)
    d3 <- cbind(item, d2)
    names(d3) <- c(title, '套数', '总成交面积', '平均面积', 
                   '平均单价', '平均总价', '总金额')
    d4 <- d3[c(order(-d3[-nrow(d3), 5]), nrow(d3)), ]
    return(d4)
  }
  d_total <- f_sum(a_total, '类型')
  d_park <- f_sum(a_park)
  d_shop <- f_sum(a_shop)
  d_storage <- f_sum(a_storage)
  d_house_downtown <- f_sum(a_house_downtown)
  d_house_country <- f_sum(a_house_country)
  d_house_downtown <- cbind(
    data.frame(板块=a_house_location$location[match(d_house_downtown[, 1], a_house_location$item)], 
               row.names=NULL), 
    d_house_downtown)
  d_house_country <- cbind(
    data.frame(板块=a_house_location$location[match(d_house_country[, 1], a_house_location$item)], 
               row.names=NULL), 
    d_house_country)
  write.csv(d_total, '0_总览.csv', row.names=F, quote=F)
  write.csv(d_park, '4_车位.csv', row.names=F, quote=F)
  write.csv(d_shop, '3_商铺.csv', row.names=F, quote=F)
  write.csv(d_storage, '5_储藏室.csv', row.names=F, quote=F)
  write.csv(d_house_downtown, '1_主城区住宅.csv', row.names=F, quote=F)
  write.csv(d_house_country, '2_乡镇住宅.csv', row.names=F, quote=F)
}

