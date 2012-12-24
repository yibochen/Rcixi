

amo_0 <- function(){
  
  pkgs <- installed.packages()[, 1]
  if(!'XML' %in% pkgs){
    install.packages('XML', 
                     repos='http://mirrors.ustc.edu.cn/CRAN/')
  }
  
  require(XML)
  a <- htmlParse('http://www.cxsfdcglzx.com', encoding='UTF-8')
  
  b_table <- readHTMLTable(a, stringsAsFactors=F)[[11]]
  b_url <- xmlChildren(getNodeSet(a, '//table')[[11]])
  b_url <- sapply(seq_len(length(b_url)), 
                  function(i){xmlGetAttr(xmlChildren(xmlChildren(b_url[[i]])[[1]])[[2]], 'href')})
  b_url <- paste('http://www.cxsfdcglzx.com/', b_url, sep='')
  jilu <- Sys.time()
  
  a2 <- htmlParse('http://www.cxsfdcglzx.com/touming/wangShangHouse.aspx', encoding='UTF-8')
  b2_table <- readHTMLTable(a2, stringsAsFactors=F)
  
  
  
  b1 <- cbind(b_table, jilu, b_url, stringsAsFactors=F)
  colnames(b1) <- c('项目名称','交易套数','交易面积','交易均价','交易时间','记录时间','链接')
  b1[, 1] <- as.character(b1[, 1])
  b1[, 2] <- as.numeric(b1[, 2])
  b1[, 3] <- as.numeric(b1[, 3])
  b1[, 4] <- as.numeric(b1[, 4])
  b1[, 5] <- as.Date(b1[, 5])
  
  b0 <- read.csv('原始成交.csv', head=T, colClasses='character')
  b0[, 1] <- as.character(b0[, 1])
  b0[, 2] <- as.numeric(b0[, 2])
  b0[, 3] <- as.numeric(b0[, 3])
  b0[, 4] <- as.numeric(b0[, 4])
  b0[, 5] <- as.Date(b0[, 5])
  
  b2 <- rbind(b1, b0)
  b3 <- b2[!duplicated(b2[, 1:5]), ]
  b4 <- b3[order(-as.numeric(b3$交易时间), b3$项目名称), ]
  write.csv(b4, '原始成交.csv', row.names=F, quote=F)
  
  if(substr(date(), 1, 3) == 'Mon'){
    if(winDialog('yesno', 
                 paste('\n', ifelse(nrow(b4) == nrow(b0), 'OK 暂时不需要更新哦', 'OK 更新完毕!!!!'), '\n\n\n', 
                 '阿摸这也太厉害了吧~~~~~~\n\n\n', 
                 '今天是星期一  要不要做周报呢?')) == 'YES'){
      ii1 <- as.character(Sys.Date() - 7)
      ii2 <- as.character(Sys.Date() - 1)
      cat('\n\n参考步骤：\n\n', 
          'amo_1(\'', ii1, '\',\'', ii2, '\')，回车\n\n', 
          '生成《添加属性.csv》\n\n', 
          '在第二列《属性》中添加属性，可参考第三列《板块》，然后保存\n\n', 
          '注意!!!《属性》的可选值有：\n\n', 
          '主城区，周巷(以及其他镇名)，车位，储藏室，商铺\n\n', 
          'amo_2()，回车\n\n大功告成!!\n\n', sep='')
    } else{
      winDialog('ok', '这也太懒了吧!!!\n\n\n')
    }
  } else{
    winDialog('ok', 
              paste('\n', ifelse(nrow(b4) == nrow(b0), 'OK 暂时不需要更新哦', 'OK 更新完毕!!!!'), '\n\n\n', 
              '阿摸这也太厉害了吧~~~~~~', sep=''))
  }
}

