

amo_9 <- function(){
  
  pkgs <- installed.packages()[, 1]
  if(!'XML' %in% pkgs){
    install.packages('XML', 
                     repos='http://mirrors.ustc.edu.cn/CRAN/')
  }
  if(!'RCurl' %in% pkgs){
    install.packages('RCurl')
  }
  
  require(XML)
  require(RCurl)
  
  d <- debugGatherer()
  cH <- getCurlHandle(followlocation=T, verbose=T, 
                      debugfunction=d$update, 
                      ssl.verifyhost=F, ssl.verifypeer=F, 
                      cookiejar='./cookies', cookiefile='./cookies')
  
  the_url <- 'http://www.cxsfdcglzx.com/touming/wangShangHouse.aspx'
  the_page <- getURL(the_url, encoding='UTF-8', curl=cH)
  
  the_doc <- htmlParse(the_page, encoding='UTF-8')
  the_data <- readHTMLTable(the_doc, stringsAsFactors=F)[[3]]
  the_cnt <- as.numeric(xmlValue(getNodeSet(the_doc, '//span[@id="labPageCount"]')[[1]]))
  EVENTVALIDATION <- xmlGetAttr(getNodeSet(the_doc, '//input[@name="__EVENTVALIDATION"]')[[1]], 'value')
  VIEWSTATE <- xmlGetAttr(getNodeSet(the_doc, '//input[@name="__VIEWSTATE"]')[[1]], 'value')
  pinfo <- c('__EVENTTARGET'='NextPage',
             '__VIEWSTATE'=VIEWSTATE,
             '__EVENTVALIDATION'=EVENTVALIDATION)
  the_page <- postForm(the_url, .params=pinfo, curl=cH, style='post')
  
  for(i in 2:the_cnt){
    the_doc <- htmlParse(the_page, encoding='UTF-8')
    the_data <- rbind(the_data, readHTMLTable(the_doc, stringsAsFactors=F)[[3]])
    EVENTVALIDATION <- xmlGetAttr(getNodeSet(the_doc, '//input[@name="__EVENTVALIDATION"]')[[1]], 'value')
    VIEWSTATE <- xmlGetAttr(getNodeSet(the_doc, '//input[@name="__VIEWSTATE"]')[[1]], 'value')
    pinfo <- c('__EVENTTARGET'='NextPage',
               '__VIEWSTATE'=VIEWSTATE,
               '__EVENTVALIDATION'=EVENTVALIDATION)
    if(i < the_cnt)  the_page <- postForm(the_url, .params=pinfo, curl=cH, style='post')
    cat('\n', i, nrow(the_data))
  }
  names(the_data) <- c('区域','板块','预售许可证号','项目名称','开发企业','可售套数','已售套数')
  write.csv(the_data, '慈溪房产项目列表.csv', row.names=F, quote=F)
  return(the_data)
}

