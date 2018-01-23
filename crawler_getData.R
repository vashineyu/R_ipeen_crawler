# get ipeen data
library(xml2)
library(plyr)
library(dplyr)
library(data.table)
library(stringi)

dir.create("R_and_EDA")
setwd("R_and_EDA")
base_url <- "http://www.ipeen.com.tw" 
# shop_url <- http://www.ipeen.com.tw/shop/27702-%E9%98%9C%E6%9D%AD%E8%B1%86%E6%BC%BF%E5%BA%97
url <- "http://www.ipeen.com.tw/search/all/000/1-0-126-0/?p="
ix <- seq(1,50,1)

df.allshop.info <- data.frame()

for (i in ix) {
  t_url <- paste0(url, i)
  doc   <- read_html(t_url, encoding = "UTF-8")
  
  # get head
  xpath <- '//div[@class="serShop"]/h3/a'
  shop.title <- xml_text(xml_find_all(doc, xpath))
  shop.url <- xml_attr(xml_find_all(doc, xpath), attr = 'href')
  shop.id <- sapply(strsplit(shop.url, "/shop/|-"), function(x) x[2])
  
  xpath <- '//div[@class="serShop"]/div/ul/li/span'
  shop.addr <- xml_text(xml_find_all(doc, xpath))
  shop.addr <- gsub("\t", "", gsub("\n","",x = shop.addr, fixed=T))
  
  xpath <- '//div[@class="serShop"]/div/ul/li[@class="cate"]'
  shop.cate <- xml_text(xml_find_all(doc, xpath))
  shop.cate <- gsub("\t", "", gsub("\n","",x = shop.cate, fixed=T))
  
  xpath <- '//div[@class="serShop"]/div/ul/li[@class="costEmpty"]'
  shop.avgcost <- xml_text(xml_find_all(doc, xpath))
  shop.avgcost <- as.numeric(stri_extract(str = shop.avgcost, regex = "\\d+"))
  
  #xpath <- '//div[@class="serShop"]/div/ul/li[@class="score"]/a'
  #shop.n.scoring <- xml_text(xml_find_all(doc, xpath))
  #shop.n.scoring <- as.numeric(stri_extract(str = shop.n.scoring, regex = "\\d+"))
  
  tmp.df <- data.frame(
    title = shop.title,
    id = shop.id,
    addr = shop.addr,
    cate.major = stri_trim(sapply(strsplit(shop.cate, split = "/"), function(x) x[1])),
    cate.minor = stri_trim(sapply(strsplit(shop.cate, split = "/"), function(x) x[2])),
    avg.cost = shop.avgcost,
    #n.scoring = shop.n.scoring,
    url = paste0(base_url, shop.url)
  )
  df.allshop.info <- rbind(df.allshop.info, tmp.df)
  print(t_url)
}

if (!dir.exists('tmp') ){
  dir.create('tmp')
}
write.csv(df.allshop.info, "tmp/df_all_shop.csv", 
          fileEncoding = "UTF-8", row.names = F)

