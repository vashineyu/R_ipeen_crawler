# parse shop details
library(xml2)
library(plyr)
library(dplyr)
library(stringi)
library(data.table)

# clean special tags
tag.clean <- function(htmlstring) {
  return(gsub("<.*?>|&.*?:", "", htmlstring))
}

if ( !dir.exists('tmp/ipeen_txt') ) {
  dir.create('tmp/ippen_txt')
} 
txt.outdir <- "tmp/ipeen_txt/"


df.allshop.info <- fread("data/df_all_shop.csv", sep = ",", header = T, 
                         stringsAsFactors = F, encoding = 'UTF-8')
baseurl <- "http://www.ipeen.com.tw/shop/"
suffix <- "/comments?p="

df.shop.details <- data.frame()
df.review <- data.frame()
# loop over shops
for (i in 1:nrow(df.allshop.info)) {
  #url <- df.allshop.info[i,]$url
  url <- paste0(baseurl, df.allshop.info[i, ]$id)
  print(url)
  doc <- read_html(url)
  # get meta info
  xpath <- '//div[@class="scalar"]'
  
  xpath_i <- paste0(xpath, "/p[1]/span[1]/meter")
  shop.score.mix <- xml_attr(xml_find_all(doc, xpath_i), attr = "value")
  
  xpath_i <- paste0(xpath, "/p[1]//em")
  shop.n.scoring <- xml_text(xml_find_all(doc, xpath_i))
  shop.n.scoring <- as.numeric(gsub(pattern = ",", 
                                 replacement = "", 
                                 x = shop.n.scoring))
  
  xpath_i <- paste0(xpath, "/p[3]//em")
  shop.n.view <- xml_text(xml_find_all(doc, xpath_i))
  shop.n.view <- as.numeric(gsub(pattern = ",", 
                                 replacement = "", 
                                 x = shop.n.view))
  
  xpath_i <- paste0(xpath, "/p[4]//em")
  shop.n.favorite <- xml_text(xml_find_all(doc, xpath_i))
  shop.n.favorite <- as.numeric(gsub(pattern = ",", 
                                 replacement = "", 
                                 x = shop.n.favorite))
  # get detail scoring
  xpath <- '//section[@id="shop-metainfo"]'
  xpath_i <- paste0(xpath, "//dd//meter")
  tmp <- xml_attr(xml_find_all(doc, xpath_i), attr = "value")
  shop.score.delicious <- as.numeric(tmp[1])
  shop.score.serivce <- as.numeric(tmp[2])
  shop.score.atom <- as.numeric(tmp[3])
  
  # get shop summary words
  xpath <- '//div[@class="summary"]'
  shop.summary <- xml_text(xml_find_all(doc, xpath))
  shop.summary <- gsub("\t", "", gsub("\n","",x = shop.summary, fixed=T))
  
  xpath <- '//section[@class="review-list"]'
  xpath_i <- paste0(xpath, "/h3")
  shop.n.share <- xml_text(xml_find_all(doc, xpath_i))
  shop.n.share <- as.numeric(stri_extract(str = shop.n.share, regex = "\\d+"))
  
  tmp <- data.frame(id = df.allshop.info[i, ]$id,
                    n.scoring = shop.n.scoring,
                    scoring.mix = shop.score.mix,
                    scoring.delicious = shop.score.delicious,
                    scoring.service = shop.score.serivce,
                    scoring.atom = shop.score.atom,
                    n.view = shop.n.view,
                    n.favorite = shop.n.favorite,
                    n.share = shop.n.share,
                    shop.summary = shop.summary
                    )
  
  
  df.shop.details <- rbind(df.shop.details, tmp)
  #
  page.i <- ceiling(shop.n.share/5)
  if (is.na(page.i)){
    page.i <- 0
  }
  
  # start get review articles, get page_url
  # loop over review articles
  for (p.i in 1:page.i) {
    if (page.i == 0) {
      break
    } 
    else {
    c_url <- paste0(baseurl, df.allshop.info[i, ]$id, suffix, p.i) # comment page index
    print(c_url)
    doc <- read_html(c_url)
    
    xpath <- '//section[@class="review-list"]'
    xpath_i <- paste0(xpath, '//h2/a')
    review.url <- xml_attr(xml_find_all(doc, xpath_i), attr = "href")
    
    xpath_i <- paste0(xpath, '//div[@class="metadata"]/p[1]')
    review.pubdate <- xml_text(xml_find_all(doc, xpath_i))
    review.pubdate <- stri_extract(str = review.pubdate, regex = "\\d.*")
    
    xpath_i <- paste0(xpath, '//div[@class="metadata"]/p[2]/*')
    review.other <- xml_text(xml_find_all(doc, xpath_i))
    # resp, views, useful, lolipop (4xn)
    review.other <- as.numeric(stri_extract(str = review.other, regex = "\\d+"))
    review.other <- matrix(review.other, ncol = 4, byrow = T)
    
    xpath_i <- paste0(xpath, '//p[@class="name"]')
    review.author <- xml_text(xml_find_all(doc, xpath_i))
    
    xpath_i <- paste0(xpath, '//p[@class="name"]/a')
    review.uid <- basename(xml_attr(xml_find_all(doc, xpath_i), attr = "href"))
    
    df.tmp <- data.frame(id = df.allshop.info[i, ]$id,
                         author.id = review.uid,
                         author.name = review.author,
                         review.date = review.pubdate,
                         n.resp = review.other[,1],
                         n.view = review.other[,2],
                         n.useful = review.other[,3],
                         n.lolipop = review.other[,4],
                         review.url = review.url)
    
    
    # get into article urls to get reviewers' opinions
    tmp_stroage <- data.frame()
    for (review.i in 1:nrow(df.tmp)) {
      
      r_url <- paste0("http://www.ipeen.com.tw", 
                      as.character(df.tmp[review.i,]$review.url))
      r_doc <- read_html(r_url, options = "HUGE")
      
      xpath <- '//div[@class="scalar"]//dl[@class="rating"]/dd'
      r.scoring <- xml_text(xml_find_all(r_doc, xpath))
      r.delicious <- r.scoring[1]
      r.service <- r.scoring[2]
      r.atom <- r.scoring[3]
      
      xpath <- '//div[@class="info user"]'
      xpath_i <- paste0(xpath, '//div[@class="tags"][1]//span')
      r.postTags <- paste0(xml_text(xml_find_all(r_doc, xpath_i)), collapse = "|")
      
      xpath_i <- paste0(xpath, '//div[@class="tags"][2]//span')
      r.favorite <- paste0(xml_text(xml_find_all(r_doc, xpath_i)), collapse = "|")
      
      xpath <- '//p[@class="price"]'
      r.cost <- stri_trim(strsplit(x = xml_text(xml_find_all(r_doc, xpath)),
                         split = "ï¼?")[[1]][2])
      
      xpath <- '//div[@class="article-keyword"]/a'
      r.articleTags <- paste0(xml_text(xml_find_all(r_doc, xpath)), collapse = "|")
      
      xpath <- '//div[@class="description"]//img'
      r.n.imgs <- length(xml_text(xml_find_all(r_doc, xpath)))
      
      #xpath <- '//div[@class="article-content-inner"]//*'
      xpath <- '//div[@class="description"]//*'
      article.content <- paste0(xml_text(xml_find_all(r_doc, xpath)), collapse = "")
      article.content <- gsub("\t", "", gsub("\n","",x = article.content, fixed=T))
      #article.content <- `Encoding<-`(article.content, "UTF-8")
      
      # if (article.content == "" ) { # dealing with some exception
      #   xpath <- '//div[@class="description"]//*'
      #   article.content <- paste0(xml_text(xml_find_all(r_doc, xpath)), collapse = "")
      #   article.content <- `Encoding<-`(article.content, "UTF-8")
      #   article.content <- gsub("\t", "", gsub("\n","",x = article.content, fixed=T))
      # }
      
      article.content <- tag.clean(article.content)
      
      xx <- data.frame(r.delicious = r.delicious,
                       r.service = r.service,
                       r.atom = r.atom,
                       r.nImgs = r.n.imgs,
                       r.favorite = r.favorite,
                       r.postTags = r.postTags,
                       r.articleTags = r.articleTags)
      tmp_stroage <- rbind(tmp_stroage, xx)
      # write article content into txt file
      out.name <- paste0("a", 
                         as.character(df.tmp[review.i,]$id),
                         "_", as.character(df.tmp[review.i,]$author.id),
                         "_", basename(r_url),
                         ".txt")
      write(article.content, file = paste0(txt.outdir, out.name))
    }
    df.tmp <- cbind(df.tmp, tmp_stroage)
    
    df.review <- rbind(df.review, df.tmp)
    
    }
  }
  print("done")
}

saveRDS(df.review, file = "tmp/df_review.rds")
saveRDS(df.shop.details, file = "tmp/df_shopDetails.rds")


## testing region
'
xpath_i <- paste0(xpath, "//dd//meter")
xml_text(xml_find_all(doc, xpath_i))
xml_attr(xml_find_all(doc, xpath_i), attr = "href")
'

#http://www.ipeen.com.tw/shop/27702/comments?p=1