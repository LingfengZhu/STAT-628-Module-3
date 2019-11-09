text_cleaned <- read.csv("C:/Users/zyjxx/Desktop/text_cleaned.csv", header=FALSE)
txt = as.character(text_cleaned[,2])
pos<- read.csv("C:/Users/zyjxx/Desktop/module 3/positive-words.txt", sep="",header = F)
neg<- read.csv("C:/Users/zyjxx/Desktop/module 3/negative-words.txt", sep="",header = F)
pos=as.character(pos[,1])
neg=as.character(neg[,1])
#find all locations of a word
index = function(word){
  out = which(txt==word | txt==paste0(word,"s"))
  return(out)
}

#find the sentiment of neighborhood of a word
senti=function(i) {
  neighbor=txt[c(i-1,i-2,i-3,i+1,i+2,i+3)]
  out = sum(neighbor %in% pos)-sum(neighbor %in% neg)
  return(sign(out))
}

wordlist_df = function(wordlist){
  wordlist_chi2 = data.frame(wordlist=wordlist)
  for (i in 1:length(wordlist)){
    wordindex = index(wordlist[i])
    sign = sapply(wordindex,senti)
    wordlist_chi2$positive[i] = sum(sign==1)
    wordlist_chi2$negative[i] = sum(sign==-1)
  } 
  return(wordlist_chi2)
}



dish_list = c('taco','salsa','chips','burrito','rice','guacamole','asada','tortilla','salad','nacho','enchiladas')
wordlist_df(dish_list)
