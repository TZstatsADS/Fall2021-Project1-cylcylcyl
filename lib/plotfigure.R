creat.wordcloud=function(ps.da_ta,colname.data,title.name){
  temp = as.data.frame(ps.da_ta)
  colnames(temp) = colnames(colname.data)
  temp.corpus = Corpus(VectorSource(temp$tokenized_txt))
  temp.corpus = tm_map(temp.corpus, removeNumbers)
  temp.corpus = tm_map(temp.corpus, removePunctuation)
  temp.corpus = tm_map(temp.corpus, removeWords, c("the", "and", stopwords("english")))
  temp.corpus =  tm_map(temp.corpus, stripWhitespace)
  temp.tdm.all<-TermDocumentMatrix(temp.corpus)
  temp.tdm.tidy=tidy(temp.tdm.all)
  temp.tdm.overall=summarise(group_by(temp.tdm.tidy, term), sum(count))
  wordcloud(temp.tdm.overall$term, temp.tdm.overall$`sum(count)`,
            scale=c(3,0.5),
            max.words=100,
            min.freq=1,
            random.order=FALSE,
            rot.per=0.3,
            use.r.layout=T,
            random.color=FALSE,
            colors=brewer.pal(8,"Dark2"))
  title(main = title.name,line = 0.1)
}