library(XML)
library(wordcloud)
library(RCurl)
library(httr)
library(RColorBrewer)

GetXmlChapter <- function(chapter=1,tipo="noun"){
  if(file.exists(paste0("chapter",chapter,".rds"))==FALSE){
    path <- "http://www.perseus.tufts.edu/hopper/xmlchunk?doc=Perseus%3Atext%3A1999.01.0133%3Abook%3D"
    path.c <- paste0(path,chapter)
    chapter.xml <- xmlParse(path.c)
    xml_data <- xmlToList(chapter.xml)
    nlineas <- length(xml_data$text$body$div1)
    lineas <- list()
    l <- 1
    for (i in 1:length(xml_data$text$body$div1)){
      lineas[[l]] <- LeeChunck(xml_data$text$body$div1[i])
      #print(LeeChunck(xml_data$text$body$div1[i]))
      l <- l+1
    }
    lineas <- unlist(lineas)
    #quitamos las comas
    lineas <- lapply(lineas, function(x) gsub(",","",x))
    lineas <- lapply(lineas, function(x) gsub('/.',"",x))
    lineas <- lapply(lineas, function(x) gsub(";","",x))
    lineas <- lapply(lineas, function(x) gsub("?","",x))
    lineas <- lapply(lineas, function(x) gsub(":","",x))
    lineas <- lapply(lineas, function(x) strsplit(x," ",))
    palabras <- unlist(lineas)
    #palabras <- sample(palabras,100)
    def <-  vector(mode="character", length=length(palabras))
    tipo <-  vector(mode="character", length=length(palabras))
    
    for (i in 1:length(palabras)){
      print(sprintf("%s:Traduciendo %s",i,palabras[i]))
      res <- try(GetWord(palabras[i]))
      if(class(res) == "try-error"){
        print("sleep an try again")
        Sys.sleep(1)
        res <- try(GetWord(palabras[i]))
        if(class(res) == "try-error"){
          print("sleep an try again")
          Sys.sleep(1)
          res <- try(GetWord(palabras[i]))
        }
      } 
      def[i] <- res[[1]]
      tipo[i] <- res[[2]]
    }
    res <- cbind(palabras,def,tipo)
    saveRDS(res,file = paste0("chapter",chapter,".rds"))
  }
  res <- readRDS(file = paste0("chapter",chapter,".rds") )
  res <- res[res[,3]==tipo,]
  summary <- as.data.frame(table(res[,2]))
  png(paste0("wordcloud_chapter",chapter,".png"), width=800, height=800, res=300)
  wordcloud(summary$Var1,summary$Freq,colors=brewer.pal(8, "Dark2"),random.order=FALSE,rot.per=0.35,scale=c(1.5,0.3), use.r.layout=FALSE,  max.words=100)
  dev.off()
  #return(g)
}

GetWord <- function(word){
  gc()
  if (word==""){
    return(list(NA,NA))
  }
  word.html <- NULL
  path <- sprintf("http://www.perseus.tufts.edu/hopper/morph?lang=greek&lookup=%s",word)
  #word.html <- htmlTreeParse(path,encoding = "UTF-8")
  while (is.null(word.html)){
    Sys.sleep(0.1)
    tabs <- GET(path)
    word.html <- htmlTreeParse(tabs,encoding = "UTF-8")
    if (!is.null(word.html$children)){
      if (grepl("503",word.html$children)){
          return(c(word,NA))
        }
      }
    }
  word.html <- xmlToList(word.html$children$html)
  
  if (word.html$body[2]$div[2]$div[2]$div[[1]]!="Sorry, no information was found for"){
    def <- word.html$body[2]$div[2]$div[2]$div[1]$div$div$div[3]
    if (!is.null(def)){
      tipo <- strsplit(word.html$body[2]$div[2]$div[2]$div$div$div[3]$table[2,1][[1]]," ")[[1]][1]
    } else {
      tipo <- NA
    }
    lemma <- word.html$body[2]$div[2]$div[2]$div[1]$div$div$div[1]
    if (class(lemma)=="list"){
      Encoding(lemma[[1]]) <- "UTF-8"
      return(list(lemma[[1]],tipo))
    } else {
      if(is.null(def)){
        return(list(word,NA))
      } else {
        Encoding(lemma) <- "UTF-8"
        return(list(lemma,tipo))
      }
    } 
  } else {
    print("Informacion no encontrada")
    return(list(NA,NA))
  }
  Encoding(lemma) <- "UTF-8"
  return(list(lemma,tipo))
}

LeeChunck <- function(chunck){
  lineas <- list()
  l <- 1
  #es una linea
  if (names(chunck)=="l"){
    #cin milestone
    if ("milestone" %in% names(chunck$l)){
      linea <- chunck$l$text
      lineas[[l]] <- linea
      l <- l+1
    } else {
      if ("text" %in% names(chunck$l)){
        linea <- chunck$l$text
        lineas[[l]] <- linea
        l <- l+1
      } else {
        linea <- chunck$l
        lineas[[l]] <- linea
        l <- l+1
      }
    }
  }
  # es un parrafo
  if (names(chunck)=="q"){
    #todos los chunkitos
    for (i in 1:length(chunck$q)){
      l2 <- LeeChunck(chunck$q[i])
      lineas[[l]] <- l2
      l <- l+1
    }
    
  }
  
  lineas <- unlist(lineas)
  return(lineas)
}