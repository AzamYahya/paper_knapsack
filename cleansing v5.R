library(tidyverse)
pm <- read_csv("Z:/paper/working/dec_15.csv")


palet <- read.csv("Z:/paper/working/palet.csv")

palet <- na.omit(palet)

art_card <- pm[grepl("COIN",  unlist(pm[,c(colnames(pm)[grep("DESC", colnames(pm))])])),]

library(stringr)

subs <- str_extract_all(unlist(art_card[,grep("DESC", colnames(art_card))]),"[[0-9]]{3,}")

#available substances
used_subs <- c(180,190,200,210,250,260,300,310,350)

subs2  <-  as.data.frame(unlist(str_split_fixed(subs, ",", 10)))

subs2 <- sapply(subs2, function(x) gsub("[[:punct:]]|c","", x)) %>% as.data.frame()

subs2 <- as.data.frame(sapply(subs2, function(x) as.numeric(as.character(x))))

subs3 <- sapply(subs2, function(x) ifelse(x %in% used_subs, x, NA)) %>% as.data.frame()

###combine subs and subs2

subs4 <- cbind.data.frame(art_card,subs3)

subs5 <- subs4 %>% 
  select(matches("DESC|IMPORTE|QUANTITY|DATE|UNIT VAL"), c(ncol(subs4):(ncol(subs4)-9)))

subs5 <- subs5 %>% select(-matches("ASS|C\\/"))

#find which grammgaes in subs5 are in palet data frame

##for loop version
dfnew<-list()
for (i in 1:nrow(subs5)){
  dfnew[[i]]<-palet[which(palet$gram %in% subs5[i,6:15]),]
}

#vectorized operation
dfnew2 <- lapply(split(subs5[6:15], as.numeric(as.character(row.names(subs5)))),
                 function(x) palet[palet$gram %in% unlist(x),])




library(FLSSS)

colnames(subs5)[4] <- "declare_quantity"

subs5 %>%   select(c(4)) %>%  
  apply(1,function(x) FLSSS(len = 0, v = rep(palet$weight.of.pallete,50),
                            target = x, ME = 3)) 

wei <- subs5 %>%   select(c(4))

gg <- list()
for(i in 1:nrow(wei)){
  gg[[i]] <- FLSSS(len = 0, v= rep(dfnew2[[i]]$weight.of.pallete, 100), target = wei$declare_quantity[i], ME = 3)  
}


###apply subset sum to get the combination
gg <- list()
for(i in 1:nrow(wei)){
  if(nrow(dfnew2[[i]]) != 0){
    gg[[i]] <- FLSSS(len = 0, v= rep(dfnew2[[i]]$weight.of.pallete, 50), target = wei$declare_quantity[i], ME = 1)  
  } else {
    gg[[i]] <- FLSSS(len = 0, v= rep(palet$weight.of.pallete, 100), target = wei$declare_quantity[i], ME = 1)
  }
}


##Map the weight of each palet
gg <- list()
for(i in 1:nrow(wei)){
  if(nrow(dfnew2[[i]]) != 0){
    gg[[i]] <- rep(dfnew2[[i]]$weight.of.pallete, 50)[FLSSS(len = 0, v= rep(dfnew2[[i]]$weight.of.pallete, 50), target = wei$declare_quantity[i], ME = 1)[[1]]]  
  } else {
    gg[[i]] <- rep(palet$weight.of.pallete, 100)[FLSSS(len = 0, v= rep(palet$weight.of.pallete, 100), target = wei$declare_quantity[i], ME = 1)[[1]]]
  }
}


##changes the name to importer

ff <- setNames(gg, subs5[,2])


##change in to data frame

df_weight <- as.data.frame(do.call(qpcR:::cbind.na, ff))



###change the columns to name of importer

# colnames(df_weight) <- subs5$`NAME OF IMPORTERS`


#Map value weight with size and grammages

df_size <- sapply(df_weight, function(x)
  ifelse(x %in% palet$weight.of.pallete,
         paste(palet$length,palet$breath,palet$gram, sep = "x"), NA)) %>%
  data.frame

colnames(df_size) <- subs5[,2]
###gather

df_size <- df_size[1:ncol(df_size)] %>% gather(importer, size)



##bind the date and declare value

df_weight <- cbind(df_weight, subs5[,c(1,5)])

df_weight <- df_weight[1:ncol(df_weight)] %>%
  gather(importer, weight, -(ncol(df_weight):(ncol(df_weight)-1)))


##combine

combine <- cbind(df_size$size, df_weight)

colnames(combine)[1] <- "size"

###remove missing rows

combine <- na.omit(combine)


###clean importer names
combine$importer <- gsub("[[:punct:]]|[[:digit:]]", "", combine$importer)


combine %>% group_by(importer) %>% summarise(sum(weight))
