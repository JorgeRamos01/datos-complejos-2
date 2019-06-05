rm(list=ls())
library(readr)
library(tidyr)
library(dplyr)

#Cargamos los datos
trn_pos<-read_tsv("~/Documentos/Datos complejos/Tareas-20180904/tarea2/training.pos",col_names =c("word","tag"),quote = "ñ")

#Quitamos espacios en blanco
trn_pos<-na.omit(trn_pos)


#####inciso a
tags<-unique(trn_pos$tag)
#Generamos la matriz de frecuencias de transiciones
trans_tags<-matrix(0L, nrow = length(tags),ncol = length(tags),dimnames = list(tags,tags))
for(i in 2:(length(trn_pos$tag)) ){
  trans_tags[trn_pos[i-1,]$tag,trn_pos[i,]$tag]=trans_tags[trn_pos[i-1,]$tag,trn_pos[i,]$tag]+1
}
#Generamos la matriz de probabilidades de transmicion
prob_trans_tags<-t(apply(trans_tags, 1, function(x){x/sum(x)}))

#Buscaremos la frecuencia de palabras con sus tageos de manera unica
temp3<-trn_pos%>% 
  group_by(word,tag) %>% 
  summarise(freq=n())

#Generamos frecuencias
temp4<-temp3 %>% 
  group_by(tag) %>%
  mutate(stag=sum(freq), prob.emision=freq/stag)

# Cuantas veces aparece la palabra en un tag entre el numero de simobolos totales que emite ese tag
Prob_Emission<-temp4[,c(-3,-4)] %>%
  spread(key = word, value = prob.emision)

#Cambiamos los Na's por 0
Prob_Emission[is.na(Prob_Emission)] <- 0

Prob_Emission2<-as.matrix(Prob_Emission[,-1])
row.names(Prob_Emission2)<-Prob_Emission$tag

####Inciso b
library(tokenizers)
token<-tokenize_ptb("Your contribution to Goodwill will mean more than you may know.")

temp<-unique(trn_pos)
temp2<-table(temp)
path<-1
for (i in 1:length(token[[1]])){
  for (j in 1:length(temp$word)){
    if(token[[1]][i]==temp$word[j]){
      print(paste0(token[[1]][i], " ", "tag->",temp$tag[j]))
    }
  }
}

####Inciso c

paths=1
for (i in token[[1]]){
  for (j in rownames(temp2)){
    if (i==j){
      paths<-paths*sum(temp2[j,])
    }
  }
}
print(paths)

####Inciso d

#Usando CoreNLP
library(coreNLP)
initCoreNLP()

txt<-"Your contribution to Goodwill will mean more than you may know."
output = annotateString(txt)
getToken(output)[,7]

#Usando Viterbi
markov<-row.names(prob_trans_tags)
alfabeto<-colnames(Prob_Emission2)
library(HMM)
hmm1<-initHMM(States = markov,Symbols = alfabeto,transProbs = prob_trans_tags,emissionProbs = Prob_Emission2[markov,])
viterbi(hmm1,observation = token[[1]])

#####Inciso e


laplace<-function(texto,Prob_Emission2,prob_trans_tags){
  require(tokenizers)
  tokens<-tokenize_ptb(texto)[[1]]
  
  #Sustituimos palabras desconocidas por "UKN"
  tokens_orig<-tokens
  tokens[!tokens %in% trn_pos$word]="UKN"
  print(tokens)
  
  ##Se agrega una columna extra a la matriz de probabilidades de emision generando nuevas probabilidades de emision
  temp5<-cbind(Prob_Emission2,UKN=rep(0.001,length(Prob_Emission2[,1])))
  temp5<-t(apply(temp5, 1, function(x){x/sum(x)}))
  markov<-row.names(prob_trans_tags)
  alfabeto<-colnames(Prob_Emission2)
  hmm1<-initHMM(States = markov,Symbols = c(alfabeto,"UKN"),transProbs = prob_trans_tags,emissionProbs = temp5[markov,])    
  cbind(viterbi(hmm1,tokens),tokens_orig)
}

#Probamos con el archivo de prueba
laplace("Coming to Goodwill was the first step toward my becoming totally.",Prob_Emission2,prob_trans_tags)

#Probado con el corpus de prueba development.pos

#Cargamos los datos
devPos<-read_tsv("~/Documentos/Datos complejos/Tareas-20180904/tarea2/development.pos",col_names =c("word","tag"),quote = "ñ")

#Quitamos espacios en blanco
devPos<-na.omit(dev_text)

laplace(paste0(devPos$word[1:50], collapse=" "),Prob_Emission2,prob_trans_tags)
