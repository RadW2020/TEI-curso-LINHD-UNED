#------------------------- Introduccion
library(coreNLP)
initCoreNLP()

#------------------------ Tokenization y sentence splitting

#Anotamos el fichero de interés
anno<-annotateFile("/home/accaminero/PoesiaIngles.txt")
anno

# Mostramos cada token (palabra) que hemos identificado
getToken(anno)$token 

# Mostramos a qué frase pertenece cada token
getToken(anno)$sentence

sentLen <- table(getToken(anno)$sentence) # sentLen contiene la longitud de cada frase
hist(sentLen, breaks=30) # pintamos un gráfico con las longitudes de las frases

#------------------------------- Lematizar, POS tag

# Obtener la info de cada token 
token <- getToken(anno)

# Presentar la info de los tokens de la primera frase
token[token$sentence==1,c(1:9)]

# ¿Cuántas ocurrencias hay de cada etiqueta?
table(token$POS)

# Utilizamos el universal tagset 
ut <- universalTagset(token$POS)

# ¿Cuántas ocurrencias hay de cada etiqueta?
table(ut)

#Calculamos la cuenta de los nombres, pronombres, … en cada frase
nounCnt <- tapply(ut == "NOUN", token$sentence, sum)
pronCnt <- tapply(ut == "PRON", token$sentence, sum)
adjCnt <- tapply(ut == "ADJ", token$sentence, sum)
verbCnt <- tapply(ut == "VERB", token$sentence, sum)

# Agrupamos los contadores en un dataframe
posDf <- data.frame(nounCnt,pronCnt,adjCnt,verbCnt)

#Mostramos las primeras posiciones del dataframe, que muestra las primeras frases
head(posDf)

# Pintamos un gráfico que compara cuántos nombres+pronombres hay con los adjetivos
plot(nounCnt+pronCnt,adjCnt,pch=19,cex=2, col=rgb(0,0,0.02))

# Pintamos un gráfico que compara cuántos nombres+pronombres hay con los verbos
plot(nounCnt+pronCnt,verbCnt,pch=19,cex=2, col=rgb(0,0,0.02))


#¿Cuáles son los 5 nombres que más se repiten?
# // Usando el universal tagset…
index <- which(ut=="NOUN")
tab <- table(token$lemma[index])
head(sort(tab,decreasing=TRUE),5)

# // Utilizando el Penn Treebank project …
index <- which(token$POS == "NNP")
tab <- table(token$lemma[index])
head(sort(tab,decreasing=TRUE),5)



#---------------- Analisis de dependencias

#Generamos el árbol de dependencias, y vemos su longitud
parseTree <- getParse(anno)
length(parseTree)

#Muestra el árbol para la primera frase
cat(parseTree[1])

# Mostramos las dependencias para la primera frase
dep <- getDependency(anno)
dep[dep$sentence == 1,]

# El siguiente ejemplo sirve para analizar cuáles son los 3 verbos que toman como sujeto una palabra determinada en mayor 
# número de ocasiones. Por ejemplo “toll”
index <- which(token$lemma[dep$depIndex] == "toll")
depSelf <- dep[index,]
depSelf <- depSelf[depSelf$type == "nsubj",]
sort(table(depSelf$governor),decreasing=TRUE)[1:3]



#------------- Reconocimiento de entidades con nombre

token <- getToken(anno)
#Mostramos cuántos tokens hemos encontrado para cada categoría
table(token$NER)

#Mostramos los tokens etiquetados como DURATION y PERSON
unique(token$lemma[token$NER=="DURATION"])
unique(token$lemma[token$NER=="PERSON"])

# Para una de las personas identificadas en el documento, ¿cuáles son las 3 primeras palabras con las que tiene dependencias de 
# tipo gobernador?, ¿y de tipo dependiente?

index <- which(token$lemma[dep$depIndex] == "Cromwell")
depSelf <- dep[index,]
sort(table(depSelf$governor),decreasing=TRUE)[1:3]
sort(table(depSelf$dependent),decreasing=TRUE)[1:3]

#------------- Correferencias 

#Calculamos y mostramos las primeras correferencias
coref <- getCoreference(anno)
head(coref)

# Mostramos de forma agregada las palabras que forman parte de una correferencia
table(token$token[coref$startIndex[coref$corefId == 10]])



