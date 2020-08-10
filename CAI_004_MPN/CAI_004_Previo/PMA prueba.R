# directorio <- "C:/Users/Edna/Documents/Konrad Lorentz/Semillero talentos/Datos"
directorio <- "/home/julian/Documentos/Neural/EDNA"
nombre_archivo <- "Datos seleccionados.csv"
nombre_red <- "RNA.rda"
datos <- read.table(file.path(directorio, nombre_archivo), sep=",", header=TRUE)
names(datos)
preguntas <- c("FRIO","ESCL","BALO","POLI","CARR","COLE","ARMA","PUER","AVIO","SATR","SADE","FOTO","BUSE","FILA")
datos_binarios <-  datos[preguntas] == "Si"
library("neuralnet")

#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library

#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe


traininginput <- datos_binarios
trainingoutput <- datos["ALTO"]

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
#colnames(trainingdata) <- c("Input","Output")
formula_ESTA <- formula(paste("ALTO", paste(preguntas, collapse = " + "), sep = " ~ "))
#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.ESTA <- neuralnet(formula_ESTA,trainingdata, hidden=c(10,5), threshold=0.01)
print(net.ESTA)

#Plot the neural network
plot(net.ESTA)
net.results <- compute(net.ESTA, datos_binarios) #Run them through the neural network
comparativo <- data.frame(real=datos[["ALTO"]], pronostico=net.results$net.result)
plot(pronostico ~ real, data=comparativo)
abline(0,1)
real<-datos[["ALTO"]]
TABLE<-data.frame(Real=real, pronostico=net.results$net.result, Error= abs(real-net.results$net.result))
head(TABLE)
max(abs(real-net.results$net.result))
prueba<- c("No","Si","No","Si","No","No","Si","No","No","No","No","No","No","No")
datos_prueba <- datos[prueba]=="Si"
pru <- c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
compute(net.ESTA, prue)
prue <- data.frame(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)

nombre_red <- "RNA.rda"
save(net.ESTA, file = file.path(directorio, nombre_red))


