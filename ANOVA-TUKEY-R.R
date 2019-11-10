#ANOVA y TUKEY con carga de datos
#Data Science Costa Rica

#Funciones

#Cargar datos

cargar_datos <- function()
{
  archivo_datos <- file.choose()
  
  datos <- read.table(archivo_datos, header=FALSE, sep = ",")
  
  print(datos)
  print(summary(datos))
  
  datos
}

#Convertir dataframe a vector

dataframe_a_vector <- function(datos)
{
  vector_final <- c(1:(nrow(datos)*ncol(datos)))
  
  n=1
  i=1
  j=1
  
  while (j<=ncol(datos))
  {
    while (i<=nrow(datos))
    {
      vector_final[n] <- datos[i,j]
      
      n=n+1
      i=i+1
      
    }
    
    i=1
    j=j+1
    
  }
  
  vector_final
  
}

#Generador de columna de factores

generador_columna_factores <- function(datos)
{
  factores <- c(1:(nrow(datos)*ncol(datos)))
  letras <- c(1:ncol(datos))
  
  j=1
  
  while (j<=ncol(datos)) {
    letras[j]= paste("V",toString(j), sep = "")
      j=j+1
  }
  
  n=1
  i=1
  j=1
  
  while (j<=ncol(datos))
  {
    while (i<=nrow(datos))
    {
      factores[n] <- letras[j]
      
      n=n+1
      i=i+1
      
    }
    
    i=1
    j=j+1
    
  }
  
  factores
  
}

#Test de normalidad

test_normalidad <- function(datos)
{
  
  i=1
  
  while(i<=ncol(datos))
  {
    print(shapiro.test(datos[,i]))
    
    i=i+1
  }
  
}

#Hacer ANOVA

hacer_ANOVA <- function(datos)
{
  test_normalidad(datos)
  boxplot(datos)
  
  dataframe_ANOVA <- data.frame(dataframe_a_vector(datos),generador_columna_factores(datos))
  
  ANOVA <- aov(dataframe_ANOVA[,1] ~ dataframe_ANOVA[,2])
  print(summary(ANOVA))
  
  TUKEY <- TukeyHSD(ANOVA)
  print(TUKEY)
  plot(TUKEY)
}

datos_ANOVA <- cargar_datos()
hacer_ANOVA(datos_ANOVA)