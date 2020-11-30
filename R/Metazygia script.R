####### Cargar archivo de datos

setwd("./data")

library(readr)
Metazygia <- read_delim("Metazygia.csv",
                        ";", escape_double = FALSE,
                        col_types = cols(area = col_number(),
                                         lugar = col_factor(levels = c("bosquecito", "carteles")),
                                         luz = col_logical(),
                                         posicion = col_factor(levels = c("alto", "bajo", "medio"))),
                        trim_ws = TRUE)
View(Metazygia)

####### Supuestos param?tricos

# Prueba de normalidad

area.test <- shapiro.test(Metazygia$area) # Prueba Shapiro para normalidad
print(area.test) # Distribuci?n no normal p>

plotn <- function(x,main="Histograma de frecuencias y distribuci?n normal",
                  xlab="Area",ylab="Variabilidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
} ## Grafico distribuci?n

plotn(Metazygia$area,main="Distribuci?n normal") # Graficamente distribuci?n tampoco es normal

# Prueba de homogeneidad de varianzas

bartlett.test(Metazygia$area ~ Metazygia$lugar)  # Las varianzas son homogeneas
bartlett.test(Metazygia$area ~ Metazygia$luz)  # Las varianzas no son homogeneas
bartlett.test(Metazygia$area ~ Metazygia$posicion)  # Las varianzas son homogeneas

# Homocedasticidad

library(car) # Para homocedasticidad
leveneTest(Metazygia$area ~ Metazygia$lugar) # Las variaciones de los lugares no son equivalentes
leveneTest(Metazygia$area ~ Metazygia$posicion) # Las variaciones de las posiciones no son equivalentes

# No se pueden realizar pruebas param?tricas al incumplir supuestos de normalidad y varianzas.


####### DAG

library(dagitty)
library(ggdag)


# Variables que interaccionan en DAG
# ?rea = a
# Luz = l
# Posici?n = p
# Lugar = y
# Presas = j
# Da?os = d

# Creaci?n de DAG con "a" como outcome o respuesta y "p" como causal o exposure
DAG <- dagitty("dag{
             l -> a ;
             p -> a;
             y -> a;
             d -> a;
             l -> p;
             p -> d;
             d -> j;
             p -> j;
             a -> j;
             l -> j;
             y -> j
             p [exposure]
             a [outcome]
             j [unobserved]
             d [unobserved]
             }")

ggdag(DAG, layout = "circle") + theme_dag() # DAG interacciones

adjustmentSets(x = DAG, exposure = "p", outcome = "a", type="all", effect = "total") # El DAG sugire
                                                                                     # condicionar "l" y "y"

impliedConditionalIndependencies(x = DAG, type = "missing.edge") # Condiciones de independencia para que
                                                                 # se cumpla el modelo

impliedConditionalIndependencies(x = DAG, type = "basis.set") # Condiciones de independencia para que
                                                              # se cumpla el modelo

# Para que se cumpla el DAG debe de haber independencia entre lugar y luz, as? como posici?n y luz
# siendo necesario condicionar lugar. En los casos en que se toman en cuenta da?o (que es inobservable)
# se debe condicionar posici?n y en ocaciones lugar.


####### Informaci?n de sesi?n

sessionInfo()
