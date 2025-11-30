load("muestras.Rdata")

# ¿Distribución Binomial (𝒏 = 𝟏𝟐, 𝒑 = 𝟎. 𝟓)? o ¿Distribución de Poisson(𝝀 = 𝟔)? 
#  compruebo dato a dato por cada distribución

n <- 12
p <- 0.5
lambda <- 6

# Media
media_binomial = n * p
media_poisson = lambda
meanX = mean(muestra_X)
meanY = mean(muestra_Y)

#Varianza
varX = var(muestra_X)
varY = var(muestra_Y)

#Desviación típica
desv_tipica_binomial = sqrt(n * p * (1-p))
desv_tipica_poisson = sqrt(lambda)
sd_X = sd(muestra_X) #Está cerca de desv tipica de poisson (2.43 - 2.44)
sd_Y = sd(muestra_Y)

#Relación entre la media y la varianza = 1 Poisson / < 1 Binomial
relacion_X <- varX /meanX #0,97 casi 1 Poisson
relacion_Y <- varY / meanY # Binomial

#Máximo valor observado (si es > 12 no puede ser Binomial)
maxval_X <- max(muestra_X) #14L Poisson
maxval_Y <- max(muestra_Y) # Binomial

#Esta es de Poisson (forma sesgada a la derecha) MuestraX = Poisson
hist_mX <- hist(muestra_X, 
                col = "blue", 
                main="Números",
                xlab="Números",
                ylab="Frecuencia de los números")

#Esta es Binomial (forma simetrica)  MuestraY = Binomial
hist_mY <- hist(muestra_Y, 
                col = "pink", 
                main="Números",
                xlab="Números",
                ylab="Frecuencia de los números")



#Una vez identificada la muestra que consideras que procede de la distribución Binomial(𝒏 =
#𝟏𝟐, 𝒑 = 𝟎. 𝟓), estudiarás hasta qué punto puede aproximarse mediante una distribución Normal.
#Para ello deberás:

 # 1. Comprobar si se dan las condiciones para que la aproximación Normal sea adecuada.
    
    # Para que sea adecuada, , tendremos que multiplicar el numero de ensayos n por la probabilidad de
    # cada uno de ellos y tendrá que ser >= 5; es decir n * p >= 5. Además teambién tendremos que ver que n * (1-p), es
    # >= 5 también, esto asegurará que la binomial no sea demasiado sesgada.

condiciones_binomial = n * p >= 5
condiciones_binomial_v2 = n * (1 - p) >= 5 #Se cumple, por tanto la distribución normal es razonable


#  2. Determinar los parámetros de la aproximación Normal (media 𝜇 y varianza 𝜎 para la distribución 
      #Binomial(𝒏 = 𝟏𝟐, 𝒑 = 𝟎. 𝟓) considerada.

media_aprox_normal <- n * p
varianza_aprox_normal <- sqrt(n * p * (1-p)) #Luego la distribución normal será N(6,3)

#3. Construir una visualización que permita comparar:
  #– La distribución de la muestra binomial.
  #– La curva de densidad de la distribución Normal elegida como aproximación.

#Simulamos muestra binomial
muestra_binomial <-  rbinom(1000,size = 12, prob = 0.5)
hist_muestra_binomial <- hist(muestra_binomial, 
                col = "green", 
                freq = FALSE,
                main="Binomial vs Normal",
                xlab="Valores",
                ylab="Densidad")

values <- c(0,1,2,3,4,5,6,7,8,9,10,11,12)

#Curva binomial
lines(values, dbinom(values, size = 12, prob = 0.5), col = "red", lwd = 4, type = "h")

#Curva normal aprox
x_seq <- seq(0, 12, by = 0.1) #Secuencia con incrementos del 0.1
lines(x_seq, dnorm(x_seq, mean = media_aprox_normal, sd = varianza_aprox_normal), col = "blue", lwd = 3)














