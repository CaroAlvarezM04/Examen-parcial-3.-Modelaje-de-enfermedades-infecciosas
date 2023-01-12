## EXAMEN PARCIAL 3 ##
## CAROLINA ÁLVAREZ ##

## CODIGO BASE PARA MODELO SIR SIN DEMOGRAFIA ##

install.packages("deSolve")
library(deSolve) #Cargar la libreria

####SIR####
SIR <- function(t, state, parameters) { #Crear una nueva función
  with(as.list(c(state, parameters)), { #Definir las ecuaciones
    dS <- -b*S*I
    dI <- b*S*I -d*I
    dR <- d*I
    list(c(dS, dI, dR)) #lista que contiene las ecuaciones del modelo
  })
}
#en el modelo no se incluye la tasa de nacimiento y muerte porque si la poblacion se conserva estas se eliminan en las ecuaciones

parameters <- c(b = 2, d = 4) #Establecer los parametros
state <- c(S = 10^6, I = 1, R = 0) #establecer las condiciones iniciales
t <- seq(0, 20, by = 0.001) 
out <- ode(state, t, SIR, parameters) 

## Grafica del modelo ##
matplot(out[ , 1], out[ , 2:4], type = "l", xlab = "time", ylab = "Población",
        main = "SIR", lwd = 2) 
legend("topright", c("Susceptible", "Infectado","Recuperado"), col = 1:3,lty=1:3,cex=0.5)
#recuadro para señalar cada ecuacion