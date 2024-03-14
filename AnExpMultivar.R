## Analisis exploratorio multivariado

carros = read.csv('cars.csv')

## explorar posibles relaciones entre variables
## numericas

library(GGally)

ggpairs(carros[,-8])

## Por medio de un parallelplot se puede
## identificar las caracteristicas principales
## de los carros segun su origen

plot(log(carros$weightlbs), carros$mpg)
cor(carros$weightlbs, carros$mpg)


library(lattice)

parallelplot(carros[,-8],group=carros$brand,horizontal.axis=F,auto.key=T)

## Como puede verse, los carros japoneses se encuentran
## abajo en el tama?o del motor, la potencia y el peso
## pero arriba en mpg, mientras que los americanos
## estan altos en tama?o y potencia pero bajos en 
## rendimiento


## Utilicemos caras de Chernoff para identificar
## individuos similares y diferentes en un set
## de datos

library(aplpack)

## para no saturar el grafico, solo representamos
## los primeros 25 carros y solo las variables numericas

faces(carros[1:25,-8])

#carros[c(11,12),]

## Utilizando los valores de las variables para
## representar caracteristicas fisicas de los rostros
## se pueden identificar similitudes y diferencias
## entre los datos, por ejemplo, los carros 2 y 5 son
## muy similares entre si pero muy diferentes por ejemplo
## al carro 24

carros[c(2,5),]

carros[c(2,24),]

## Evolucion de varias variables en funcion
## del tiempo

library(tidyverse)
library(highcharter)

## Funcion para calcular la moda de una
## variable cuantitativa discreta

moda = function(x) {
  t = sort(table(x),decreasing=T)
  m = as.numeric(names(t)[1])
  return(m)
}

tenxpais = carros %>% group_by(year,brand) %>% summarise(mpg=mean(mpg), brand=unique(brand), peso=mean(weightlbs), cil=moda(cylinders))

hchart(tenxpais,type='bubble',hcaes(x=year,y=mpg,size=peso,color=cil,group=brand)) %>% hc_title(text="MPG x A?o")

## Utilizando un diagrama de radar se pueden
## visualizar los valores promedio de varias
## variables numericas por categorias

library(fmsb)

resumcarros = carros %>% 
  group_by(brand) %>% 
  summarize(mpg = mean(mpg), pot=mean(hp), peso = mean(weightlbs),acel = mean(time.to.60), motor=mean(cubicinches),cyl=moda(cylinders))

marcas = as.factor(resumcarros$brand)

radarchart(resumcarros[,-1], maxmin=F)      

legend("topleft",legend=marcas,fill=marcas)

