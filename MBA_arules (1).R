"Arules package


El paquete de R arules implementa el algoritmo Apriori para la identificación de itemsets frecuentes 
y la creación de reglas de asociación a través de la función apriori(). También implementa el algoritmo 
Eclat con la función eclat().

Tanto apriori() como eclat() reciben como argumento un objeto transaction con los datos de las 
transacciones, un argumento parameter que determina las características de los itemsets o reglas 
generadas (por ejemplo, el soporte mínimo) y un argumento control que determina el comportamiento del
algoritmo (por ejemplo, ordenación de los resultados). La función apriori() también se incluye el 
argumento aparence que impone restricciones sobre las reglas generadas, por ejemplo, crear solo reglas 
que contengan un determinado item. El resultado de ambas funciones es un objeto de tipo association 
que puede ser manipulado con toda una serie de funciones que ofrece arules. Entre las principales destacan:
  
summary(): muestra un resumen de los resultados.

inspect(): muestra los resultados.

length(): número de elementos (reglas o itemsets) almacenados.

items(): extrae los items que forman un itemset o una regla. En el caso de reglas, combina los items de
antecedente (LHS) y del consecuente (RHS). .

sort(): ordena los resultados.

subset: filtrado de los resultados.


Ejemplo cesta de la compra


Supóngase que se dispone del registro de todas las compras que se han realizado en un supermercado. 
El objetivo del análisis es identificar productos que tiendan a comprarse de forma conjunta para así 
poder situarlos en posiciones cercanas dentro de la tienda y maximizar la probabilidad de que los 
clientes compren.

Para este ejemplo se emplea el set de datos Groceries del paquete arules, que contiene un registro de 
todas las ventas realizadas por un supermercado durante 30 días. En total se dispone de 9835 transacciones 
formadas por combinaciones de 169 productos. El objeto Groceries almacena la información en un formato 
propio de este paquete (descrito más adelante). Para representar mejor lo que suele ocurrir en la práctica,
se ha reestructurado la información en formato de tabla." 

library(tidyverse)
datos <- read_xlsx("C:/Users/hijinio/Downloads/datos_groceries.xlsx")
head(datos)

"Cada línea del archivo contiene la información de un item y el identificador de la transacción (compra) 
a la que pertenece. Esta es la estructura en la que comúnmente se almacenan los datos dentro de una base 
de datos y que, en el ámbito de las transacciones, recibe el nombre de tabla larga o single. Otra forma 
en la que se pueden encontrar los datos de transacciones es en formato matriz o tabla ancha, en el que 
cada fila contiene todos los items que forman parte de una misma transacción, este formato recibe el 
nombre de cesta o basket.

Tal y como se ha definido previamente, el concepto de transacción hace referencia al conjunto de items 
o eventos que ocurren de forma conjunta. Para este caso de estudio, compras de supermercado, cada 
transacción está formada por todos los productos que se compran a la vez, es decir, el vínculo de unión
no es el cliente sino cada una de las cestas de la compra. Por ejemplo, la transacción con 
id_compra == 14 está formada por 3 items."

datos %>% filter(id_compra == "14") %>% pull(item)


"Lectura de Datos


Con la función read.transactions() se pueden leer directamente los datos de archivos tipo texto y 
almacenarlos en un objeto de tipo transactions, que es la estructura de almacenamiento que emplea arules. 
Esta función tiene los siguientes argumentos:
  
file: nombre del archivo que se quiere leer.

format: estructura en la que se encuentran almacenados los datos, "basket" si cada línea del archivo es 
una transacción completa, o "single" si cada línea representa un item.

sep: tipo de separación de los campos.

cols: si el formato es de tipo "basket", un entero que indica la columna que contiene el identificador 
de las transacciones. Para el formato "single", es un vector con los nombres (o posiciones) de las dos 
columnas que identifican las transacciones y los items, respectivamente.

rm.duplicates: valor lógico indicando si se eliminan los items duplicados en una misma transacción. 
Por lo general, es conveniente eliminarlos, el interés suele estar en qué items ocurren de forma conjunta, 
no en qué cantidad.

quote: carácter que se utiliza como comillas.

skip: número de líneas que hay que saltar desde el comienzo del fichero.

Es importante recordar que los objetos de tipo transactions solo trabajan con información booleana, 
es decir, con la presencia o no de cada uno de los items en la transacción. Por lo tanto, si el set 
de datos contiene variables numéricas, estas deben de ser discretizadas en intervalos o niveles. Para
discretizar una variable numérica, se puede emplear la función discretize() o bien otras alternativas 
como la función case_when() del paquete dplyr.

Los objetos transactions, se almacenan internamente como un tipo de matriz binaria. Se trata de una matriz
de valores 0/1, con una fila por cada transacción, en este caso cada compra, y una columna por cada posible
item, en este caso productos. La posición de la matriz (i,j) tiene el valor 1 si la transacción i contiene
el item j.


También es posible convertir un objeto dataframe en uno de tipo transactions con la función 
as(dataframe, ''transactions''). Para lograrlo, primero hay que convertir el dataframe en una lista en 
la que cada elemento contiene los items de una transacción. Este proceso puede ser muy lento si el 
dataframe tiene muchos registros, por lo que suele ser mejor crear un archivo de texto con los datos 
e importarlo mediante read.transactions().

# CONVERSIÓN DE UN DATAFRAME A UN OBJETO TIPO TRANSACTION
# Se convierte el dataframe a una lista en la que cada elemento  contiene los
# items de una transacción"


datos_split <- split(x = datos$item, f = datos$id_compra)
transacciones <- as(datos_split, Class = "transactions")
transacciones


## transactions in sparse format with
##  9835 transactions (rows) and
##  169 items (columns)

"Otra alternativa es convertir el dataframe en una matriz 0/1 en la que cada fila es una transacción 
y cada columna uno de los posibles items.

# CONVERSIÓN DE UNA MATRIZ A UN OBJETO TIPO TRANSACTION"

datos_matriz <- datos %>%
  as.data.frame() %>%
  mutate(valor = 1) %>%
  spread(key = item, value = valor, fill = 0) %>%
  column_to_rownames(var = "id_compra") %>%
  as.matrix()
transacciones <- as(datos_matriz, Class = "transactions")
transacciones


## transactions in sparse format with
##  9835 transactions (rows) and
##  169 items (columns)



"Exploración de items


Uno de los primeros análisis que conviene realizar cuando se trabaja con transacciones es explorar 
su contenido y tamaño, es decir, el número de items que las forman y cuáles son. Mediante la función 
inspect() se muestran los items que forman cada transacción."

inspect(transacciones[1:5])

##     items                      transactionID
## [1] {citrus fruit,                          
##      margarine,                             
##      ready soups,                           
##      semi-finished bread}                  1
## [2] {coffee,                                
##      tropical fruit,                        
##      yogurt}                               2
## [3] {whole milk}                           3
## [4] {cream cheese,                          
##      meat spreads,                          
##      pip fruit,                             
##      yogurt}                               4
## [5] {condensed milk,                        
##      long life bakery product,              
##      other vegetables,                      
##      whole milk}                           5

"También es posible mostrar los resultados en formato de dataframe con la función DATAFRAME() o con 
as(transacciones, ''dataframe'')."

df_transacciones <- as(transacciones, Class = "data.frame")
# Para que el tamaño de la tabla se ajuste mejor, se convierte el dataframe a tibble
as.tibble(df_transacciones) %>% head()

#Para extraer el tamaño de cada transacción se emplea la función size().

tamanyos <- size(transacciones)
summary(tamanyos)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   3.000   4.409   6.000  32.000

quantile(tamanyos, probs = seq(0,1,0.1))
##   0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
##    1    1    1    2    3    3    4    5    7    9   32


data.frame(tamanyos) %>%
  ggplot(aes(x = tamanyos)) +
  geom_histogram() +
  labs(title = "Distribución del tamaño de las transacciones",
       x = "Tamaño") +
  theme_bw()


"La gran mayoría de clientes compra entre 3 y 4 productos y el 90% de ellos compra como máximo 9.

El siguiente análisis básico consiste en identificar cuáles son los items más frecuentes 
(los que tienen mayor soporte) dentro del conjunto de todas las transacciones. 
Con la función itemFrequency() se puede extraer esta información de un objeto tipo transactions.
El nombre de esta función puede causar confusión. Por "frecuencia" se hace referencia al soporte de
cada item, que es la fracción de transacciones que contienen dicho item respecto al total de todas 
las transacciones. Esto es distinto a la frecuencia de un item respecto al total de items, de ahí 
que la suma de todos los soportes no sea 1."

frecuencia_items <- itemFrequency(x = transacciones, type = "relative")
frecuencia_items %>% sort(decreasing = TRUE) %>% head(5)

##       whole milk other vegetables       rolls/buns             soda 
##        0.2555160        0.1934926        0.1839349        0.1743772 
##           yogurt 
##        0.1395018

"Si se indica el argumento type =''absolut'', la función itemFrequency() devuelve el número de 
transacciones en las que aparece cada item."

frecuencia_items <- itemFrequency(x = transacciones, type = "absolute")
frecuencia_items %>% sort(decreasing = TRUE) %>% head(5)


##       whole milk other vegetables       rolls/buns             soda 
##             2513             1903             1809             1715 
##           yogurt 
##             1372


"El listado anterior muestra que los 5 productos que más se venden son: whole milk, other vegetables, 
rolls/buns, soda y yohurt

Es muy importante estudiar cómo se distribuye el soporte de los items individuales en un conjunto 
de transacciones antes identificar itemsets frecuentes o crear reglas de asociación, ya que, 
dependiendo del caso, tendrá sentido emplear un límite de soporte u otro. Por lo general, 
cuando el número de posibles items es muy grande (varios miles) prácticamente todos los 
artículos son raros, por lo que los soportes son muy bajos.


Itemsets


Con la función apriori() se puede aplicar el algoritmo Apriori a un objeto de tipo transactions y 
extraer tanto itemsets frecuentes como reglas de asociación que superen un determinado soporte y confianza. 
Los argumentos de esta función son:
  
data: un objeto del tipo transactions o un objeto que pueda ser convertido a tipo transactions,
por ejemplo un dataframe o una matriz binaria.

parameter: lista en la que se indican los parámetros del algoritmo.

support: soporte mínimo que debe tener un itemset para ser considerado frecuente. 
Por defecto es 0.1.

minlen: número mínimo de items que debe tener un itemset para ser incluido en los resultados. 
Por defecto 1.

maxlen: número máximo de items que puede tener un itemset para ser incluido en los resultados. 
Por defecto 10.

target: tipo de resultados que debe de generar el algoritmo, pueden ser "frequent itemsets", 
"maximally frequent itemsets", "closed frequent itemsets", "rules" o "hyperedgesets".

confidence: confianza mínima que debe de tener una regla para ser incluida en los resultados. 
Por defecto 0.8.

maxtime: tiempo máximo que puede estar el algoritmo buscando subsets. Por defecto 5 segundos.

appearance: lista que permite definir patrones para restringir el espacio de búsqueda, 
por ejemplo, especificando qué items pueden o no pueden aparecer.

control: lista que permite modificar aspectos internos de algoritmo como la ordenación de los itemsets, 
si se construye un árbol con las transacciones, aspectos relacionados con el uso de memoria, etc.

Se procede a extraer aquellos itemsets, incluidos los formados por un único item, que hayan sido comprados
al menos 30 veces. En un caso real, este valor sería excesivamente bajo si se tiene en cuenta la cantidad 
total de transacciones, sin embargo, se emplea 30 para que en los resultados aparezcan un número suficiente
de itemsets y reglas de asociación que permitan mostrar las posibilidades de análisis que ofrece el paquete
arules."

soporte <- 30 / dim(transacciones)[1]
itemsets <- apriori(data = transacciones,
                    parameter = list(support = soporte,
                                     minlen = 1,
                                     maxlen = 20,
                                     target = "frequent itemset"))

## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime    support
##          NA    0.1    1 none FALSE            TRUE       5 0.00305033
##  minlen maxlen            target   ext
##       1     20 frequent itemsets FALSE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 30 
## 
## set item appearances ...[0 item(s)] done [0.00s].
## set transactions ...[169 item(s), 9835 transaction(s)] done [0.01s].
## sorting and recoding items ... [136 item(s)] done [0.00s].
## creating transaction tree ... done [0.00s].
## checking subsets of size 1 2 3 4 5 done [0.01s].
## writing ... [2226 set(s)] done [0.00s].
## creating S4 object  ... done [0.00s].

summary(itemsets)
## set of 2226 itemsets
## 
## most frequent items:
##       whole milk other vegetables           yogurt  root vegetables 
##              556              468              316              251 
##       rolls/buns          (Other) 
##              241             3536 
## 
## element (itemset/transaction) length distribution:sizes
##    1    2    3    4    5 
##  136 1140  850   98    2 
## 
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.000   2.000   2.412   3.000   5.000 
## 
## summary of quality measures:
##     support             count        
##  Min.   :0.003050   Min.   :  30.00  
##  1st Qu.:0.003660   1st Qu.:  36.00  
##  Median :0.004779   Median :  47.00  
##  Mean   :0.007879   Mean   :  77.49  
##  3rd Qu.:0.007219   3rd Qu.:  71.00  
##  Max.   :0.255516   Max.   :2513.00  
## 
## includes transaction ID lists: FALSE 
## 
## mining info:
##           data ntransactions    support confidence
##  transacciones          9835 0.00305033          1

"Se han encontrado un total de 2226 itemsets frecuentes que superan el soporte mínimo de 0.003908286, 
la mayoría de ellos (1140) formados por dos items. En el siguiente listado se muestran los 20 itemsets 
con mayor soporte que, como cabe esperar, son los formados por items individuales (los itemsets de menor 
tamaño).

# Se muestran los top 20 itemsets de mayor a menor soporte"

top_20_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)[1:20]
inspect(top_20_itemsets)

##      items                         support    count
## [1]  {whole milk}                  0.25551601 2513 
## [2]  {other vegetables}            0.19349263 1903 
## [3]  {rolls/buns}                  0.18393493 1809 
## [4]  {soda}                        0.17437722 1715 
## [5]  {yogurt}                      0.13950178 1372 
## [6]  {bottled water}               0.11052364 1087 
## [7]  {root vegetables}             0.10899847 1072 
## [8]  {tropical fruit}              0.10493137 1032 
## [9]  {shopping bags}               0.09852567  969 
## [10] {sausage}                     0.09395018  924 
## [11] {pastry}                      0.08896797  875 
## [12] {citrus fruit}                0.08276563  814 
## [13] {bottled beer}                0.08052872  792 
## [14] {newspapers}                  0.07981698  785 
## [15] {canned beer}                 0.07768175  764 
## [16] {pip fruit}                   0.07564820  744 
## [17] {other vegetables,whole milk} 0.07483477  736 
## [18] {fruit/vegetable juice}       0.07229283  711 
## [19] {whipped/sour cream}          0.07168277  705 
## [20] {brown bread}                 0.06487036  638

# Para representarlos con ggplot se convierte a dataframe 

as(top_20_itemsets, Class = "data.frame") %>%
  ggplot(aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Itemsets más frecuentes", x = "itemsets") +
  theme_bw()


"Si se quieren excluir del análisis los itemsets formados únicamente por un solo item, se puede, 
o bien aplicar de nuevo la función apriori() especificando minlen = 2, o filtrar los resultados con 
la función size().

# Se muestran los 20 itemsets más frecuentes formados por más de un item."

inspect(sort(itemsets[size(itemsets) > 1], decreasing = TRUE)[1:20])

##      items                              support    count
## [1]  {other vegetables,whole milk}      0.07483477 736  
## [2]  {rolls/buns,whole milk}            0.05663447 557  
## [3]  {whole milk,yogurt}                0.05602440 551  
## [4]  {root vegetables,whole milk}       0.04890696 481  
## [5]  {other vegetables,root vegetables} 0.04738180 466  
## [6]  {other vegetables,yogurt}          0.04341637 427  
## [7]  {other vegetables,rolls/buns}      0.04260295 419  
## [8]  {tropical fruit,whole milk}        0.04229792 416  
## [9]  {soda,whole milk}                  0.04006101 394  
## [10] {rolls/buns,soda}                  0.03833249 377  
## [11] {other vegetables,tropical fruit}  0.03589222 353  
## [12] {bottled water,whole milk}         0.03436706 338  
## [13] {rolls/buns,yogurt}                0.03436706 338  
## [14] {pastry,whole milk}                0.03324860 327  
## [15] {other vegetables,soda}            0.03274021 322  
## [16] {whipped/sour cream,whole milk}    0.03223183 317  
## [17] {rolls/buns,sausage}               0.03060498 301  
## [18] {citrus fruit,whole milk}          0.03050330 300  
## [19] {pip fruit,whole milk}             0.03009659 296  
## [20] {domestic eggs,whole milk}         0.02999492 295



"Filtrado de itemsets


Una vez que los itemsets frecuentes han sido identificados mediante el algoritmo Apripori, pueden ser 
filtrados con la función subset(). Esta función recibe dos argumentos: un objeto itemset o rules y
una condición lógica que tienen que cumplir las reglas/itemsets para ser seleccionados. La siguiente 
tabla muestra los operadores permitidos:
  
  Operador	Significado
&	          AND
%in%	      contiene cualquier de los siguientes elementos
%ain%	      contiene todos de los siguientes elementos
%pin%	      contiene parcialmente los siguientes elementos

Como esta función tiene el mismo nombre que una función del paquete básico de R, para evitar errores, 
es conveniente especificar el paquete donde se encuentra.

Se procede a identificar aquellos itemsets frecuentes que contienen el item newspapers."

itemsets_filtrado <- arules::subset(itemsets,
                                    subset = items %in% "newspapers")
itemsets_filtrado

## set of 80 itemsets

# Se muestran 10 de ellos
inspect(itemsets_filtrado[1:10])
##      items                                 support     count
## [1]  {newspapers}                          0.079816980 785  
## [2]  {meat,newspapers}                     0.003050330  30  
## [3]  {newspapers,sliced cheese}            0.003152008  31  
## [4]  {newspapers,UHT-milk}                 0.004270463  42  
## [5]  {newspapers,oil}                      0.003152008  31  
## [6]  {newspapers,onions}                   0.003152008  31  
## [7]  {hygiene articles,newspapers}         0.003050330  30  
## [8]  {newspapers,sugar}                    0.003152008  31  
## [9]  {newspapers,waffles}                  0.004168785  41  
## [10] {long life bakery product,newspapers} 0.003457041  34

#Se repite el proceso pero, esta vez, con aquellos itemsets que contienen newspapers y whole milk.

itemsets_filtrado <- arules::subset(itemsets,
                                    subset = items %ain% c("newspapers", "whole milk"))
itemsets_filtrado

## set of 16 itemsets
# Se muestran 10 de ellos

inspect(itemsets_filtrado[1:10])
##      items                                  support     count
## [1]  {newspapers,whole milk}                0.027351296 269  
## [2]  {chocolate,newspapers,whole milk}      0.003152008  31  
## [3]  {brown bread,newspapers,whole milk}    0.004067107  40  
## [4]  {margarine,newspapers,whole milk}      0.003152008  31  
## [5]  {butter,newspapers,whole milk}         0.003152008  31  
## [6]  {newspapers,pastry,whole milk}         0.003863752  38  
## [7]  {citrus fruit,newspapers,whole milk}   0.003355363  33  
## [8]  {newspapers,sausage,whole milk}        0.003050330  30  
## [9]  {bottled water,newspapers,whole milk}  0.004067107  40  
## [10] {newspapers,tropical fruit,whole milk} 0.005083884  50

"Puede observarse que muchos itemsets están a su vez contenidos en itemsets de orden superior, es decir,
existen itemsets que son subsets de otros. Para identificar cuáles son, o cuales no lo son, se puede 
emplear la función is.subset(). Encontrar los itemsets que son subsets de otros itemsets implica comparar 
todos los pares de itemsets y determinar si uno está contenido en el otro. La función is.subset() realiza 
comparaciones entre dos conjuntos de itemsets y devuelve una matriz lógica que determina si el itemset de 
la fila está contenido en cada itemset de las columnas.

# Para encontrar los subsets dentro de un conjunto de itemsets, se compara el
# conjunto de itemsets consigo mismo."


subsets <- is.subset(x = itemsets, y = itemsets, sparse = FALSE)

#Para conocer el total de itemsets que son subsets de otros itemsets se cuenta el número total de TRUE 
#en la matriz resultante.

# El suummary de una matriz lógica devuelve el número de TRUEs

sum(subsets)
## [1] 11038



"Reglas de asociación


Para crear las reglas de asociación se sigue el mismo proceso que para obtener itemsets frecuentes pero,
además de especificar un soporte mínimo, se tiene que establecer una confianza mínima para que una regla
se incluya en los resultados. En este caso, se emplea una confianza mínima del 70%."

soporte <- 30 / dim(transacciones)[1]
reglas <- apriori(data = transacciones,
                  parameter = list(support = soporte,
                                   confidence = 0.70,
                                   # Se especifica que se creen reglas
                                   target = "rules"))
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime    support
##         0.7    0.1    1 none FALSE            TRUE       5 0.00305033
##  minlen maxlen target   ext
##       1     10  rules FALSE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 30 
## 
## set item appearances ...[0 item(s)] done [0.00s].
## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
## sorting and recoding items ... [136 item(s)] done [0.00s].
## creating transaction tree ... done [0.01s].
## checking subsets of size 1 2 3 4 5 done [0.01s].
## writing ... [19 rule(s)] done [0.00s].
## creating S4 object  ... done [0.00s].


summary(reglas)
## set of 19 rules
## 
## rule length distribution (lhs + rhs):sizes
## 3 4 5 
## 7 9 3 
## 
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   3.000   3.000   4.000   3.789   4.000   5.000 
## 
## summary of quality measures:
##     support           confidence          lift           count      
##  Min.   :0.003050   Min.   :0.7000   Min.   :2.740   Min.   :30.00  
##  1st Qu.:0.003203   1st Qu.:0.7047   1st Qu.:2.758   1st Qu.:31.50  
##  Median :0.003559   Median :0.7164   Median :2.804   Median :35.00  
##  Mean   :0.003767   Mean   :0.7373   Mean   :3.044   Mean   :37.05  
##  3rd Qu.:0.004169   3rd Qu.:0.7500   3rd Qu.:2.984   3rd Qu.:41.00  
##  Max.   :0.005694   Max.   :0.8857   Max.   :4.578   Max.   :56.00  
## 
## mining info:
##           data ntransactions    support confidence
##  transacciones          9835 0.00305033        0.7

"Se han identificado un total de 19 reglas, la mayoría de ellas formadas por 4 items en el antecedente 
(parte izquierda de la regla)."

inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
##      lhs                     rhs                    support confidence     lift count
## [1]  {citrus fruit,                                                                  
##       root vegetables,                                                               
##       tropical fruit,                                                                
##       whole milk}         => {other vegetables} 0.003152008  0.8857143 4.577509    31
## [2]  {butter,                                                                        
##       root vegetables,                                                               
##       yogurt}             => {whole milk}       0.003050330  0.7894737 3.089723    30
## [3]  {citrus fruit,                                                                  
##       root vegetables,                                                               
##       tropical fruit}     => {other vegetables} 0.004473818  0.7857143 4.060694    44
## [4]  {brown bread,                                                                   
##       other vegetables,                                                              
##       root vegetables}    => {whole milk}       0.003152008  0.7750000 3.033078    31
## [5]  {butter,                                                                        
##       onions}             => {whole milk}       0.003050330  0.7500000 2.935237    30
## [6]  {curd,                                                                          
##       tropical fruit,                                                                
##       yogurt}             => {whole milk}       0.003965430  0.7500000 2.935237    39
## [7]  {curd,                                                                          
##       domestic eggs}      => {whole milk}       0.004778851  0.7343750 2.874086    47
## [8]  {butter,                                                                        
##       tropical fruit,                                                                
##       yogurt}             => {whole milk}       0.003355363  0.7333333 2.870009    33
## [9]  {root vegetables,                                                               
##       tropical fruit,                                                                
##       whipped/sour cream} => {other vegetables} 0.003355363  0.7333333 3.789981    33
## [10] {butter,                                                                        
##       curd}               => {whole milk}       0.004880529  0.7164179 2.803808    48
## [11] {domestic eggs,                                                                 
##       sugar}              => {whole milk}       0.003558719  0.7142857 2.795464    35
## [12] {other vegetables,                                                              
##       root vegetables,                                                               
##       tropical fruit,                                                                
##       yogurt}             => {whole milk}       0.003558719  0.7142857 2.795464    35
## [13] {baking powder,                                                                 
##       yogurt}             => {whole milk}       0.003253686  0.7111111 2.783039    32
## [14] {tropical fruit,                                                                
##       whipped/sour cream,                                                            
##       yogurt}             => {whole milk}       0.004372140  0.7049180 2.758802    43
## [15] {citrus fruit,                                                                  
##       other vegetables,                                                              
##       root vegetables,                                                               
##       tropical fruit}     => {whole milk}       0.003152008  0.7045455 2.757344    31
## [16] {butter,                                                                        
##       pork}               => {whole milk}       0.003863752  0.7037037 2.754049    38
## [17] {butter,                                                                        
##       coffee}             => {whole milk}       0.003355363  0.7021277 2.747881    33
## [18] {domestic eggs,                                                                 
##       other vegetables,                                                              
##       whipped/sour cream} => {whole milk}       0.003558719  0.7000000 2.739554    35
## [19] {root vegetables,                                                               
##       tropical fruit,                                                                
##       yogurt}             => {whole milk}       0.005693950  0.7000000 2.739554    56



"Evaluación de las reglas


Además de la confianza y el soporte, existen otras métricas que permiten cuantificar la calidad de las
reglas y la probabilidad de que reflejen relaciones reales. Algunas de las más empleadas son:
  
Lift: el estadístico lift compara la frecuencia observada de una regla con la frecuencia esperada
simplemente por azar (si la regla no existe realmente). El valor lift de una regla "si X, entonces Y" 
se obtiene acorde a la siguiente ecuación:
  soporte(union(X,Y))/soporte(X) * soporte(Y)
  
Cuanto más se aleje el valor de lift de 1, más evidencias de que la regla no se debe a un artefacto 
aleatorio, es decir, mayor la evidencia de que la regla representa un patrón real.

Coverage: es el soporte de la parte izquierda de la regla (antecedente). Se interpreta como la frecuencia
con la que el antecedente aparece en el conjunto de transacciones.

Fisher exact test: devuelve el p-value asociado a la probabilidad de observar la regla solo por azar.

Con la función interestMeasure() se pueden calcular más de 20 métricas distintas para un conjunto de 
reglas creadas con la función apriori()."

metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest"),
                            transactions = transacciones)
metricas

"Estas nuevas métricas pueden añadirse al objeto que contiene las reglas."

quality(reglas) <- cbind(quality(reglas), metricas)
# inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
df_reglas <- as(reglas, Class = "data.frame") 
df_reglas %>% as.tibble() %>% arrange(desc(confidence)) %>% head()


"Filtrado de reglas


Cuando se crean reglas de asociación, pueden ser interesantes únicamente aquellas que contienen un 
determinado conjunto de items en el antecedente o en el consecuente. Con arules existen varias formas 
de seleccionar solo determinadas reglas.


Restringir las reglas que se crean

Es posible restringir los items que aparecen en el lado izquierdo y/o derecho de la reglas a la hora de 
crearlas, por ejemplo, supóngase que solo son de interés reglas que muestren productos que se vendan 
junto con other vegetables. Esto significa que el item other vegetables, debe aparecer en el lado 
derecho (rhs)."

soporte <- 30 / dim(transacciones)[1]
reglas_vegetables <- apriori(data = transacciones,
                             parameter = list(support = soporte,
                                              confidence = 0.70,
                                              # Se especifica que se creen reglas
                                              target = "rules"),
                             appearance = list(rhs = "other vegetables"))
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime    support
##         0.7    0.1    1 none FALSE            TRUE       5 0.00305033
##  minlen maxlen target   ext
##       1     10  rules FALSE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 30 
## 
## set item appearances ...[1 item(s)] done [0.00s].
## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
## sorting and recoding items ... [136 item(s)] done [0.00s].
## creating transaction tree ... done [0.00s].
## checking subsets of size 1 2 3 4 5 done [0.01s].
## writing ... [3 rule(s)] done [0.00s].
## creating S4 object  ... done [0.00s].

summary(reglas_vegetables)
## set of 3 rules
## 
## rule length distribution (lhs + rhs):sizes
## 4 5 
## 2 1 
## 
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   4.000   4.000   4.000   4.333   4.500   5.000 
## 
## summary of quality measures:
##     support           confidence          lift           count     
##  Min.   :0.003152   Min.   :0.7333   Min.   :3.790   Min.   :31.0  
##  1st Qu.:0.003254   1st Qu.:0.7595   1st Qu.:3.925   1st Qu.:32.0  
##  Median :0.003355   Median :0.7857   Median :4.061   Median :33.0  
##  Mean   :0.003660   Mean   :0.8016   Mean   :4.143   Mean   :36.0  
##  3rd Qu.:0.003915   3rd Qu.:0.8357   3rd Qu.:4.319   3rd Qu.:38.5  
##  Max.   :0.004474   Max.   :0.8857   Max.   :4.578   Max.   :44.0  
## 
## mining info:
##           data ntransactions    support confidence
##  transacciones          9835 0.00305033        0.7

inspect(reglas_vegetables)
##     lhs                     rhs                    support confidence     lift count
## [1] {root vegetables,                                                               
##      tropical fruit,                                                                
##      whipped/sour cream} => {other vegetables} 0.003355363  0.7333333 3.789981    33
## [2] {citrus fruit,                                                                  
##      root vegetables,                                                               
##      tropical fruit}     => {other vegetables} 0.004473818  0.7857143 4.060694    44
## [3] {citrus fruit,                                                                  
##      root vegetables,                                                               
##      tropical fruit,                                                                
##      whole milk}         => {other vegetables} 0.003152008  0.8857143 4.577509    31

"Esto mismo puede hacerse con el lado izquierdo (lhs) o en ambos (both).


Filtrar reglas creadas

También es posible filtrar las reglas una vez que han sido creadas. Por ejemplo, se procede a filtrar 
aquellas reglas que contienen citrus fruit en el antecedente."

filtrado_reglas <- subset(x = reglas,
                          subset = lhs %ain% c("citrus fruit"))
inspect(filtrado_reglas)

##     lhs                   rhs              support confidence     lift count    coverage fishersExactTest
## [1] {citrus fruit,                                                                                       
##      other vegetables,                                                                                   
##      root vegetables,                                                                                    
##      tropical fruit}   => {whole milk} 0.003152008  0.7045455 2.757344    31 0.004473818     5.003096e-10



"Reglas maximales

Un itemset es maximal si no existe otro itemset que sea su superset. Una regla de asociación se define
como regla maximal si está generada con un itemset maximal. Con la función is.maximal() se pueden 
identificar las reglas maximales."

reglas_maximales <- reglas[is.maximal(reglas)]
reglas_maximales
## set of 17 rules

inspect(reglas_maximales[1:10])
##      lhs                   rhs              support confidence     lift count    coverage fishersExactTest
## [1]  {baking powder,                                                                                      
##       yogurt}           => {whole milk} 0.003253686  0.7111111 2.783039    32 0.004575496     1.775138e-10
## [2]  {butter,                                                                                             
##       onions}           => {whole milk} 0.003050330  0.7500000 2.935237    30 0.004067107     7.502990e-11
## [3]  {domestic eggs,                                                                                      
##       sugar}            => {whole milk} 0.003558719  0.7142857 2.795464    35 0.004982206     1.990017e-11
## [4]  {butter,                                                                                             
##       coffee}           => {whole milk} 0.003355363  0.7021277 2.747881    33 0.004778851     1.582670e-10
## [5]  {butter,                                                                                             
##       curd}             => {whole milk} 0.004880529  0.7164179 2.803808    48 0.006812405     2.857678e-15
## [6]  {curd,                                                                                               
##       domestic eggs}    => {whole milk} 0.004778851  0.7343750 2.874086    47 0.006507372     1.142131e-15
## [7]  {butter,                                                                                             
##       pork}             => {whole milk} 0.003863752  0.7037037 2.754049    38 0.005490595     5.673757e-12
## [8]  {curd,                                                                                               
##       tropical fruit,                                                                                     
##       yogurt}           => {whole milk} 0.003965430  0.7500000 2.935237    39 0.005287239     1.003422e-13
## [9]  {brown bread,                                                                                        
##       other vegetables,                                                                                   
##       root vegetables}  => {whole milk} 0.003152008  0.7750000 3.033078    31 0.004067107     8.092894e-12
## [10] {butter,                                                                                             
##       tropical fruit,                                                                                     
##       yogurt}           => {whole milk} 0.003355363  0.7333333 2.870009    33 0.004575496     2.336708e-11



"Reglas redundantes

Dos reglas son idénticas si tienen el mismo antecedente (parte izquierda) y consecuente (parte derecha).
Supóngase ahora que una de estas reglas tiene en su antecedente los mismos items que forman el antecedente
de la otra, junto con algunos items más. La regla más genérica se considera redundante, ya que no aporta 
información adicional. En concreto, se considera que una regla X => Y es redundante si existe un 
subset X' tal que existe una regla X' => Y cuyo soporte es mayor.

X => Y es redundante si existe un subset X' tal que: conf(X' -> Y) >= conf(X -> Y)"

reglas_redundantes <- reglas[is.redundant(x = reglas, measure = "confidence")]
reglas_redundantes
## set of 0 rules

"Para este ejemplo no se detectan reglas redundantes.


Transacciones que verifican una determinada regla

Una vez identificada una determinada regla, puede ser interesante recuperar todas aquellas transacciones
en las que se cumple. A continuación, se recuperan aquellas transacciones para las que se cumple la regla
con mayor confianza de entre todas las encontradas.

# Se identifica la regla con mayor confianza"
as(reglas, "data.frame") %>%
  arrange(desc(confidence)) %>%
  head(1) %>%
  pull(rules)

## [1] {citrus fruit,root vegetables,tropical fruit,whole milk} => {other vegetables}
## 19 Levels: {baking powder,yogurt} => {whole milk} ...

"Las transacciones que cumplen esta regla son todas aquellas que contienen los items: citrus fruit, 
root vegetables, tropical fruit, whole milk y other vegetables."

filtrado_transacciones <- subset(x = transacciones,
                                 subset = items %ain% c("citrus fruit",
                                                        "root vegetables",
                                                        "tropical fruit",
                                                        "whole milk",
                                                        "other vegetables"))
filtrado_transacciones
## transactions in sparse format with
##  31 transactions (rows) and
##  169 items (columns)
# Se muestran 3 de las 31 transacciones

inspect(filtrado_transacciones[1:3])
##     items                   transactionID
## [1] {berries,                            
##      bottled water,                      
##      butter,                             
##      citrus fruit,                       
##      hygiene articles,                   
##      napkins,                            
##      other vegetables,                   
##      root vegetables,                    
##      rubbing alcohol,                    
##      tropical fruit,                     
##      whole milk}                     596 
## [2] {bottled water,                      
##      citrus fruit,                       
##      curd,                               
##      dessert,                            
##      frozen meals,                       
##      frozen vegetables,                  
##      fruit/vegetable juice,              
##      grapes,                             
##      napkins,                            
##      other vegetables,                   
##      pip fruit,                          
##      root vegetables,                    
##      specialty chocolate,                
##      tropical fruit,                     
##      UHT-milk,                           
##      whipped/sour cream,                 
##      whole milk}                     1122
## [3] {beef,                               
##      beverages,                          
##      butter,                             
##      candles,                            
##      chicken,                            
##      citrus fruit,                       
##      cream cheese,                       
##      curd,                               
##      domestic eggs,                      
##      flour,                              
##      frankfurter,                        
##      ham,                                
##      hard cheese,                        
##      hygiene articles,                   
##      liver loaf,                         
##      margarine,                          
##      mayonnaise,                         
##      other vegetables,                   
##      pasta,                              
##      roll products,                      
##      rolls/buns,                         
##      root vegetables,                    
##      sausage,                            
##      skin care,                          
##      soft cheese,                        
##      soups,                              
##      specialty fat,                      
##      sugar,                              
##      tropical fruit,                     
##      whipped/sour cream,                 
##      whole milk,                         
##      yogurt}                         1217



#Bibliografía

#Practical Data Science with R by Nina Zumel and John Mount, Foreword by Jim Porzak