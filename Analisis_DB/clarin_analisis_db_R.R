library(DBI)
library(RSQLite)

url <- "https://github.com/GiselaVillanueva/Portfolio_R/raw/refs/heads/main/Analisis_DB/clarinds.db"

temp_file <- tempfile(fileext = ".db")
download.file(url, temp_file, mode = "wb")

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = temp_file)

dbListTables(con)

# Ejercicio 1 -------------------------------------------------------------

# Para el siguiente ejercicio se utilizar?n las tablas
# "navegacion" y "notas".
# 
# La tabla "notas" contiene informaci?n de las notas publicadas.
# Est? compuesta por los campos:
# - "fecha_publicacion": fecha en la que fue publicada la nota, 
# - "id_nota": c?digo ?nico de identificaci?n de la nota, 
# - "seccion": clasificaci?n un?voca por tem?tica a la que corresponde.
# 
# La tabla "navegacion" contiene informaci?n de las visitas
# de los lectores a las notas. Est? compuesta por los campos: 
# - "fecha_navegacion": fecha en la que el usuario ley? la nota, 
# - "id_usuario": c?digo ?nico de identificaci?n del lector),
# - "id_nota": c?digo ?nico de identificaci?n de la nota que ley? el usuario.
# 
# Las respuesta deben estar acompa?adas con sus respectivas queries. 


#### Preguntas:

# 01 - ?Cu?ntas secciones diferentes existen?

query_1 <- (
  "SELECT COUNT(DISTINCT seccion) as Total_de_secciones FROM notas;"
)

respuesta_1 <- dbGetQuery(con, query_1)

# 02 - ?Cu?ntas notas publicadas no fueron vistas por los usuarios?

query_2 <- (
  "SELECT COUNT (DISTINCT id_nota) AS no_vistas
  FROM notas
  WHERE id_nota NOT IN (SELECT id_nota FROM navegacion);"
)

respuesta_2 <- dbGetQuery(con, query_2)

# 03 - ?Cu?ntas visitas tuvo cada secci?n? (Ordenadas de mayor a menor)

query_3 <- (
  "SELECT seccion,
  COUNT(navegacion.id_nota) as total_visitas
  FROM notas
  JOIN navegacion
  on notas.id_nota = navegacion.id_nota
  GROUP BY seccion
  ORDER BY total_visitas DESC;"
)

respuesta_3 <- dbGetQuery(con, query_3)

# 04 - ?Cu?l es el promedio diario de visualizaciones por secci?n?

query_4 <- (
  "SELECT seccion, ROUND(AVG(navegaciones_diarias), 2) AS promedio_diario_navegaciones
  FROM (
    SELECT seccion, fecha_navegacion, COUNT(*) / COUNT(DISTINCT fecha_navegacion) AS navegaciones_diarias
    FROM notas
    JOIN navegacion ON notas.id_nota = navegacion.id_nota
    GROUP BY seccion, fecha_navegacion
  )
  GROUP BY seccion
  ORDER BY promedio_diario_navegaciones DESC;"
)

respuesta_4 <- dbGetQuery(con, query_4)

# Ejercicio 2 -------------------------------------------------------------

# A continuaci?n en necesario traerse los datos de la tabla "summary".
# 
# En este punto pod?s usar la herramienta que desees
# y pod?s encarar el an?lisis como vos lo consideres mejor.
# La idea es que puedas describir las variables y analizar el dataset.  
# 
# Las variables son:   
#   
# - "Id_usuario*: c?digo ?nico de identificaci?n del lector.  
# - "Sexo" : si el usuario es H (hombre) o M (mujer).  
# - "Edad": la edad del usuario.  
# - "Total_notas_leidas": el total de notas que ley? el usuario en los ?ltimos 30 d?as.  
# - "Notas_leidas_desktop": el total de notas que ley? el usuario en los ?ltimos 30 d?as desde una computadora.  
# - "Notas_leidas_mobile": el total de notas que ley? el usuario en los ?ltimos 30 d?as desde un smartphone o tablet.  
# - "Scoring": un puntaje creado por el equipo de marketing para cada usuario.  


#### Consigna:

# Imagin? que ten?s que contarles a personas que no est?n en el ?rea,
# qu? particularidades tiene la tabla. Brevemente, realiz? un an?lisis
# exploratorio de los datos con este fin de obtener  algunos resultados
# descriptivos para comentar. Algunas preguntas que te pueden guiar:

# - ?Qu? se puede decir acerca del conjunto de datos en general? 
# - ?Qu? se puede decir acerca de las lecturas en particular? 

# La tabla "summary" nos muestra el comportamiento de los usuarios con 
# respecto a las notas publicadas en el usuario: quién lee más o menos,
# y en qué tipo de dispositivos. Asimismo, hay información personal de 
# cada usuario (sexo y edad), además de un "scoring", que corresponde a
# un puntaje establecido por Marketing. Estos, posiblemente, nos pueden 
# servir para identificar patrones.
# A continuación, haremos una exploración de los datos contenidos, para 
# ver si encontramos alguna particularidad, patrón, etc.

# Comencemos por analizar edad y sexo:

edad_sexo <- (
  "SELECT sexo, AVG(edad) as promedio, MIN(edad) as minimo, MAX(edad) as maximo
  FROM summary
  GROUP BY sexo;"
)

respuesta_edad_sexo <- dbGetQuery(con, edad_sexo)

print(data.frame(respuesta_edad_sexo))

# Evidentemente, en cuanto a edad, hay varios outliers. Esto significa 
# que, para tener información más real, deberemos hacer algunas
# exclusiones: en primer lugar, la edad mínima debe ser 18 años, ya que
# es la que marca Clarín para poder ser suscriptor. En cuanto a la 
# máxima, si bien no hay reglas, la estableceremos en 100, a efectos de 
# tener un análisis lo más acertado posible.
# Aclaración: lo ideal sería que se puedan corregir estos errores en la 
# base de datos. Mientras tanto, tomamos las medidas indicadas arriba.

edad_sexo_2 <- (
  "SELECT sexo, ROUND(AVG(edad)) as promedio_edad, ROUND(MIN(edad)) as minimo_edad, ROUND(MAX(edad)) as maximo_edad
  FROM summary
  WHERE edad BETWEEN 18 AND 100
  GROUP BY sexo;"
)

respuesta_edad_sexo_2 <- dbGetQuery(con, edad_sexo_2)

print(data.frame(respuesta_edad_sexo_2))

# Como podemos ver, los números han cambiado completamente.
# En cuanto a la respuesta que hemos obtenido: no hay diferencias
# significativas en edad promedio y máxima, entre ambos sexos. No es
# así con la edad mínima: la de los hombres es mucho más baja, en 8 años.

# Ahora, veamos qué dispositivos se utilizan más para leer las notas:

uso_dispositivo <- (
  "SELECT ROUND(100.00*SUM(notas_leidas_desktop)/SUM(total_notas_leidas), 2) AS porc_desktop, ROUND(100.00*SUM(notas_leidas_mobile)/SUM(total_notas_leidas), 2) AS porc_mobile
  FROM summary;"
)

respuesta_uso_dispositivo <- dbGetQuery(con, uso_dispositivo)

print(data.frame(respuesta_uso_dispositivo))

library(ggplot2)

values <- c(respuesta_uso_dispositivo$porc_desktop, respuesta_uso_dispositivo$porc_mobile)
labels <- c("Desktop", "Mobile")

pie(x = values, labels = paste(labels, "-", values, "%"), main = "Uso de dispositivos")

# Como podemos ver, en la lectura de artículos es mucho mayor el uso de 
# dispositivos móviles.

# Ahora estudiaremos la naturaleza del scoring, concretamente si hay 
# una relación entre scoring y notas leídas (es decir: ¿a más notas 
# leídas, mayor scoring?)

notas_leidas_vs_scoring <- (
  "SELECT total_notas_leidas, AVG(scoring) as promedio_scoring
  FROM summary
  GROUP BY total_notas_leidas;"
)

respuesta_notas_leidas_vs_scoring <- dbGetQuery(con, notas_leidas_vs_scoring)

ggplot(respuesta_notas_leidas_vs_scoring, 
       aes (x = total_notas_leidas, y = promedio_scoring)) +
  geom_line()

# Conclusión: si bien se observa una subida hacia el final del gráfico,
# no puede establecerse una relación entre cantidad de notas leídas y
# scoring, siendo este último un número arbitrario o relacionado a un
# factor que no puede observarse en esta tabla.

# Ejercicio 3 -------------------------------------------------------------

# Esta consigna es opcional.

 
# Teniendo a disposici?n los datos de los puntos anteriores, te invitamos a
# que crees (y respondas) tu propio ejercicio. Puede ser una interrogante
# puntual con su respuesta, un gr?fico, un modelo, un boceto de un esquema,
# una acci?n posible a realizar, etc.
# ?La creatividad es simplemente conectar cosas!

# ¿Hay una relación entre uso de dispositivos (mobile o desktop) y edad?
# Podemos pensar instintivamente que sí; no obstante, lo más correcto
# es verificarlo.
# Aquí reproduciremos el filtro aplicado en la sección anterior: edad
# entre 18 y 100 años.

edad_vs_dispositivo <- (
  "SELECT
  	CASE
	  	WHEN edad BETWEEN 18 AND 30 THEN '18-30'
	  	WHEN edad BETWEEN 31 AND 40 THEN '31-40'
	  	WHEN edad BETWEEN 41 AND 50 THEN '41-50'
		  WHEN edad BETWEEN 51 AND 60 THEN '51-60'
	  	WHEN edad BETWEEN 61 AND 70 THEN '61-70'
	  	WHEN edad BETWEEN 71 AND 80 THEN '71-80'
  		WHEN edad BETWEEN 81 AND 90 THEN '81-90'
  		WHEN edad BETWEEN 91 AND 100 THEN '91-100'
  	END AS rango_edad,
  	SUM(notas_leidas_desktop) AS total_leidas_desktop, SUM(notas_leidas_mobile) AS total_leidas_mobile
  FROM summary
  WHERE edad BETWEEN 18 AND 100 AND rango_edad IS NOT NULL
  GROUP BY rango_edad;"
)

respuesta_edad_vs_dispositivo <- dbGetQuery(con, edad_vs_dispositivo)

ggplot(respuesta_edad_vs_dispositivo, aes(x = rango_edad)) +
  geom_line(aes(y = total_leidas_desktop, group = 1), color = "black") +
  geom_line(aes(y = total_leidas_mobile, group = 1), color = "aquamarine4") +
  geom_point(aes(y = total_leidas_desktop), color = "black") +
  geom_point(aes(y = total_leidas_mobile), color = "aquamarine4") +
  labs(title = "Total de notas leídas por rango etario y dispositivo") +
  xlab("Rango etario") +
  ylab("Total") +
  geom_text(aes(x = "51-60", y = 2100, label = "Mobile"), color = "aquamarine4") +
  geom_text(aes(x = "51-60", y = 1350, label = "Desktop"), color = "black")

# Como nos muestra el gráfico, ambas curvas crecen y decrecen 
# prácticamente al mismo ritmo, lo cual demuestra que, si bien el uso
# de dispositivos móviles es predominante, esto no tiene que ver tanto
# con la edad en general.
# No obstante, sí se nota que las curvas se "despegan" más en el rango
# de 51 a 60 años: ¿qué quiere decir esto? Que, a esa edad, sí hay mucho
# mayor uso de móviles que en otras edades. 


# Fin ---------------------------------------------------------------------

dbDisconnect(con)