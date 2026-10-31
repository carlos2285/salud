# Aplicación interactiva de Econometría I — Salud y desarrollo

El archivo `app.R` es una aplicación Shiny independiente que utiliza por defecto `salud_desarrollo_paises_2019.csv`. Los archivos `paper_salud_ECO1.R` (estudiante) y `paper_salud_ECO1_docente.R` (docente) no se modificaron.

## Probar desde Posit Cloud (sin instalar R localmente)

1. Abre tu proyecto de Posit Cloud. Si se había importado antes de este cambio, sincroniza el proyecto con la última versión de GitHub; alternativamente, crea un proyecto nuevo desde `https://github.com/carlos2285/salud`.
2. Abre `app.R`.
3. Ejecuta en la consola `install.packages("shiny")` si todavía no está instalado.
4. Pulsa **Run App** o ejecuta `shiny::runApp()`.

## Publicar para estudiantes

La ejecución dentro de Posit Cloud es una prueba, no un enlace público de una aplicación desplegada. Para compartirla sin exigir R a los estudiantes, publícala en Posit Connect Cloud o shinyapps.io. Selecciona la carpeta de la aplicación con `app.R` y el CSV. Verifica que el despliegue termina correctamente y prueba el enlace en una ventana privada.

## Contenido

Exploración de los datos 2019, regresión MCO simple o múltiple, intervalos de confianza, Breusch-Pagan, RESET, VIF, gráficos y predicción de El Salvador. Los cálculos son asociaciones y no deben interpretarse como efectos causales. No se presenta la versión resuelta del script docente en la interfaz.

## Advertencias

La base predeterminada debe estar junto a `app.R`. Una vez publicada, la app sí se ejecuta en el servidor del proveedor; no en el navegador del alumno. Esta primera versión no se ha probado aún en una sesión R real y puede requerir ajustes antes del despliegue público.
