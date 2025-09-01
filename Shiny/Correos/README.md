Libro-Estadistica/
├─ data/
│   ├─ Indice_Autores.xlsx
│   └─ Cronograma_Libro_Estadistica_CON_INDICE.xlsx
├─ docs/
│   └─ Rubrica_Capitulo.pdf
├─ Shiny/
│   └─ Correos/
│       ├─ app.R
│       └─ README.md   👈 este archivo

📂 Estructura esperada del proyecto
Libro-Estadistica/
├─ data/
│  ├─ Indice_Autores.xlsx
│  └─ Cronograma_Libro_Estadistica_CON_INDICE.xlsx
├─ docs/
│  └─ Rubrica_Capitulo.pdf        # opcional (adjunto)
└─ Shiny/
   └─ Correos/
      ├─ app.R
      └─ README.md

⚙️ Dependencias (solo una vez)

Ejecuta en la consola de R la primera vez que uses la app:

install.packages(c(
  "shiny","bslib","shinyWidgets","DT","readxl","janitor",
  "dplyr","stringr","glue","blastula","here","keyring"
))


⚠️ Esto no se pone en app.R. Solo se instala una vez en tu computadora.

🔑 Configuración de credenciales SMTP (solo una vez)

Antes de enviar correos, debes guardar una credencial segura con tu cuenta institucional.
Ejecuta esto en la consola de R (no en el Shiny):

library(blastula)

create_smtp_creds_key(
  id   = "office365",                         # nombre de la credencial
  user = "cbsarmiento@utmachala.edu.ec",      # tu correo institucional
  host = "smtp.office365.com",
  port = 587,
  use_ssl = TRUE
)


Se abrirá una ventana para escribir tu contraseña de correo.

Si tu cuenta tiene MFA (autenticación en dos pasos), debes usar una App Password en lugar de tu clave normal.

Esa credencial queda guardada en tu sistema (keyring), no en el código.

En la app Shiny se usará con:

credentials = creds_key("office365")


💡 Si necesitas actualizar la credencial, repite el comando con overwrite = TRUE.

▶️ Ejecutar la app

Desde la raíz del proyecto:

shiny::runApp("Shiny/Correos")


Requisitos:

data/Indice_Autores.xlsx con la información de autores.

(Opcional) docs/Rubrica_Capitulo.pdf si deseas adjuntar la rúbrica.

🧾 Formato mínimo de Indice_Autores.xlsx

Columnas recomendadas (se limpian con janitor::clean_names()):

capitulo

titulo_capitulo

autor_principal

correo_principal

coautor

correo_coautor

subcapitulo_1, subcapitulo_2, subcapitulo_3 (opcionales)

fase_1_fin … fase_5_fin (opcionales)

🖥️ Uso de la app

Cargar & revisar

Botón 🔄 “Recargar índice” lee Indice_Autores.xlsx.

Switches:

Adjuntar rúbrica (PDF).

CC al coautor.

Modo prueba (no envía; guarda HTML).

Redactar & enviar

Selecciona autores/capítulos.

👁️ Previsualizar correo.

✉️ Enviar correos.

🧪 Modo prueba

Guarda los correos como .html en Shiny/Correos/outbox/.

Útil para revisar texto y adjuntos sin enviar nada.

Añade a .gitignore si usas Git:

Shiny/Correos/outbox/

🛠️ Solución de problemas

Error keyring requerido → instala install.packages("keyring").

El id ya existe → repite con overwrite = TRUE o usa otro id.

Error de autenticación → si tienes MFA, usa una App Password.

No se encuentra Indice_Autores.xlsx → verifica que esté en data/.

No se adjunta la rúbrica → confirma que el PDF exista en docs/ y el switch esté activo.

🔒 Seguridad

Tu contraseña nunca queda en el repo.

Se guarda en tu sistema operativo (keyring).

La app solo usa la referencia: creds_key("office365").