# 📘 Libro-Estadística

Proyecto colaborativo del libro de estadística.

## 📜 Licencia
Este proyecto está bajo licencia **MIT**, lo que permite su libre uso, distribución y modificación, siempre dando el debido crédito.

## 📖 Proyecto Libro de Estadística
Repositorio oficial del *Libro de Estadística*, un proyecto colaborativo con autores de distintos países de Latinoamérica.  
Aquí se organizan las **plantillas de redacción**, **rúbricas de evaluación**, cronogramas y materiales complementarios.

---

## 📂 Estructura del Repositorio

### 📘 plantillas/
- Plantillas oficiales que deben seguir los autores al redactar sus capítulos.  
- [Plantilla del Capítulo Teórico](plantillas/Plantilla_Capitulo_Teorico.pdf)  
- Cada plantilla incluye un enlace directo a su rúbrica asociada en la carpeta `/rubricas`.

### 📑 rubricas/
- Rúbricas oficiales de evaluación de capítulos.  
- [Rúbrica de Capítulo Teórico](rubricas/Rubrica_Capitulo_Teorico.pdf)  

### 🗂️ planificacion/
- Cronogramas y documentos de gestión del proyecto.  
- Cronograma con índice de capítulos.  
- Índice de autores.  

### 📊 data/
- Archivos de datos (Excel, CSV) para análisis y ejemplos prácticos.  

---

## 👥 Colaboradores
Este proyecto reúne a docentes e investigadores de distintos países, comprometidos en crear un recurso abierto y de alta calidad para la enseñanza de la estadística.


# 📘 Modelo de Base de Datos: Gestión de Capítulos y Subcapítulos

Este repositorio documenta el modelo relacional para un sistema de gestión de capítulos, subcapítulos, autores, entregas y revisiones. El diseño sigue buenas prácticas de normalización (1FN, 2FN, 3FN) y utiliza tablas puente para resolver relaciones N:M.

---

## 🧱 Estructura de Tablas

### `capitulo`
- `capitulo_id` (PK)
- `numero` (UNIQUE)
- `titulo`
- `estado_id` (FK → `estado_capitulo.estado_id`)

### `capitulo_fase`
- `capitulo_fase_id` (PK)
- `capitulo_id` (FK → `capitulo.capitulo_id`, ON DELETE CASCADE)
- `fase_id` (FK → `fase.fase_id`, ON DELETE RESTRICT)
- `fecha_asignacion`
- `fecha_cierre`
- UNIQUE (`capitulo_id`, `fase_id`)

### `fase`
- `fase_id` (PK)
- `nombre`
- `fecha_inicio`
- `fecha_fin`

### `subcapitulo`
- `subcapitulo_id` (PK)
- `capitulo_id` (FK → `capitulo.capitulo_id`)
- `numero` (UNIQUE por capítulo)
- `titulo`
- `estado_id` (FK → `estado_subcapitulo.estado_id`)

### `autor`
- `autor_id` (PK)
- `email` (UNIQUE)
- ...otros datos del autor...

### `capitulo_autor`
- `capitulo_id` (FK → `capitulo.capitulo_id`)
- `autor_id` (FK → `autor.autor_id`)
- `rol_id` (FK → `rol.rol_id`)
- `orden`
- PK compuesta: (`capitulo_id`, `autor_id`, `rol_id`)

### `subcapitulo_autor`
- `subcapitulo_id` (FK → `subcapitulo.subcapitulo_id`)
- `autor_id` (FK → `autor.autor_id`)
- `rol_id` (FK → `rol.rol_id`)
- `orden`
- PK compuesta: (`subcapitulo_id`, `autor_id`, `rol_id`)

### `rol`
- `rol_id` (PK)
- `nombre`

### `estado_capitulo`
- `estado_id` (PK)
- `nombre`

### `estado_subcapitulo`
- `estado_id` (PK)
- `nombre`

### `entrega`
- `entrega_id` (PK)
- `subcapitulo_id` (FK → `subcapitulo.subcapitulo_id`)
- `version`
- `fecha`
- `url`

### `revision`
- `revision_id` (PK)
- `entrega_id` (FK → `entrega.entrega_id`)
- `revisor_id` (FK → `autor.autor_id`)
- `rubrica`
- `decision`

---

## 🔗 Relaciones Clave

- `capitulo 1 ──< subcapitulo`
- `autor >──< capitulo` (vía `capitulo_autor`)
- `autor >──< subcapitulo` (vía `subcapitulo_autor`)
- `subcapitulo 1 ──< entrega 1 ──< revision`
- `capitulo >──< fase` (vía `capitulo_fase`)

---

## ✅ Normalización

- **1FN**: Columnas atómicas.
- **2FN**: Separación de atributos dependientes de claves compuestas.
- **3FN**: Uso de catálogos (`rol`, `estado_*`) para evitar dependencias transitivas.

---

## 📌 Índices Recomendados

- `subcapitulo(capitulo_id)`, `subcapitulo(estado_id)`
- `capitulo(estado_id)`
- `capitulo_autor(autor_id)`, `capitulo_autor(rol_id)`
- `subcapitulo_autor(autor_id)`, `subcapitulo_autor(rol_id)`
- `entrega(subcapitulo_id)`, `revision(entrega_id)`, `revision(revisor_id)`

---

## 13/09/2025 Se cambia de directorio

## 📄 Licencia
Este modelo puede ser reutilizado y adaptado bajo licencia MIT.