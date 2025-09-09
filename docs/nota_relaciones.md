# Nota técnica -- Relaciones entre entidades en la base de datos del Libro de Estadística

El modelo de datos implementado para el proyecto del **Libro de
Estadística** se organiza bajo un enfoque relacional, con el objetivo de
garantizar consistencia en la gestión de capítulos, autores y flujo
editorial.

1.  **Autores y Afiliaciones**
    -   Cada autor puede tener una o varias instituciones de
        afiliación.\
    -   La relación se representa con una tabla puente
        (`autor_afiliacion`), que soporta el vínculo N:M entre `autor` y
        `afiliacion`.
2.  **Autores y Capítulos**
    -   Un capítulo puede contar con varios autores y coautores,
        mientras que un autor puede participar en múltiples capítulos.\
    -   Esta relación se resuelve mediante la tabla `capitulo_autor`,
        que además incluye el **rol** desempeñado (autor principal,
        coautor, revisor, coordinador) y un campo de orden para definir
        jerarquía de aparición.
3.  **Autores y Subcapítulos**
    -   Se replica la lógica de N:M con la tabla `subcapitulo_autor`,
        permitiendo granularidad en la asignación de contribuciones a
        secciones específicas de cada capítulo.
4.  **Capítulos y Subcapítulos**
    -   La relación es jerárquica 1:N: cada capítulo contiene tres
        subcapítulos estandarizados (introducción, desarrollo,
        conclusiones).\
    -   Esta estructura asegura uniformidad en la organización de
        contenidos.
5.  **Estados y Fases**
    -   Tanto capítulos como subcapítulos se vinculan a catálogos de
        estado (`estado_capitulo`, `estado_subcapitulo`), permitiendo
        controlar el avance editorial (asignado, en revisión, aprobado,
        maquetado).\
    -   Las fases (`fase`) representan hitos cronológicos globales del
        proyecto, con fechas de inicio y fin predefinidas (redacción,
        revisión, entrega, diagramación).
6.  **Entregas y Revisiones**
    -   Cada subcapítulo puede tener múltiples entregas (`entrega`)
        versionadas.\
    -   Las revisiones (`revision`) se asocian a cada entrega y
        registran la evaluación de revisores, junto con rúbricas y
        decisiones editoriales.

En conjunto, este modelo de datos habilita un control integral del
proceso editorial, facilitando la asignación de responsabilidades, el
seguimiento de avances y la trazabilidad entre autores, capítulos y
fases.
