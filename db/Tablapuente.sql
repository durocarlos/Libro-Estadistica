-- ============================================================
-- PMP (PMBOK7) - Compatibilidad Shiny (Vistas + Tablas mínimas)
-- Proyecto: Libro de Estadística  |  Fecha: 2025-09-14
-- NOTA: Respeta ERD/Diccionario V5+V6; no altera tablas base.
-- ============================================================

-- 0) Contexto
USE libro;
SET FOREIGN_KEY_CHECKS = 1;

-- ============================================================
-- 1) VISTAS DE COMPATIBILIDAD (NO ALTERAN TUS TABLAS)
--    - Capítulos (Gantt/KPIs)
--    - Videos (entregable_tipo = 'Video')
--    - Casos de alumnos (con puente opcional caso_capitulo)
-- ============================================================

-- 1.1 Capítulos para Gantt/KPIs (usa fechas de ENTREGABLE tipo 'Capítulo')
CREATE OR REPLACE VIEW vw_pmp_capitulos AS
SELECT
  c.capitulo_id,
  COALESCE(c.titulo, CONCAT('Capítulo ', c.capitulo_id)) AS titulo,
  (SELECT ca.autor_id
     FROM capitulo_autor ca
     WHERE ca.capitulo_id = c.capitulo_id
     ORDER BY COALESCE(ca.orden, 999999) ASC
     LIMIT 1) AS autor_id,
  MIN(CASE WHEN LOWER(et.nombre) IN ('capítulo','capitulo')
           THEN e.fecha_compromiso END) AS fecha_inicio,
  MAX(CASE WHEN LOWER(et.nombre) IN ('capítulo','capitulo')
           THEN e.fecha_entrega END)    AS fecha_entrega
FROM capitulo c
LEFT JOIN entregable e       ON e.capitulo_id = c.capitulo_id
LEFT JOIN entregable_tipo et ON et.tipo_id    = e.tipo_id
GROUP BY c.capitulo_id, c.titulo;

-- 1.2 Videos (desde ENTREGABLE con tipo = 'Video')
CREATE OR REPLACE VIEW vw_pmp_videos AS
SELECT
  e.entregable_id     AS video_id,
  e.capitulo_id,
  e.url_repositorio   AS url,
  e.estado            AS estado_texto,
  CASE
    WHEN LOWER(e.estado) IN ('planificado','planificada') THEN 1
    WHEN LOWER(e.estado) IN ('en_produccion','en producción','producción') THEN 2
    WHEN LOWER(e.estado) IN ('entregado','entregada') THEN 3
    WHEN LOWER(e.estado) IN ('publicado','publicada') THEN 4
    ELSE 1
  END AS estado_rank,
  e.created_at        AS creado_en,
  e.updated_at        AS actualizado_en
FROM entregable e
JOIN entregable_tipo et ON et.tipo_id = e.tipo_id
WHERE LOWER(et.nombre) = 'video';

-- 1.3 Casos prácticos de alumnos (requiere tabla puente caso_capitulo)

-- Crear tabla puente (si aún no existe)
CREATE TABLE IF NOT EXISTS caso_capitulo (
  caso_id     INT NOT NULL,
  capitulo_id INT NOT NULL,
  PRIMARY KEY (caso_id, capitulo_id),
  CONSTRAINT fk_cc_caso      FOREIGN KEY (caso_id)     REFERENCES caso(caso_id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_cc_capitulo  FOREIGN KEY (capitulo_id) REFERENCES capitulo(capitulo_id)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Vista de casos (usa la tabla puente)
CREATE OR REPLACE VIEW vw_pmp_casos_alumnos AS
SELECT
  ac.caso_id,
  cc.capitulo_id,
  a.alumno_id,
  a.nombre_completo AS alumno_nombre,
  cs.estado_id,
  cs.titulo         AS caso_titulo,
  cs.descripcion    AS caso_descripcion,
  cs.fecha_inicio,
  cs.fecha_entrega,
  cs.creado_en,
  cs.actualizado_en
FROM alumno_caso ac
JOIN alumno a  ON a.alumno_id  = ac.alumno_id
JOIN caso   cs ON cs.caso_id   = ac.caso_id
LEFT JOIN caso_capitulo cc ON cc.caso_id = cs.caso_id;
