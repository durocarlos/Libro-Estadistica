
-- ============================================================
-- VERIFICADOR PMP — Libro de Estadística (MySQL 8+)
-- Ejecuta este script en MySQL Workbench conectado a tu BD.
-- Cada bloque devuelve "posibles problemas". Si todo está bien,
-- muchos SELECT deberían devolver 0 filas.
-- ============================================================

-- 0) Contexto
SELECT DATABASE() AS base_en_uso;

-- 1) Existencia de tablas clave (debe dar 0 faltantes)
SELECT t.nombre_tabla AS tabla_faltante
FROM (
  SELECT 'capitulo' AS nombre_tabla UNION ALL
  SELECT 'subcapitulo' UNION ALL
  SELECT 'autor' UNION ALL
  SELECT 'rol' UNION ALL
  SELECT 'fase' UNION ALL
  SELECT 'estado' UNION ALL
  SELECT 'capitulo_autor' UNION ALL
  SELECT 'subcapitulo_autor' UNION ALL
  -- Entregables
  SELECT 'entregable_tipo' UNION ALL
  SELECT 'entregable' UNION ALL
  SELECT 'entregable_revision' UNION ALL
  SELECT 'entregable_nota' UNION ALL
  -- Riesgos
  SELECT 'riesgo_categoria' UNION ALL
  SELECT 'riesgo_escala' UNION ALL
  SELECT 'riesgo_matriz' UNION ALL
  SELECT 'riesgo' UNION ALL
  SELECT 'riesgo_evento' UNION ALL
  SELECT 'riesgo_accion' UNION ALL
  SELECT 'riesgo_nota'
) t
LEFT JOIN information_schema.tables it
  ON it.table_schema = DATABASE() AND it.table_name = t.nombre_tabla
WHERE it.table_name IS NULL;

-- 2) Vistas esperadas (debe dar 0 faltantes)
SELECT v.nombre_vista AS vista_faltante
FROM (
  SELECT 'v_riesgo_registro' AS nombre_vista UNION ALL
  SELECT 'v_riesgo_acciones' UNION ALL
  SELECT 'v_riesgo_heatmap'
) v
LEFT JOIN information_schema.views iv
  ON iv.table_schema = DATABASE() AND iv.table_name = v.nombre_vista
WHERE iv.table_name IS NULL;

-- 3) Matriz 5x5 completa (debe dar 0 problemas por escala)
--    Requiere que exista al menos una escala con rango 1..5
SELECT e.escala_id, e.nombre,
       SUM(CASE WHEN rm.matriz_id IS NOT NULL THEN 1 ELSE 0 END) AS celdas_presentes,
       (SELECT (e.prob_max - e.prob_min + 1) * (e.imp_max - e.imp_min + 1)) AS celdas_esperadas
FROM riesgo_escala e
LEFT JOIN riesgo_matriz rm
  ON rm.escala_id = e.escala_id
GROUP BY e.escala_id, e.nombre, e.prob_min, e.prob_max, e.imp_min, e.imp_max
HAVING celdas_presentes <> (e.prob_max - e.prob_min + 1) * (e.imp_max - e.imp_min + 1);

-- 3.1) Prioridades y colores nulos o fuera de dominio (debe dar 0)
SELECT rm.*
FROM riesgo_matriz rm
LEFT JOIN riesgo_escala e ON e.escala_id = rm.escala_id
WHERE rm.prioridad NOT IN ('Baja','Media','Alta','Crítica','Extrema')
   OR rm.color_hex IS NULL OR rm.color_hex NOT REGEXP '^#[0-9A-Fa-f]{6}$'
   OR rm.prob_nivel < e.prob_min OR rm.prob_nivel > e.prob_max
   OR rm.imp_nivel  < e.imp_min  OR rm.imp_nivel  > e.imp_max;

-- 4) Riesgos con escala/prob/imp fuera de rango (debe dar 0)
SELECT r.*
FROM riesgo r
JOIN riesgo_escala e ON e.escala_id = r.escala_id
WHERE (r.prob_nivel IS NULL OR r.imp_nivel IS NULL)
   OR r.prob_nivel < e.prob_min OR r.prob_nivel > e.prob_max
   OR r.imp_nivel  < e.imp_min  OR r.imp_nivel  > e.imp_max;

-- 4.1) Riesgos que NO mapean en la matriz (debe dar 0)
SELECT r.riesgo_id, r.titulo, r.escala_id, r.prob_nivel, r.imp_nivel
FROM riesgo r
LEFT JOIN riesgo_matriz rm
  ON rm.escala_id = r.escala_id AND rm.prob_nivel = r.prob_nivel AND rm.imp_nivel = r.imp_nivel
WHERE rm.matriz_id IS NULL AND r.escala_id IS NOT NULL;

-- 4.2) Estados de riesgo “raros” (revisa salida)
SELECT DISTINCT estado FROM riesgo WHERE estado IS NOT NULL ORDER BY estado;

-- 5) Eventos de riesgo con problemas (debe dar 0)
SELECT re.*
FROM riesgo_evento re
LEFT JOIN riesgo r ON r.riesgo_id = re.riesgo_id
WHERE r.riesgo_id IS NULL
   OR re.fecha_evento IS NULL
   OR re.tipo NOT IN ('Identificación','Actualización','Ocurrencia','Escalamiento','Cierre');

-- 6) Acciones con problemas (debe dar 0)
SELECT ra.*
FROM riesgo_accion ra
LEFT JOIN riesgo r ON r.riesgo_id = ra.riesgo_id
LEFT JOIN autor a  ON a.autor_id = ra.responsable_id
WHERE r.riesgo_id IS NULL
   OR ra.estado NOT IN ('Pendiente','En curso','Cerrada')
   OR (ra.fecha_cierre IS NOT NULL AND ra.fecha_compromiso IS NOT NULL AND ra.fecha_cierre < ra.fecha_compromiso);

-- 7) Entregables con problemas (debe dar 0)
SELECT e.*
FROM entregable e
LEFT JOIN entregable_tipo t ON t.tipo_id = e.tipo_id
LEFT JOIN autor a           ON a.autor_id = e.responsable_id
LEFT JOIN fase f            ON f.fase_id = e.fase_id
LEFT JOIN capitulo c        ON c.capitulo_id = e.capitulo_id
LEFT JOIN subcapitulo s     ON s.subcapitulo_id = e.subcapitulo_id
WHERE (e.tipo_id IS NOT NULL AND t.tipo_id IS NULL)
   OR (e.responsable_id IS NOT NULL AND a.autor_id IS NULL)
   OR (e.fase_id IS NOT NULL AND f.fase_id IS NULL)
   OR (e.capitulo_id IS NOT NULL AND c.capitulo_id IS NULL)
   OR (e.subcapitulo_id IS NOT NULL AND s.subcapitulo_id IS NULL)
   OR (e.fecha_entrega IS NOT NULL AND e.fecha_compromiso IS NOT NULL AND e.fecha_entrega < e.fecha_compromiso);

-- 8) Asignaciones huérfanas (debe dar 0)
SELECT ca.*
FROM capitulo_autor ca
LEFT JOIN capitulo c ON c.capitulo_id = ca.capitulo_id
LEFT JOIN autor   a  ON a.autor_id   = ca.autor_id
LEFT JOIN rol     r  ON r.rol_id     = ca.rol_id
WHERE c.capitulo_id IS NULL OR a.autor_id IS NULL OR r.rol_id IS NULL;

SELECT sa.*
FROM subcapitulo_autor sa
LEFT JOIN subcapitulo s ON s.subcapitulo_id = sa.subcapitulo_id
LEFT JOIN autor       a ON a.autor_id       = sa.autor_id
LEFT JOIN rol         r ON r.rol_id         = sa.rol_id
WHERE s.subcapitulo_id IS NULL OR a.autor_id IS NULL OR r.rol_id IS NULL;

-- 9) Cobertura de autores principales por capítulo/subcapítulo (revisa output)
SELECT c.capitulo_id, c.numero, c.titulo,
       SUM(ca.rol_id = 1) AS autores_principales,
       COUNT(*) AS total_asignados
FROM capitulo c
LEFT JOIN capitulo_autor ca ON ca.capitulo_id = c.capitulo_id
GROUP BY c.capitulo_id, c.numero, c.titulo
ORDER BY c.numero;

SELECT s.subcapitulo_id, c.numero AS cap, s.numero AS sub, s.titulo,
       SUM(sca.rol_id = 1) AS autores_principales,
       COUNT(*) AS total_asignados
FROM subcapitulo s
JOIN capitulo c ON c.capitulo_id = s.capitulo_id
LEFT JOIN subcapitulo_autor sca ON sca.subcapitulo_id = s.subcapitulo_id
GROUP BY s.subcapitulo_id, c.numero, s.numero, s.titulo
ORDER BY c.numero, s.numero;

-- 10) Conteos rápidos de sanidad
SELECT 'riesgo' AS tabla, COUNT(*) AS n FROM riesgo
UNION ALL SELECT 'riesgo_evento', COUNT(*) FROM riesgo_evento
UNION ALL SELECT 'riesgo_accion', COUNT(*) FROM riesgo_accion
UNION ALL SELECT 'riesgo_nota', COUNT(*) FROM riesgo_nota
UNION ALL SELECT 'entregable', COUNT(*) FROM entregable
UNION ALL SELECT 'entregable_revision', COUNT(*) FROM entregable_revision
UNION ALL SELECT 'entregable_nota', COUNT(*) FROM entregable_nota
UNION ALL SELECT 'riesgo_matriz', COUNT(*) FROM riesgo_matriz;

-- 11) Índices recomendados presentes (debe dar 0 faltantes)
SELECT * FROM (
  SELECT 'entregable(fase_id)'              AS idx, EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='entregable' AND index_name='idx_ent_fase'
  ) AS existe
  UNION ALL SELECT 'entregable(capitulo_id)', EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='entregable' AND index_name='idx_ent_cap'
  )
  UNION ALL SELECT 'entregable(subcapitulo_id)', EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='entregable' AND index_name='idx_ent_sub'
  )
  UNION ALL SELECT 'riesgo(categoria_id)', EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='riesgo' AND index_name='idx_r_cat'
  )
  UNION ALL SELECT 'riesgo(escala_id)', EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='riesgo' AND index_name='idx_r_esc'
  )
  UNION ALL SELECT 'riesgo(owner_autor_id)', EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='riesgo' AND index_name='idx_r_owner'
  )
  UNION ALL SELECT 'riesgo(fase_id)', EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='riesgo' AND index_name='idx_r_fase'
  )
  UNION ALL SELECT 'riesgo(capitulo_id)', EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='riesgo' AND index_name='idx_r_cap'
  )
  UNION ALL SELECT 'riesgo(subcapitulo_id)', EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='riesgo' AND index_name='idx_r_sub'
  )
  UNION ALL SELECT 'riesgo_evento(riesgo_id, fecha_evento)', EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='riesgo_evento' AND index_name='idx_re_riesgo'
  )
  UNION ALL SELECT 'riesgo_accion(riesgo_id, estado, fecha_compromiso)', EXISTS(
    SELECT 1 FROM information_schema.statistics 
     WHERE table_schema=DATABASE() AND table_name='riesgo_accion' AND index_name='idx_ra_riesgo'
  )
) z
WHERE existe = 0;

-- 12) Charset/engine (revisión rápida; ajusta si quieres exigir utf8mb4/InnoDB)
SELECT table_name, engine, table_collation
FROM information_schema.tables
WHERE table_schema = DATABASE()
  AND table_name IN ('entregable','entregable_revision','entregable_nota',
                     'riesgo','riesgo_evento','riesgo_accion','riesgo_nota','riesgo_matriz');
