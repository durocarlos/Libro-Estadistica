Use libro;
-- Conteo rápido
SELECT COUNT(*) FROM entregable;
SELECT COUNT(*) FROM fase;
SELECT COUNT(*) FROM riesgo;
SELECT COUNT(*) FROM riesgo_accion;
SELECT COUNT(*) FROM riesgo_evento;

-- Muestra algunos registros
SELECT * FROM entregable LIMIT 5;
SELECT * FROM riesgo LIMIT 5;
SELECT * FROM fase LIMIT 15;

INSERT INTO fase (fase_id, nombre, fecha_inicio, fecha_fin)
VALUES (6, 'Fase 6 – Cierre y seguimiento', '2026-03-01', '2026-03-15');

SELECT * FROM fase WHERE fase_id = 6;


-- 1) Quita el placeholder general si aún existe
DELETE FROM fase WHERE fase_id = 0;

-- 2) Asegura (upsert) que las fases 1–5 tengan los nombres y fechas “baseline”
INSERT INTO fase (fase_id, nombre, fecha_inicio, fecha_fin) VALUES
(1, 'Fase 1 – Arranque y coordinación',       '2025-09-01', '2025-09-03'),
(2, 'Fase 2 – Redacción',                      '2025-09-04', '2025-10-30'),
(3, 'Fase 3 – Revisión y corrección de estilo','2025-11-01', '2025-11-07'),
(4, 'Fase 4 – Entrega de borrador (editorial)', '2025-11-08', '2025-12-15'),
(5, 'Fase 5 – Diagramación',                   '2026-01-01', '2026-02-28')
ON DUPLICATE KEY UPDATE
  nombre = VALUES(nombre),
  fecha_inicio = VALUES(fecha_inicio),
  fecha_fin = VALUES(fecha_fin);

-- 3) La Fase 6 ya está correcta según tu captura
-- (si quisieras reforzar el upsert)
INSERT INTO fase (fase_id, nombre, fecha_inicio, fecha_fin) VALUES
(6, 'Fase 6 – Cierre y seguimiento', '2026-03-01', '2026-03-15')
ON DUPLICATE KEY UPDATE
  nombre = VALUES(nombre),
  fecha_inicio = VALUES(fecha_inicio),
  fecha_fin = VALUES(fecha_fin);


-- Fases 1–5 (upsert limpio, sin VALUES())
INSERT INTO fase (fase_id, nombre, fecha_inicio, fecha_fin)
VALUES
  (1, 'Fase 1 – Arranque y coordinación',         '2025-09-01', '2025-09-03'),
  (2, 'Fase 2 – Redacción',                        '2025-09-04', '2025-10-30'),
  (3, 'Fase 3 – Revisión y corrección de estilo',  '2025-11-01', '2025-11-07'),
  (4, 'Fase 4 – Entrega de borrador (editorial)',  '2025-11-08', '2025-12-15'),
  (5, 'Fase 5 – Diagramación',                     '2026-01-01', '2026-02-28')
AS ins
ON DUPLICATE KEY UPDATE
  nombre       = ins.nombre,
  fecha_inicio = ins.fecha_inicio,
  fecha_fin    = ins.fecha_fin;


SELECT fase_id, nombre, fecha_inicio, fecha_fin
FROM fase
ORDER BY fase_id;


-- Fase 1: Arranque y coordinación (cambiar fecha_fin)
UPDATE fase
SET fecha_fin = '2025-09-14'
WHERE fase_id = 1;

-- Fase 2: Redacción (cambiar fecha_inicio y fecha_fin)
UPDATE fase
SET fecha_inicio = '2025-09-15',
    fecha_fin    = '2025-10-30'
WHERE fase_id = 2;
