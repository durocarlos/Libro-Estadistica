-- === Asegura el esquema correcto
USE libro;

-- === Vista de KPIs rápidos
DROP VIEW IF EXISTS v_alumno_kpis;
CREATE VIEW v_alumno_kpis AS
SELECT 
  COUNT(*)                      AS total_alumnos,
  SUM(seccion = 'Diurna')       AS diurna,
  SUM(seccion = 'Nocturna')     AS nocturna
FROM alumno;

-- === Vista de resumen (para tablas de la pestaña KPIs)
DROP VIEW IF EXISTS v_alumno_resumen;
CREATE VIEW v_alumno_resumen AS
SELECT 
  seccion, 
  curso, 
  asignatura, 
  carrera_facultad,
  COUNT(*) AS total
FROM alumno
GROUP BY seccion, curso, asignatura, carrera_facultad;

-- (opcional) Vista de “portafolio” por alumno
DROP VIEW IF EXISTS v_alumno_portafolio;
CREATE VIEW v_alumno_portafolio AS
SELECT 
  a.alumno_id,
  a.id_nico,
  a.nombre_completo,
  a.correo_electronico,
  a.carrera_facultad,
  a.seccion,
  a.curso,
  a.asignatura,
  0 AS casos_participa   -- placeholder hasta que ligues con casos
FROM alumno a;
