USE libro;
SET NAMES utf8mb4;

-- ---------------------------------------------------
-- 0) Índices útiles (solo se crean una vez)
-- ---------------------------------------------------
ALTER TABLE alumno
  ADD INDEX idx_alumno_seccion   (seccion),
  ADD INDEX idx_alumno_curso     (curso),
  ADD INDEX idx_alumno_asignatura(asignatura),
  ADD INDEX idx_alumno_correo    (correo_electronico);

-- ---------------------------------------------------
-- 1) KPIs básicos
-- ---------------------------------------------------
CREATE OR REPLACE VIEW v_alumno_kpis AS
SELECT
  COUNT(*)                                                AS total_alumnos,
  SUM(seccion='Diurna')                                   AS total_diurna,
  SUM(seccion='Nocturna')                                 AS total_nocturna,
  SUM(curso='II Curso')                                   AS total_curso_ii,
  SUM(curso='III Curso')                                  AS total_curso_iii,
  SUM(asignatura='Descriptiva')                           AS total_descriptiva,
  SUM(asignatura='Inferencial')                           AS total_inferencial,
  SUM(correo_electronico LIKE '%@utmachala.edu.ec')       AS total_correo_utmachala
FROM alumno;

-- ---------------------------------------------------
-- 2) Distribuciones
-- ---------------------------------------------------
CREATE OR REPLACE VIEW v_alumno_por_seccion AS
SELECT seccion, COUNT(*) AS total
FROM alumno GROUP BY seccion ORDER BY total DESC;

CREATE OR REPLACE VIEW v_alumno_por_curso AS
SELECT curso, COUNT(*) AS total
FROM alumno GROUP BY curso ORDER BY curso;

CREATE OR REPLACE VIEW v_alumno_por_asignatura AS
SELECT asignatura, COUNT(*) AS total
FROM alumno GROUP BY asignatura ORDER BY total DESC;

CREATE OR REPLACE VIEW v_alumno_seccion_curso AS
SELECT seccion, curso, COUNT(*) AS total
FROM alumno GROUP BY seccion, curso
ORDER BY seccion, curso;

CREATE OR REPLACE VIEW v_alumno_curso_asignatura AS
SELECT curso, asignatura, COUNT(*) AS total
FROM alumno GROUP BY curso, asignatura
ORDER BY curso, asignatura;

-- ---------------------------------------------------
-- 3) Calidad de datos
-- ---------------------------------------------------
CREATE OR REPLACE VIEW v_alumno_correo_fuera_dominio AS
SELECT alumno_id, id_nico, nombre_completo, correo_electronico
FROM alumno
WHERE correo_electronico NOT LIKE '%@utmachala.edu.ec';

CREATE OR REPLACE VIEW v_alumno_dup_id AS
SELECT id_nico, COUNT(*) AS repeticiones
FROM alumno GROUP BY id_nico HAVING COUNT(*)>1;

CREATE OR REPLACE VIEW v_alumno_dup_correo AS
SELECT correo_electronico, COUNT(*) AS repeticiones
FROM alumno GROUP BY correo_electronico HAVING COUNT(*)>1;

CREATE OR REPLACE VIEW v_alumno_campos_vacios AS
SELECT alumno_id, id_nico, nombre_completo, correo_electronico,
       seccion, curso, asignatura
FROM alumno
WHERE TRIM(id_nico)=''
   OR TRIM(nombre_completo)=''
   OR TRIM(correo_electronico)=''
   OR TRIM(seccion)=''
   OR TRIM(curso)=''
   OR TRIM(asignatura)='';

-- ---------------------------------------------------
-- 4) Chequeos de formato
-- ---------------------------------------------------
CREATE OR REPLACE VIEW v_alumno_id_con_cero AS
SELECT id_nico FROM alumno
WHERE id_nico REGEXP '^[0]';

CREATE OR REPLACE VIEW v_alumno_correo_malo AS
SELECT alumno_id, id_nico, correo_electronico
FROM alumno
WHERE correo_electronico NOT REGEXP '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$';

-- ---------------------------------------------------
-- 5) Consultas rápidas de muestra
-- ---------------------------------------------------
-- Total de KPIs
SELECT * FROM v_alumno_kpis;

-- Distribución por seccion y curso
SELECT * FROM v_alumno_seccion_curso;

-- Correos fuera del dominio
SELECT * FROM v_alumno_correo_fuera_dominio;

-- IDs que empiezan con cero
SELECT * FROM v_alumno_id_con_cero LIMIT 20;

-- Correos con formato inválido
SELECT * FROM v_alumno_correo_malo;
