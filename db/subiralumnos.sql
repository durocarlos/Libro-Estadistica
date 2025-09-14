-- ==========================================================
-- Script: Creación y carga de tabla ALUMNO
-- Proyecto: Libro de Estadística - Módulo ALUMNOS ⇄ CASOS
-- ==========================================================

-- 0) Seleccionar la base de datos de trabajo
USE libro;

-- 1) Crear tabla ALUMNO
CREATE TABLE IF NOT EXISTS alumno (
  alumno_id INT AUTO_INCREMENT PRIMARY KEY,
  id_nico VARCHAR(20) NOT NULL UNIQUE,   -- ID institucional (ej. cédula con ceros a la izquierda)
  nombre_completo VARCHAR(160) NOT NULL,
  correo_electronico VARCHAR(160) NOT NULL UNIQUE,
  carrera_facultad VARCHAR(160),
  seccion VARCHAR(64),
  curso VARCHAR(160),
  asignatura VARCHAR(160),
  creado_en DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  actualizado_en DATETIME NULL ON UPDATE CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- 3) Verificar cantidad de registros cargados
SELECT COUNT(*) AS total_alumnos FROM alumno;

-- 4) Vista rápida de alumnos cargados
SELECT alumno_id, id_nico, nombre_completo, correo_electronico
FROM alumno
ORDER BY alumno_id
LIMIT 20;
