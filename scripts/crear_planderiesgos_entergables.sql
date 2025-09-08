-- =========================================
-- TABLAS: Plan de Gestión de Riesgos/una sola vez
-- =========================================
-- Bloque 1) CREACIÓN (solo DDL)
USE libro;

-- =======================
-- 1. Catálogos de Riesgos
-- =======================
CREATE TABLE IF NOT EXISTS riesgo_categoria (
  categoria_id   INT AUTO_INCREMENT PRIMARY KEY,
  nombre         VARCHAR(80) NOT NULL UNIQUE,
  descripcion    VARCHAR(255)
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS riesgo_escala (
  escala_id   INT AUTO_INCREMENT PRIMARY KEY,
  nombre      VARCHAR(60) NOT NULL UNIQUE,
  prob_min    INT NOT NULL DEFAULT 1,
  prob_max    INT NOT NULL DEFAULT 5,
  imp_min     INT NOT NULL DEFAULT 1,
  imp_max     INT NOT NULL DEFAULT 5
) ENGINE=InnoDB;

-- =======================
-- 2. Núcleo de Riesgos
-- =======================
CREATE TABLE IF NOT EXISTS riesgo (
  riesgo_id       INT AUTO_INCREMENT PRIMARY KEY,
  titulo          VARCHAR(160) NOT NULL,
  descripcion     TEXT,
  causa           TEXT,
  efecto          TEXT,
  categoria_id    INT,
  escala_id       INT,
  prob_nivel      INT NOT NULL,
  imp_nivel       INT NOT NULL,
  puntaje         INT AS (prob_nivel * imp_nivel) STORED,
  estado          ENUM('Identificado','Analizado','Planificado','En seguimiento','Cerrado') DEFAULT 'Identificado',
  estrategia      ENUM('Evitar','Mitigar','Transferir','Aceptar','Explotar','Compartir','Mejorar','Aceptar Oportunidad'),
  plan_mitigacion   TEXT,
  plan_contingencia TEXT,
  fecha_revision    DATE,
  -- referencias a tu modelo (sin FK para compatibilidad)
  owner_autor_id  INT NULL,
  fase_id         INT NULL,
  capitulo_id     INT NULL,
  subcapitulo_id  INT NULL,
  entrega_id      INT NULL,
  created_at      DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at      DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX idx_riesgo_owner (owner_autor_id),
  INDEX idx_riesgo_obj   (capitulo_id, subcapitulo_id, entrega_id),
  CONSTRAINT fk_riesgo_categoria
    FOREIGN KEY (categoria_id) REFERENCES riesgo_categoria(categoria_id)
    ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT fk_riesgo_escala
    FOREIGN KEY (escala_id)    REFERENCES riesgo_escala(escala_id)
    ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS riesgo_evento (
  evento_id       INT AUTO_INCREMENT PRIMARY KEY,
  riesgo_id       INT NOT NULL,
  fecha_evento    DATETIME DEFAULT CURRENT_TIMESTAMP,
  tipo            ENUM('Identificacion','Actualizacion','Ocurrencia','Escalamiento','Cierre') NOT NULL,
  detalle         TEXT,
  actor_autor_id  INT NULL,
  INDEX idx_evt_actor (actor_autor_id),
  CONSTRAINT fk_evento_riesgo
    FOREIGN KEY (riesgo_id) REFERENCES riesgo(riesgo_id)
    ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS riesgo_accion (
  accion_id        INT AUTO_INCREMENT PRIMARY KEY,
  riesgo_id        INT NOT NULL,
  descripcion      VARCHAR(255) NOT NULL,
  responsable_id   INT NULL,
  fecha_compromiso DATE,
  fecha_cierre     DATE,
  estado           ENUM('Pendiente','En progreso','Completada','Bloqueada','Cancelada') DEFAULT 'Pendiente',
  INDEX idx_acc_resp (responsable_id),
  CONSTRAINT fk_accion_riesgo
    FOREIGN KEY (riesgo_id) REFERENCES riesgo(riesgo_id)
    ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB;

-- =======================
-- 3. Entregables PMP
-- =======================
CREATE TABLE IF NOT EXISTS entregable_tipo (
  tipo_id      INT AUTO_INCREMENT PRIMARY KEY,
  nombre       VARCHAR(80) NOT NULL UNIQUE,
  descripcion  VARCHAR(255)
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS entregable (
  entregable_id     INT AUTO_INCREMENT PRIMARY KEY,
  nombre            VARCHAR(160) NOT NULL,
  descripcion       TEXT,
  tipo_id           INT,
  responsable_id    INT NULL,
  fase_id           INT NULL,
  capitulo_id       INT NULL,
  subcapitulo_id    INT NULL,
  entrega_id        INT NULL,
  criterios_acept   TEXT,
  version           VARCHAR(30),
  url_repositorio   VARCHAR(255),
  fecha_compromiso  DATE,
  fecha_entrega     DATE,
  estado            ENUM('Planificado','En progreso','En revisión','Aceptado','Rechazado','Cancelado') DEFAULT 'Planificado',
  created_at        DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at        DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX idx_ent_resp (responsable_id),
  INDEX idx_ent_obj  (capitulo_id, subcapitulo_id, entrega_id),
  INDEX idx_ent_fechas (fecha_compromiso, fecha_entrega),
  CONSTRAINT fk_ent_tipo
    FOREIGN KEY (tipo_id) REFERENCES entregable_tipo(tipo_id)
    ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB;

CREATE TABLE IF NOT EXISTS entregable_revision (
  revision_id    INT AUTO_INCREMENT PRIMARY KEY,
  entregable_id  INT NOT NULL,
  fecha_revision DATETIME DEFAULT CURRENT_TIMESTAMP,
  revisor_id     INT NULL,
  decision       ENUM('Aprobado','Aprobado con observaciones','Rechazado') NOT NULL,
  observaciones  TEXT,
  INDEX idx_rev_revisor (revisor_id),
  CONSTRAINT fk_rev_entregable
    FOREIGN KEY (entregable_id) REFERENCES entregable(entregable_id)
    ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB;
-- 2) VERIFICACIÓN + PRUEBA RÁPIDA (DML/SELECT)
USE libro;
-- Bloque 2
-- 2.1 Tablas presentes
SHOW TABLES LIKE 'riesgo%';
SHOW TABLES LIKE 'entregable%';
-- Bloque 3 – Consultas de tablero (seguimiento)
USE libro;

-- ==========================
-- 3.1 Entregables
-- ==========================

-- Entregables vencidos o próximos a vencer (14 días por defecto)
SELECT 
  e.entregable_id, e.nombre, et.nombre AS tipo, e.estado,
  e.fecha_compromiso, e.fecha_entrega,
  DATEDIFF(CURDATE(), e.fecha_compromiso) AS dias_atraso,
  DATEDIFF(e.fecha_compromiso, CURDATE()) AS dias_para_vencer,
  e.url_repositorio
FROM entregable e
LEFT JOIN entregable_tipo et ON et.tipo_id = e.tipo_id
WHERE e.estado IN ('Planificado','En progreso','En revisión')
  AND e.fecha_compromiso IS NOT NULL
  AND (
        e.fecha_compromiso <  CURDATE() OR
        e.fecha_compromiso BETWEEN CURDATE() AND DATE_ADD(CURDATE(), INTERVAL 14 DAY)
      )
ORDER BY (e.fecha_compromiso < CURDATE()) DESC, e.fecha_compromiso ASC;

-- Resumen por estado
SELECT e.estado, COUNT(*) AS total
FROM entregable e
GROUP BY e.estado
ORDER BY total DESC;

-- Vista rápida de entregables abiertos
CREATE OR REPLACE VIEW dashboard_entregables_abiertos AS
SELECT 
  e.entregable_id, e.nombre, et.nombre AS tipo, e.estado,
  e.fecha_compromiso, e.fecha_entrega,
  DATEDIFF(CURDATE(), e.fecha_compromiso) AS dias_atraso,
  e.url_repositorio, e.capitulo_id, e.subcapitulo_id, e.entrega_id, e.fase_id,
  e.created_at, e.updated_at
FROM entregable e
LEFT JOIN entregable_tipo et ON et.tipo_id = e.tipo_id
WHERE e.estado IN ('Planificado','En progreso','En revisión');

-- ==========================
-- 3.2 Riesgos
-- ==========================

-- Riesgos abiertos con prioridad
SELECT 
  r.riesgo_id, r.titulo, r.estado, r.prob_nivel, r.imp_nivel, r.puntaje,
  COALESCE(m.prioridad,'Media') AS prioridad,
  c.nombre AS categoria,
  r.fecha_revision, r.updated_at
FROM riesgo r
LEFT JOIN riesgo_categoria c ON c.categoria_id = r.categoria_id
LEFT JOIN riesgo_escala e    ON e.escala_id    = r.escala_id
LEFT JOIN riesgo_matriz m    ON m.escala_id = r.escala_id
                             AND m.prob_nivel = r.prob_nivel
                             AND m.imp_nivel  = r.imp_nivel
WHERE r.estado IN ('Identificado','Analizado','Planificado','En seguimiento')
ORDER BY FIELD(COALESCE(m.prioridad,'Media'),'Crítica','Alta','Media','Baja'),
         r.puntaje DESC;

-- Conteo de riesgos por prioridad
SELECT COALESCE(m.prioridad,'Media') AS prioridad, COUNT(*) AS total
FROM riesgo r
LEFT JOIN riesgo_matriz m 
  ON m.escala_id = r.escala_id
  AND m.prob_nivel = r.prob_nivel
  AND m.imp_nivel  = r.imp_nivel
WHERE r.estado <> 'Cerrado'
GROUP BY COALESCE(m.prioridad,'Media')
ORDER BY FIELD(COALESCE(m.prioridad,'Media'),'Crítica','Alta','Media','Baja');

-- Heatmap (conteo P x I)
SELECT r.prob_nivel, r.imp_nivel, COUNT(*) AS total
FROM riesgo r
WHERE r.estado <> 'Cerrado'
GROUP BY r.prob_nivel, r.imp_nivel
ORDER BY r.prob_nivel DESC, r.imp_nivel DESC;

-- Vista rápida de riesgos abiertos
CREATE OR REPLACE VIEW dashboard_riesgos_abiertos AS
SELECT 
  r.riesgo_id, r.titulo, r.estado, r.prob_nivel, r.imp_nivel, r.puntaje,
  COALESCE(m.prioridad,'Media') AS prioridad,
  c.nombre AS categoria,
  r.owner_autor_id, r.capitulo_id, r.subcapitulo_id, r.entrega_id, r.fase_id,
  r.fecha_revision, r.updated_at
FROM riesgo r
LEFT JOIN riesgo_categoria c ON c.categoria_id = r.categoria_id
LEFT JOIN riesgo_matriz m    ON m.escala_id = r.escala_id
                             AND m.prob_nivel = r.prob_nivel
                             AND m.imp_nivel  = r.imp_nivel
WHERE r.estado IN ('Identificado','Analizado','Planificado','En seguimiento');

-- 2.2 FKs internas
SELECT rc.CONSTRAINT_NAME, rc.TABLE_NAME, rc.REFERENCED_TABLE_NAME
FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS rc
WHERE rc.CONSTRAINT_SCHEMA = DATABASE()
  AND rc.TABLE_NAME IN ('riesgo','riesgo_evento','riesgo_accion','entregable','entregable_revision');

-- 2.3 Tipos de columnas clave
SELECT TABLE_NAME, COLUMN_NAME, COLUMN_TYPE, IS_NULLABLE, COLUMN_DEFAULT
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_SCHEMA = DATABASE()
  AND TABLE_NAME IN ('riesgo_categoria','riesgo_escala','riesgo','riesgo_evento','riesgo_accion',
                     'entregable_tipo','entregable','entregable_revision')
ORDER BY TABLE_NAME, ORDINAL_POSITION;

-- 2.4 Smoke test (catálogos mínimos + registros)
INSERT INTO riesgo_categoria (nombre) VALUES ('Cronograma');
INSERT INTO riesgo_escala (nombre, prob_min, prob_max, imp_min, imp_max)
VALUES ('Escala 1-5',1,5,1,5);

INSERT INTO entregable_tipo (nombre) VALUES ('Plan de Gestión de Riesgos');

INSERT INTO riesgo (titulo, descripcion, categoria_id, escala_id, prob_nivel, imp_nivel, estado)
VALUES ('Retraso por entregas', 'Demoras en subcapítulos',
        (SELECT categoria_id FROM riesgo_categoria WHERE nombre='Cronograma'),
        (SELECT escala_id FROM riesgo_escala WHERE nombre='Escala 1-5'),
        3, 3, 'Identificado');
SET @r := LAST_INSERT_ID();

INSERT INTO riesgo_evento (riesgo_id, tipo, detalle)
VALUES (@r, 'Identificacion', 'Riesgo registrado');

INSERT INTO riesgo_accion (riesgo_id, descripcion, estado)
VALUES (@r, 'Ajustar cronograma y recordatorios', 'Pendiente');

INSERT INTO entregable (nombre, tipo_id, estado)
VALUES ('Plan de Gestión de Riesgos',
        (SELECT tipo_id FROM entregable_tipo WHERE nombre='Plan de Gestión de Riesgos'),
        'Planificado');
SET @e := LAST_INSERT_ID();

INSERT INTO entregable_revision (entregable_id, decision, observaciones)
VALUES (@e, 'Aprobado con observaciones', 'Agregar matriz de priorización');

-- 2.5 Comprobación de cascadas
SELECT COUNT(*) AS eventos_antes  FROM riesgo_evento        WHERE riesgo_id=@r;
SELECT COUNT(*) AS acciones_antes FROM riesgo_accion        WHERE riesgo_id=@r;
DELETE FROM riesgo WHERE riesgo_id=@r;
SELECT COUNT(*) AS eventos_despues  FROM riesgo_evento      WHERE riesgo_id=@r;
SELECT COUNT(*) AS acciones_despues FROM riesgo_accion      WHERE riesgo_id=@r;

SELECT COUNT(*) AS revs_antes FROM entregable_revision WHERE entregable_id=@e;
DELETE FROM entregable WHERE entregable_id=@e;
SELECT COUNT(*) AS revs_despues FROM entregable_revision WHERE entregable_id=@e;
-- Bloque 3 – Consultas de tablero (seguimiento)
USE libro;

-- =========================================================
-- 3.A  MATRIZ PROBABILIDAD–IMPACTO (creación + semilla)
--      (idempotente: puedes ejecutar este bloque varias veces)
-- =========================================================

-- 1) Tabla de matriz
CREATE TABLE IF NOT EXISTS riesgo_matriz (
  matriz_id   INT AUTO_INCREMENT PRIMARY KEY,
  escala_id   INT NOT NULL,
  prob_nivel  INT NOT NULL,
  imp_nivel   INT NOT NULL,
  prioridad   ENUM('Baja','Media','Alta','Crítica') NOT NULL,
  color_hex   CHAR(7) DEFAULT '#FFFFFF',
  UNIQUE KEY uq_matriz (escala_id, prob_nivel, imp_nivel),
  CONSTRAINT fk_mat_escala
    FOREIGN KEY (escala_id) REFERENCES riesgo_escala(escala_id)
    ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB;

-- 2) Asegura la escala base 1–5
INSERT INTO riesgo_escala (nombre, prob_min, prob_max, imp_min, imp_max)
SELECT 'Escala 1-5', 1, 5, 1, 5
WHERE NOT EXISTS (SELECT 1 FROM riesgo_escala WHERE nombre='Escala 1-5');

-- 3) Semilla 5×5 (prioridad y color) – sin duplicar
INSERT IGNORE INTO riesgo_matriz (escala_id, prob_nivel, imp_nivel, prioridad, color_hex)
SELECT e.escala_id, p, i,
       CASE
         WHEN p*i >= 16 THEN 'Crítica'
         WHEN p*i >=  9 THEN 'Alta'
         WHEN p*i >=  4 THEN 'Media'
         ELSE 'Baja'
       END AS prioridad,
       CASE
         WHEN p*i >= 16 THEN '#E53935'   -- rojo
         WHEN p*i >=  9 THEN '#FB8C00'   -- naranja
         WHEN p*i >=  4 THEN '#FDD835'   -- amarillo
         ELSE '#43A047'                  -- verde
       END AS color_hex
FROM riesgo_escala e
JOIN (SELECT 1 p UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 UNION SELECT 5) P
JOIN (SELECT 1 i UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 UNION SELECT 5) I
WHERE e.nombre='Escala 1-5';

-- =========================================================
-- 3.B  VISTAS basadas en la matriz
-- =========================================================

-- Registro de riesgos con prioridad/color desde la matriz
CREATE OR REPLACE VIEW v_riesgo_registro AS
SELECT
  r.riesgo_id,
  r.titulo,
  r.estado,
  r.prob_nivel,
  r.imp_nivel,
  r.puntaje,
  COALESCE(m.prioridad,'Media')   AS prioridad,
  COALESCE(m.color_hex,'#FFFFFF') AS color_hex,
  r.categoria_id,
  r.owner_autor_id,
  r.capitulo_id,
  r.subcapitulo_id,
  r.entrega_id,
  r.fase_id,
  r.fecha_revision,
  r.updated_at
FROM riesgo r
LEFT JOIN riesgo_matriz m
  ON m.escala_id = r.escala_id
 AND m.prob_nivel = r.prob_nivel
 AND m.imp_nivel  = r.imp_nivel;

-- Dashboard de riesgos abiertos (solo estados activos)
CREATE OR REPLACE VIEW dashboard_riesgos_abiertos AS
SELECT
  rr.riesgo_id, rr.titulo, rr.estado,
  rr.prob_nivel, rr.imp_nivel, rr.puntaje,
  rr.prioridad, rr.color_hex,
  rr.categoria_id, rr.owner_autor_id,
  rr.capitulo_id, rr.subcapitulo_id, rr.entrega_id, rr.fase_id,
  rr.fecha_revision, rr.updated_at
FROM v_riesgo_registro rr
WHERE rr.estado IN ('Identificado','Analizado','Planificado','En seguimiento');

-- (Opcional) Dashboard de entregables abiertos (sin cambios)
CREATE OR REPLACE VIEW dashboard_entregables_abiertos AS
SELECT 
  e.entregable_id, e.nombre, et.nombre AS tipo, e.estado,
  e.fecha_compromiso, e.fecha_entrega,
  DATEDIFF(CURDATE(), e.fecha_compromiso) AS dias_atraso,
  e.url_repositorio, e.capitulo_id, e.subcapitulo_id, e.entrega_id, e.fase_id,
  e.created_at, e.updated_at
FROM entregable e
LEFT JOIN entregable_tipo et ON et.tipo_id = e.tipo_id
WHERE e.estado IN ('Planificado','En progreso','En revisión');

-- =========================================================
-- 3.C  CONSULTAS de operación (listas para usar)
-- =========================================================

-- Riesgos abiertos ordenados por prioridad (Crítica > Alta > Media > Baja) y puntaje
SELECT 
  riesgo_id, titulo, estado, prob_nivel, imp_nivel, puntaje, prioridad, color_hex,
  capitulo_id, subcapitulo_id, entrega_id, fase_id, updated_at
FROM dashboard_riesgos_abiertos
ORDER BY FIELD(prioridad,'Crítica','Alta','Media','Baja'), puntaje DESC, updated_at DESC;

-- Conteo por prioridad (solo abiertos)
SELECT prioridad, COUNT(*) AS total
FROM dashboard_riesgos_abiertos
GROUP BY prioridad
ORDER BY FIELD(prioridad,'Crítica','Alta','Media','Baja');

-- Heatmap (conteo por probabilidad x impacto)
SELECT prob_nivel, imp_nivel, COUNT(*) AS total
FROM dashboard_riesgos_abiertos
GROUP BY prob_nivel, imp_nivel
ORDER BY prob_nivel DESC, imp_nivel DESC;
