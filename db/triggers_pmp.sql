/* ==========================================================
   PMP — Riesgos: Smoke test + Triggers completos
   ========================================================== */

USE libro;

/* ------------------------------------------
   0) Limpieza de triggers previos (idempotente)
   ------------------------------------------ */
DROP TRIGGER IF EXISTS trg_smoke_test;
DROP TRIGGER IF EXISTS trg_riesgo_bi_defaults;
DROP TRIGGER IF EXISTS trg_riesgo_bi_ranges;
DROP TRIGGER IF EXISTS trg_riesgo_bu_ranges;
DROP TRIGGER IF EXISTS trg_riesgo_au_estado;

/* ------------------------------------------
   1) Smoke test (verifica DELIMITER/BEGIN..END)
   ------------------------------------------ */
DELIMITER //
CREATE TRIGGER trg_smoke_test
BEFORE INSERT ON riesgo
FOR EACH ROW
BEGIN
  -- No hace nada: sólo confirma que compila y se puede ejecutar BEGIN..END
  SET @ok := 1;
END//
DELIMITER ;

-- Limpieza del smoke (opcional, si lo quieres dejar persistente, comenta la línea de abajo)
DROP TRIGGER IF EXISTS trg_smoke_test;

/* ------------------------------------------
   2) TRIGGER: Defaults seguros en BEFORE INSERT
      - Asegura valores por defecto coherentes si vienen NULL.
   ------------------------------------------ */
DELIMITER //
CREATE TRIGGER trg_riesgo_bi_defaults
BEFORE INSERT ON riesgo
FOR EACH ROW
BEGIN
  -- Estado por defecto si viene NULL
  IF NEW.estado IS NULL OR NEW.estado = '' THEN
    SET NEW.estado := 'Identificado';
  END IF;

  -- Probabilidad/Impacto por defecto si vienen NULL
  IF NEW.prob_nivel IS NULL THEN
    SET NEW.prob_nivel := 1;
  END IF;

  IF NEW.imp_nivel IS NULL THEN
    SET NEW.imp_nivel := 1;
  END IF;

  -- Fecha de revisión por defecto (hoy) si viene NULL
  IF NEW.fecha_revision IS NULL THEN
    SET NEW.fecha_revision := CURDATE();
  END IF;
END//
DELIMITER ;

/* ------------------------------------------
   3) BEFORE INSERT: validar y ajustar a rango
   (sin SIGNAL; compatible con versiones antiguas)
   ------------------------------------------ */
DROP TRIGGER IF EXISTS trg_riesgo_bi_ranges;
DELIMITER //
CREATE TRIGGER trg_riesgo_bi_ranges
BEFORE INSERT ON riesgo
FOR EACH ROW
BEGIN
  DECLARE v_prob_min INT DEFAULT NULL;
  DECLARE v_prob_max INT DEFAULT NULL;
  DECLARE v_imp_min  INT DEFAULT NULL;
  DECLARE v_imp_max  INT DEFAULT NULL;
  DECLARE v_msg VARCHAR(255) DEFAULT NULL;

  IF NEW.escala_id IS NOT NULL THEN
    SELECT prob_min, prob_max, imp_min, imp_max
      INTO v_prob_min, v_prob_max, v_imp_min, v_imp_max
    FROM riesgo_escala
    WHERE escala_id = NEW.escala_id
    LIMIT 1;

    IF v_prob_min IS NOT NULL AND v_imp_min IS NOT NULL THEN
      IF NEW.prob_nivel < v_prob_min THEN
        SET NEW.prob_nivel = v_prob_min;
        SET v_msg = CONCAT('prob_nivel ajustado a ', v_prob_min);
      ELSEIF NEW.prob_nivel > v_prob_max THEN
        SET NEW.prob_nivel = v_prob_max;
        SET v_msg = CONCAT('prob_nivel ajustado a ', v_prob_max);
      END IF;

      IF NEW.imp_nivel < v_imp_min THEN
        SET NEW.imp_nivel = v_imp_min;
        SET v_msg = CONCAT(IFNULL(v_msg,''), IF(v_msg IS NULL,'', '; '), 'imp_nivel ajustado a ', v_imp_min);
      ELSEIF NEW.imp_nivel > v_imp_max THEN
        SET NEW.imp_nivel = v_imp_max;
        SET v_msg = CONCAT(IFNULL(v_msg,''), IF(v_msg IS NULL,'', '; '), 'imp_nivel ajustado a ', v_imp_max);
      END IF;
    END IF;
  END IF;

  /* si hubo ajustes, dejamos constancia al insertar (AFTER INSERT no aplica aquí) */
  /* nota: no podemos insertar aún en AFTER INSERT, así que solo anotamos en campos de la fila */
  /* opcional: podrías crear un AFTER INSERT que lea un flag y escriba la nota */
END//
DELIMITER ;

/* ------------------------------------------
   4) BEFORE UPDATE: validar y ajustar a rango
   (sin SIGNAL; compatible)
   ------------------------------------------ */
DROP TRIGGER IF EXISTS trg_riesgo_bu_ranges;
DELIMITER //
CREATE TRIGGER trg_riesgo_bu_ranges
BEFORE UPDATE ON riesgo
FOR EACH ROW
BEGIN
  DECLARE v_prob_min INT DEFAULT NULL;
  DECLARE v_prob_max INT DEFAULT NULL;
  DECLARE v_imp_min  INT DEFAULT NULL;
  DECLARE v_imp_max  INT DEFAULT NULL;

  IF NEW.escala_id IS NOT NULL THEN
    SELECT prob_min, prob_max, imp_min, imp_max
      INTO v_prob_min, v_prob_max, v_imp_min, v_imp_max
    FROM riesgo_escala
    WHERE escala_id = NEW.escala_id
    LIMIT 1;

    IF v_prob_min IS NOT NULL AND v_imp_min IS NOT NULL THEN
      IF NEW.prob_nivel < v_prob_min THEN
        SET NEW.prob_nivel = v_prob_min;
      ELSEIF NEW.prob_nivel > v_prob_max THEN
        SET NEW.prob_nivel = v_prob_max;
      END IF;

      IF NEW.imp_nivel < v_imp_min THEN
        SET NEW.imp_nivel = v_imp_min;
      ELSEIF NEW.imp_nivel > v_imp_max THEN
        SET NEW.imp_nivel = v_imp_max;
      END IF;
    END IF;
  END IF;
END//
DELIMITER ;

/* ------------------------------------------
   5) AFTER UPDATE: bitácora cuando cambia el estado
   ------------------------------------------ */
DROP TRIGGER IF EXISTS trg_riesgo_au_estado;
DELIMITER //
CREATE TRIGGER trg_riesgo_au_estado
AFTER UPDATE ON riesgo
FOR EACH ROW
BEGIN
  IF NEW.estado <> OLD.estado THEN
    INSERT INTO riesgo_evento
      (riesgo_id, fecha_evento, tipo, detalle, actor_autor_id)
    VALUES
      (NEW.riesgo_id,
       NOW(),
       CASE WHEN NEW.estado = 'Cerrado' THEN 'Cierre' ELSE 'Actualizacion' END,
       CONCAT('Estado: ', OLD.estado, ' -> ', NEW.estado),
       NEW.owner_autor_id);
  END IF;
END//
DELIMITER ;

/* ==========================================================
   PMP — Riesgos: Smoke test + Triggers + Bloque de prueba
   ========================================================== */

USE libro;

/* ------------------------------------------
   0) Limpieza de triggers previos (idempotente)
   ------------------------------------------ */
DROP TRIGGER IF EXISTS trg_smoke_test;
DROP TRIGGER IF EXISTS trg_riesgo_bi_defaults;
DROP TRIGGER IF EXISTS trg_riesgo_bi_ranges;
DROP TRIGGER IF EXISTS trg_riesgo_bu_ranges;
DROP TRIGGER IF EXISTS trg_riesgo_au_estado;

/* ------------------------------------------
   1) Smoke test (verifica DELIMITER/BEGIN..END)
   ------------------------------------------ */
DELIMITER //
CREATE TRIGGER trg_smoke_test
BEFORE INSERT ON riesgo
FOR EACH ROW
BEGIN
  -- No hace nada: sólo confirma que compila y se ejecuta BEGIN..END
  SET @ok := 1;
END//
DELIMITER ;

-- (Opcional) Borrar el smoke si no quieres dejarlo permanente
DROP TRIGGER IF EXISTS trg_smoke_test;

/* ------------------------------------------
   2) TRIGGER: Defaults seguros en BEFORE INSERT
      - Asegura valores por defecto coherentes si vienen NULL.
   ------------------------------------------ */
DELIMITER //
CREATE TRIGGER trg_riesgo_bi_defaults
BEFORE INSERT ON riesgo
FOR EACH ROW
BEGIN
  -- Estado por defecto si viene NULL o vacío
  IF NEW.estado IS NULL OR NEW.estado = '' THEN
    SET NEW.estado := 'Identificado';
  END IF;

  -- Probabilidad/Impacto por defecto si vienen NULL
  IF NEW.prob_nivel IS NULL THEN
    SET NEW.prob_nivel := 1;
  END IF;

  IF NEW.imp_nivel IS NULL THEN
    SET NEW.imp_nivel := 1;
  END IF;

  -- Fecha de revisión por defecto (hoy) si viene NULL
  IF NEW.fecha_revision IS NULL THEN
    SET NEW.fecha_revision := CURDATE();
  END IF;
END//
DELIMITER ;

/* ------------------------------------------
   3) TRIGGER: Validación de rangos en BEFORE INSERT
      - Verifica que prob_nivel / imp_nivel estén dentro
        de los límites definidos por la escala (riesgo_escala).
   ------------------------------------------ */
DELIMITER //
CREATE TRIGGER trg_riesgo_bi_ranges
BEFORE INSERT ON riesgo
FOR EACH ROW
BEGIN
  DECLARE v_prob_min INT;
  DECLARE v_prob_max INT;
  DECLARE v_imp_min  INT;
  DECLARE v_imp_max  INT;

  -- Debe existir una escala válida
  IF NEW.escala_id IS NULL THEN
    SIGNAL SQLSTATE '45000'
      SET MESSAGE_TEXT = 'La escala (escala_id) no puede ser NULL';
  END IF;

  -- Lee límites de la escala
  SELECT prob_min, prob_max, imp_min, imp_max
    INTO v_prob_min, v_prob_max, v_imp_min, v_imp_max
  FROM riesgo_escala
  WHERE escala_id = NEW.escala_id
  LIMIT 1;

  -- Si no hay fila en riesgo_escala, v_prob_min quedará NULL
  IF v_prob_min IS NULL THEN
    SIGNAL SQLSTATE '45000'
      SET MESSAGE_TEXT = 'Escala no encontrada en riesgo_escala';
  END IF;

  -- Valida probabilidad
  IF (NEW.prob_nivel < v_prob_min OR NEW.prob_nivel > v_prob_max) THEN
    SIGNAL SQLSTATE '45000'
      SET MESSAGE_TEXT = CONCAT(
        'prob_nivel fuera de rango [',
        v_prob_min, '-', v_prob_max,
        '] para escala_id=', NEW.escala_id
      );
  END IF;

  -- Valida impacto
  IF (NEW.imp_nivel < v_imp_min OR NEW.imp_nivel > v_imp_max) THEN
    SIGNAL SQLSTATE '45000'
      SET MESSAGE_TEXT = CONCAT(
        'imp_nivel fuera de rango [',
        v_imp_min, '-', v_imp_max,
        '] para escala_id=', NEW.escala_id
      );
  END IF;
END//
DELIMITER ;

/* ------------------------------------------
   4) TRIGGER: Validación de rangos en BEFORE UPDATE
      - Igual que el anterior, pero para cambios en UPDATE.
   ------------------------------------------ */
DELIMITER //
CREATE TRIGGER trg_riesgo_bu_ranges
BEFORE UPDATE ON riesgo
FOR EACH ROW
BEGIN
  DECLARE v_prob_min INT;
  DECLARE v_prob_max INT;
  DECLARE v_imp_min  INT;
  DECLARE v_imp_max  INT;

  -- Si cambia escala o cambian prob/imp, se valida
  IF NEW.escala_id IS NULL THEN
    SIGNAL SQLSTATE '45000'
      SET MESSAGE_TEXT = 'La escala (escala_id) no puede ser NULL';
  END IF;

  SELECT prob_min, prob_max, imp_min, imp_max
    INTO v_prob_min, v_prob_max, v_imp_min, v_imp_max
  FROM riesgo_escala
  WHERE escala_id = NEW.escala_id
  LIMIT 1;

  IF v_prob_min IS NULL THEN
    SIGNAL SQLSTATE '45000'
      SET MESSAGE_TEXT = 'Escala no encontrada en riesgo_escala';
  END IF;

  -- Valida probabilidad
  IF (NEW.prob_nivel < v_prob_min OR NEW.prob_nivel > v_prob_max) THEN
    SIGNAL SQLSTATE '45000'
      SET MESSAGE_TEXT = CONCAT(
        'prob_nivel fuera de rango [',
        v_prob_min, '-', v_prob_max,
        '] para escala_id=', NEW.escala_id
      );
  END IF;

  -- Valida impacto
  IF (NEW.imp_nivel < v_imp_min OR NEW.imp_nivel > v_imp_max) THEN
    SIGNAL SQLSTATE '45000'
      SET MESSAGE_TEXT = CONCAT(
        'imp_nivel fuera de rango [',
        v_imp_min, '-', v_imp_max,
        '] para escala_id=', NEW.escala_id
      );
  END IF;
END//
DELIMITER ;

/* ------------------------------------------
   5) TRIGGER: Bitácora de cambio de estado en AFTER UPDATE
      - Cuando cambia el estado, registra un evento en riesgo_evento.
   ------------------------------------------ */
DELIMITER //
CREATE TRIGGER trg_riesgo_au_estado
AFTER UPDATE ON riesgo
FOR EACH ROW
BEGIN
  IF NEW.estado IS NOT NULL AND OLD.estado IS NOT NULL
     AND NEW.estado <> OLD.estado THEN
    INSERT INTO riesgo_evento (riesgo_id, fecha_evento, tipo, detalle, actor_autor_id)
    VALUES (
      NEW.riesgo_id,
      NOW(),
      'Actualizacion',
      CONCAT('Estado: ', OLD.estado, ' -> ', NEW.estado),
      NEW.owner_autor_id
    );
  END IF;
END//
DELIMITER ;

/* ==========================================================
   6) Bloque de prueba (OPCIONAL) — Ejecutar manualmente
   ========================================================== */
/*
-- (a) Inserta un riesgo de prueba dentro de rango (escala_id=1 tiene 1..5)
INSERT INTO riesgo (titulo, descripcion, estado, prob_nivel, imp_nivel, escala_id, owner_autor_id)
VALUES ('TRG TEST - Rango ok', 'Prueba trigger INSERT RANGO', 'Identificado', 3, 4, 1, 1);

-- (b) Fuerza un rango inválido (debe lanzar error SQLSTATE 45000)
INSERT INTO riesgo (titulo, descripcion, estado, prob_nivel, imp_nivel, escala_id, owner_autor_id)
VALUES ('TRG TEST - Rango inválido', 'Debe fallar', 'Identificado', 10, 4, 1, 1);

-- (c) Cambia estado (debe registrar evento en riesgo_evento)
UPDATE riesgo SET estado = 'En seguimiento'
WHERE titulo = 'TRG TEST - Rango ok' LIMIT 1;

-- (d) Ver eventos creados recientemente
SELECT * FROM riesgo_evento ORDER BY evento_id DESC LIMIT 10;

-- (e) Limpieza segura (usa la PK para evitar Safe Update 1175)
DELETE FROM riesgo
WHERE riesgo_id IN (
  SELECT r.riesgo_id FROM (SELECT riesgo_id FROM riesgo WHERE titulo LIKE 'TRG TEST%') AS r
);
*/
