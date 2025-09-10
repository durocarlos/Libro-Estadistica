USE libro;

DROP TRIGGER IF EXISTS trg_smoke_test;

DELIMITER //
CREATE TRIGGER trg_smoke_test
BEFORE INSERT ON riesgo
FOR EACH ROW
BEGIN
  -- No hace nada, solo prueba BEGIN..END con DELIMITER
  SET @ok := 1;
END//
DELIMITER ;

-- Si esto compila sin 1064, el DELIMITER ya está bien tomado.
DROP TRIGGER IF EXISTS trg_smoke_test;
