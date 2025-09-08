Use libro;
CREATE TABLE correo_log (
  correo_id        INT AUTO_INCREMENT PRIMARY KEY,
  autor_id         INT NOT NULL,
  capitulo_id      INT DEFAULT NULL,
  subcapitulo_id   INT DEFAULT NULL,
  entrega_id       INT DEFAULT NULL,
  asunto           VARCHAR(255) NOT NULL,
  cuerpo           TEXT,
  fecha_envio      DATETIME DEFAULT CURRENT_TIMESTAMP,
  estado_envio     VARCHAR(50) DEFAULT 'pendiente',
  FOREIGN KEY (autor_id) REFERENCES autor(autor_id),
  FOREIGN KEY (capitulo_id) REFERENCES capitulo(capitulo_id),
  FOREIGN KEY (subcapitulo_id) REFERENCES subcapitulo(subcapitulo_id),
  FOREIGN KEY (entrega_id) REFERENCES entrega(entrega_id)
);
SHOW TABLES;

-- Ver registros de la tabla capitulo
SELECT * FROM correo_log LIMIT 100;
