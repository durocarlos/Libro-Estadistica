USE libro;
SELECT DATABASE();
INSERT INTO capitulo (numero, titulo) VALUES
(1,'Conceptos básicos de estadística'),
(2,'Variables y escalas de medición'),
(3,'Medidas de tendencia central y dispersión'),
(4,'Representación gráfica'),
(5,'Fundamentos del muestreo'),
(6,'Distribuciones de probabilidad'),
(7,'Distribución normal y sus aplicaciones'),
(8,'Estimación de parámetros'),
(9,'Pruebas de hipótesis: fundamentos'),
(10,'Pruebas para medias'),
(11,'ANOVA'),
(12,'Pruebas no paramétricas'),
(13,'Correlación'),
(14,'Regresión lineal simple'),
(15,'Regresión múltiple'),
(16,'Diagnóstico y validación de modelos'),
(17,'Fundamentos del análisis multivariado'),
(18,'Modelos de ecuaciones estructurales (SEM)'),
(19,'SmartPLS: uso y aplicaciones'),
(20,'Interpretación de resultados en PLS-SEM'),
(21,'Introducción a las series de tiempo'),
(22,'Métodos de suavizamiento'),
(23,'Modelos ARIMA y Box–Jenkins'),
(24,'Aplicaciones prácticas de pronósticos'),
(25,'Análisis con R'),
(26,'Análisis con Python'),
(27,'Análisis con SPSS'),
(28,'Análisis con Excel'),
(29,'Aplicaciones en educación superior'),
(30,'Aplicaciones en negocios y empresas'),
(31,'Aplicaciones en políticas públicas'),
(32,'Tendencias futuras de la estadística aplicada');
SELECT * FROM capitulo;
-- Crear tabla de subcapítulos
CREATE TABLE IF NOT EXISTS subcapitulo (
  subcapitulo_id INT AUTO_INCREMENT PRIMARY KEY,
  capitulo_id INT NOT NULL,
  numero VARCHAR(10) NOT NULL,
  titulo TEXT NOT NULL,
  FOREIGN KEY (capitulo_id) REFERENCES capitulo(capitulo_id)
    ON DELETE CASCADE
) ENGINE=InnoDB;

-- Insertar subcapítulos (3 por cada capítulo)
INSERT INTO subcapitulo (capitulo_id, numero, titulo) VALUES
(1,  '1.1', 'Introducción y contexto teórico'),
(1,  '1.2', 'Desarrollo del contenido principal'),
(1,  '1.3', 'Conclusiones y referencias'),
(2,  '2.1', 'Introducción y contexto teórico'),
(2,  '2.2', 'Desarrollo del contenido principal'),
(2,  '2.3', 'Conclusiones y referencias'),
(3,  '3.1', 'Introducción y contexto teórico'),
(3,  '3.2', 'Desarrollo del contenido principal'),
(3,  '3.3', 'Conclusiones y referencias'),
(4,  '4.1', 'Introducción y contexto teórico'),
(4,  '4.2', 'Desarrollo del contenido principal'),
(4,  '4.3', 'Conclusiones y referencias'),
(5,  '5.1', 'Introducción y contexto teórico'),
(5,  '5.2', 'Desarrollo del contenido principal'),
(5,  '5.3', 'Conclusiones y referencias'),
(6,  '6.1', 'Introducción y contexto teórico'),
(6,  '6.2', 'Desarrollo del contenido principal'),
(6,  '6.3', 'Conclusiones y referencias'),
(7,  '7.1', 'Introducción y contexto teórico'),
(7,  '7.2', 'Desarrollo del contenido principal'),
(7,  '7.3', 'Conclusiones y referencias'),
(8,  '8.1', 'Introducción y contexto teórico'),
(8,  '8.2', 'Desarrollo del contenido principal'),
(8,  '8.3', 'Conclusiones y referencias'),
(9,  '9.1', 'Introducción y contexto teórico'),
(9,  '9.2', 'Desarrollo del contenido principal'),
(9,  '9.3', 'Conclusiones y referencias'),
(10, '10.1', 'Introducción y contexto teórico'),
(10, '10.2', 'Desarrollo del contenido principal'),
(10, '10.3', 'Conclusiones y referencias'),
(11, '11.1', 'Introducción y contexto teórico'),
(11, '11.2', 'Desarrollo del contenido principal'),
(11, '11.3', 'Conclusiones y referencias'),
(12, '12.1', 'Introducción y contexto teórico'),
(12, '12.2', 'Desarrollo del contenido principal'),
(12, '12.3', 'Conclusiones y referencias'),
(13, '13.1', 'Introducción y contexto teórico'),
(13, '13.2', 'Desarrollo del contenido principal'),
(13, '13.3', 'Conclusiones y referencias'),
(14, '14.1', 'Introducción y contexto teórico'),
(14, '14.2', 'Desarrollo del contenido principal'),
(14, '14.3', 'Conclusiones y referencias'),
(15, '15.1', 'Introducción y contexto teórico'),
(15, '15.2', 'Desarrollo del contenido principal'),
(15, '15.3', 'Conclusiones y referencias'),
(16, '16.1', 'Introducción y contexto teórico'),
(16, '16.2', 'Desarrollo del contenido principal'),
(16, '16.3', 'Conclusiones y referencias'),
(17, '17.1', 'Introducción y contexto teórico'),
(17, '17.2', 'Desarrollo del contenido principal'),
(17, '17.3', 'Conclusiones y referencias'),
(18, '18.1', 'Introducción y contexto teórico'),
(18, '18.2', 'Desarrollo del contenido principal'),
(18, '18.3', 'Conclusiones y referencias'),
(19, '19.1', 'Introducción y contexto teórico'),
(19, '19.2', 'Desarrollo del contenido principal'),
(19, '19.3', 'Conclusiones y referencias'),
(20, '20.1', 'Introducción y contexto teórico'),
(20, '20.2', 'Desarrollo del contenido principal'),
(20, '20.3', 'Conclusiones y referencias'),
(21, '21.1', 'Introducción y contexto teórico'),
(21, '21.2', 'Desarrollo del contenido principal'),
(21, '21.3', 'Conclusiones y referencias'),
(22, '22.1', 'Introducción y contexto teórico'),
(22, '22.2', 'Desarrollo del contenido principal'),
(22, '22.3', 'Conclusiones y referencias'),
(23, '23.1', 'Introducción y contexto teórico'),
(23, '23.2', 'Desarrollo del contenido principal'),
(23, '23.3', 'Conclusiones y referencias'),
(24, '24.1', 'Introducción y contexto teórico'),
(24, '24.2', 'Desarrollo del contenido principal'),
(24, '24.3', 'Conclusiones y referencias'),
(25, '25.1', 'Introducción y contexto teórico'),
(25, '25.2', 'Desarrollo del contenido principal'),
(25, '25.3', 'Conclusiones y referencias'),
(26, '26.1', 'Introducción y contexto teórico'),
(26, '26.2', 'Desarrollo del contenido principal'),
(26, '26.3', 'Conclusiones y referencias'),
(27, '27.1', 'Introducción y contexto teórico'),
(27, '27.2', 'Desarrollo del contenido principal'),
(27, '27.3', 'Conclusiones y referencias'),
(28, '28.1', 'Introducción y contexto teórico'),
(28, '28.2', 'Desarrollo del contenido principal'),
(28, '28.3', 'Conclusiones y referencias'),
(29, '29.1', 'Introducción y contexto teórico'),
(29, '29.2', 'Desarrollo del contenido principal'),
(29, '29.3', 'Conclusiones y referencias'),
(30, '30.1', 'Introducción y contexto teórico'),
(30, '30.2', 'Desarrollo del contenido principal'),
(30, '30.3', 'Conclusiones y referencias'),
(31, '31.1', 'Introducción y contexto teórico'),
(31, '31.2', 'Desarrollo del contenido principal'),
(31, '31.3', 'Conclusiones y referencias'),
(32, '32.1', 'Introducción y contexto teórico'),
(32, '32.2', 'Desarrollo del contenido principal'),
(32, '32.3', 'Conclusiones y referencias');

-- Relación Autor–Subcapítulo (si asignarás a este nivel)
CREATE TABLE IF NOT EXISTS subcapitulo_autor (
  subcapitulo_autor_id INT AUTO_INCREMENT PRIMARY KEY,
  subcapitulo_id INT NOT NULL,
  autor_id       INT NOT NULL,
  rol_id         SMALLINT NOT NULL,
  orden          SMALLINT NULL,
  CONSTRAINT uq_sub_aut UNIQUE (subcapitulo_id, autor_id, rol_id),
  FOREIGN KEY (subcapitulo_id) REFERENCES subcapitulo(subcapitulo_id) ON DELETE CASCADE,
  FOREIGN KEY (autor_id)       REFERENCES autor(autor_id)             ON DELETE CASCADE,
  FOREIGN KEY (rol_id)         REFERENCES rol(rol_id)
) ENGINE=InnoDB;
SELECT COUNT(*) capitulos     FROM capitulo;
SELECT COUNT(*) subcapitulos  FROM subcapitulo;
SELECT COUNT(*) roles         FROM rol;
SHOW TABLES;
INSERT INTO rol (rol_id, nombre) VALUES
 (1,'Autor principal'), (2,'Coautor'), (3,'Revisor'), (4,'Coordinador')
ON DUPLICATE KEY UPDATE nombre=VALUES(nombre);
SELECT * FROM capitulo;
SELECT * FROM subcapitulo;
SELECT * FROM rol;
SELECT * FROM autor;

SELECT DATABASE();
USE libro; 
SHOW TABLES;

CREATE TABLE IF NOT EXISTS autor (
  ID INT AUTO_INCREMENT PRIMARY KEY,
  Nombre_completo VARCHAR(255) NOT NULL,
  Correo_electronico VARCHAR(320) NOT NULL UNIQUE,
  Telefono_WhatsApp VARCHAR(50),
  Pais_ciudad VARCHAR(150),
  Institucion_afiliacion VARCHAR(255),
  Cargo_academico VARCHAR(150),
  ORCID_ID VARCHAR(1000),
  Breve_resena_biografica TEXT,
  Agradecimiento_Financiamiento TEXT,
  Foto LONGBLOB, -- aquí se almacenará la imagen subida
  Sin_conflicto_interes BOOLEAN DEFAULT 0,
  Acepta_participar BOOLEAN DEFAULT 0
) ENGINE=InnoDB;
ALTER TABLE autor
  ADD COLUMN breve_resena_biografica TEXT AFTER foto_fecha,
  ADD COLUMN agradecimiento_financiamiento TEXT AFTER breve_resena_biografica,
  ADD COLUMN sin_conflicto_interes BOOLEAN NOT NULL DEFAULT 0 AFTER agradecimiento_financiamiento,
  ADD COLUMN acepta_participar BOOLEAN NOT NULL DEFAULT 0 AFTER sin_conflicto_interes;

DESCRIBE autor;
USE libro;
ALTER TABLE autor  MODIFY email VARCHAR(320) NULL;
INSERT INTO autor (nombre_completo) VALUES
('Daniel Román'),
('Migdalia Viez'),
('Víctor Barreto'),
('Ricardo Santa'),
('Gael Velásquez'),
('Andrés Galvis'),
('Edgar Sansores'),
('Jesús Reyes'),
('Jetro López'),
('Francisco Cedeño'),
('Yoskira Cordero'),
('Diana Emilce Barrera Cuta'),
('Luis Maguiña Custodio'),
('Diana Emilce Barrera + José Carlos Rodríguez'),
('Mario Gómez'),
('Grace Viteri');

SELECT autor_id, nombre_completo FROM autor;
-- Actualizar país de los autores
UPDATE autor SET pais_ciudad = 'Venezuela' WHERE autor_id = 1;
UPDATE autor SET pais_ciudad = 'Venezuela' WHERE autor_id = 2;
UPDATE autor SET pais_ciudad = 'Bolivia' WHERE autor_id = 3;
UPDATE autor SET pais_ciudad = 'Colombia' WHERE autor_id = 4;
UPDATE autor SET pais_ciudad = 'Bolivia' WHERE autor_id = 5;
UPDATE autor SET pais_ciudad = 'Ecuador' WHERE autor_id = 6;
UPDATE autor SET pais_ciudad = 'Bolivia' WHERE autor_id = 7;
UPDATE autor SET pais_ciudad = 'Perú' WHERE autor_id = 8;
UPDATE autor SET pais_ciudad = 'Venezuela' WHERE autor_id = 9;
UPDATE autor SET pais_ciudad = 'Ecuador' WHERE autor_id = 10;
UPDATE autor SET pais_ciudad = 'Venezuela' WHERE autor_id = 11;
UPDATE autor SET pais_ciudad = 'Colombia' WHERE autor_id = 12;
UPDATE autor SET pais_ciudad = 'Perú' WHERE autor_id = 13;
UPDATE autor SET pais_ciudad = 'México' WHERE autor_id = 14;
UPDATE autor SET pais_ciudad = 'México' WHERE autor_id = 15;
UPDATE autor SET pais_ciudad = 'Ecuador' WHERE autor_id = 16;
SELECT autor_id, nombre_completo, email, pais_ciudad FROM autor;
-- Actualizar institución de los autores
UPDATE autor SET institucion = NULL WHERE autor_id = 1;
UPDATE autor SET institucion = NULL WHERE autor_id = 2;
UPDATE autor SET institucion = NULL WHERE autor_id = 3;
UPDATE autor SET institucion = NULL WHERE autor_id = 4;
UPDATE autor SET institucion = NULL WHERE autor_id = 5;
UPDATE autor SET institucion = 'Universidad de las Fuerzas Armadas - ESPE; Sociedad Ecuatoriana de Estadística - SEE' WHERE autor_id = 6;
UPDATE autor SET institucion = NULL WHERE autor_id = 7;
UPDATE autor SET institucion = NULL WHERE autor_id = 8;
UPDATE autor SET institucion = NULL WHERE autor_id = 9;
UPDATE autor SET institucion = 'Universidad Tecnologica Empresarial de Guayaquil' WHERE autor_id = 10;
UPDATE autor SET institucion = 'Universidad Tecnologica Empresarial de Guayaquil' WHERE autor_id = 11;
UPDATE autor SET institucion = NULL WHERE autor_id = 12;
UPDATE autor SET institucion = 'Universidad Cesar Vallejo' WHERE autor_id = 13;
UPDATE autor SET institucion = NULL WHERE autor_id = 14;
UPDATE autor SET institucion = NULL WHERE autor_id = 15;
UPDATE autor SET institucion = 'Universidad Tecnologica Empresarial de Guayaquil' WHERE autor_id = 16;
SELECT autor_id, nombre_completo, email, pais_ciudad,institucion FROM autor;
-- Actualizar cargo académico de los autores
UPDATE autor SET cargo = NULL WHERE autor_id = 1;
UPDATE autor SET cargo = NULL WHERE autor_id = 2;
UPDATE autor SET cargo = NULL WHERE autor_id = 3;
UPDATE autor SET cargo = NULL WHERE autor_id = 4;
UPDATE autor SET cargo = NULL WHERE autor_id = 5;
UPDATE autor SET cargo = 'Profesor Titular / Director Académico' WHERE autor_id = 6;
UPDATE autor SET cargo = NULL WHERE autor_id = 7;
UPDATE autor SET cargo = NULL WHERE autor_id = 8;
UPDATE autor SET cargo = NULL WHERE autor_id = 9;
UPDATE autor SET cargo = NULL WHERE autor_id = 10;
UPDATE autor SET cargo = 'Directora Doctorado en Educación' WHERE autor_id = 11;
UPDATE autor SET cargo = NULL WHERE autor_id = 12;
UPDATE autor SET cargo = 'Consultor independiente' WHERE autor_id = 13;
UPDATE autor SET cargo = NULL WHERE autor_id = 14;
UPDATE autor SET cargo = NULL WHERE autor_id = 15;
UPDATE autor SET cargo = 'Profesor Tiempo completo' WHERE autor_id = 16;
SELECT autor_id, nombre_completo, email, pais_ciudad,institucion,cargo FROM autor;
SELECT autor_id, nombre_completo, cargo FROM autor;
-- Actualizar ORCID de los autores
UPDATE autor SET orcid = NULL WHERE autor_id = 1;
UPDATE autor SET orcid = NULL WHERE autor_id = 2;
UPDATE autor SET orcid = NULL WHERE autor_id = 3;
UPDATE autor SET orcid = NULL WHERE autor_id = 4;
UPDATE autor SET orcid = NULL WHERE autor_id = 5;
UPDATE autor SET orcid = 'https://orcid.org/0000-0001-7762-2893' WHERE autor_id = 6;
UPDATE autor SET orcid = NULL WHERE autor_id = 7;
UPDATE autor SET orcid = NULL WHERE autor_id = 8;
UPDATE autor SET orcid = NULL WHERE autor_id = 9;
UPDATE autor SET orcid = 'https://orcid.org/0000-0003-4982-4185' WHERE autor_id = 10;
UPDATE autor SET orcid = 'https://orcid.org/0000-0003-0292-6897' WHERE autor_id = 11;
UPDATE autor SET orcid = NULL WHERE autor_id = 12;
UPDATE autor SET orcid = 'https://orcid.org/0000-0003-1117-8368' WHERE autor_id = 13;
UPDATE autor SET orcid = NULL WHERE autor_id = 14;
UPDATE autor SET orcid = NULL WHERE autor_id = 15;
UPDATE autor SET orcid = 'https://orcid.org/0000-0002-5645-2634' WHERE autor_id = 16;
SELECT autor_id, nombre_completo, orcid FROM autor;
-- Actualizar reseña biográfica de los autores
UPDATE autor SET breve_resena_biografica = NULL WHERE autor_id = 1;
UPDATE autor SET breve_resena_biografica = NULL WHERE autor_id = 2;
UPDATE autor SET breve_resena_biografica = NULL WHERE autor_id = 3;
UPDATE autor SET breve_resena_biografica = NULL WHERE autor_id = 4;
UPDATE autor SET breve_resena_biografica = NULL WHERE autor_id = 5;

UPDATE autor SET breve_resena_biografica = 'Doctor en Modelación Matemática y Computación Científica. Doctor en Economía. MSc en Estadística. Ingeniero Financiero. Director Académico Sociedad Ecuatoriana de Estadística - SEE. Profesor Titular de Teoría de la Probabilidad y Estadística de la Universidad de las Fuerzas Armadas - ESPE. Sus áreas de investigación son: Teoría de la Probabilidad, Procesos Estocásticos, Análisis Multivariante, Ecuaciones Diferenciales Estocásticas, Econometría y Modelos DSGE/HANK. Ha colaborado como revisor y editor en revistas científicas. Actualmente impulsa proyectos de investigación aplicada en ingeniería, economía, educación y ciencia sociales.' WHERE autor_id = 6;

UPDATE autor SET breve_resena_biografica = 'Magíster en Estadística y docente de posgrado en análisis cuantitativo. Sus áreas de especialización son métodos estadísticos avanzados y validación de diseños de investigación. Ha colaborado como revisor y editor en revistas científicas. Actualmente impulsa proyectos de investigación aplicada en ciencias sociales y economía.' WHERE autor_id = 7;

UPDATE autor SET breve_resena_biografica = 'Magíster en Estadística y docente de posgrado en análisis cuantitativo. Sus áreas de especialización son métodos estadísticos avanzados y validación de diseños de investigación. Ha colaborado como revisor y editor en revistas científicas. Actualmente impulsa proyectos de investigación aplicada en ciencias sociales y economía.' WHERE autor_id = 8;

UPDATE autor SET breve_resena_biografica = 'Licenciada en Matemáticas y especialista en sistemas informáticos. Se enfoca en estadística descriptiva e inferencial aplicada a la educación. Ha trabajado como docente universitaria y formadora en el uso de SPSS y Excel. Actualmente participa en proyectos de innovación académica con énfasis en investigación cuantitativa.' WHERE autor_id = 9;

UPDATE autor SET breve_resena_biografica = 'Licenciado en Estadística y docente en universidades ecuatorianas. Se especializa en estadística descriptiva, inferencial y diseño de instrumentos de recolección de datos. Ha dirigido proyectos de investigación aplicada en educación y ciencias sociales. Actualmente trabaja en la formación académica y asesoría metodológica.' WHERE autor_id = 10;

UPDATE autor SET breve_resena_biografica = 'Doctora en Educación, Economista y docente en universidades ecuatorianas. Se especializa en estadística descriptiva, inferencial y diseño de instrumentos de recolección de datos. Ha dirigido proyectos de investigación aplicada en educación y ciencias sociales. Actualmente trabaja en la formación académica y asesoría metodológica.' WHERE autor_id = 11;

UPDATE autor SET breve_resena_biografica = 'Magister en Economía y Finzas, Especialista en Administración Financiera Estrategica, Contador Público, Docente Universitario, actualmente Lidera el programa de Contaduría Pública de Unitrópico, procesos de acreditación y Autoevluación' WHERE autor_id = 12;

UPDATE autor SET breve_resena_biografica = 'Licenciado en Psicología, con experiencia como asesor independiente en investigación académica. Actualmente cursa una Maestría en Docencia Universitaria y un programa de especialización en Evaluación Psicológica y Psicometría. Su trayectoria se ha centrado en el desarrollo de investigaciones instrumentales, especialmente en la evaluación de la validez basada en la estructura interna mediante análisis factorial confirmatorio (AFC), estudios de invarianza factorial y estimaciones de confiabilidad. Asimismo, ha trabajado en el análisis de redes psicométricas, estudios multivariantes y modelamiento de ecuaciones estructurales (SEM), consolidando un perfil técnico y metodológico sólido. Complementariamente, posee conocimientos intermedios en RStudio, lo que le permite implementar procedimientos estadísticos avanzados con precisión.' WHERE autor_id = 13;

UPDATE autor SET breve_resena_biografica = NULL WHERE autor_id = 14;

UPDATE autor SET breve_resena_biografica = 'He participado en investigaciones sobre bienestar psicológico en personal de salud, acoso escolar y competencias socioemocionales en adolescentes. En la actualidad, desarrollo estudios psicométricos sobre instrumentos como la Escala de Conducta Prosocial en Adultos de Lima Metropolitana (Caprara et al.)' WHERE autor_id = 15;

UPDATE autor SET breve_resena_biografica = 'Soy docente e investigadora en la Universidad Tecnológica Empresarial de Guayaquil, donde enseño en las carreras de Ingeniería de Software, Telecomunicaciones y Sistemas de Información. Soy Licenciada en Sistemas de Información, Magíster en Educación Informática y curso una Maestría en Big Data y Ciencia de Datos. Mi trayectoria combina experiencia en desarrollo de sistemas, dirección de proyectos tecnológicos, tutoría de tesis y publicaciones científicas en áreas de inteligencia artificial, IoT y transformación digital, siempre con el compromiso de aportar a la innovación y sostenibilidad educativa y empresarial' WHERE autor_id = 16;
SELECT autor_id, nombre_completo, breve_resena_biografica FROM autor;