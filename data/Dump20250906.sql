-- MySQL dump 10.13  Distrib 8.0.43, for Win64 (x86_64)
--
-- Host: localhost    Database: libro
-- ------------------------------------------------------
-- Server version	8.0.43

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `afiliacion`
--

DROP TABLE IF EXISTS `afiliacion`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `afiliacion` (
  `afiliacion_id` int NOT NULL AUTO_INCREMENT,
  `institucion` text NOT NULL,
  `pais` text,
  PRIMARY KEY (`afiliacion_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `afiliacion`
--

LOCK TABLES `afiliacion` WRITE;
/*!40000 ALTER TABLE `afiliacion` DISABLE KEYS */;
/*!40000 ALTER TABLE `afiliacion` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `autor`
--

DROP TABLE IF EXISTS `autor`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `autor` (
  `autor_id` int NOT NULL AUTO_INCREMENT,
  `nombre_completo` varchar(255) NOT NULL,
  `email` varchar(320) NOT NULL,
  `pais_ciudad` varchar(150) DEFAULT NULL,
  `institucion` varchar(255) DEFAULT NULL,
  `cargo` varchar(150) DEFAULT NULL,
  `orcid` varchar(50) DEFAULT NULL,
  `telefono` varchar(50) DEFAULT NULL,
  `foto` longblob,
  `foto_mime` varchar(100) DEFAULT NULL,
  `foto_nombre` varchar(255) DEFAULT NULL,
  `foto_bytes` int unsigned DEFAULT NULL,
  `foto_fecha` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`autor_id`),
  UNIQUE KEY `email` (`email`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `autor`
--

LOCK TABLES `autor` WRITE;
/*!40000 ALTER TABLE `autor` DISABLE KEYS */;
/*!40000 ALTER TABLE `autor` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `autor_afiliacion`
--

DROP TABLE IF EXISTS `autor_afiliacion`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `autor_afiliacion` (
  `autor_id` int NOT NULL,
  `afiliacion_id` int NOT NULL,
  `es_principal` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`autor_id`,`afiliacion_id`),
  KEY `fk_aa_afili` (`afiliacion_id`),
  CONSTRAINT `fk_aa_afili` FOREIGN KEY (`afiliacion_id`) REFERENCES `afiliacion` (`afiliacion_id`) ON DELETE RESTRICT,
  CONSTRAINT `fk_aa_autor` FOREIGN KEY (`autor_id`) REFERENCES `autor` (`autor_id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `autor_afiliacion`
--

LOCK TABLES `autor_afiliacion` WRITE;
/*!40000 ALTER TABLE `autor_afiliacion` DISABLE KEYS */;
/*!40000 ALTER TABLE `autor_afiliacion` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `capitulo`
--

DROP TABLE IF EXISTS `capitulo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `capitulo` (
  `capitulo_id` int NOT NULL AUTO_INCREMENT,
  `numero` smallint NOT NULL,
  `titulo` text NOT NULL,
  `estado_id` smallint DEFAULT NULL,
  PRIMARY KEY (`capitulo_id`),
  UNIQUE KEY `uq_cap_numero` (`numero`),
  KEY `fk_cap_estado` (`estado_id`),
  CONSTRAINT `fk_cap_estado` FOREIGN KEY (`estado_id`) REFERENCES `estado_capitulo` (`estado_id`)
) ENGINE=InnoDB AUTO_INCREMENT=33 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `capitulo`
--

LOCK TABLES `capitulo` WRITE;
/*!40000 ALTER TABLE `capitulo` DISABLE KEYS */;
INSERT INTO `capitulo` VALUES (1,1,'Conceptos básicos de estadística',NULL),(2,2,'Variables y escalas de medición',NULL),(3,3,'Medidas de tendencia central y dispersión',NULL),(4,4,'Representación gráfica',NULL),(5,5,'Fundamentos del muestreo',NULL),(6,6,'Distribuciones de probabilidad',NULL),(7,7,'Distribución normal y sus aplicaciones',NULL),(8,8,'Estimación de parámetros',NULL),(9,9,'Pruebas de hipótesis: fundamentos',NULL),(10,10,'Pruebas para medias',NULL),(11,11,'ANOVA',NULL),(12,12,'Pruebas no paramétricas',NULL),(13,13,'Correlación',NULL),(14,14,'Regresión lineal simple',NULL),(15,15,'Regresión múltiple',NULL),(16,16,'Diagnóstico y validación de modelos',NULL),(17,17,'Fundamentos del análisis multivariado',NULL),(18,18,'Modelos de ecuaciones estructurales (SEM)',NULL),(19,19,'SmartPLS: uso y aplicaciones',NULL),(20,20,'Interpretación de resultados en PLS-SEM',NULL),(21,21,'Introducción a las series de tiempo',NULL),(22,22,'Métodos de suavizamiento',NULL),(23,23,'Modelos ARIMA y Box–Jenkins',NULL),(24,24,'Aplicaciones prácticas de pronósticos',NULL),(25,25,'Análisis con R',NULL),(26,26,'Análisis con Python',NULL),(27,27,'Análisis con SPSS',NULL),(28,28,'Análisis con Excel',NULL),(29,29,'Aplicaciones en educación superior',NULL),(30,30,'Aplicaciones en negocios y empresas',NULL),(31,31,'Aplicaciones en políticas públicas',NULL),(32,32,'Tendencias futuras de la estadística aplicada',NULL);
/*!40000 ALTER TABLE `capitulo` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `capitulo_autor`
--

DROP TABLE IF EXISTS `capitulo_autor`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `capitulo_autor` (
  `capitulo_id` int NOT NULL,
  `autor_id` int NOT NULL,
  `rol_id` smallint NOT NULL,
  `orden` smallint DEFAULT NULL,
  PRIMARY KEY (`capitulo_id`,`autor_id`,`rol_id`),
  UNIQUE KEY `uq_cap_orden` (`capitulo_id`,`orden`),
  KEY `fk_ca_aut` (`autor_id`),
  KEY `fk_ca_rol` (`rol_id`),
  CONSTRAINT `fk_ca_aut` FOREIGN KEY (`autor_id`) REFERENCES `autor` (`autor_id`) ON DELETE CASCADE,
  CONSTRAINT `fk_ca_cap` FOREIGN KEY (`capitulo_id`) REFERENCES `capitulo` (`capitulo_id`) ON DELETE CASCADE,
  CONSTRAINT `fk_ca_rol` FOREIGN KEY (`rol_id`) REFERENCES `rol` (`rol_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `capitulo_autor`
--

LOCK TABLES `capitulo_autor` WRITE;
/*!40000 ALTER TABLE `capitulo_autor` DISABLE KEYS */;
/*!40000 ALTER TABLE `capitulo_autor` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `entrega`
--

DROP TABLE IF EXISTS `entrega`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `entrega` (
  `entrega_id` int NOT NULL AUTO_INCREMENT,
  `subcapitulo_id` int NOT NULL,
  `version` smallint NOT NULL,
  `fecha` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `url` text,
  PRIMARY KEY (`entrega_id`),
  UNIQUE KEY `uq_entrega` (`subcapitulo_id`,`version`),
  CONSTRAINT `fk_ent_sub` FOREIGN KEY (`subcapitulo_id`) REFERENCES `subcapitulo` (`subcapitulo_id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `entrega`
--

LOCK TABLES `entrega` WRITE;
/*!40000 ALTER TABLE `entrega` DISABLE KEYS */;
/*!40000 ALTER TABLE `entrega` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `estado_capitulo`
--

DROP TABLE IF EXISTS `estado_capitulo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `estado_capitulo` (
  `estado_id` smallint NOT NULL,
  `nombre` varchar(30) NOT NULL,
  PRIMARY KEY (`estado_id`),
  UNIQUE KEY `nombre` (`nombre`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `estado_capitulo`
--

LOCK TABLES `estado_capitulo` WRITE;
/*!40000 ALTER TABLE `estado_capitulo` DISABLE KEYS */;
INSERT INTO `estado_capitulo` VALUES (4,'Aprobado'),(1,'Asignado'),(3,'En revisión'),(2,'Enviado'),(5,'Maquetado');
/*!40000 ALTER TABLE `estado_capitulo` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `estado_subcapitulo`
--

DROP TABLE IF EXISTS `estado_subcapitulo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `estado_subcapitulo` (
  `estado_id` smallint NOT NULL,
  `nombre` varchar(30) NOT NULL,
  PRIMARY KEY (`estado_id`),
  UNIQUE KEY `nombre` (`nombre`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `estado_subcapitulo`
--

LOCK TABLES `estado_subcapitulo` WRITE;
/*!40000 ALTER TABLE `estado_subcapitulo` DISABLE KEYS */;
INSERT INTO `estado_subcapitulo` VALUES (4,'Aprobado'),(1,'Asignado'),(3,'En revisión'),(2,'Enviado'),(5,'Maquetado');
/*!40000 ALTER TABLE `estado_subcapitulo` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `fase`
--

DROP TABLE IF EXISTS `fase`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `fase` (
  `fase_id` tinyint NOT NULL,
  `nombre` varchar(80) NOT NULL,
  `fecha_inicio` date NOT NULL,
  `fecha_fin` date NOT NULL,
  PRIMARY KEY (`fase_id`),
  UNIQUE KEY `nombre` (`nombre`),
  CONSTRAINT `fase_chk_1` CHECK ((`fecha_fin` >= `fecha_inicio`))
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `fase`
--

LOCK TABLES `fase` WRITE;
/*!40000 ALTER TABLE `fase` DISABLE KEYS */;
INSERT INTO `fase` VALUES (1,'Fase 1 – Arranque y coordinación','2025-09-01','2025-09-03'),(2,'Fase 2 – Redacción','2025-09-04','2025-10-30'),(3,'Fase 3 – Revisión y corrección de estilo','2025-11-01','2025-11-07'),(4,'Fase 4 – Entrega de borrador (editorial)','2025-11-08','2025-12-15'),(5,'Fase 5 – Diagramación','2026-01-01','2026-02-28');
/*!40000 ALTER TABLE `fase` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `revision`
--

DROP TABLE IF EXISTS `revision`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `revision` (
  `revision_id` int NOT NULL AUTO_INCREMENT,
  `entrega_id` int NOT NULL,
  `revisor_id` int DEFAULT NULL,
  `rubrica` json DEFAULT NULL,
  `decision` varchar(20) DEFAULT NULL,
  PRIMARY KEY (`revision_id`),
  KEY `fk_rev_ent` (`entrega_id`),
  KEY `fk_rev_aut` (`revisor_id`),
  CONSTRAINT `fk_rev_aut` FOREIGN KEY (`revisor_id`) REFERENCES `autor` (`autor_id`) ON DELETE SET NULL,
  CONSTRAINT `fk_rev_ent` FOREIGN KEY (`entrega_id`) REFERENCES `entrega` (`entrega_id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `revision`
--

LOCK TABLES `revision` WRITE;
/*!40000 ALTER TABLE `revision` DISABLE KEYS */;
/*!40000 ALTER TABLE `revision` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `rol`
--

DROP TABLE IF EXISTS `rol`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `rol` (
  `rol_id` smallint NOT NULL,
  `nombre` varchar(30) NOT NULL,
  PRIMARY KEY (`rol_id`),
  UNIQUE KEY `nombre` (`nombre`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `rol`
--

LOCK TABLES `rol` WRITE;
/*!40000 ALTER TABLE `rol` DISABLE KEYS */;
INSERT INTO `rol` VALUES (1,'Autor principal'),(2,'Coautor'),(4,'Coordinador'),(3,'Revisor');
/*!40000 ALTER TABLE `rol` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `subcapitulo`
--

DROP TABLE IF EXISTS `subcapitulo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `subcapitulo` (
  `subcapitulo_id` int NOT NULL AUTO_INCREMENT,
  `capitulo_id` int NOT NULL,
  `numero` smallint NOT NULL,
  `titulo` text NOT NULL,
  `estado_id` smallint DEFAULT NULL,
  PRIMARY KEY (`subcapitulo_id`),
  UNIQUE KEY `uq_sub_num` (`capitulo_id`,`numero`),
  KEY `fk_sub_estado` (`estado_id`),
  CONSTRAINT `fk_sub_cap` FOREIGN KEY (`capitulo_id`) REFERENCES `capitulo` (`capitulo_id`) ON DELETE CASCADE,
  CONSTRAINT `fk_sub_estado` FOREIGN KEY (`estado_id`) REFERENCES `estado_subcapitulo` (`estado_id`)
) ENGINE=InnoDB AUTO_INCREMENT=128 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `subcapitulo`
--

LOCK TABLES `subcapitulo` WRITE;
/*!40000 ALTER TABLE `subcapitulo` DISABLE KEYS */;
INSERT INTO `subcapitulo` VALUES (1,1,3,'Conclusiones y referencias',NULL),(2,1,2,'Desarrollo del contenido principal',NULL),(3,1,1,'Introducción y contexto teórico',NULL),(4,2,3,'Conclusiones y referencias',NULL),(5,2,2,'Desarrollo del contenido principal',NULL),(6,2,1,'Introducción y contexto teórico',NULL),(7,3,3,'Conclusiones y referencias',NULL),(8,3,2,'Desarrollo del contenido principal',NULL),(9,3,1,'Introducción y contexto teórico',NULL),(10,4,3,'Conclusiones y referencias',NULL),(11,4,2,'Desarrollo del contenido principal',NULL),(12,4,1,'Introducción y contexto teórico',NULL),(13,5,3,'Conclusiones y referencias',NULL),(14,5,2,'Desarrollo del contenido principal',NULL),(15,5,1,'Introducción y contexto teórico',NULL),(16,6,3,'Conclusiones y referencias',NULL),(17,6,2,'Desarrollo del contenido principal',NULL),(18,6,1,'Introducción y contexto teórico',NULL),(19,7,3,'Conclusiones y referencias',NULL),(20,7,2,'Desarrollo del contenido principal',NULL),(21,7,1,'Introducción y contexto teórico',NULL),(22,8,3,'Conclusiones y referencias',NULL),(23,8,2,'Desarrollo del contenido principal',NULL),(24,8,1,'Introducción y contexto teórico',NULL),(25,9,3,'Conclusiones y referencias',NULL),(26,9,2,'Desarrollo del contenido principal',NULL),(27,9,1,'Introducción y contexto teórico',NULL),(28,10,3,'Conclusiones y referencias',NULL),(29,10,2,'Desarrollo del contenido principal',NULL),(30,10,1,'Introducción y contexto teórico',NULL),(31,11,3,'Conclusiones y referencias',NULL),(32,11,2,'Desarrollo del contenido principal',NULL),(33,11,1,'Introducción y contexto teórico',NULL),(34,12,3,'Conclusiones y referencias',NULL),(35,12,2,'Desarrollo del contenido principal',NULL),(36,12,1,'Introducción y contexto teórico',NULL),(37,13,3,'Conclusiones y referencias',NULL),(38,13,2,'Desarrollo del contenido principal',NULL),(39,13,1,'Introducción y contexto teórico',NULL),(40,14,3,'Conclusiones y referencias',NULL),(41,14,2,'Desarrollo del contenido principal',NULL),(42,14,1,'Introducción y contexto teórico',NULL),(43,15,3,'Conclusiones y referencias',NULL),(44,15,2,'Desarrollo del contenido principal',NULL),(45,15,1,'Introducción y contexto teórico',NULL),(46,16,3,'Conclusiones y referencias',NULL),(47,16,2,'Desarrollo del contenido principal',NULL),(48,16,1,'Introducción y contexto teórico',NULL),(49,17,3,'Conclusiones y referencias',NULL),(50,17,2,'Desarrollo del contenido principal',NULL),(51,17,1,'Introducción y contexto teórico',NULL),(52,18,3,'Conclusiones y referencias',NULL),(53,18,2,'Desarrollo del contenido principal',NULL),(54,18,1,'Introducción y contexto teórico',NULL),(55,19,3,'Conclusiones y referencias',NULL),(56,19,2,'Desarrollo del contenido principal',NULL),(57,19,1,'Introducción y contexto teórico',NULL),(58,20,3,'Conclusiones y referencias',NULL),(59,20,2,'Desarrollo del contenido principal',NULL),(60,20,1,'Introducción y contexto teórico',NULL),(61,21,3,'Conclusiones y referencias',NULL),(62,21,2,'Desarrollo del contenido principal',NULL),(63,21,1,'Introducción y contexto teórico',NULL),(64,22,3,'Conclusiones y referencias',NULL),(65,22,2,'Desarrollo del contenido principal',NULL),(66,22,1,'Introducción y contexto teórico',NULL),(67,23,3,'Conclusiones y referencias',NULL),(68,23,2,'Desarrollo del contenido principal',NULL),(69,23,1,'Introducción y contexto teórico',NULL),(70,24,3,'Conclusiones y referencias',NULL),(71,24,2,'Desarrollo del contenido principal',NULL),(72,24,1,'Introducción y contexto teórico',NULL),(73,25,3,'Conclusiones y referencias',NULL),(74,25,2,'Desarrollo del contenido principal',NULL),(75,25,1,'Introducción y contexto teórico',NULL),(76,26,3,'Conclusiones y referencias',NULL),(77,26,2,'Desarrollo del contenido principal',NULL),(78,26,1,'Introducción y contexto teórico',NULL),(79,27,3,'Conclusiones y referencias',NULL),(80,27,2,'Desarrollo del contenido principal',NULL),(81,27,1,'Introducción y contexto teórico',NULL),(82,28,3,'Conclusiones y referencias',NULL),(83,28,2,'Desarrollo del contenido principal',NULL),(84,28,1,'Introducción y contexto teórico',NULL),(85,29,3,'Conclusiones y referencias',NULL),(86,29,2,'Desarrollo del contenido principal',NULL),(87,29,1,'Introducción y contexto teórico',NULL),(88,30,3,'Conclusiones y referencias',NULL),(89,30,2,'Desarrollo del contenido principal',NULL),(90,30,1,'Introducción y contexto teórico',NULL),(91,31,3,'Conclusiones y referencias',NULL),(92,31,2,'Desarrollo del contenido principal',NULL),(93,31,1,'Introducción y contexto teórico',NULL),(94,32,3,'Conclusiones y referencias',NULL),(95,32,2,'Desarrollo del contenido principal',NULL),(96,32,1,'Introducción y contexto teórico',NULL);
/*!40000 ALTER TABLE `subcapitulo` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `subcapitulo_autor`
--

DROP TABLE IF EXISTS `subcapitulo_autor`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `subcapitulo_autor` (
  `subcapitulo_id` int NOT NULL,
  `autor_id` int NOT NULL,
  `rol_id` smallint NOT NULL,
  `orden` smallint DEFAULT NULL,
  PRIMARY KEY (`subcapitulo_id`,`autor_id`,`rol_id`),
  UNIQUE KEY `uq_sub_orden` (`subcapitulo_id`,`orden`),
  KEY `fk_sa_aut` (`autor_id`),
  KEY `fk_sa_rol` (`rol_id`),
  CONSTRAINT `fk_sa_aut` FOREIGN KEY (`autor_id`) REFERENCES `autor` (`autor_id`) ON DELETE CASCADE,
  CONSTRAINT `fk_sa_rol` FOREIGN KEY (`rol_id`) REFERENCES `rol` (`rol_id`),
  CONSTRAINT `fk_sa_sub` FOREIGN KEY (`subcapitulo_id`) REFERENCES `subcapitulo` (`subcapitulo_id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `subcapitulo_autor`
--

LOCK TABLES `subcapitulo_autor` WRITE;
/*!40000 ALTER TABLE `subcapitulo_autor` DISABLE KEYS */;
/*!40000 ALTER TABLE `subcapitulo_autor` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Temporary view structure for view `vw_autores_subcapitulo`
--

DROP TABLE IF EXISTS `vw_autores_subcapitulo`;
/*!50001 DROP VIEW IF EXISTS `vw_autores_subcapitulo`*/;
SET @saved_cs_client     = @@character_set_client;
/*!50503 SET character_set_client = utf8mb4 */;
/*!50001 CREATE VIEW `vw_autores_subcapitulo` AS SELECT 
 1 AS `cap_numero`,
 1 AS `sub_numero`,
 1 AS `cap_titulo`,
 1 AS `sub_titulo`,
 1 AS `nombre_completo`,
 1 AS `rol`,
 1 AS `orden`*/;
SET character_set_client = @saved_cs_client;

--
-- Temporary view structure for view `vw_avance_capitulo`
--

DROP TABLE IF EXISTS `vw_avance_capitulo`;
/*!50001 DROP VIEW IF EXISTS `vw_avance_capitulo`*/;
SET @saved_cs_client     = @@character_set_client;
/*!50503 SET character_set_client = utf8mb4 */;
/*!50001 CREATE VIEW `vw_avance_capitulo` AS SELECT 
 1 AS `capitulo_id`,
 1 AS `numero`,
 1 AS `titulo`,
 1 AS `sub_total`,
 1 AS `sub_maquetados`,
 1 AS `sub_aprobados`*/;
SET character_set_client = @saved_cs_client;

--
-- Temporary view structure for view `vw_portada_capitulo`
--

DROP TABLE IF EXISTS `vw_portada_capitulo`;
/*!50001 DROP VIEW IF EXISTS `vw_portada_capitulo`*/;
SET @saved_cs_client     = @@character_set_client;
/*!50503 SET character_set_client = utf8mb4 */;
/*!50001 CREATE VIEW `vw_portada_capitulo` AS SELECT 
 1 AS `cap_numero`,
 1 AS `cap_titulo`,
 1 AS `nombre_completo`,
 1 AS `rol`,
 1 AS `orden`*/;
SET character_set_client = @saved_cs_client;

--
-- Final view structure for view `vw_autores_subcapitulo`
--

/*!50001 DROP VIEW IF EXISTS `vw_autores_subcapitulo`*/;
/*!50001 SET @saved_cs_client          = @@character_set_client */;
/*!50001 SET @saved_cs_results         = @@character_set_results */;
/*!50001 SET @saved_col_connection     = @@collation_connection */;
/*!50001 SET character_set_client      = utf8mb4 */;
/*!50001 SET character_set_results     = utf8mb4 */;
/*!50001 SET collation_connection      = utf8mb4_0900_ai_ci */;
/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=`root`@`localhost` SQL SECURITY DEFINER */
/*!50001 VIEW `vw_autores_subcapitulo` AS select `c`.`numero` AS `cap_numero`,`sc`.`numero` AS `sub_numero`,`c`.`titulo` AS `cap_titulo`,`sc`.`titulo` AS `sub_titulo`,`a`.`nombre_completo` AS `nombre_completo`,`r`.`nombre` AS `rol`,`sca`.`orden` AS `orden` from ((((`subcapitulo` `sc` join `capitulo` `c` on((`c`.`capitulo_id` = `sc`.`capitulo_id`))) join `subcapitulo_autor` `sca` on((`sca`.`subcapitulo_id` = `sc`.`subcapitulo_id`))) join `autor` `a` on((`a`.`autor_id` = `sca`.`autor_id`))) join `rol` `r` on((`r`.`rol_id` = `sca`.`rol_id`))) */;
/*!50001 SET character_set_client      = @saved_cs_client */;
/*!50001 SET character_set_results     = @saved_cs_results */;
/*!50001 SET collation_connection      = @saved_col_connection */;

--
-- Final view structure for view `vw_avance_capitulo`
--

/*!50001 DROP VIEW IF EXISTS `vw_avance_capitulo`*/;
/*!50001 SET @saved_cs_client          = @@character_set_client */;
/*!50001 SET @saved_cs_results         = @@character_set_results */;
/*!50001 SET @saved_col_connection     = @@collation_connection */;
/*!50001 SET character_set_client      = utf8mb4 */;
/*!50001 SET character_set_results     = utf8mb4 */;
/*!50001 SET collation_connection      = utf8mb4_0900_ai_ci */;
/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=`root`@`localhost` SQL SECURITY DEFINER */
/*!50001 VIEW `vw_avance_capitulo` AS select `c`.`capitulo_id` AS `capitulo_id`,`c`.`numero` AS `numero`,`c`.`titulo` AS `titulo`,count(`sc`.`subcapitulo_id`) AS `sub_total`,sum((`sc`.`estado_id` = 5)) AS `sub_maquetados`,sum((`sc`.`estado_id` = 4)) AS `sub_aprobados` from (`capitulo` `c` left join `subcapitulo` `sc` on((`sc`.`capitulo_id` = `c`.`capitulo_id`))) group by `c`.`capitulo_id`,`c`.`numero`,`c`.`titulo` order by `c`.`numero` */;
/*!50001 SET character_set_client      = @saved_cs_client */;
/*!50001 SET character_set_results     = @saved_cs_results */;
/*!50001 SET collation_connection      = @saved_col_connection */;

--
-- Final view structure for view `vw_portada_capitulo`
--

/*!50001 DROP VIEW IF EXISTS `vw_portada_capitulo`*/;
/*!50001 SET @saved_cs_client          = @@character_set_client */;
/*!50001 SET @saved_cs_results         = @@character_set_results */;
/*!50001 SET @saved_col_connection     = @@collation_connection */;
/*!50001 SET character_set_client      = utf8mb4 */;
/*!50001 SET character_set_results     = utf8mb4 */;
/*!50001 SET collation_connection      = utf8mb4_0900_ai_ci */;
/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=`root`@`localhost` SQL SECURITY DEFINER */
/*!50001 VIEW `vw_portada_capitulo` AS select `c`.`numero` AS `cap_numero`,`c`.`titulo` AS `cap_titulo`,`a`.`nombre_completo` AS `nombre_completo`,`r`.`nombre` AS `rol`,`ca`.`orden` AS `orden` from (((`capitulo` `c` join `capitulo_autor` `ca` on((`ca`.`capitulo_id` = `c`.`capitulo_id`))) join `autor` `a` on((`a`.`autor_id` = `ca`.`autor_id`))) join `rol` `r` on((`r`.`rol_id` = `ca`.`rol_id`))) order by `c`.`numero`,coalesce(`ca`.`orden`,9999),`r`.`nombre` */;
/*!50001 SET character_set_client      = @saved_cs_client */;
/*!50001 SET character_set_results     = @saved_cs_results */;
/*!50001 SET collation_connection      = @saved_col_connection */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2025-09-06  5:39:17
