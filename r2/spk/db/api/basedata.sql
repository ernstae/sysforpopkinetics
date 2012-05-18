-- MySQL dump 10.13  Distrib 5.1.61, for redhat-linux-gnu (x86_64)
--
-- Host: localhost    Database: spkdb
-- ------------------------------------------------------
-- Server version	5.1.61

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `class`
--

DROP TABLE IF EXISTS `class`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `class` (
  `class_code` char(2) NOT NULL DEFAULT '',
  `class_name` char(20) DEFAULT NULL,
  `parent_required` tinyint(1) DEFAULT '0',
  PRIMARY KEY (`class_code`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `class`
--

LOCK TABLES `class` WRITE;
/*!40000 ALTER TABLE `class` DISABLE KEYS */;
INSERT INTO `class` VALUES ('al','Approx. Likelihood',0),('le','Likelihood Eval.',1),('so','Simulation Only',0),('id','identifiability',0);
/*!40000 ALTER TABLE `class` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `end`
--

DROP TABLE IF EXISTS `end`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `end` (
  `end_code` char(4) NOT NULL DEFAULT '',
  `end_name` char(20) DEFAULT NULL,
  PRIMARY KEY (`end_code`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `end`
--

LOCK TABLES `end` WRITE;
/*!40000 ALTER TABLE `end` DISABLE KEYS */;
INSERT INTO `end` VALUES ('cerr','Compiler Error'),('herr','Hard Fault'),('serr','Software Error'),('srun','Successful Run'),('abrt','User Abort'),('staf','Opt OK, Stat Failure'),('othe','Unsuccessful Run'),('othf','Unknown Run Failure'),('acce','File Access Error'),('accf','File Access Failure'),('sime','Simulation Error'),('simf','Simulation Failure'),('opte','Optimization Error'),('optf','Optimization Failure'),('stae','Opt OK, Stat Error'),('usre','Input Error'),('usrf','Input Failure'),('deve','Known Prog Error'),('devf','Known Prog Failure'),('pose','Post-opt Error'),('posf','Post-opt Failure'),('spku','SPK Unavailable'),('idee','Model Ident Error'),('idef','Model Ident Failure'),('pvmf','PVM Failure'),('rese','Opt OK, Resid Error'),('resf','Opt OK, Resid Fail'),('optm','Opt Max Iter Error'),('omif','Opt Max Iter Failure'),('mise','Opt Max Iter Sta Err'),('mire','Opt Max Iter Res Err'),('omir','Opt Max Iter Reached');
/*!40000 ALTER TABLE `end` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `method`
--

DROP TABLE IF EXISTS `method`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `method` (
  `method_code` char(2) NOT NULL DEFAULT '',
  `method_name` char(20) DEFAULT NULL,
  `class_code` char(2) NOT NULL DEFAULT '',
  `test_only` tinyint(1) DEFAULT '0',
  PRIMARY KEY (`method_code`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `method`
--

LOCK TABLES `method` WRITE;
/*!40000 ALTER TABLE `method` DISABLE KEYS */;
INSERT INTO `method` VALUES ('fo','First Order','al',0),('eh','Expected Hessian','al',0),('la','Laplace Approx.','al',0),('ml','M.C. Likelihood','le',0),('mc','Markov Chain M.C.','al',0),('gr','Grid Likelihood','le',0),('ad','Adapt Likelihood','le',0),('so','Simulation Only','so',0),('ia','Individual Analysis','al',0),('mi','Miser Likelihood','le',0),('vl','Vegas Likelihood','le',0),('s2','Std. Two-stage','al',0),('i2','Iter. Two-stage','al',0),('g2','Global Two-stage','al',0),('sm','MAP Std. Two-stage','al',0),('im','MAP Iter. Two-stage','al',0),('gm','MAP Global Two-stage','al',0),('an','Analytic Likelihood','le',1),('id','identifiability','id',0),('gn','Grid Nonparam.','al',0),('un','Uniform Nonparam.','al',0);
/*!40000 ALTER TABLE `method` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `state`
--

DROP TABLE IF EXISTS `state`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `state` (
  `state_code` char(4) NOT NULL DEFAULT '',
  `state_name` char(20) DEFAULT NULL,
  PRIMARY KEY (`state_code`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `state`
--

LOCK TABLES `state` WRITE;
/*!40000 ALTER TABLE `state` DISABLE KEYS */;
INSERT INTO `state` VALUES ('q2c','Queued to compile'),('q2ml','Queued to M.L.'),('cmp','Compiling'),('q2r','Queued to run'),('run','Running'),('end','End'),('q2ac','Queued to abort cmp'),('q2ar','Queued to abort run'),('acmp','Aborting compilation'),('arun','Aborting run');
/*!40000 ALTER TABLE `state` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2012-05-18  0:40:37
