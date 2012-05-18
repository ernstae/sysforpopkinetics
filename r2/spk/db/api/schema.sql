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
-- Table structure for table `dataset`
--

DROP TABLE IF EXISTS `dataset`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `dataset` (
  `dataset_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) NOT NULL DEFAULT '',
  `abstract` varchar(100) NOT NULL,
  `archive` longblob,
  `user_id` int(10) unsigned NOT NULL DEFAULT '0',
  `n_version` int(10) unsigned NOT NULL DEFAULT '1',
  `reversion_time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`dataset_id`),
  UNIQUE KEY `user_id` (`user_id`,`name`)
) ENGINE=MyISAM AUTO_INCREMENT=4446 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `email`
--

DROP TABLE IF EXISTS `email`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `email` (
  `email_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `send_time` datetime NOT NULL,
  `sender` varchar(100) NOT NULL,
  `receiver` varchar(10) NOT NULL,
  `subject` varchar(100) NOT NULL DEFAULT '',
  `message` text,
  PRIMARY KEY (`email_id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

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
-- Table structure for table `folder`
--

DROP TABLE IF EXISTS `folder`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `folder` (
  `folder_id` int(10) unsigned NOT NULL DEFAULT '0',
  `name` varchar(40) NOT NULL DEFAULT '',
  `parent` int(10) unsigned NOT NULL DEFAULT '0',
  `user_id` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`user_id`,`folder_id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `history`
--

DROP TABLE IF EXISTS `history`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `history` (
  `history_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `event_time` int(10) unsigned NOT NULL DEFAULT '0',
  `state_code` varchar(4) NOT NULL DEFAULT '',
  `job_id` int(10) unsigned NOT NULL DEFAULT '0',
  `host` varchar(100) NOT NULL DEFAULT '',
  PRIMARY KEY (`history_id`),
  KEY `idx_history_job_id` (`job_id`)
) ENGINE=MyISAM AUTO_INCREMENT=70616 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `job`
--

DROP TABLE IF EXISTS `job`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `job` (
  `job_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `user_id` int(10) unsigned NOT NULL DEFAULT '0',
  `abstract` varchar(100) NOT NULL,
  `dataset_id` int(10) unsigned NOT NULL DEFAULT '0',
  `dataset_version` varchar(10) NOT NULL DEFAULT '',
  `model_id` int(10) unsigned NOT NULL DEFAULT '0',
  `model_version` varchar(10) NOT NULL DEFAULT '',
  `xml_source` longblob,
  `state_code` varchar(4) NOT NULL DEFAULT '',
  `report` longblob,
  `start_time` int(10) unsigned NOT NULL DEFAULT '0',
  `event_time` int(10) unsigned NOT NULL DEFAULT '0',
  `cpp_source` longblob,
  `end_code` varchar(4) DEFAULT NULL,
  `method_code` char(2) DEFAULT NULL,
  `parent` int(10) unsigned DEFAULT '0',
  `checkpoint` longblob,
  `mail` tinyint(1) DEFAULT '0',
  `share_with` int(10) unsigned NOT NULL DEFAULT '0',
  `parallel` int(10) NOT NULL DEFAULT '0',
  `folder_id` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`job_id`),
  KEY `idx_job_share_with` (`share_with`),
  KEY `idx_job_user_id` (`user_id`) USING HASH
) ENGINE=MyISAM AUTO_INCREMENT=14002 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

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
-- Table structure for table `model`
--

DROP TABLE IF EXISTS `model`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `model` (
  `model_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) NOT NULL DEFAULT '',
  `abstract` varchar(100) NOT NULL,
  `archive` longblob,
  `user_id` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`model_id`),
  UNIQUE KEY `user_id` (`user_id`,`name`)
) ENGINE=MyISAM AUTO_INCREMENT=6557 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

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
-- Table structure for table `team`
--

DROP TABLE IF EXISTS `team`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `team` (
  `team_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `team_name` varchar(20) NOT NULL DEFAULT '',
  PRIMARY KEY (`team_id`),
  UNIQUE KEY `team_name` (`team_name`)
) ENGINE=MyISAM AUTO_INCREMENT=6 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user` (
  `user_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `first_name` varchar(30) NOT NULL DEFAULT '',
  `surname` varchar(40) NOT NULL DEFAULT '',
  `password` varchar(32) NOT NULL DEFAULT '',
  `username` varchar(20) NOT NULL DEFAULT '',
  `company` varchar(40) NOT NULL DEFAULT '',
  `country` varchar(20) NOT NULL DEFAULT '',
  `state` varchar(20) NOT NULL DEFAULT '',
  `email` varchar(40) DEFAULT NULL,
  `test` tinyint(1) DEFAULT '0',
  `dev` tinyint(1) DEFAULT '0',
  `team_id` int(10) unsigned NOT NULL DEFAULT '0',
  `register_time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `contact` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`user_id`),
  UNIQUE KEY `username` (`username`)
) ENGINE=MyISAM AUTO_INCREMENT=585 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping routines for database 'spkdb'
--
/*!50003 DROP PROCEDURE IF EXISTS `folder_tree` */;
/*!50003 SET @saved_cs_client      = @@character_set_client */ ;
/*!50003 SET @saved_cs_results     = @@character_set_results */ ;
/*!50003 SET @saved_col_connection = @@collation_connection */ ;
/*!50003 SET character_set_client  = latin1 */ ;
/*!50003 SET character_set_results = latin1 */ ;
/*!50003 SET collation_connection  = latin1_swedish_ci */ ;
/*!50003 SET @saved_sql_mode       = @@sql_mode */ ;
/*!50003 SET sql_mode              = '' */ ;
DELIMITER ;;
/*!50003 CREATE*/ /*!50020 DEFINER=`root`@`localhost`*/ /*!50003 PROCEDURE `folder_tree`(userId INT,parentId INT,INOUT tree TEXT)
BEGIN
    DECLARE folderId INT;
    DECLARE folderName TEXT;
    DECLARE done INT DEFAULT 0;
    DECLARE cur CURSOR FOR
      SELECT folder_id,name FROM folder WHERE user_id=userId AND parent=parentId;
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET done=1;
    
    OPEN cur;
    folder_loop:LOOP
        
        FETCH cur INTO folderId,folderName;
        IF done=1 THEN
            LEAVE folder_loop;
        END IF;
        SET tree=CONCAT(tree,'<n',folderId,' name="',folderName,'">');
        CALL folder_tree(userId,folderId,tree);
        SET tree=CONCAT(tree,'</n',folderId,'>');
    END LOOP folder_loop;
    CLOSE cur;
END */;;
DELIMITER ;
/*!50003 SET sql_mode              = @saved_sql_mode */ ;
/*!50003 SET character_set_client  = @saved_cs_client */ ;
/*!50003 SET character_set_results = @saved_cs_results */ ;
/*!50003 SET collation_connection  = @saved_col_connection */ ;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2012-05-18  0:35:10
