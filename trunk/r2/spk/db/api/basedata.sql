
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

LOCK TABLES `class` WRITE;
/*!40000 ALTER TABLE `class` DISABLE KEYS */;
INSERT INTO `class` VALUES ('al','Approx. Likelihood',0),('le','Likelihood Eval.',1),('so','Simulation Only',0),('id','identifiability',0);
/*!40000 ALTER TABLE `class` ENABLE KEYS */;
UNLOCK TABLES;

LOCK TABLES `end` WRITE;
/*!40000 ALTER TABLE `end` DISABLE KEYS */;
INSERT INTO `end` VALUES ('cerr','Compiler Error'),('herr','Hard Fault'),('serr','Software Error'),('srun','Successful Run'),('abrt','User Abort'),('staf','Opt OK, Stat Failure'),('othe','Unsuccessful Run'),('othf','Unknown Run Failure'),('acce','File Access Error'),('accf','File Access Failure'),('sime','Simulation Error'),('simf','Simulation Failure'),('opte','Optimization Error'),('optf','Optimization Failure'),('stae','Opt OK, Stat Error'),('usre','Input Error'),('usrf','Input Failure'),('deve','Known Prog Error'),('devf','Known Prog Failure'),('pose','Post-opt Error'),('posf','Post-opt Failure'),('spku','SPK Unavailable'),('idee','Model Ident Error'),('idef','Model Ident Failure'),('pvmf','PVM Failure'),('rese','Opt OK, Resid Error'),('resf','Opt OK, Resid Fail'),('optm','Opt Max Iter Error'),('omif','Opt Max Iter Failure'),('mise','Opt Max Iter Sta Err'),('mire','Opt Max Iter Res Err'),('omir','Opt Max Iter Reached');
/*!40000 ALTER TABLE `end` ENABLE KEYS */;
UNLOCK TABLES;

LOCK TABLES `method` WRITE;
/*!40000 ALTER TABLE `method` DISABLE KEYS */;
INSERT INTO `method` VALUES ('fo','First Order','al',0),('eh','Expected Hessian','al',0),('la','Laplace Approx.','al',0),('ml','M.C. Likelihood','le',0),('mc','Markov Chain M.C.','al',0),('gr','Grid Likelihood','le',0),('ad','Adapt Likelihood','le',0),('so','Simulation Only','so',0),('ia','Individual Analysis','al',0),('mi','Miser Likelihood','le',0),('vl','Vegas Likelihood','le',0),('s2','Std. Two-stage','al',0),('i2','Iter. Two-stage','al',0),('g2','Global Two-stage','al',0),('sm','MAP Std. Two-stage','al',0),('im','MAP Iter. Two-stage','al',0),('gm','MAP Global Two-stage','al',0),('an','Analytic Likelihood','le',1),('id','identifiability','id',0),('gn','Grid Nonparam.','al',0),('un','Uniform Nonparam.','al',0);
/*!40000 ALTER TABLE `method` ENABLE KEYS */;
UNLOCK TABLES;

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

