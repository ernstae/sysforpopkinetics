use spkdb;

-- MySQL dump 9.08
--
-- Host: localhost    Database: spktest
---------------------------------------------------------
-- Server version	4.0.14-standard-log

--
-- Table structure for table 'end'
--

CREATE TABLE end (
  end_code char(4) NOT NULL default '',
  end_name char(20) default NULL,
  PRIMARY KEY  (end_code)
) TYPE=MyISAM;

--
-- Dumping data for table 'end'
--

INSERT INTO end VALUES ('cerr','Compiler Error');
INSERT INTO end VALUES ('srun','Successful Run');

--
-- Table structure for table 'history'
--

CREATE TABLE history (
  history_id int(10) unsigned NOT NULL auto_increment,
  time timestamp(14) NOT NULL,
  state_code char(3) default NULL,
  job_id int(10) unsigned default NULL,
  PRIMARY KEY  (history_id)
) TYPE=InnoDB;

--
-- Dumping data for table 'history'
--


--
-- Table structure for table 'job'
--

CREATE TABLE job (
  job_id int(10) unsigned NOT NULL auto_increment,
  user_id int(10) NOT NULL default '0',
  model_id int(10) NOT NULL default '0',
  version varchar(10) NOT NULL default '',
  xml_source longblob,
  xml_data longblob,
  state_code char(3) default NULL,
  report longblob,
  time timestamp(14) NOT NULL,
  cpp_source longblob,
  end_code varchar(4) default NULL,
  PRIMARY KEY  (job_id)
) TYPE=InnoDB;

--
-- Dumping data for table 'job'
--


--
-- Table structure for table 'model'
--

CREATE TABLE model (
  model_id int(10) unsigned NOT NULL auto_increment,
  time timestamp(14) NOT NULL,
  name varchar(20) default NULL,
  abstract varchar(30) default NULL,
  archive longblob,
  user_id int(10) unsigned NOT NULL default '0',
  PRIMARY KEY  (model_id),
  UNIQUE KEY user_id (user_id,name)
) TYPE=InnoDB;

--
-- Dumping data for table 'model'
--


--
-- Table structure for table 'state'
--

CREATE TABLE state (
  state_code char(3) NOT NULL default '',
  state_name char(20) default NULL,
  PRIMARY KEY  (state_code)
) TYPE=MyISAM;

--
-- Dumping data for table 'state'
--

INSERT INTO state VALUES ('q2c','Queued to compile');
INSERT INTO state VALUES ('cmp','Compiling');
INSERT INTO state VALUES ('q2r','Queued to run');
INSERT INTO state VALUES ('run','Running');
INSERT INTO state VALUES ('end','End');

--
-- Table structure for table 'user'
--

CREATE TABLE user (
  user_id int(10) unsigned NOT NULL auto_increment,
  first_name varchar(30) default NULL,
  surname varchar(40) default NULL,
  password varchar(32) NOT NULL default '',
  username varchar(20) NOT NULL default '',
  PRIMARY KEY  (user_id),
  UNIQUE KEY username (username)
) TYPE=InnoDB;

--
-- Dumping data for table 'user'
--


