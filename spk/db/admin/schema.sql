--
-- Table structure for table `dataset`
--

CREATE TABLE dataset (
  dataset_id int(10) unsigned NOT NULL auto_increment,
  name varchar(20) NOT NULL default '',
  abstract varchar(100) NOT NULL default '',
  archive longblob,
  user_id int(10) unsigned NOT NULL default '0',
  PRIMARY KEY  (dataset_id),
  UNIQUE KEY user_id (user_id,name)
) TYPE=InnoDB;

--
-- Table structure for table `end`
--

CREATE TABLE end (
  end_code char(4) NOT NULL default '',
  end_name char(20) default NULL,
  PRIMARY KEY  (end_code)
) TYPE=MyISAM;

--
-- Dumping data for table `end`
--

INSERT INTO end VALUES ('cerr','Compiler Error');
INSERT INTO end VALUES ('herr','Hard Fault');
INSERT INTO end VALUES ('serr','Software Error');
INSERT INTO end VALUES ('srun','Successful Run');

--
-- Table structure for table `history`
--

CREATE TABLE history (
  history_id int(10) unsigned NOT NULL auto_increment,
  event_time int(10) unsigned NOT NULL default '0',
  state_code char(3) NOT NULL default '',
  job_id int(10) unsigned NOT NULL default '0',
  host varchar(100) NOT NULL default '',
  PRIMARY KEY  (history_id)
) TYPE=InnoDB;

--
-- Table structure for table `job`
--

CREATE TABLE job (
  job_id int(10) unsigned NOT NULL auto_increment,
  user_id int(10) unsigned NOT NULL default '0',
  abstract varchar(100) NOT NULL default '',
  dataset_id int(10) unsigned NOT NULL default '0',
  dataset_version varchar(10) NOT NULL default '',
  model_id int(10) unsigned NOT NULL default '0',
  model_version varchar(10) NOT NULL default '',
  xml_source longblob,
  state_code char(3) NOT NULL default '',
  report longblob,
  start_time int(10) unsigned NOT NULL default '0',
  event_time int(10) unsigned NOT NULL default '0',
  cpp_source longblob,
  end_code varchar(4) default NULL,
  PRIMARY KEY  (job_id)
) TYPE=InnoDB;

--
-- Table structure for table `model`
--

CREATE TABLE model (
  model_id int(10) unsigned NOT NULL auto_increment,
  name varchar(20) NOT NULL default '',
  abstract varchar(100) NOT NULL default '',
  archive longblob,
  user_id int(10) unsigned NOT NULL default '0',
  PRIMARY KEY  (model_id),
  UNIQUE KEY user_id (user_id,name)
) TYPE=InnoDB;

--
-- Table structure for table `state`
--

CREATE TABLE state (
  state_code char(3) NOT NULL default '',
  state_name char(20) default NULL,
  PRIMARY KEY  (state_code)
) TYPE=MyISAM;

--
-- Dumping data for table `state`
--

INSERT INTO state VALUES ('q2c','Queued to compile');
INSERT INTO state VALUES ('cmp','Compiling');
INSERT INTO state VALUES ('q2r','Queued to run');
INSERT INTO state VALUES ('run','Running');
INSERT INTO state VALUES ('end','End');

--
-- Table structure for table `user`
--

CREATE TABLE user (
  user_id int(10) unsigned NOT NULL auto_increment,
  first_name varchar(30) NOT NULL default '',
  surname varchar(40) NOT NULL default '',
  password varchar(32) NOT NULL default '',
  username varchar(20) NOT NULL default '',
  PRIMARY KEY  (user_id),
  UNIQUE KEY username (username)
) TYPE=InnoDB;
