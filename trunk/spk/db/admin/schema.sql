CREATE TABLE dataset (
  dataset_id int(10) unsigned NOT NULL auto_increment,
  name varchar(20) NOT NULL,
  abstract varchar(100) NOT NULL default '',
  archive longblob,
  user_id int(10) unsigned NOT NULL,
  PRIMARY KEY  (dataset_id),
  UNIQUE KEY user_id (user_id,name)
) TYPE=InnoDB;


CREATE TABLE end (
  end_code char(4) NOT NULL default '',
  end_name char(20) default NULL,
  PRIMARY KEY  (end_code)
) TYPE=MyISAM;

INSERT INTO end VALUES ('cerr','Compiler Error');
INSERT INTO end VALUES ('srun','Successful Run');


CREATE TABLE history (
  history_id int(10) unsigned NOT NULL auto_increment,
  event_time int(10) unsigned NOT NULL,
  state_code char(3) NOT NULL,
  job_id int(10) unsigned NOT NULL,
  PRIMARY KEY  (history_id)
) TYPE=InnoDB;


CREATE TABLE job (
  job_id int(10) unsigned NOT NULL auto_increment,
  user_id int(10) unsigned NOT NULL,
  abstract varchar(100) NOT NULL default '',
  dataset_id int(10) unsigned NOT NULL,
  dataset_version varchar(10) NOT NULL default '',
  model_id int(10) unsigned NOT NULL,
  model_version varchar(10) NOT NULL default '',
  xml_source longblob,
  state_code char(3) NOT NULL,
  report longblob,
  start_time int(10) unsigned NOT NULL,
  event_time int(10) unsigned NOT NULL,
  cpp_source longblob,
  end_code varchar(4) default NULL,
  PRIMARY KEY  (job_id)
) TYPE=InnoDB;


CREATE TABLE model (
  model_id int(10) unsigned NOT NULL auto_increment,
  name varchar(20) NOT NULL,
  abstract varchar(100) NOT NULL default '',
  archive longblob,
  user_id int(10) unsigned NOT NULL,
  PRIMARY KEY  (model_id),
  UNIQUE KEY user_id (user_id,name)
) TYPE=InnoDB;


CREATE TABLE state (
  state_code char(3) NOT NULL default '',
  state_name char(20) default NULL,
  PRIMARY KEY  (state_code)
) TYPE=MyISAM;

INSERT INTO state VALUES ('q2c','Queued to compile');
INSERT INTO state VALUES ('cmp','Compiling');
INSERT INTO state VALUES ('q2r','Queued to run');
INSERT INTO state VALUES ('run','Running');
INSERT INTO state VALUES ('end','End');


CREATE TABLE user (
  user_id int(10) unsigned NOT NULL auto_increment,
  first_name varchar(30) NOT NULL default '',
  surname varchar(40) NOT NULL default '',
  password varchar(32) NOT NULL,
  username varchar(20) NOT NULL,
  PRIMARY KEY  (user_id),
  UNIQUE KEY username (username)
) TYPE=InnoDB;

