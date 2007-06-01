drop database SGID;
create database SGID;

use SGID;

grant all on SGID.* to 'sgid'@localhost IDENTIFIED BY 'sgid_web';
grant all on SGID.* to 'sgid'@'%' IDENTIFIED BY 'sgid_web';

create table if not exists job (
	id		bigint 	NOT NULL AUTO_INCREMENT,
	seed		bigint NOT NULL,
	equations	longblob,
	email_address	varchar(128) NOT NULL,
	xml_input	longblob,
	result_xml	longblob,
	state_code	ENUM('queue','run','end') DEFAULT 'queue',
	end_code	ENUM('srun','serr','herr'),
	ts_submit	TIMESTAMP DEFAULT 0,
	ts_modify	TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (id)
);

create table if not exists job_history (
	job_id		bigint,
	state_code	ENUM('queue','run','end'),
	ts_modify	TIMESTAMP,
	FOREIGN KEY (job_id) REFERENCES job(id) ON DELETE CASCADE
);

create table if not exists error_reports (
	job_id		bigint,
	error_message	varchar(4000),
	FOREIGN KEY (job_id) REFERENCES job(id) ON DELETE CASCADE
);	

DELIMITER $$
CREATE TRIGGER queue_job AFTER INSERT ON job FOR EACH ROW
BEGIN
 INSERT INTO job_history (job_id, state_code) VALUES ( NEW.id, 'queue' );
END$$

-- CREATE UPDATE TRIGGER
CREATE TRIGGER update_jobhistory AFTER UPDATE ON job FOR EACH ROW 
BEGIN
  INSERT INTO job_history (job_id, state_code) VALUES ( OLD.id, NEW.state_code );
END;
$$

DELIMITER ;

