-- MySQL dump 9.10
--
-- Host: localhost    Database: spktest
-- ------------------------------------------------------
-- Server version	4.0.17-standard

--
-- Dumping data for table `dataset`
--


--
-- Dumping data for table `end`
--

INSERT INTO end (end_code, end_name) VALUES ('cerr','Compiler Error');
INSERT INTO end (end_code, end_name) VALUES ('herr','Hard Fault');
INSERT INTO end (end_code, end_name) VALUES ('serr','Software Error');
INSERT INTO end (end_code, end_name) VALUES ('srun','Successful Run');

--
-- Dumping data for table `history`
--


--
-- Dumping data for table `job`
--


--
-- Dumping data for table `model`
--


--
-- Dumping data for table `state`
--

INSERT INTO state (state_code, state_name) VALUES ('q2c','Queued to compile');
INSERT INTO state (state_code, state_name) VALUES ('cmp','Compiling');
INSERT INTO state (state_code, state_name) VALUES ('q2r','Queued to run');
INSERT INTO state (state_code, state_name) VALUES ('run','Running');
INSERT INTO state (state_code, state_name) VALUES ('end','End');

--
-- Dumping data for table `user`
--


