-- MySQL dump 9.10
--
-- Host: localhost    Database: spkdb
-- ------------------------------------------------------
-- Server version	4.0.17-standard

--
-- Dumping data for table `end`
--

INSERT INTO end (end_code, end_name) VALUES ('cerr','Compiler Error');
INSERT INTO end (end_code, end_name) VALUES ('herr','Hard Fault');
INSERT INTO end (end_code, end_name) VALUES ('serr','Software Error');
INSERT INTO end (end_code, end_name) VALUES ('srun','Successful Run');

--
-- Dumping data for table `state`
--

INSERT INTO state (state_code, state_name) VALUES ('q2c','Queued to compile');
INSERT INTO state (state_code, state_name) VALUES ('q2ml','Queued to M.L.');
INSERT INTO state (state_code, state_name) VALUES ('cmp','Compiling');
INSERT INTO state (state_code, state_name) VALUES ('q2r','Queued to run');
INSERT INTO state (state_code, state_name) VALUES ('run','Running');
INSERT INTO state (state_code, state_name) VALUES ('end','End');

--
-- Dumping data for table `method`
--

INSERT INTO method (method_code, method_name) VALUES ('fo','First Order');
INSERT INTO method (method_code, method_name) VALUES ('eh','Expected Hessian');
INSERT INTO method (method_code, method_name) VALUES ('la','Laplace Approx.');
INSERT INTO method (method_code, method_name) VALUES ('ml','M.C. Likelihood');
INSERT INTO method (method_code, method_name) VALUES ('mc','Markov Chain M.C.');

