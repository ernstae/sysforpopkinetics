-- MySQL dump 9.10
--
-- Host: dbserver    Database: spkdb
-- ------------------------------------------------------
-- Server version	4.0.17-standard

--
-- Dumping data for table `class`
--

INSERT INTO class (class_code, class_name, parent_required) VALUES ('al','Approx. Likelihood',0);
INSERT INTO class (class_code, class_name, parent_required) VALUES ('le','Likelihood Eval.',1);
INSERT INTO class (class_code, class_name, parent_required) VALUES ('so','Simulation Only',0);

--
-- Dumping data for table `end`
--

INSERT INTO end (end_code, end_name) VALUES ('cerr','Compiler Error');
INSERT INTO end (end_code, end_name) VALUES ('herr','Hard Fault');
INSERT INTO end (end_code, end_name) VALUES ('serr','Software Error');
INSERT INTO end (end_code, end_name) VALUES ('srun','Successful Run');
INSERT INTO end (end_code, end_name) VALUES ('abrt','User Abort');
INSERT INTO end (end_code, end_name) VALUES ('staf','Statistics Fail');
INSERT INTO end (end_code, end_name) VALUES ('othe','Unsuccessful Run');
INSERT INTO end (end_code, end_name) VALUES ('othf','Unknown Run Failure');
INSERT INTO end (end_code, end_name) VALUES ('acce','File Access Error');
INSERT INTO end (end_code, end_name) VALUES ('accf','File Access Failure');
INSERT INTO end (end_code, end_name) VALUES ('sime','Simulation Error');
INSERT INTO end (end_code, end_name) VALUES ('simf','Simulation Failure');
INSERT INTO end (end_code, end_name) VALUES ('opte','Optimization Error');
INSERT INTO end (end_code, end_name) VALUES ('optf','Optimization Failure');
INSERT INTO end (end_code, end_name) VALUES ('stae','Statistics Error');
INSERT INTO end (end_code, end_name) VALUES ('usre','Input Error');
INSERT INTO end (end_code, end_name) VALUES ('usrf','Input Failure');
INSERT INTO end (end_code, end_name) VALUES ('deve','Known Prog Error');
INSERT INTO end (end_code, end_name) VALUES ('devf','Known Prog Failure');
INSERT INTO end (end_code, end_name) VALUES ('pose','Post-opt Error');
INSERT INTO end (end_code, end_name) VALUES ('posf','Post-opt Failure');

--
-- Dumping data for table `method`
--

INSERT INTO method (method_code, method_name, class_code, test_only) VALUES ('fo','First Order','al',0);
INSERT INTO method (method_code, method_name, class_code, test_only) VALUES ('eh','Expected Hessian','al',0);
INSERT INTO method (method_code, method_name, class_code, test_only) VALUES ('la','Laplace Approx.','al',0);
INSERT INTO method (method_code, method_name, class_code, test_only) VALUES ('ml','M.C. Likelihood','le',0);
INSERT INTO method (method_code, method_name, class_code, test_only) VALUES ('mc','Markov Chain M.C.','al',0);
INSERT INTO method (method_code, method_name, class_code, test_only) VALUES ('gr','Grid Likelihood','le',0);
INSERT INTO method (method_code, method_name, class_code, test_only) VALUES ('an','Analytic Likelihood','le',1);
INSERT INTO method (method_code, method_name, class_code, test_only) VALUES ('so','Simulation Only','so',0);
INSERT INTO method (method_code, method_name, class_code, test_only) VALUES ('ia','Individual Analysis','al',0);
INSERT INTO method (method_code, method_name, class_code, test_only) VALUES ('mi','Miser Likelihood','le',0);

--
-- Dumping data for table `state`
--

INSERT INTO state (state_code, state_name) VALUES ('q2c','Queued to compile');
INSERT INTO state (state_code, state_name) VALUES ('q2ml','Queued to M.L.');
INSERT INTO state (state_code, state_name) VALUES ('cmp','Compiling');
INSERT INTO state (state_code, state_name) VALUES ('q2r','Queued to run');
INSERT INTO state (state_code, state_name) VALUES ('run','Running');
INSERT INTO state (state_code, state_name) VALUES ('end','End');
INSERT INTO state (state_code, state_name) VALUES ('q2ac','Queued to abort cmp');
INSERT INTO state (state_code, state_name) VALUES ('q2ar','Queued to abort run');
INSERT INTO state (state_code, state_name) VALUES ('acmp','Aborting compilation');
INSERT INTO state (state_code, state_name) VALUES ('arun','Aborting run');

