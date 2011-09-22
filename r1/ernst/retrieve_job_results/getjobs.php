#!/usr/bin/php

<?php

// get_spk_jobs.php
//
// takes list of jobs as arguments.
//
// requires the following PEAR modules:
//   PEAR::Console_Getopt

require_once('MDB2.php');

PEAR::setErrorHandling(PEAR_ERROR_PRINT);

$OPTIONS = array (
                  'DSN'         => array ('phptype'  => 'mysql',
                                          'hostspec' => 'dbserver.rfpk.washington.edu',
                                          'database' => 'spkdb',
                                          'username' => 'daemon',
                                          'password' => 'daemon' ),
                  );

$GLOBALS['OPTIONS'] = $OPTIONS;



if (!function_exists('gzdecode')) {
  function gzdecode ($data) {
    $flags = ord(substr($data, 3, 1));
    $headerlen = 10;
    $extralen = 0;
    $filenamelen = 0;
    if ($flags & 4) {
      $extralen = unpack('v' ,substr($data, 10, 2));
      $extralen = $extralen[1];
      $headerlen += 2 + $extralen;
    }
    if ($flags & 8) // Filename
      $headerlen = strpos($data, chr(0), $headerlen) + 1;
    if ($flags & 16) // Comment
      $headerlen = strpos($data, chr(0), $headerlen) + 1;
    if ($flags & 2) // CRC at end of file
      $headerlen += 2;
    $unpacked = gzinflate(substr($data, $headerlen));
    if ($unpacked === FALSE)
      $unpacked = $data;
    return $unpacked;
  }
 }





$db = MDB2::connect($GLOBALS['OPTIONS']['DSN']);
$db->setFetchMode(MDB2_FETCHMODE_OBJECT);

$jobs = "";

$workdir_default = "./jobs";

fwrite(STDOUT, "Please enter your working directory:  ");
$workdir = fgets(STDIN);
$workdir = chop($workdir);

if ( strlen($workdir) == 0 ) {
  $workdir = $workdir_default;
 }
  

while ( strlen($jobs <= 0) ) {
  fwrite(STDOUT, "SPK Job Retrieval System\n\nPlease enter a list of jobs separated by spaces.  Only enter the job numbers.\n\nJobs?: ");
  $jobs = fgets(STDIN);
 }

fwrite(STDOUT, "Starting... Please wait while I retrieve these jobs");

// parse the job list, and put in an array:
$delims = " ,\n\t";
$tok = strtok( $jobs, $delims);

while ( $tok !== false) {
  $job_list[] = $tok;
  $tok = strtok($delims);
 }


// loop through jobs, connecting to the database and pulling out & unzipping the results.xml
foreach ( $job_list as $job ) {
  fwrite(STDOUT, "Job: $job\n");

  $sql = 'select j.xml_source, j.report report, j.cpp_source, j.model_id, j.dataset_id, j.model_version, j.dataset_id, d.archive d_archive, m.archive m_archive from job j, dataset d, model m where j.job_id IN (' . $job . ') AND j.model_id=m.model_id AND d.dataset_id=j.dataset_id';

  $res =& $db->query($sql);
  
  if ( PEAR::isError($res) ) {
    die ($res->getMessage());
  }

  $xml_source = "";
  $cpp_source = "";
  $report = "";

  while ( ($row = $res->fetchRow())) {
    $xml_source = $row->xml_source;
    $cpp_source = $row->cpp_source;
    $report = $row->report;
    $source_file = "source.xml";
    $job_dir = "spkrun-job-" . $job;
    $tmp_dir = $workdir . "/" . $job_dir;
  }

  
  if ( $report === NULL ) {
    die ("Report field is null:  $report");
  }

echo "\nDirectory: " . $tmp_dir . "\n";
  system("mkdir -p $tmp_dir");

  // write source.xml
  if ( ! $handle = fopen($tmp_dir . "/" . $source_file, "w" ) ) {
    die("Cannot open file for writing: $tmp_dir/$source_file");
  }
  else {
    fwrite($handle, $xml_source);
    fclose($handle);
  }

  // write result.xml
  if ( ! $handle = fopen($tmp_dir . "/" . "result.xml", "w") ) {
    die("Cannot open result.xml for writinig:  $tmp_dir/result.xml");
  }
  else {
    fwrite($handle, $report);
    fclose($handle);
  }
  
	

  
}
  
?>