#!/usr/bin/php -q

<?php


require_once("../html/conf/SGID.php");

declare(ticks = 1);

#pcntl_signal(SIGCHLD, "signal_handler");
error_reporting(E_ALL);




/**************************************************************************
 *
 *
 ***************************************************************************/
$concurrent = 0;
$max_concurrent = 2;
$check_ident = "/usr/local/sgid/bin/check_ident";

$q_queued = array();
$q_running = array();

function signal_handler($signal) {
  global $concurrent;
  global $db;
  global $job;

# echo "************************************************************\n";
# echo "Inside Signal Handler:  $signal  | job_id = " . $job['id'] . " | concurrent == $concurrent\n";
# echo "************************************************************\n";

  // we might not have a DB in the signal handler... so create one.
  //  $dba = DB::connect($GLOBALS['OPTIONS']['DSN']);
  
  switch($signal) {
  case SIGCHLD:
    while (pcntl_waitpid(0, $status) != -1) {
      $status = pcntl_wexitstatus($status);
      echo "Child $status completed with signal $signal\n";
    }
    break;
  case SIGSEGV:
    while ( pcntl_waitpid(0, $status ) != -1 ) {
      $status = pcntl_wexitstatus($status);
      // update the database, file a bug, etc
      echo "Segmentation fault found so we will exit with signal " . $signal . "\n";
    }
    break;
    
  case SIGQUIT:
    while ( pcntl_waitpid(0, $status ) != -1 ) {
      $status = pcntl_wexitstatus($status);
      
      echo "SIGQUIT found so we will exit with signal " . $signal . "\n";
    }
    break;
  default:
    while ( pcntl_waitpid(0, $status ) != -1 ) {
      $status = pcntl_wexitstatus($status);
      echo "uncaught $signal received from child\n";
    }
    break;
  }


  $concurrent--;
}

$dba = MDB2::connect($GLOBALS['OPTIONS']['DSN']);
$dba->setFetchMode(MDB2_FETCHMODE_OBJECT);

$result = $dba->query("SELECT id, xml_input, email_address, md5(concat(id,seed)) as jobid FROM job WHERE state_code='queue'");

while ( $row = $result->fetchRow() )
  {
    echo "Queuing: $row->id for user $row->email_address \n";
    // add to the stack.
    $q_queued[] = array ( 'id' => $row->id, 
			  'jobid' => $row->jobid,
			  'xml_input' => $row->xml_input, 
			  'email_address' => $row->email_address,
			  'xml_output' => NULL);

    $m = $dba->query("UPDATE job set state_code='run' WHERE id in (" . $row->id . ")");
  }



$dba->disconnect();
unset ($dba);


while(1) {
  while ( $concurrent < $max_concurrent && sizeof($q_queued) > 0 )
    {
      $job = array_shift($q_queued);

      $workdir = "../jobs/sgid-job-" . $job['id'];
      $infile = $workdir . "/input.xml";
      $outfile = $workdir . "/output.xml";
      
      $end_code = "srun";
      
      // add to run queue
      $q_running[] = $job;
      ++$concurrent;
      $err_msg = '';

      $dba = MDB2::connect($GLOBALS['OPTIONS']['DSN']);
      $result = $dba->query("UPDATE job SET state_code='run' WHERE id='" . $job['id'] . "'");
      $dba->disconnect();
      unset ($dba);
      
      $pid = pcntl_fork();

      if ( !$pid ) 
	{
	// we're in the child now.
	  
	  sleep(1);

	  $data = array();
	  
	  
#	  echo "Forked PID (concur == $concurrent): $pid for job " . $job['id'] . "\n";
	  
	  
	  // delete working directory (if exists)
	  if ( is_dir ( $workdir ) ) 
	    {
	      recursiveRemoveDirectory( $workdir );
	    }
	  
	  // create workign directory
	  if ( mkdir( $workdir ) ) 
	    {
	      if ( file_put_contents( $infile, $job['xml_input'],  LOCK_EX ) === FALSE )
		{
		  $err_msg .= "job-runner could not make working directory and input file\n";
		}
	      
#	      echo "errors: " . strlen($err_msg) . "\n";
	      if ( strlen($err_msg) <= 0 )
		{
		  echo "Running check_ident for job " . $job['id'] . "\n";
		  
		  $driver_pid = exec( $check_ident . " " . $infile . " " . $outfile . "; echo $?", $output );
		  
		  if ( $output !== 0 ) 
		    {
		      $end_code = "serr";
		      $err_msg = "job-runner received exit code of $driver_pid from $check_ident";
		    }
		}
	      else 
		{
		  $end_code = "serr";
		}	
	    }
	  else 
	    {
	      $err_msg .= "job-runner could not create working directory.\n";
	      $end_code = "serr";
	    }

	    exit(0);
	}
      else if ( $pid )
	{
	  // we're the parent

	  echo "job-runner: spawned child $pid for job_id=" . $job['id'] . "\n";

	  while ( pcntl_waitpid( $pid, $status ) != -1 ) 
	    {
	      $status = pcntl_wexitstatus($status);
	      echo "Job ID: " . $job['id'] . " child $pid exited with ", pcntl_wexitstatus($status), " : end_code= $end_code\n";
	      --$concurrent;
	      
	      // get the results of the run.
	      if ( file_exists($outfile) ) 
		{
		  $job["xml_output"] = file_get_contents( $outfile );
		  $xml = SGID_getXML( $job["xml_output"] );
		  
		  if ( isset($xml['error_messages']) && strlen($xml['error_messages'] ))
		    {
		      $err_msg .= $xml['error_messages'];
		      $end_code = 'liberr';
		    }
		}
	      else 
		{
		  $end_code = "serr";
		  $err_msg .= "job-runner: could not find XML output file $outfile\n";
		}
	      
	      $dba = MDB2::connect($GLOBALS['OPTIONS']['DSN']);
	      $result = $dba->query("UPDATE job set state_code='end', end_code='" . $end_code . "', result_xml='" . addslashes($job['xml_output']) . "' WHERE id=" . $job['id']);
	      
	      
	      send_report ( $job );
	      
	      if ( PEAR::isError( $dba ) ) 
		{
		  $err_msg .= $dba->getMessage() . "\n";
		  
		  echo "============================================================\n";
		  echo "ERROR MESSAGES:\n\n" . $err_msg . "\n";
		  echo "============================================================\n\n";
		  
		}
	      
	      if ( strlen( $err_msg ) > 0 ) 
		{
		  $data = array ( $job['id'], $err_msg );
		  $results = $dba->query("INSERT INTO error_reports VALUES (" . $job['id'] . ", '" . addslashes($err_msg) . "'");
		}
	      
	      $dba->disconnect();
	      unset($dba);
	    }
	  
	}
      //  echo "Currently " . $concurrent . "\n";
      
    }
  
  
  if ( $concurrent <= 0 && sizeof($q_queued) <= 0) 
    {
      echo "No more jobs to process.\n";
      exit(0);
    }
  
  
  sleep(1);
  
 }



function is_empty_dir ( $dirname ) {
  $result = false;

  if ( is_dir( $dirname ) ) {
    $result = true;
    $handle = opendir( $dirname );
    while ( ( $name = readdir($handle) ) !== FALSE ) {
      if ( $name != "." && $name != ".." ) {
	$result = false;
	break;
      }
      closedir ( $handle );
    }
  }
  
  return ( $result );
  
}


function create_workdir ( $job_id ) {
  
  return 0;
  

}


   function recursiveRemoveDirectory($path)
    {   
        $dir = new RecursiveDirectoryIterator($path);

        //Remove all files
        foreach(new RecursiveIteratorIterator($dir) as $file)
        {
            unlink($file);
        }

        //Remove all subdirectories
        foreach($dir as $subDir)
        {
            //If a subdirectory can't be removed, it's because it has subdirectories, so recursiveRemoveDirectory is called again passing the subdirectory as path
            if(!@rmdir($subDir)) //@ suppress the warning message
            {
                recursiveRemoveDirectory($subDir);
            }
        }

        //Remove main directory
        rmdir($path);
    }


function send_report ( $job ) {

  $headers = array ( 'From' => $GLOBALS['OPTIONS']['email_from'],
		     'To' => $job['email_address'],
		     'Subject' => $GLOBALS['OPTIONS']['email_subject'] . $job['id'],
		     'Bcc' => $GLOBALS['OPTIONS']['email_bcc']
		     );

  $message = "";

  $xml = SGID_getXML( $job['xml_output'] );

  //  var_dump ( $xml );

  /*
  array(6) {
    ["elapsed_seconds"]=>
      string(1) "0"
      ["parameter_seed"]=>
      string(4) "3931"
      ["error_messages"]=>
      string(0) ""
      ["number_of_solutions"]=>
      string(1) "1"
      ["status_of_the_solutions"]=>
      string(32) "Globally (Uniquely) Identifiable"
      ["calculation_details"]=>
      string(1547) "Begin parameter identifiability calculation.
  */

  $sep = "============================================================\n";


  $message = "Thank you for using the " . $GLOBALS['OPTIONS']['site_name'] . " provided by " . $GLOBALS['OPTIONS']['service_of'] . "\n\n";
  $message .= "The results of your job are as follows\n\n";
  $message .= $sep . "Job Number: " . $job['id'] . "\n";
  $message .= "Job ID: " . $job['jobid'] . "\n" . "Solutions found: " . $xml['number_of_solutions'] . "\n" . $xml['status_of_the_solutions'] . "\n\n\n";

  if ( strlen($xml['error_messages']) ) {
    $message .= $sep;
    $message .= "Unfortunately, we found errors with your job:\n" . $xml['error_messages'] . "\n" . $sep;
  }

  $message .= "The calculation details follow...\n\n" . $xml['calculation_details'] . "\n\n";

  $message .= $sep . "This job can be re-run by clicking on " . $GLOBALS['OPTIONS']['site_url'] . "/runjob.php?jobid=" . $job['jobid'] . "\n";
  $message .= "This report is available by clicking on " . $GLOBALS['OPTIONS']['site_url'] . "/showjob.php?jobid=" . $job['jobid'] . "\n";

  if ( validate_email ( $job['email_address'] ) ) 
    {
      
      $m =& Mail::factory('sendmail');
      //      $m->send( $GLOBALS['OPTIONS']['email_bcc'], $headers, $message );
      $m->send( $job['email_address'], $headers, $message );
    }

}

?>
