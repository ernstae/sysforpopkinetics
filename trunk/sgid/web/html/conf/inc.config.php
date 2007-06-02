<?

/***************************************************************************
 * SGID Global Options
 *
 *
 ***************************************************************************/

$OPTIONS = array (
		  'site_name'   => 'System for Global IDentifiability (SGID)',
		  'site_url'    => 'http://toronto.rfpk.washington.edu/sgid',
		  'service_of'  => 'Resource for Population Kinetics (RFPK) at the University of Washington',
		  'seed_min'    => 10000,
		  'seed_max'    => 99999,
		  'daemons_max' => 6,              
		  'debug'       => false,
		  'operators'   => '+-/*',
		  'DSN'         => array ('phptype'  => 'mysql',
					  'hostspec' => 'whitechuck.rfpk.washington.edu',
					  'database' => 'SGID',
					  'username' => 'sgid',
					  'password' => 'sgid_web' ),
		  'serializer_options'   => array (
						   'encoding' => 'ISO-8859-1',
						   'indent' => "   ",
						   'rootName' => 'identifiability_input',
						   'XML_SERIALIZER_OPTION_ATTRIBUTES_KEY' => 'attributes',
						   'XML_SERIALIZER_OPTION_MODE' => 'XML_SERIALIZER_MODE_SIMPLEXML'
						   ),
		  'email_from'  => "rfpksoft@u.washington.edu",
		  'email_subject' => "[SGID] Your job results: ",
		  'email_bcc'   => 'ernst@washington.edu'
		  );

$GLOBALS['OPTIONS'] = $OPTIONS;
?>
