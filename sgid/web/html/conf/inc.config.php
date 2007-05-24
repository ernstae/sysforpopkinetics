<?

/***************************************************************************
 * SGID Global Options
 *
 *
 ***************************************************************************/

$OPTIONS = array (
		  'site_name'   => 'Service for Global Identifiability Detection (SGID)',
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
						   )
		  );

$GLOBALS['OPTIONS'] = $OPTIONS;
?>
