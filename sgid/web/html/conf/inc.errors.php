<?

/************************************************************
 * ERROR DECLARATIONS
 *
 ************************************************************/

$errors = 
    array ( 'parse_in_mismatch'
	    => 'Inputs must be consecutive.  A "U" and "U1" have been detected.',
	    'parse_in_consecutive'
	    => 'Inputs must be consecutive',
	    'parse_in_U1missing'
	    => 'With multiple inputs, U or U1 must be present.',
	    'parse_in_Umissing'
	    => 'Your system-experiment model has only a single input. When there is only a single input, it must be named U.   Try this model again with U for your input.  I found',
	    'parse_illegal'
	    => 'The following illegal character was found',
	    'parse_out_Ymissing'
	    => 'Your system-experiment model has only a single output.  When there is only a single output, it must be named Y.',
	    'parse_out_Ymulti'
	    => 'Your system-experiment model has multiple outputs.  When there are multiple outputs, they must be named {Y1, Y2, Y3, .., Yn}.',
	    'parse_out'
	    => 'No outputs were defined.  Please try again and enter one or more output equations',
	    'parse'
	    => '',
	    'parse_exponent'
	    => 'I detected "**" in your equations.  Exponentials must be expressed using a "^" character.',
	    'parse_unknown'
	    => 'Parser error I Could not understand',
	    'web_required'
	    => 'The following required fields must be entered',
	    'web_email'
	    => 'The following e-mail address is not valid'
	    );


?>

