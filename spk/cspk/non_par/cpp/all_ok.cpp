/*
$begin all_ok.cpp$$ 
$spell
	std
$$

$section Run All the C++ Version of Examples$$

$codep */
# include <iostream>
# include <cstdlib>

extern bool bender_ok(void);
extern bool data_ok(void);
extern bool npipp_ok(void);
extern bool opt_measure_ok(void);
extern bool relaxed_ok(void);

bool run_test(bool test(void), char *name)
{	bool ok = test();
	if( ok )
		std::cout << "ok:    " << name << std::endl;
	else	std::cout << "error: " << name << std::endl;
	return ok;
}
int main()
{	bool ok   = true;
	
	// initialize the random number generator
	unsigned seed = 5;
	std::srand(seed);

	// run all the tests
	ok   = ok & run_test(bender_ok,  "bender_ok");
	ok   = ok & run_test(data_ok,    "data_ok");
	ok   = ok & run_test(npipp_ok,   "npipp_ok");
	ok   = ok & run_test(opt_measure_ok, "opt_measure_ok");
	ok   = ok & run_test(relaxed_ok, "relaxed_ok");
	std::cout << std::endl;
	if( ok )
		std::cout << "All tests passed" << std::endl;
	else	std::cout << "At least one test failed" << std::endl;
	return ok;
}
/*  $$

$end
*/
