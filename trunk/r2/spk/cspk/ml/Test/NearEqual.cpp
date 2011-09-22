
# include <cstdlib>
# include <iostream>
# include <fstream>

# include <CppAD/NearEqual.h>

main(int argc, char *argv[])
{	using std::fstream;
	using CppAD::NearEqual;
	int error_code = 0; 
	int count = 0;

	// check number of command line arguments
	const char *usage=
	"NearEqual file_one file_two relative_error absolute_error [skip]";
	if( (argc < 5) | (argc > 6)  )
	{	std::cerr << usage << std::endl;
		exit(1);
	}

	// open files one for reading
	fstream file_one;
	file_one.open(argv[1], fstream::in);
	if(  ! file_one.is_open() )
	{	std::cerr << "NearEqual: cannot open " << argv[1] << std::endl;
		exit(1);
	}
	fstream file_two;
	file_two.open(argv[2], fstream::in);
	if(  ! file_two.is_open() )
	{	std::cerr << "NearEqual: cannot open " << argv[2] << std::endl;
		exit(1);
	}

	// relative and absolute error criteria
	double relative = std::atof(argv[3]);
	double absolute = std::atof(argv[4]);

	// skip
	size_t skip = 0;
	if( argc > 5 )
		skip = std::atoi(argv[5]);

	double value_one;
	double value_two;
 	file_one >> value_one;
	file_two >> value_two;
	while( ! ( file_one.eof() | file_two.eof() ) )
	{	count++;
		if( (count > skip) && (! NearEqual(value_one, value_two, relative, absolute) ) )
		{	if( error_code == 0 )
				std::cout 
				<< "NearEqual: count = " << count
				<< ", value_one = " << value_one
				<< ", value_two = " << value_two
				<< std::endl;
		 	error_code = 1;
		}
 		file_one >> value_one;
		file_two >> value_two;
	}
	if( ! ( file_one.eof() & file_two.eof() ) )
	{	std::cerr << "NearEqual: " << argv[1] << " and " << argv[2];
		std::cerr << " do not have same number of values" << std::endl;
		exit(1);
	}
	
	return error_code;
}
