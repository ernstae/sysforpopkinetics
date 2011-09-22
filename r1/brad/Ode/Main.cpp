# include <iostream>

extern bool OdeBreakOk(void);
extern bool OneBreakOk(void);
extern bool ZeroBreakOk(void);

int main(void)
{	if( OdeBreakOk() )
		std::cout << "Ok:    OdeBreak" << std::endl;
	else	std::cout << "Error: OdeBreak" << std::endl;
	if( OneBreakOk() )
		std::cout << "Ok:    OneBreak" << std::endl;
	else	std::cout << "Error: OneBreak" << std::endl;
	if( ZeroBreakOk() )
		std::cout << "Ok:    ZeroBreak" << std::endl;
	else	std::cout << "Error: ZeroBreak" << std::endl;
	return 0;
}
