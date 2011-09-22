s%^#include "DataSet.h"%&\
#include "../AnalyticIntegral.h"\
#include "../MontePopObj.h"\
#include "../AnalyticIntegral.h"%
#
s%\(const bool isOptRequested *=\).*%\1 false;\
const bool isMonteRequested = true;\
bool isMonteSuccess = false;\
double monteIntegralOut;\
double monteStdOut;% 
#
/^haveCompleteData = true;/s%$%\
/*******************************************************************/\
/*                                                                 */\
/*   Monte Carlo Integration                                       */\
/*                                                                 */\
/*******************************************************************/\
timeval monteBegin, monteEnd;\
double monteTimeSec = 0.0;\
if( isMonteRequested )\
{\
	// value to check against \
	double analyticIntegral = 0;\
	size_t i;\
	for(i = 0; i < nPop; i++)\
	{\
		analyticIntegral += AnalyticIntegral(\
			model               , \
			N                   ,\
			y                   ,\
			alpIn               ,\
			bLow                ,\
			bUp                 ,\
			i\
		);\
	}\
	cout << "analyticIntegral = " << analyticIntegral << endl;\
	//\
	gettimeofday( \&monteBegin, NULL );\
	try\
	{\
		MontePopObj(\
			model,\
			N,\
			y,\
			alpIn,\
			bLow,\
			bUp,\
			monteIntegralOut,\
			monteStdOut\
		);\
		isMonteSuccess = true;\
	}\
	catch( const SpkException\& e )\
	{\
		oRuntimeError << e << endl;\
		cerr << e << endl;\
		isMonteSuccess = false;\
	}\
	catch( ... )\
	{	char *message = \
		"Unknown exception: failed in monte carlo integration.";\
		//\
		oRuntimeError << message << endl;\
		cerr << message << endl;\
		isMonteSuccess = false;\
	}\
	gettimeofday( \&monteEnd, NULL );\
	monteTimeSec = difftime( monteEnd.tv_sec, monteBegin.tv_sec );\
	//\
	cout << "monteIntegralOut = " << monteIntegralOut << endl;\
	cout << "monteStdOut      = " << monteStdOut      << endl;\
}%
