# include <stdio.h>

extern metropolis_hastings_ok(void);

int main(void)
{	int ok = 1;

	ok &= metropolis_hastings_ok();

	if( ok )
	{	printf("All tests passed\n");
		return 0;
	}
	printf("At least one test failed\n");
	return 1;
}
