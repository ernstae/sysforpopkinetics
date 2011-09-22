#! /bin/bash
#
cat > gsl_ran_flat.c << EOF
# include <stdio.h>
# include <gsl/gsl_rng.h>
# include <gsl/gsl_randist.h>
# include <string.h>

int main(int argc, char *argv[])
{	size_t i;
	int uniform;
	int flat;
	double gamma;
	double w[] = {1., 2.};

	uniform = strcmp(argv[1], "uniform") == 0;
	flat    = strcmp(argv[1], "flat")    == 0;

	// initialize random number generator
	const gsl_rng_type *T;
	gsl_rng_env_setup();
	T = gsl_rng_default;
	gsl_rng *rng = gsl_rng_alloc(T);

	for(i = 1; i < 7; i++)
	{
		if( uniform )
			gsl_rng_uniform(rng);
		if( flat )
			gsl_ran_flat(rng, 0., 1.);
	}

	gamma    = w[0] + 0. * w[1];
	printf("gamma = %f\n", gamma);

	gsl_rng_free(rng);
	return 0;
}
EOF
libs=`gsl-config --libs`
echo "> gcc -g -Wall gsl_ran_flat.c $libs -o gsl_ran_flat"
gcc -g -Wall gsl_ran_flat.c $libs -o gsl_ran_flat
for option in uniform flat
do
	echo "> ./gsl_ran_flat $option"
	./gsl_ran_flat $option
done
echo "> gsl-config --version"
gsl-config --version
echo "> uname -sr"
uname -sr
if [ -e /etc/redhat-release ]
then
	echo "> rpm -q gsl"
	rpm -q gsl
	echo "> cat /etc/redhat-release"
	cat /etc/redhat-release
fi
