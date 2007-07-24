use 5.008;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Spkdb',
    'VERSION_FROM'	=> 'Spkdb.pm', # finds $VERSION
    'PREREQ_PM'		=> {}, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM => 'Spkdb.pm', # retrieve abstract from module
       AUTHOR     => 'Alan Westhagen (afw@u.washington.edu)'
       ) : ()),
);
sub MY::test_via_harness {
    "\tPERL_DL_NONLAZY=0 \$(FULLPERLRUN) \"-MExtUtils::Command::MM\" \"-e\" \"test_harness(\$(TEST_VERBOSE), \'\$(INST_LIB)', \'\$(INST_ARCHLIB)\')\" \$(TEST_FILES)";
}
sub MY::test_via_script {
    "\ttPERL_DL_NONLAZY=0 \$(FULLPERLRUN) \$(TESTDB_SW) \"-I\$(INST_LIB)\" \"-I\$(INST_ARCHLIB)\" \$(TEST_FILE)";
}
