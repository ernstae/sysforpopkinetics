grant all on spkdb to daemon@'localhost' identified by 'daemon';
grant all on spktest to tester@'localhost' identified by 'tester';
grant SELECT,INSERT,UPDATE ON mysql.proc to daemon@'localhost';
flush privileges;
