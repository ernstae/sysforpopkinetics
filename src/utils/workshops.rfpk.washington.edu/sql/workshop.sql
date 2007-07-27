create table if not exists events
	( id			bigint not null auto_increment,
	  name  		varchar(192) not null,	
	  event_email		varchar(128) default 'vicini@u.washington.edu',
	  event_date		date,
	  cost			float,
	  discount_cost 	float,
	  max_participants	int default 20,
	  registration_status   enum('open','closed') default 'open',
	  registration_open	datetime,
	  registration_closed   datetime,
	  details		text,
	  INDEX (id, event_date),
	  PRIMARY KEY(id)
         );

create table if not exists participants (
	id		bigint not null auto_increment,
	firstname	varchar(128) not null,
	lastname	varchar(128) not null,
	affiliation	varchar(255),
	address		varchar(255) not null,
	address2	varchar(255),
	city		varchar(128),
	state		varchar(80) default 'WA',
	country		varchar(80) default 'USA',
	postal_code	varchar(15),
	email		varchar(128) not null,
	password	varchar(20) not null,
	question	varchar(200), 
	answer		varchar(200),
	user_status	enum('A','X') default 'A' not NULL,
	register_date	TIMESTAMP default CURRENT_TIMESTAMP,
	INDEX (email,password),
	PRIMARY KEY (id)
);

create table if not exists registrations (
	event_id 		bigint not null,
	participant_id 		bigint not null,
	user_type		enum('R','D') not NULL,
	total_cost		float,
	paid			enum('false','true') default 'false' not NULL,
	request_date 		TIMESTAMP,
	last_modifiied 		TIMESTAMP,
	registration_status 	enum('pending','approved','waitlist','cancelled') default 'pending' not NULL,
	FOREIGN KEY (event_id) REFERENCES events.id ON DELETE CASCADE,
	FOREIGN KEY (participant_id) REFERENCES participants.id ON DELETE CASCADE
);	  
	 
create table if not exists email_messages (
	email_id		bigint not null auto_increment,
	event_type		enum('workshop','newaccount') default 'workshop',
	message			text,
	message_type		enum('pending','approved','waitlist') default 'pending',
	PRIMARY KEY (email_id),
	index (message_type)
);



