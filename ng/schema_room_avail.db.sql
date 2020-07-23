BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "room_booked" (
	"booking_no"	int,
	"date"	char(20),
	"time"	char(20),
	"room_no"	int,
	"booker"	char(20),
	PRIMARY KEY("room_no","date","time")
);
CREATE TABLE IF NOT EXISTS "new_room_status" (
	"Date"	char(20),
	"Weekday"	char(20),
	"Room_no"	int,
	"9am_10am"	char(20),
	"10am_11am"	char(20),
	"11am_12pm"	char(20),
	"12pm_1pm"	char(20),
	"1pm_2pm"	char(20),
	"2pm_3pm"	char(20),
	"3pm_4pm"	char(20),
	"4pm_5pm"	char(20),
	PRIMARY KEY("Date","Room_no")
);
CREATE TABLE IF NOT EXISTS "individual_information" (
	"UserName"	varchar(20),
	"Password"	varchar(9),
	"RoomNumber"	int,
	"Permissions"	char(20)
);
INSERT INTO "individual_information" VALUES ('Admin','Password',0,'admin');
INSERT INTO "individual_information" VALUES ('Simon','Password1',1,'standard');
INSERT INTO "individual_information" VALUES ('Elinor','Password2',2,'standard');
INSERT INTO "individual_information" VALUES ('Richard','Password3',3,'standard');
INSERT INTO "individual_information" VALUES ('Gianluca','Password4',4,'standard');
COMMIT;
