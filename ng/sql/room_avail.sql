BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "room_booked" (
	"booking_no"	int,
	"date"	char(20),
	"time"	char(20),
	"room_no"	int,
	"booker"	char(20)
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
	"4pm_5pm"	char(20)
);
CREATE TABLE IF NOT EXISTS "individual_information" (
	"UserName"	varchar(20),
	"Password"	varchar(9),
	"RoomNumber"	int,
	"Permissions"	char(20)
);
INSERT INTO "new_room_status" VALUES ('2020-01-27','Mon',1,'Available','Available','Available','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-01-28','Tue',1,'Unavailable','Unavailable','Unavailable','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-01-29','Wed',1,'Available','Available','Available','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-01-30','Thu',1,'Available','Available','Available','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable');
INSERT INTO "new_room_status" VALUES ('2020-02-01','Sat',1,'Available','Available','Available','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-02-02','Sun',1,'Unavailable','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable');
INSERT INTO "new_room_status" VALUES ('2020-02-03','Mon',1,'Unavailable','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable');
INSERT INTO "new_room_status" VALUES ('2020-02-04','Tue',1,'Unavailable','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable');
INSERT INTO "new_room_status" VALUES ('2020-02-05','Wed',1,'Unavailable','Unavailable','Unavailable','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-02-06','Thu',1,'Unavailable','Unavailable','Unavailable','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-02-14','Fri',1,'Unavailable','Unavailable','Unavailable','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-02-21','Fri',1,'Unavailable','Unavailable','Unavailable','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-02-28','Fri',1,'Unavailable','Unavailable','Unavailable','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-01-31','Fri',2,'Booked','Booked','Booked','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-01-31','Fri',3,'Available','Booked','Booked','Available','Available','Available','Available','Available');
INSERT INTO "new_room_status" VALUES ('2020-02-03','Mon',3,'Booked','Booked','Booked','Booked','Booked','Booked','Booked','Booked');
INSERT INTO "individual_information" VALUES ('Admin','Password',0,'admin');
INSERT INTO "individual_information" VALUES ('Simon','Password1',1,'standard');
INSERT INTO "individual_information" VALUES ('Elinor','Password2',2,'standard');
INSERT INTO "individual_information" VALUES ('Richard','Password3',3,'standard');
INSERT INTO "individual_information" VALUES ('Gianluca','Password4',4,'standard');
COMMIT;
