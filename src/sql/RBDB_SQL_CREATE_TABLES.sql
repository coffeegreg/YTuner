PRAGMA temp_store=2;

CREATE TABLE "countries"(
    [IDCountry] INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    [Name] TEXT NOT NULL);
	
CREATE TABLE "languages"(
    [IDLanguage] INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    [Name] TEXT NOT NULL);
	
CREATE TABLE "codecs"(
    [IDCodec] INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    [Name] TEXT NOT NULL);
	
CREATE TABLE "tags"(
    [IDTag] INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    [Name] TEXT NOT NULL);
	
CREATE TABLE "stations"(
    [IDStation] INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    [stationuuid] TEXT NOT NULL,
    [name] TEXT NOT NULL,
    [url] TEXT NOT NULL,
    [url_resolved] TEXT NULL,
    [homepage] TEXT NULL,
    [favicon] TEXT NULL,
    [IDCountry] INTEGER NULL,
    [IDLanguage] INTEGER NULL,
    [votes] INTEGER NULL,
    [IDCodec] INTEGER NULL,
    [bitrate] INTEGER NULL,
    [lastcheckok] INTEGER NULL,
    [lastcheckoktime] DATETIME NULL,
    [geo_lat] REAL NULL,
    [geo_long] REAL NULL);
	
CREATE TABLE "stationstags"(
    [IDStationTag] INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    [IDStation] INTEGER NOT NULL,
    [IDTag] INTEGER NOT NULL);