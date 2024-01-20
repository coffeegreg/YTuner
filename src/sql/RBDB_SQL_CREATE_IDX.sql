CREATE UNIQUE INDEX IF NOT EXISTS countries_IDCountry_IDX ON countries (IDCountry);
CREATE INDEX IF NOT EXISTS countries_Name_IDX ON countries (Name);
CREATE UNIQUE INDEX IF NOT EXISTS languages_IDLanguage_IDX ON languages (IDLanguage);
CREATE INDEX IF NOT EXISTS languages_Name_IDX ON languages (Name);
CREATE UNIQUE INDEX IF NOT EXISTS codecs_IDCodec_IDX ON codecs (IDCodec);
CREATE INDEX IF NOT EXISTS codecs_Name_IDX ON codecs (Name);
CREATE UNIQUE INDEX IF NOT EXISTS tags_IDTag_IDX ON tags (IDTag);
CREATE INDEX IF NOT EXISTS tags_Name_IDX ON tags (Name);