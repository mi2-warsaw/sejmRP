# grant privileges for sejmrp and reader

GRANT ALL ON counter TO sejmrp;
GRANT ALL ON deputies TO sejmrp;
GRANT ALL ON statements TO sejmrp;
GRANT ALL ON votes TO sejmrp;
GRANT ALL ON votings TO sejmrp;

GRANT SELECT ON counter TO reader;
GRANT SELECT ON deputies TO reader;
GRANT SELECT ON statements TO reader;
GRANT SELECT ON votes TO reader;
GRANT SELECT ON votings TO reader;
