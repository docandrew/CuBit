------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The books of the Bible (APP-005): one enumeration over the full canon,
--  the deuterocanon included, each with its testament, its section, its
--  name and its USFM code. Order is reading order: the Old Testament, the
--  Apocrypha / deuterocanon between the Testaments (as the 1611 King James
--  Version printed it), then the New Testament.
--
--  A translation need not have every book (versification is per
--  translation: Bible_References.Versification).
------------------------------------------------------------------------------
package Bible_Canon with SPARK_Mode, Pure is

   type Book is
     (Genesis, Exodus, Leviticus, Numbers, Deuteronomy,
      Joshua, Judges, Ruth, First_Samuel, Second_Samuel, First_Kings, Second_Kings,
      First_Chronicles, Second_Chronicles, Ezra, Nehemiah, Esther,
      Job, Psalms, Proverbs, Ecclesiastes, Song_Of_Solomon,
      Isaiah, Jeremiah, Lamentations, Ezekiel, Daniel,
      Hosea, Joel, Amos, Obadiah, Jonah, Micah, Nahum, Habakkuk, Zephaniah, Haggai, Zechariah,
      Malachi,
      --  The Apocrypha / deuterocanon.
      Tobit, Judith, Esther_Greek, Wisdom, Sirach, Baruch, Letter_Of_Jeremiah,
      Song_Of_The_Three, Susanna, Bel_And_The_Dragon, Daniel_Greek,
      First_Maccabees, Second_Maccabees, Third_Maccabees, Fourth_Maccabees,
      First_Esdras, Second_Esdras, Prayer_Of_Manasseh, Psalm_151,
      --  The New Testament.
      Matthew, Mark, Luke, John, Acts,
      Romans, First_Corinthians, Second_Corinthians, Galatians, Ephesians, Philippians,
      Colossians, First_Thessalonians, Second_Thessalonians, First_Timothy, Second_Timothy,
      Titus, Philemon,
      Hebrews, James, First_Peter, Second_Peter, First_John, Second_John, Third_John, Jude,
      Revelation);

   BOOK_COUNT : constant := Book'Pos (Book'Last) + 1;

   type Testament is (Old_Testament, Deuterocanon, New_Testament);

   type Section is
     (Law, History, Wisdom_And_Poetry, Major_Prophets, Minor_Prophets,
      Apocrypha,
      Gospels, Acts_Of_The_Apostles, Pauline_Epistles, General_Epistles, Prophecy);

   function Section_Of (Item : Book) return Section is
     (case Item is
         when Genesis .. Deuteronomy => Law,
         when Joshua .. Esther => History,
         when Job .. Song_Of_Solomon => Wisdom_And_Poetry,
         when Isaiah .. Daniel => Major_Prophets,
         when Hosea .. Malachi => Minor_Prophets,
         when Tobit .. Psalm_151 => Apocrypha,
         when Matthew .. John => Gospels,
         when Acts => Acts_Of_The_Apostles,
         when Romans .. Philemon => Pauline_Epistles,
         when Hebrews .. Jude => General_Epistles,
         when Revelation => Prophecy);

   function Testament_Of (Value : Section) return Testament is
     (case Value is
         when Law .. Minor_Prophets => Old_Testament,
         when Apocrypha => Deuterocanon,
         when Gospels .. Prophecy => New_Testament);

   function Testament_Of (Item : Book) return Testament is (Testament_Of (Section_Of (Item)));

   function Is_Deuterocanonical (Item : Book) return Boolean is (Testament_Of (Item) = Deuterocanon);

   function First_Section (Value : Testament) return Section is
     (case Value is
         when Old_Testament => Law, when Deuterocanon => Apocrypha, when New_Testament => Gospels);
   function Last_Section (Value : Testament) return Section is
     (case Value is
         when Old_Testament => Minor_Prophets, when Deuterocanon => Apocrypha,
         when New_Testament => Prophecy);

   function First_Book (Value : Section) return Book is
     (case Value is
         when Law => Genesis, when History => Joshua, when Wisdom_And_Poetry => Job,
         when Major_Prophets => Isaiah, when Minor_Prophets => Hosea, when Apocrypha => Tobit,
         when Gospels => Matthew, when Acts_Of_The_Apostles => Acts, when Pauline_Epistles => Romans,
         when General_Epistles => Hebrews, when Prophecy => Revelation);
   function Last_Book (Value : Section) return Book is
     (case Value is
         when Law => Deuteronomy, when History => Esther, when Wisdom_And_Poetry => Song_Of_Solomon,
         when Major_Prophets => Daniel, when Minor_Prophets => Malachi, when Apocrypha => Psalm_151,
         when Gospels => John, when Acts_Of_The_Apostles => Acts, when Pauline_Epistles => Philemon,
         when General_Epistles => Jude, when Prophecy => Revelation);

   function Testament_Title (Value : Testament) return String is
     (case Value is
         when Old_Testament => "Old Testament",
         when Deuterocanon => "Apocrypha / Deuterocanon",
         when New_Testament => "New Testament");

   function Section_Title (Value : Section) return String is
     (case Value is
         when Law => "The Law",
         when History => "History",
         when Wisdom_And_Poetry => "Wisdom and Poetry",
         when Major_Prophets => "Major Prophets",
         when Minor_Prophets => "Minor Prophets",
         when Apocrypha => "Apocrypha / Deuterocanon",
         when Gospels => "Gospels",
         when Acts_Of_The_Apostles => "Acts",
         when Pauline_Epistles => "Letters of Paul",
         when General_Epistles => "General Letters",
         when Prophecy => "Prophecy");

   --  The longest Name ("Song of the Three", "2 Thessalonians"); a bound
   --  for layouts.
   MAXIMUM_NAME : constant := 18;

   --  The book's name as a reader writes it ("1 Corinthians").
   function Name (Item : Book) return String is
     (case Item is
         when Genesis => "Genesis", when Exodus => "Exodus", when Leviticus => "Leviticus",
         when Numbers => "Numbers", when Deuteronomy => "Deuteronomy", when Joshua => "Joshua",
         when Judges => "Judges", when Ruth => "Ruth", when First_Samuel => "1 Samuel",
         when Second_Samuel => "2 Samuel", when First_Kings => "1 Kings", when Second_Kings => "2 Kings",
         when First_Chronicles => "1 Chronicles", when Second_Chronicles => "2 Chronicles",
         when Ezra => "Ezra", when Nehemiah => "Nehemiah", when Esther => "Esther", when Job => "Job",
         when Psalms => "Psalms", when Proverbs => "Proverbs", when Ecclesiastes => "Ecclesiastes",
         when Song_Of_Solomon => "Song of Solomon", when Isaiah => "Isaiah", when Jeremiah => "Jeremiah",
         when Lamentations => "Lamentations", when Ezekiel => "Ezekiel", when Daniel => "Daniel",
         when Hosea => "Hosea", when Joel => "Joel", when Amos => "Amos", when Obadiah => "Obadiah",
         when Jonah => "Jonah", when Micah => "Micah", when Nahum => "Nahum", when Habakkuk => "Habakkuk",
         when Zephaniah => "Zephaniah", when Haggai => "Haggai", when Zechariah => "Zechariah",
         when Malachi => "Malachi",
         when Tobit => "Tobit", when Judith => "Judith", when Esther_Greek => "Esther (Greek)",
         when Wisdom => "Wisdom of Solomon", when Sirach => "Sirach", when Baruch => "Baruch",
         when Letter_Of_Jeremiah => "Letter of Jeremiah", when Song_Of_The_Three => "Song of the Three",
         when Susanna => "Susanna", when Bel_And_The_Dragon => "Bel and the Dragon",
         when Daniel_Greek => "Daniel (Greek)", when First_Maccabees => "1 Maccabees",
         when Second_Maccabees => "2 Maccabees", when Third_Maccabees => "3 Maccabees",
         when Fourth_Maccabees => "4 Maccabees", when First_Esdras => "1 Esdras",
         when Second_Esdras => "2 Esdras", when Prayer_Of_Manasseh => "Prayer of Manasseh",
         when Psalm_151 => "Psalm 151",
         when Matthew => "Matthew", when Mark => "Mark", when Luke => "Luke", when John => "John",
         when Acts => "Acts", when Romans => "Romans", when First_Corinthians => "1 Corinthians",
         when Second_Corinthians => "2 Corinthians", when Galatians => "Galatians",
         when Ephesians => "Ephesians", when Philippians => "Philippians", when Colossians => "Colossians",
         when First_Thessalonians => "1 Thessalonians", when Second_Thessalonians => "2 Thessalonians",
         when First_Timothy => "1 Timothy", when Second_Timothy => "2 Timothy", when Titus => "Titus",
         when Philemon => "Philemon", when Hebrews => "Hebrews", when James => "James",
         when First_Peter => "1 Peter", when Second_Peter => "2 Peter", when First_John => "1 John",
         when Second_John => "2 John", when Third_John => "3 John", when Jude => "Jude",
         when Revelation => "Revelation")
   with Post => Name'Result'First = 1 and then Name'Result'Length in 1 .. MAXIMUM_NAME;

   --  The three-character USFM book code (tools/build_bible.py keys the
   --  sources by it).
   subtype Code_Text is String (1 .. 3);
   function Code (Item : Book) return Code_Text is
     (case Item is
         when Genesis => "GEN", when Exodus => "EXO", when Leviticus => "LEV", when Numbers => "NUM",
         when Deuteronomy => "DEU", when Joshua => "JOS", when Judges => "JDG", when Ruth => "RUT",
         when First_Samuel => "1SA", when Second_Samuel => "2SA", when First_Kings => "1KI",
         when Second_Kings => "2KI", when First_Chronicles => "1CH", when Second_Chronicles => "2CH",
         when Ezra => "EZR", when Nehemiah => "NEH", when Esther => "EST", when Job => "JOB",
         when Psalms => "PSA", when Proverbs => "PRO", when Ecclesiastes => "ECC",
         when Song_Of_Solomon => "SNG", when Isaiah => "ISA", when Jeremiah => "JER",
         when Lamentations => "LAM", when Ezekiel => "EZK", when Daniel => "DAN", when Hosea => "HOS",
         when Joel => "JOL", when Amos => "AMO", when Obadiah => "OBA", when Jonah => "JON",
         when Micah => "MIC", when Nahum => "NAM", when Habakkuk => "HAB", when Zephaniah => "ZEP",
         when Haggai => "HAG", when Zechariah => "ZEC", when Malachi => "MAL",
         when Tobit => "TOB", when Judith => "JDT", when Esther_Greek => "ESG", when Wisdom => "WIS",
         when Sirach => "SIR", when Baruch => "BAR", when Letter_Of_Jeremiah => "LJE",
         when Song_Of_The_Three => "S3Y", when Susanna => "SUS", when Bel_And_The_Dragon => "BEL",
         when Daniel_Greek => "DAG", when First_Maccabees => "1MA", when Second_Maccabees => "2MA",
         when Third_Maccabees => "3MA", when Fourth_Maccabees => "4MA", when First_Esdras => "1ES",
         when Second_Esdras => "2ES", when Prayer_Of_Manasseh => "MAN", when Psalm_151 => "PS2",
         when Matthew => "MAT", when Mark => "MRK", when Luke => "LUK", when John => "JHN",
         when Acts => "ACT", when Romans => "ROM", when First_Corinthians => "1CO",
         when Second_Corinthians => "2CO", when Galatians => "GAL", when Ephesians => "EPH",
         when Philippians => "PHP", when Colossians => "COL", when First_Thessalonians => "1TH",
         when Second_Thessalonians => "2TH", when First_Timothy => "1TI", when Second_Timothy => "2TI",
         when Titus => "TIT", when Philemon => "PHM", when Hebrews => "HEB", when James => "JAS",
         when First_Peter => "1PE", when Second_Peter => "2PE", when First_John => "1JN",
         when Second_John => "2JN", when Third_John => "3JN", when Jude => "JUD",
         when Revelation => "REV");

end Bible_Canon;
