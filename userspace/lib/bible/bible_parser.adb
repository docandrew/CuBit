package body Bible_Parser with SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Book names. A key is a name normalized: an ordinal as one digit, then
   --  the letters in lower case ("1 Cor." is "1cor", "Song of Songs" is
   --  "songofsongs"). Each alias is a key and the book it names; no key
   --  names two books (tests/bible checks the table).
   ---------------------------------------------------------------------------
   MAXIMUM_KEY : constant := 24;
   subtype Key_Length is Natural range 0 .. MAXIMUM_KEY;
   --  A shorter prefix is too ambiguous to be worth trying ("j").
   MINIMUM_PREFIX : constant := 2;

   ALIASES : constant := 455;
   subtype Alias_Index is Positive range 1 .. ALIASES;
   ALIAS_TEXT : constant String :=
     "genesisgengegnexodusexoexexodleviticuslevlelvnumbersnumnunmnbnumerideuteronomydeutdeudtded" &
     "euteronomiumjoshuajoshjosjshiosuejosueiosjudgesjudgjdgjgjdgsiudicumjudicumiudicesidcruthru" &
     "trurthrt1samuel1sam1sa1sm1s1regum1reg1regnorum1samuelis2samuel2sam2sa2sm2s2regum2reg2regno" &
     "rum2samuelis1kings1kgs1ki1kg1k3regum3reg3regnorum2kings2kgs2ki2kg2k4regum4reg4regnorum1chr" &
     "onicles1chron1chr1ch1paralipomenon1par1paralip2chronicles2chron2chr2ch2paralipomenon2par2p" &
     "aralipezraezr1esdraenehemiahnehne2esdraenehemiasestheresthesteshesterjobjbiobpsalmspsalmps" &
     "psapsspsmpsalmipsalmusliberpsalmorumproverbsprovproprvprproverbiaecclesiastesecclesecclecc" &
     "ecqohqohelethsongofsolomonsongofsongssongsngsoscanticlescantcanticleofcanticlescanticumcan" &
     "ticorumcanticumctisaiahisaisisaiasesaiasjeremiahjerjejrieremiasjeremiasierlamentationslaml" &
     "alamentationesthreniezekielezekezeezkezechielezdanieldandadnhoseahoshooseeosjoeljlioelamos" &
     "amobadiahobadobabdiasabdjonahjnhjonionasmicahmicmcmichaeasmichaeanahumnahnahabakkukhabhbha" &
     "bacuczephaniahzephzepzpsophoniassophhaggaihaghgaggaeusaggzechariahzechzeczczachariaszachma" &
     "lachimalmlmalachiastobittobtbtobiastobiaejudithjdtjdthiudithidtesthergreekgreekestheraddit" &
     "ionstoestheraddesthesgrestofestherwisdomofsolomonwisdomwiswswisdsapientiasapsirachsireccle" &
     "siasticusecclussiracidesecclibaruchbarletterofjeremiahljeepjerepistleofjeremiahepistleofje" &
     "remysongofthethreesongofthethreeyoungmensongofthreeprayerofazariahazariahazarsusannasusbel" &
     "andthedragonbeldanielgreekgreekdanieldag1maccabees1macc1mac1ma1machabaeorum1mach2maccabees" &
     "2macc2mac2ma2machabaeorum2mach3maccabees3macc3mac3ma4maccabees4macc4mac4ma1esdras1esd1es3e" &
     "sdrae2esdras2esd2es4esdraeprayerofmanassehprayerofmanassesmanassehmanassesmanprmanoratioma" &
     "nassematthewmattmatmtmatthaeusmatthaeummarkmrkmarmkmrmarcusmarcumlukeluklklucaslucamlcjohn" &
     "jhnjohjnioannesiohannesioannemiohannemioiohactsactacactsoftheapostlesactusapostolorumactus" &
     "romansromrormromanos1corinthians1cor1co1corinthios2corinthians2cor2co2corinthiosgalatiansg" &
     "algagalatasephesiansephephesephesiosphilippiansphilphpppphilippensescolossianscolcolossens" &
     "es1thessalonians1thess1thes1th1thessalonicenses2thessalonians2thess2thes2th2thessalonicens" &
     "es1timothy1tim1ti1tm1timotheum2timothy2tim2ti2tm2timotheumtitustittititumphilemonphilemphm" &
     "phlmphilemonemhebrewshebhebraeosjamesjasjmjamiacobijacobiiac1peter1pet1pe1pt1p1petri2peter" &
     "2pet2pe2pt2p2petri1john1jhn1jn1jo1ioannis1iohannis1io2john2jhn2jn2jo2ioannis2iohannis2io3j" &
     "ohn3jhn3jn3jo3ioannis3iohannis3iojudejudjdiudaejudaeiudasrevelationrevelationsrevrervapoca" &
     "lypseapocalypsisapoc";
   ALIAS_BOOK : constant array (Alias_Index) of Book :=
     [Genesis, Genesis, Genesis, Genesis, Exodus, Exodus, Exodus, Exodus, Leviticus, Leviticus,
      Leviticus, Leviticus, Numbers, Numbers, Numbers, Numbers, Numbers, Numbers, Deuteronomy,
      Deuteronomy, Deuteronomy, Deuteronomy, Deuteronomy, Deuteronomy, Joshua, Joshua, Joshua, Joshua,
      Joshua, Joshua, Joshua, Judges, Judges, Judges, Judges, Judges, Judges, Judges, Judges, Judges,
      Ruth, Ruth, Ruth, Ruth, Ruth, First_Samuel, First_Samuel, First_Samuel, First_Samuel, First_Samuel,
      First_Samuel, First_Samuel, First_Samuel, First_Samuel, Second_Samuel, Second_Samuel,
      Second_Samuel, Second_Samuel, Second_Samuel, Second_Samuel, Second_Samuel, Second_Samuel,
      Second_Samuel, First_Kings, First_Kings, First_Kings, First_Kings, First_Kings, First_Kings,
      First_Kings, First_Kings, Second_Kings, Second_Kings, Second_Kings, Second_Kings, Second_Kings,
      Second_Kings, Second_Kings, Second_Kings, First_Chronicles, First_Chronicles, First_Chronicles,
      First_Chronicles, First_Chronicles, First_Chronicles, First_Chronicles, Second_Chronicles,
      Second_Chronicles, Second_Chronicles, Second_Chronicles, Second_Chronicles, Second_Chronicles,
      Second_Chronicles, Ezra, Ezra, Ezra, Nehemiah, Nehemiah, Nehemiah, Nehemiah, Nehemiah, Esther,
      Esther, Esther, Esther, Esther, Job, Job, Job, Psalms, Psalms, Psalms, Psalms, Psalms, Psalms,
      Psalms, Psalms, Psalms, Proverbs, Proverbs, Proverbs, Proverbs, Proverbs, Proverbs, Ecclesiastes,
      Ecclesiastes, Ecclesiastes, Ecclesiastes, Ecclesiastes, Ecclesiastes, Ecclesiastes,
      Song_Of_Solomon, Song_Of_Solomon, Song_Of_Solomon, Song_Of_Solomon, Song_Of_Solomon,
      Song_Of_Solomon, Song_Of_Solomon, Song_Of_Solomon, Song_Of_Solomon, Song_Of_Solomon,
      Song_Of_Solomon, Isaiah, Isaiah, Isaiah, Isaiah, Isaiah, Jeremiah, Jeremiah, Jeremiah, Jeremiah,
      Jeremiah, Jeremiah, Jeremiah, Lamentations, Lamentations, Lamentations, Lamentations, Lamentations,
      Ezekiel, Ezekiel, Ezekiel, Ezekiel, Ezekiel, Ezekiel, Daniel, Daniel, Daniel, Daniel, Hosea, Hosea,
      Hosea, Hosea, Hosea, Joel, Joel, Joel, Amos, Amos, Obadiah, Obadiah, Obadiah, Obadiah, Obadiah,
      Jonah, Jonah, Jonah, Jonah, Micah, Micah, Micah, Micah, Micah, Nahum, Nahum, Nahum, Habakkuk,
      Habakkuk, Habakkuk, Habakkuk, Zephaniah, Zephaniah, Zephaniah, Zephaniah, Zephaniah, Zephaniah,
      Haggai, Haggai, Haggai, Haggai, Haggai, Zechariah, Zechariah, Zechariah, Zechariah, Zechariah,
      Zechariah, Malachi, Malachi, Malachi, Malachi, Tobit, Tobit, Tobit, Tobit, Tobit, Judith, Judith,
      Judith, Judith, Judith, Esther_Greek, Esther_Greek, Esther_Greek, Esther_Greek, Esther_Greek,
      Esther_Greek, Wisdom, Wisdom, Wisdom, Wisdom, Wisdom, Wisdom, Wisdom, Sirach, Sirach, Sirach,
      Sirach, Sirach, Sirach, Baruch, Baruch, Letter_Of_Jeremiah, Letter_Of_Jeremiah, Letter_Of_Jeremiah,
      Letter_Of_Jeremiah, Letter_Of_Jeremiah, Song_Of_The_Three, Song_Of_The_Three, Song_Of_The_Three,
      Song_Of_The_Three, Song_Of_The_Three, Song_Of_The_Three, Susanna, Susanna, Bel_And_The_Dragon,
      Bel_And_The_Dragon, Daniel_Greek, Daniel_Greek, Daniel_Greek, First_Maccabees, First_Maccabees,
      First_Maccabees, First_Maccabees, First_Maccabees, First_Maccabees, Second_Maccabees,
      Second_Maccabees, Second_Maccabees, Second_Maccabees, Second_Maccabees, Second_Maccabees,
      Third_Maccabees, Third_Maccabees, Third_Maccabees, Third_Maccabees, Fourth_Maccabees,
      Fourth_Maccabees, Fourth_Maccabees, Fourth_Maccabees, First_Esdras, First_Esdras, First_Esdras,
      First_Esdras, Second_Esdras, Second_Esdras, Second_Esdras, Second_Esdras, Prayer_Of_Manasseh,
      Prayer_Of_Manasseh, Prayer_Of_Manasseh, Prayer_Of_Manasseh, Prayer_Of_Manasseh, Prayer_Of_Manasseh,
      Prayer_Of_Manasseh, Matthew, Matthew, Matthew, Matthew, Matthew, Matthew, Mark, Mark, Mark, Mark,
      Mark, Mark, Mark, Luke, Luke, Luke, Luke, Luke, Luke, John, John, John, John, John, John, John,
      John, John, John, Acts, Acts, Acts, Acts, Acts, Acts, Romans, Romans, Romans, Romans, Romans,
      First_Corinthians, First_Corinthians, First_Corinthians, First_Corinthians, Second_Corinthians,
      Second_Corinthians, Second_Corinthians, Second_Corinthians, Galatians, Galatians, Galatians,
      Galatians, Ephesians, Ephesians, Ephesians, Ephesians, Philippians, Philippians, Philippians,
      Philippians, Philippians, Colossians, Colossians, Colossians, First_Thessalonians,
      First_Thessalonians, First_Thessalonians, First_Thessalonians, First_Thessalonians,
      Second_Thessalonians, Second_Thessalonians, Second_Thessalonians, Second_Thessalonians,
      Second_Thessalonians, First_Timothy, First_Timothy, First_Timothy, First_Timothy, First_Timothy,
      Second_Timothy, Second_Timothy, Second_Timothy, Second_Timothy, Second_Timothy, Titus, Titus,
      Titus, Titus, Philemon, Philemon, Philemon, Philemon, Philemon, Hebrews, Hebrews, Hebrews, James,
      James, James, James, James, James, James, First_Peter, First_Peter, First_Peter, First_Peter,
      First_Peter, First_Peter, Second_Peter, Second_Peter, Second_Peter, Second_Peter, Second_Peter,
      Second_Peter, First_John, First_John, First_John, First_John, First_John, First_John, First_John,
      Second_John, Second_John, Second_John, Second_John, Second_John, Second_John, Second_John,
      Third_John, Third_John, Third_John, Third_John, Third_John, Third_John, Third_John, Jude, Jude,
      Jude, Jude, Jude, Jude, Revelation, Revelation, Revelation, Revelation, Revelation, Revelation,
      Revelation, Revelation];
   ALIAS_START : constant array (Alias_Index) of Positive :=
     [1, 8, 11, 13, 15, 21, 24, 26, 30, 39, 42, 44, 46, 53, 56, 58, 60, 62, 68, 79, 83, 86, 88, 90, 103,
      109, 113, 116, 119, 124, 129, 132, 138, 142, 145, 147, 151, 158, 165, 172, 175, 179, 182, 184, 187,
      189, 196, 200, 203, 206, 208, 214, 218, 227, 236, 243, 247, 250, 253, 255, 261, 265, 274, 283, 289,
      293, 296, 299, 301, 307, 311, 320, 326, 330, 333, 336, 338, 344, 348, 357, 368, 374, 378, 381, 395,
      399, 407, 418, 424, 428, 431, 445, 449, 457, 461, 464, 471, 479, 482, 484, 491, 499, 505, 509, 512,
      514, 520, 523, 525, 528, 534, 539, 541, 544, 547, 550, 556, 563, 577, 585, 589, 592, 595, 597, 606,
      618, 624, 628, 631, 633, 636, 644, 657, 668, 672, 675, 678, 687, 691, 710, 728, 736, 738, 744, 747,
      749, 755, 761, 769, 772, 774, 776, 784, 792, 795, 807, 810, 812, 825, 831, 838, 842, 845, 848, 856,
      858, 864, 867, 869, 871, 876, 879, 881, 885, 887, 891, 893, 897, 901, 903, 910, 914, 916, 922, 925,
      930, 933, 936, 941, 946, 949, 951, 959, 966, 971, 974, 976, 984, 987, 989, 996, 1005, 1009, 1012,
      1014, 1023, 1027, 1033, 1036, 1038, 1045, 1048, 1057, 1061, 1064, 1066, 1075, 1079, 1086, 1089,
      1091, 1100, 1105, 1108, 1110, 1116, 1122, 1128, 1131, 1135, 1141, 1144, 1155, 1166, 1183, 1190,
      1193, 1205, 1220, 1226, 1229, 1231, 1235, 1244, 1247, 1253, 1256, 1270, 1276, 1285, 1290, 1296,
      1299, 1315, 1318, 1323, 1340, 1355, 1369, 1391, 1402, 1417, 1424, 1428, 1435, 1438, 1453, 1456,
      1467, 1478, 1481, 1491, 1496, 1500, 1503, 1516, 1521, 1531, 1536, 1540, 1543, 1556, 1561, 1571,
      1576, 1580, 1583, 1593, 1598, 1602, 1605, 1612, 1616, 1619, 1626, 1633, 1637, 1640, 1647, 1663,
      1679, 1687, 1695, 1698, 1703, 1716, 1723, 1727, 1730, 1732, 1741, 1750, 1754, 1757, 1760, 1762,
      1764, 1770, 1776, 1780, 1783, 1785, 1790, 1795, 1797, 1801, 1804, 1807, 1809, 1816, 1824, 1831,
      1839, 1841, 1844, 1848, 1851, 1853, 1870, 1886, 1891, 1897, 1900, 1902, 1904, 1911, 1923, 1927,
      1930, 1941, 1953, 1957, 1960, 1971, 1980, 1983, 1985, 1992, 2001, 2004, 2009, 2017, 2028, 2032,
      2035, 2037, 2049, 2059, 2062, 2073, 2087, 2093, 2098, 2101, 2118, 2132, 2138, 2143, 2146, 2163,
      2171, 2175, 2178, 2181, 2191, 2199, 2203, 2206, 2209, 2219, 2224, 2227, 2229, 2234, 2242, 2248,
      2251, 2255, 2265, 2272, 2275, 2283, 2288, 2291, 2293, 2296, 2302, 2308, 2311, 2317, 2321, 2324,
      2327, 2329, 2335, 2341, 2345, 2348, 2351, 2353, 2359, 2364, 2368, 2371, 2374, 2382, 2391, 2394,
      2399, 2403, 2406, 2409, 2417, 2426, 2429, 2434, 2438, 2441, 2444, 2452, 2461, 2464, 2468, 2471,
      2473, 2478, 2483, 2488, 2498, 2509, 2512, 2514, 2516, 2526, 2537];
   ALIAS_LENGTH : constant array (Alias_Index) of Key_Length :=
     [7, 3, 2, 2, 6, 3, 2, 4, 9, 3, 2, 2, 7, 3, 2, 2, 2, 6, 11, 4, 3, 2, 2, 13, 6, 4, 3, 3, 5, 5, 3, 6,
      4, 3, 2, 4, 7, 7, 7, 3, 4, 3, 2, 3, 2, 7, 4, 3, 3, 2, 6, 4, 9, 9, 7, 4, 3, 3, 2, 6, 4, 9, 9, 6, 4,
      3, 3, 2, 6, 4, 9, 6, 4, 3, 3, 2, 6, 4, 9, 11, 6, 4, 3, 14, 4, 8, 11, 6, 4, 3, 14, 4, 8, 4, 3, 7, 8,
      3, 2, 7, 8, 6, 4, 3, 2, 6, 3, 2, 3, 6, 5, 2, 3, 3, 3, 6, 7, 14, 8, 4, 3, 3, 2, 9, 12, 6, 4, 3, 2,
      3, 8, 13, 11, 4, 3, 3, 9, 4, 19, 18, 8, 2, 6, 3, 2, 6, 6, 8, 3, 2, 2, 8, 8, 3, 12, 3, 2, 13, 6, 7,
      4, 3, 3, 8, 2, 6, 3, 2, 2, 5, 3, 2, 4, 2, 4, 2, 4, 4, 2, 7, 4, 2, 6, 3, 5, 3, 3, 5, 5, 3, 2, 8, 7,
      5, 3, 2, 8, 3, 2, 7, 9, 4, 3, 2, 9, 4, 6, 3, 2, 7, 3, 9, 4, 3, 2, 9, 4, 7, 3, 2, 9, 5, 3, 2, 6, 6,
      6, 3, 4, 6, 3, 11, 11, 17, 7, 3, 12, 15, 6, 3, 2, 4, 9, 3, 6, 3, 14, 6, 9, 5, 6, 3, 16, 3, 5, 17,
      15, 14, 22, 11, 15, 7, 4, 7, 3, 15, 3, 11, 11, 3, 10, 5, 4, 3, 13, 5, 10, 5, 4, 3, 13, 5, 10, 5, 4,
      3, 10, 5, 4, 3, 7, 4, 3, 7, 7, 4, 3, 7, 16, 16, 8, 8, 3, 5, 13, 7, 4, 3, 2, 9, 9, 4, 3, 3, 2, 2, 6,
      6, 4, 3, 2, 5, 5, 2, 4, 3, 3, 2, 7, 8, 7, 8, 2, 3, 4, 3, 2, 17, 16, 5, 6, 3, 2, 2, 7, 12, 4, 3, 11,
      12, 4, 3, 11, 9, 3, 2, 7, 9, 3, 5, 8, 11, 4, 3, 2, 12, 10, 3, 11, 14, 6, 5, 3, 17, 14, 6, 5, 3, 17,
      8, 4, 3, 3, 10, 8, 4, 3, 3, 10, 5, 3, 2, 5, 8, 6, 3, 4, 10, 7, 3, 8, 5, 3, 2, 3, 6, 6, 3, 6, 4, 3,
      3, 2, 6, 6, 4, 3, 3, 2, 6, 5, 4, 3, 3, 8, 9, 3, 5, 4, 3, 3, 8, 9, 3, 5, 4, 3, 3, 8, 9, 3, 4, 3, 2,
      5, 5, 5, 10, 11, 3, 2, 2, 10, 11, 4];

   type Key is record
      Length : Key_Length := 0;
      Text   : String (1 .. MAXIMUM_KEY) := [others => ' '];
   end record;

   function Alias_Fits (I : Alias_Index) return Boolean is
     (ALIAS_TEXT'First = 1 and then ALIAS_START (I) <= ALIAS_TEXT'Last
      and then ALIAS_LENGTH (I) <= ALIAS_TEXT'Last - ALIAS_START (I) + 1);

   --  Alias I is Item (Whole), or starts with it.
   function Matches (I : Alias_Index; Item : Key; Whole : Boolean) return Boolean is
     (Alias_Fits (I) and then Item.Length <= ALIAS_LENGTH (I)
      and then (if Whole then Item.Length = ALIAS_LENGTH (I))
      and then (for all J in 1 .. Item.Length => ALIAS_TEXT (ALIAS_START (I) + J - 1) = Item.Text (J)));

   procedure Find_Key (Item : Key; Found_Book : out Book; Found, Ambiguous : out Boolean) is
   begin
      Found_Book := Genesis;
      Found := False;
      Ambiguous := False;
      if Item.Length = 0 then
         return;
      end if;
      for I in Alias_Index loop
         if Matches (I, Item, Whole => True) then
            Found_Book := ALIAS_BOOK (I);
            Found := True;
            return;
         end if;
      end loop;
      if Item.Length < MINIMUM_PREFIX then
         return;
      end if;
      for I in Alias_Index loop
         if Matches (I, Item, Whole => False) then
            if not Found then
               Found_Book := ALIAS_BOOK (I);
               Found := True;
            elsif ALIAS_BOOK (I) /= Found_Book then
               Found := False;
               Ambiguous := True;
               return;
            end if;
         end if;
      end loop;
   end Find_Key;

   ---------------------------------------------------------------------------
   --  Characters.
   ---------------------------------------------------------------------------
   function Is_Letter (C : Character) return Boolean is (C in 'a' .. 'z' | 'A' .. 'Z');
   function Is_Digit (C : Character) return Boolean is (C in '0' .. '9');
   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);
   --  Ignored inside a book's name: "1 Cor.", "Song of Songs", "Esther (Greek)".
   function Is_Name_Filler (C : Character) return Boolean is (C in ' ' | '.' | '(' | ')');
   function Is_Space (C : Character) return Boolean is (C in ' ' | ASCII.HT);

   --  The input, copied to 1-based indices (Parse's positions are mapped
   --  back to Text's).
   subtype Text_Length is Natural range 0 .. MAXIMUM_TEXT;
   subtype Text_Index is Positive range 1 .. MAXIMUM_TEXT;
   subtype Cursor is Positive range 1 .. MAXIMUM_TEXT + 1;
   type Buffer is record
      Length : Text_Length := 0;
      Text   : String (Text_Index) := [others => ' '];
   end record;

   function At_End (B : Buffer; P : Cursor) return Boolean is (P > B.Length);

   procedure Skip_Spaces (B : Buffer; P : in out Cursor)
     with Post => P >= P'Old
   is
   begin
      while P <= B.Length and then Is_Space (B.Text (P)) loop
         pragma Loop_Invariant (P >= P'Loop_Entry);
         P := P + 1;
      end loop;
   end Skip_Spaces;

   --  A dash at P: "-", or an en or em dash in UTF-8; Width is its bytes.
   EN_DASH_LAST : constant Character := Character'Val (16#93#);
   EM_DASH_LAST : constant Character := Character'Val (16#94#);
   function Dash_Width (B : Buffer; P : Cursor) return Natural is
     (if P > B.Length then 0
      elsif B.Text (P) = '-' then 1
      elsif P <= B.Length - 2 and then B.Text (P) = Character'Val (16#E2#)
        and then B.Text (P + 1) = Character'Val (16#80#)
        and then B.Text (P + 2) in EN_DASH_LAST | EM_DASH_LAST then 3
      else 0)
   with Post => Dash_Width'Result <= 3 and then (if Dash_Width'Result > 0 then P + Dash_Width'Result - 1 <= B.Length);

   function Is_Separator (B : Buffer; P : Cursor) return Boolean is
     (P <= B.Length and then B.Text (P) in ':' | '.');

   type Read_Status is (Read_Number, No_Digits, Too_Large);

   --  Decimal digits at P (at most MAXIMUM_NUMBER).
   procedure Read (B : Buffer; P : in out Cursor; Value : out Number; Outcome : out Read_Status)
     with Post => P >= P'Old
   is
      Any : Boolean := False;
   begin
      Value := 0;
      Outcome := Read_Number;
      while P <= B.Length and then Is_Digit (B.Text (P)) loop
         pragma Loop_Invariant (P >= P'Loop_Entry and then Value <= MAXIMUM_NUMBER);
         if Value > (MAXIMUM_NUMBER - 9) / 10 then
            Outcome := Too_Large;
            return;
         end if;
         Value := Value * 10 + (Character'Pos (B.Text (P)) - Character'Pos ('0'));
         Any := True;
         P := P + 1;
      end loop;
      if not Any then
         Outcome := No_Digits;
      end if;
   end Read;

   ---------------------------------------------------------------------------
   --  The book part: an optional ordinal, then letters, spaces and periods,
   --  up to the first digit or punctuation after them.
   ---------------------------------------------------------------------------
   --  "first" .. "fourth", "iv" .. "i", at P, followed by a space or a
   --  period: the ordinal's length and digit (Length 0: none).
   procedure Ordinal_At (B : Buffer; P : Cursor; Length : out Natural; Digit : out Character)
     with Post => Length <= 6 and then (if Length > 0 then P + Length <= B.Length)
   is
      function Word_Is (Word : String) return Boolean is
        (Word'Length in 1 .. 6 and then Word'First = 1 and then P <= B.Length
         and then Word'Length < B.Length - P + 1
         and then (for all J in Word'Range => Lower (B.Text (P + J - 1)) = Word (J))
         and then (B.Text (P + Word'Length) = ' ' or else B.Text (P + Word'Length) = '.'))
      with Post => (if Word_Is'Result then P + Word'Length <= B.Length);
      type Candidate is record
         Word  : String (1 .. 6);
         Size  : Positive;
         Digit : Character;
      end record;
      CANDIDATES : constant array (1 .. 8) of Candidate :=
        [("first ", 5, '1'), ("second", 6, '2'), ("third ", 5, '3'), ("fourth", 6, '4'),
         ("iii   ", 3, '3'), ("ii    ", 2, '2'), ("iv    ", 2, '4'), ("i     ", 1, '1')];
   begin
      Length := 0;
      Digit := '0';
      for C of CANDIDATES loop
         if C.Size <= 6 and then Word_Is (C.Word (1 .. C.Size)) then
            Length := C.Size;
            Digit := C.Digit;
            return;
         end if;
      end loop;
   end Ordinal_At;

   --  Normalize the book part B.Text (First .. Last) into Item. Valid False:
   --  no letters, or a key too long for any book.
   procedure Book_Key (B : Buffer; First, Last : Cursor; Item : out Key; Valid : out Boolean)
     with Pre => First <= Last and then Last <= B.Length
   is
      P : Cursor := First;
      Letters : Boolean := False;
      Ordinal_Length : Natural;
      Digit : Character;
      procedure Append (C : Character) is
      begin
         if Item.Length < MAXIMUM_KEY then
            Item.Length := Item.Length + 1;
            Item.Text (Item.Length) := C;
         else
            Valid := False;
         end if;
      end Append;
   begin
      Item := (others => <>);
      Valid := True;
      if Is_Digit (B.Text (P)) then
         Append (B.Text (P));
         P := P + 1;
         --  "1st", "2nd", "3rd", "4th".
         if P < Last and then Is_Letter (B.Text (P)) and then Is_Letter (B.Text (P + 1))
           and then (Lower (B.Text (P)) & Lower (B.Text (P + 1))) in "st" | "nd" | "rd" | "th"
           and then (P + 1 = Last or else Is_Name_Filler (B.Text (P + 2)))
         then
            P := P + 2;
         end if;
      else
         Ordinal_At (B, P, Ordinal_Length, Digit);
         if Ordinal_Length > 0 and then P + Ordinal_Length <= Last then
            Append (Digit);
            P := P + Ordinal_Length;
         end if;
      end if;
      while P <= Last loop
         pragma Loop_Invariant (P >= First and then Item.Length <= MAXIMUM_KEY);
         if Is_Letter (B.Text (P)) then
            Append (Lower (B.Text (P)));
            Letters := True;
         elsif not Is_Name_Filler (B.Text (P)) then
            Valid := False;
         end if;
         exit when P = Last;
         P := P + 1;
      end loop;
      Valid := Valid and then Letters;
   end Book_Key;

   procedure Find_Book (Name : String; Item : out Book; Found, Ambiguous : out Boolean) is
      B : Buffer;
      Matched : Key;
      Valid : Boolean;
   begin
      Item := Genesis;
      Found := False;
      Ambiguous := False;
      if Name'Length = 0 then
         return;
      end if;
      B.Length := Name'Length;
      for I in 1 .. Name'Length loop
         pragma Loop_Invariant (B.Length = Name'Length);
         B.Text (I) := Name (Name'First - 1 + I);
      end loop;
      Book_Key (B, 1, B.Length, Matched, Valid);
      if Valid then
         Find_Key (Matched, Item, Found, Ambiguous);
      end if;
   end Find_Book;

   ---------------------------------------------------------------------------
   --  Parsing.
   ---------------------------------------------------------------------------

   --  What the numbers after the book say, before the versification.
   type Shape is
     (Whole_Book,      --  "Obadiah"
      Whole_Chapter,   --  "John 3"         C1
      One_Verse,       --  "John 3:16"      C1:V1
      Verse_Range,     --  "John 3:16-18"   C1:V1-V2
      Chapter_Range,   --  "John 3-4"       C1-C2
      Span);           --  "John 3:16-4:2"  C1:V1-C2:V2 ("John 3-4:2" from V1 = 1)

   type Numbers is record
      Form : Shape := Whole_Book;
      C1, V1, C2, V2 : Number := 0;
   end record;

   procedure Parse (Text : String; Table : Versification; Item : out Result) is
      B : Buffer;
      P : Cursor := 1;
      Book_Start, Book_End : Cursor;
      Letters_Seen : Boolean := False;
      Matched : Key;
      Valid, Found, Ambiguous : Boolean;
      Named : Book := Genesis;
      N : Numbers;
      Value : Number;
      Width : Natural;

      --  Text's index of buffer position Q.
      function Original (Q : Text_Index) return Positive is (Text'First - 1 + Q)
        with Pre => Q <= B.Length and then B.Length = Text'Length;

      procedure Fail (Outcome : Status; At_Position : Text_Index)
        with Pre => At_Position <= B.Length and then B.Length = Text'Length
                    and then Outcome in Malformed | Number_Too_Large,
             Post => Item.Outcome = Outcome and then Item.Position in Text'Range
      is
      begin
         Item.Outcome := Outcome;
         Item.Position := Original (At_Position);
      end Fail;

      --  The end of the text, or a dash or separator, is next.
      function Clean_End return Boolean is (At_End (B, P));

      --  Read a number at P after spaces into Value; False (Item set) when
      --  there is none.
      procedure Number_At (Read_Ok : out Boolean)
        with Pre  => B.Length = Text'Length,
             Post => (if not Read_Ok then Item.Outcome in Malformed | Number_Too_Large
                                          and then Item.Position in Text'Range)
      is
         Start : Cursor;
         Got : Read_Status;
      begin
         Skip_Spaces (B, P);
         Start := P;
         Read (B, P, Value, Got);
         Read_Ok := Got = Read_Number;
         if not Read_Ok then
            if Start <= B.Length then
               Fail ((if Got = Too_Large then Number_Too_Large else Malformed), Start);
            elsif B.Length > 0 then
               Fail (Malformed, B.Length);
            else
               Item.Outcome := Empty_Text;
               Read_Ok := True;   --  unreachable: the text has a book
               Value := 0;
            end if;
         end if;
      end Number_At;

      Read_Ok : Boolean;
   begin
      Item := (others => <>);
      if Text'Length > MAXIMUM_TEXT then
         Item.Outcome := Text_Too_Long;
         return;
      end if;
      B.Length := Text'Length;
      for I in 1 .. B.Length loop
         pragma Loop_Invariant (B.Length = Text'Length);
         B.Text (I) := Text (Text'First - 1 + I);
      end loop;

      Skip_Spaces (B, P);
      if At_End (B, P) then
         Item.Outcome := Empty_Text;
         return;
      end if;

      --  The book part.
      Book_Start := P;
      if Is_Digit (B.Text (P)) then
         P := P + 1;   --  an ordinal; Book_Key checks it is followed by letters
      end if;
      while P <= B.Length and then (Is_Letter (B.Text (P)) or else Is_Name_Filler (B.Text (P))) loop
         pragma Loop_Invariant (P >= Book_Start and then (if Letters_Seen then P > Book_Start));
         Letters_Seen := Letters_Seen or else Is_Letter (B.Text (P));
         P := P + 1;
      end loop;
      if not Letters_Seen then
         Fail (Malformed, Book_Start);
         return;
      end if;
      pragma Assert (P > Book_Start);
      Book_End := P - 1;
      --  Trailing spaces and periods are not part of the name shown.
      while Book_End > Book_Start and then Is_Name_Filler (B.Text (Book_End)) loop
         Book_End := Book_End - 1;
      end loop;
      Book_Key (B, Book_Start, Book_End, Matched, Valid);
      if Valid then
         Find_Key (Matched, Named, Found, Ambiguous);
      else
         Found := False;
         Ambiguous := False;
      end if;
      if not Found then
         Item.Outcome := (if Ambiguous then Ambiguous_Book else Unknown_Book);
         Item.Book_First := Original (Book_Start);
         Item.Book_Last := Original (Book_End);
         return;
      end if;
      Item.Book := Named;

      --  The numbers.
      Skip_Spaces (B, P);
      if not Clean_End then
         Number_At (Read_Ok);
         if not Read_Ok then return; end if;
         N := (Form => Whole_Chapter, C1 => Value, others => 0);
         --  "Psalm 151 1:1": the separate book Psalm 151, by its name.
         if Named = Psalms and then Value = 151 and then P <= B.Length and then Is_Space (B.Text (P)) then
            Skip_Spaces (B, P);
            if P <= B.Length and then Is_Digit (B.Text (P)) then
               Named := Psalm_151;
               Item.Book := Named;
               Number_At (Read_Ok);
               if not Read_Ok then return; end if;
               N := (Form => Whole_Chapter, C1 => Value, others => 0);
            end if;
         end if;
         Skip_Spaces (B, P);
         if Is_Separator (B, P) then
            P := P + 1;
            Number_At (Read_Ok);
            if not Read_Ok then return; end if;
            N := (Form => One_Verse, C1 => N.C1, V1 => Value, others => 0);
         end if;
         Skip_Spaces (B, P);
         Width := Dash_Width (B, P);
         if Width > 0 then
            P := P + Width;
            Number_At (Read_Ok);
            if not Read_Ok then return; end if;
            Skip_Spaces (B, P);
            if Is_Separator (B, P) then
               P := P + 1;
               N.C2 := Value;
               Number_At (Read_Ok);
               if not Read_Ok then return; end if;
               N := (Form => Span, C1 => N.C1, V1 => (if N.Form = One_Verse then N.V1 else 1), C2 => N.C2,
                     V2 => Value);
            elsif N.Form = One_Verse then
               N := (Form => Verse_Range, C1 => N.C1, V1 => N.V1, C2 => N.C1, V2 => Value);
            else
               N := (Form => Chapter_Range, C1 => N.C1, C2 => Value, others => 0);
            end if;
         end if;
         Skip_Spaces (B, P);
         if not Clean_End then
            Fail (Malformed, P);
            return;
         end if;
      end if;

      --  The versification. Psalms has at most 150 chapters
      --  (MAXIMUM_CHAPTER): "Psalm 151" is the separate book, if any.
      if Named = Psalms and then N.Form in Whole_Chapter | One_Verse | Verse_Range and then N.C1 = 151
        and then Has_Book (Table, Psalm_151)
      then
         Named := Psalm_151;
         Item.Book := Named;
         N.C1 := 1;
         N.C2 := 1;
      end if;
      if not Has_Book (Table, Named) then
         Item.Outcome := Book_Not_In_Translation;
         return;
      end if;
      --  A lone number in a one-chapter book is a verse ("Jude 5").
      if Table.Chapters (Named) = 1 then
         if N.Form = Whole_Chapter and then N.C1 > 1 then
            N := (Form => One_Verse, C1 => 1, V1 => N.C1, others => 0);
         elsif N.Form = Chapter_Range then
            N := (Form => Verse_Range, C1 => 1, V1 => N.C1, C2 => 1, V2 => N.C2);
         end if;
      end if;
      case N.Form is
         when Whole_Book =>
            --  From the first chapter with verses (the King James Version's
            --  Additions to Esther start at chapter 10).
            N := (Form => Span, C1 => 1, V1 => 1, C2 => Table.Chapters (Named), V2 => 0);
            for C in 1 .. Table.Chapters (Named) loop
               if Verses_In (Table, Named, C) > 0 then
                  N.C1 := C;
                  exit;
               end if;
            end loop;
         when Whole_Chapter =>
            N := (Form => Span, C1 => N.C1, V1 => 1, C2 => N.C1, V2 => 0);
         when One_Verse =>
            N := (Form => Span, C1 => N.C1, V1 => N.V1, C2 => N.C1, V2 => N.V1);
         when Verse_Range =>
            N := (Form => Span, C1 => N.C1, V1 => N.V1, C2 => N.C1, V2 => N.V2);
         when Chapter_Range =>
            N := (Form => Span, C1 => N.C1, V1 => 1, C2 => N.C2, V2 => 0);
         when Span =>
            null;
      end case;
      --  V2 = 0: to the end of chapter C2.
      if N.C1 = 0 or else N.C1 > Table.Chapters (Named) then
         Item.Outcome := Chapter_Out_Of_Range;
         Item.Value := N.C1;
         Item.Limit := Table.Chapters (Named);
         return;
      elsif N.C2 = 0 or else N.C2 > Table.Chapters (Named) then
         Item.Outcome := Chapter_Out_Of_Range;
         Item.Value := N.C2;
         Item.Limit := Table.Chapters (Named);
         return;
      end if;
      pragma Assert (N.C1 in 1 .. Table.Chapters (Named) and then N.C2 in 1 .. Table.Chapters (Named));
      if N.V2 = 0 then
         N.V2 := Verses_In (Table, Named, N.C2);
      end if;
      if N.V1 = 0 or else N.V1 > Verses_In (Table, Named, N.C1) then
         Item.Outcome := Verse_Out_Of_Range;
         Item.Value := N.V1;
         Item.Limit := Verses_In (Table, Named, N.C1);
         Item.Chapter := N.C1;
         return;
      end if;
      if N.V2 = 0 or else N.V2 > Verses_In (Table, Named, N.C2) then
         Item.Outcome := Verse_Out_Of_Range;
         Item.Value := N.V2;
         Item.Limit := Verses_In (Table, Named, N.C2);
         Item.Chapter := N.C2;
         return;
      end if;
      Item.Passage := (From => (Named, N.C1, N.V1), To => (Named, N.C2, N.V2));
      if not Ordered (Item.Passage) then
         Item.Outcome := Reversed_Range;
         return;
      end if;
      Item.Outcome := Parsed;
   end Parse;

   ---------------------------------------------------------------------------
   --  Messages.
   ---------------------------------------------------------------------------
   MAXIMUM_SHOWN : constant := 40;

   function Decimal (Value : Number) return String renames Bible_References.Number_Image;

   function Quoted (Text : String; First, Last : Natural) return String is
     (if First in Text'Range and then Last in Text'Range and then First <= Last
         and then Last - First < MAXIMUM_SHOWN
      then """" & Text (First .. Last) & """" else "that name");

   function Plural (Count : Number; One, Many : String) return String is
     (Decimal (Count) & " " & (if Count = 1 then One else Many))
   with Pre  => One'Length <= 16 and then Many'Length <= 16,
        Post => Plural'Result'First = 1 and then Plural'Result'Length <= 20;

   function Describe (Text : String; Item : Result) return String is
     (case Item.Outcome is
         when Parsed => "",
         when Empty_Text => "Type a reference, such as John 3:16",
         when Text_Too_Long => "That is too long to be a reference",
         when Unknown_Book => "No book is called " & Quoted (Text, Item.Book_First, Item.Book_Last),
         when Ambiguous_Book =>
            Quoted (Text, Item.Book_First, Item.Book_Last) & " could be more than one book",
         when Malformed => "Expected a chapter and verse, such as 3:16",
         when Number_Too_Large => "That number is too large for a chapter or verse",
         when Book_Not_In_Translation => Name (Item.Book) & " is not in this translation",
         when Chapter_Out_Of_Range =>
            Name (Item.Book) & " has " & Plural (Item.Limit, "chapter", "chapters"),
         when Verse_Out_Of_Range =>
            Name (Item.Book) & " " & Decimal (Item.Chapter) & " has " & Plural (Item.Limit, "verse", "verses"),
         when Reversed_Range => "That range ends before it starts");

end Bible_Parser;
