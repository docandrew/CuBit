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

   ALIASES : constant := 325;
   subtype Alias_Index is Positive range 1 .. ALIASES;
   ALIAS_TEXT : constant String :=
     "genesisgengegnexodusexoexexodleviticuslevlelvnumbersnumnunmnbdeuteronomydeutdeudtdejoshuaj" &
     "oshjosjshjudgesjudgjdgjgjdgsruthrutrurth1samuel1sam1sa1sm1s2samuel2sam2sa2sm2s1kings1kgs1k" &
     "i1kg1k2kings2kgs2ki2kg2k1chronicles1chron1chr1ch2chronicles2chron2chr2chezraezrnehemiahneh" &
     "neestheresthestesjobjbpsalmspsalmpspsapsspsmproverbsprovproprvprecclesiastesecclesecclecce" &
     "cqohqohelethsongofsolomonsongofsongssongsngsoscanticlescantcanticleofcanticlesisaiahisaisj" &
     "eremiahjerjejrlamentationslamlaezekielezekezeezkdanieldandadnhoseahoshojoeljlamosamobadiah" &
     "obadobjonahjnhjonmicahmicmcnahumnahnahabakkukhabhbzephaniahzephzepzphaggaihaghgzechariahze" &
     "chzeczcmalachimalmltobittobtbjudithjdtjdthesthergreekgreekestheradditionstoestheraddesthes" &
     "grestofestherwisdomofsolomonwisdomwiswswisdsirachsirecclesiasticusecclusbaruchbarletterofj" &
     "eremiahljeepjerepistleofjeremiahepistleofjeremysongofthethreesongofthethreeyoungmensongoft" &
     "hreeprayerofazariahazariahazarsusannasusbelandthedragonbeldanielgreekgreekdanieldag1maccab" &
     "ees1macc1mac1ma2maccabees2macc2mac2ma3maccabees3macc3mac3ma4maccabees4macc4mac4ma1esdras1e" &
     "sd1es2esdras2esd2esprayerofmanassehprayerofmanassesmanassehmanassesmanprmanmatthewmattmatm" &
     "tmarkmrkmarmkmrlukeluklkjohnjhnjohjnactsactacactsoftheapostlesromansromrorm1corinthians1co" &
     "r1co2corinthians2cor2cogalatiansgalgaephesiansephephesphilippiansphilphpppcolossianscol1th" &
     "essalonians1thess1thes1th2thessalonians2thess2thes2th1timothy1tim1ti1tm2timothy2tim2ti2tmt" &
     "itustittiphilemonphilemphmphlmhebrewshebjamesjasjmjam1peter1pet1pe1pt1p2peter2pet2pe2pt2p1" &
     "john1jhn1jn1jo2john2jhn2jn2jo3john3jhn3jn3jojudejudjdrevelationrevelationsrevrervapocalyps" &
     "e";
   ALIAS_BOOK : constant array (Alias_Index) of Book :=
     [Genesis, Genesis, Genesis, Genesis, Exodus, Exodus, Exodus, Exodus, Leviticus, Leviticus,
      Leviticus, Leviticus, Numbers, Numbers, Numbers, Numbers, Numbers, Deuteronomy, Deuteronomy,
      Deuteronomy, Deuteronomy, Deuteronomy, Joshua, Joshua, Joshua, Joshua, Judges, Judges, Judges,
      Judges, Judges, Ruth, Ruth, Ruth, Ruth, First_Samuel, First_Samuel, First_Samuel, First_Samuel,
      First_Samuel, Second_Samuel, Second_Samuel, Second_Samuel, Second_Samuel, Second_Samuel,
      First_Kings, First_Kings, First_Kings, First_Kings, First_Kings, Second_Kings, Second_Kings,
      Second_Kings, Second_Kings, Second_Kings, First_Chronicles, First_Chronicles, First_Chronicles,
      First_Chronicles, Second_Chronicles, Second_Chronicles, Second_Chronicles, Second_Chronicles, Ezra,
      Ezra, Nehemiah, Nehemiah, Nehemiah, Esther, Esther, Esther, Esther, Job, Job, Psalms, Psalms,
      Psalms, Psalms, Psalms, Psalms, Proverbs, Proverbs, Proverbs, Proverbs, Proverbs, Ecclesiastes,
      Ecclesiastes, Ecclesiastes, Ecclesiastes, Ecclesiastes, Ecclesiastes, Ecclesiastes,
      Song_Of_Solomon, Song_Of_Solomon, Song_Of_Solomon, Song_Of_Solomon, Song_Of_Solomon,
      Song_Of_Solomon, Song_Of_Solomon, Song_Of_Solomon, Isaiah, Isaiah, Isaiah, Jeremiah, Jeremiah,
      Jeremiah, Jeremiah, Lamentations, Lamentations, Lamentations, Ezekiel, Ezekiel, Ezekiel, Ezekiel,
      Daniel, Daniel, Daniel, Daniel, Hosea, Hosea, Hosea, Joel, Joel, Amos, Amos, Obadiah, Obadiah,
      Obadiah, Jonah, Jonah, Jonah, Micah, Micah, Micah, Nahum, Nahum, Nahum, Habakkuk, Habakkuk,
      Habakkuk, Zephaniah, Zephaniah, Zephaniah, Zephaniah, Haggai, Haggai, Haggai, Zechariah, Zechariah,
      Zechariah, Zechariah, Malachi, Malachi, Malachi, Tobit, Tobit, Tobit, Judith, Judith, Judith,
      Esther_Greek, Esther_Greek, Esther_Greek, Esther_Greek, Esther_Greek, Esther_Greek, Wisdom, Wisdom,
      Wisdom, Wisdom, Wisdom, Sirach, Sirach, Sirach, Sirach, Baruch, Baruch, Letter_Of_Jeremiah,
      Letter_Of_Jeremiah, Letter_Of_Jeremiah, Letter_Of_Jeremiah, Letter_Of_Jeremiah, Song_Of_The_Three,
      Song_Of_The_Three, Song_Of_The_Three, Song_Of_The_Three, Song_Of_The_Three, Song_Of_The_Three,
      Susanna, Susanna, Bel_And_The_Dragon, Bel_And_The_Dragon, Daniel_Greek, Daniel_Greek, Daniel_Greek,
      First_Maccabees, First_Maccabees, First_Maccabees, First_Maccabees, Second_Maccabees,
      Second_Maccabees, Second_Maccabees, Second_Maccabees, Third_Maccabees, Third_Maccabees,
      Third_Maccabees, Third_Maccabees, Fourth_Maccabees, Fourth_Maccabees, Fourth_Maccabees,
      Fourth_Maccabees, First_Esdras, First_Esdras, First_Esdras, Second_Esdras, Second_Esdras,
      Second_Esdras, Prayer_Of_Manasseh, Prayer_Of_Manasseh, Prayer_Of_Manasseh, Prayer_Of_Manasseh,
      Prayer_Of_Manasseh, Prayer_Of_Manasseh, Matthew, Matthew, Matthew, Matthew, Mark, Mark, Mark, Mark,
      Mark, Luke, Luke, Luke, John, John, John, John, Acts, Acts, Acts, Acts, Romans, Romans, Romans,
      Romans, First_Corinthians, First_Corinthians, First_Corinthians, Second_Corinthians,
      Second_Corinthians, Second_Corinthians, Galatians, Galatians, Galatians, Ephesians, Ephesians,
      Ephesians, Philippians, Philippians, Philippians, Philippians, Colossians, Colossians,
      First_Thessalonians, First_Thessalonians, First_Thessalonians, First_Thessalonians,
      Second_Thessalonians, Second_Thessalonians, Second_Thessalonians, Second_Thessalonians,
      First_Timothy, First_Timothy, First_Timothy, First_Timothy, Second_Timothy, Second_Timothy,
      Second_Timothy, Second_Timothy, Titus, Titus, Titus, Philemon, Philemon, Philemon, Philemon,
      Hebrews, Hebrews, James, James, James, James, First_Peter, First_Peter, First_Peter, First_Peter,
      First_Peter, Second_Peter, Second_Peter, Second_Peter, Second_Peter, Second_Peter, First_John,
      First_John, First_John, First_John, Second_John, Second_John, Second_John, Second_John, Third_John,
      Third_John, Third_John, Third_John, Jude, Jude, Jude, Revelation, Revelation, Revelation,
      Revelation, Revelation, Revelation];
   ALIAS_START : constant array (Alias_Index) of Positive :=
     [1, 8, 11, 13, 15, 21, 24, 26, 30, 39, 42, 44, 46, 53, 56, 58, 60, 62, 73, 77, 80, 82, 84, 90, 94,
      97, 100, 106, 110, 113, 115, 119, 123, 126, 128, 131, 138, 142, 145, 148, 150, 157, 161, 164, 167,
      169, 175, 179, 182, 185, 187, 193, 197, 200, 203, 205, 216, 222, 226, 229, 240, 246, 250, 253, 257,
      260, 268, 271, 273, 279, 283, 286, 288, 291, 293, 299, 304, 306, 309, 312, 315, 323, 327, 330, 333,
      335, 347, 353, 357, 360, 362, 365, 373, 386, 397, 401, 404, 407, 416, 420, 439, 445, 448, 450, 458,
      461, 463, 465, 477, 480, 482, 489, 493, 496, 499, 505, 508, 510, 512, 517, 520, 522, 526, 528, 532,
      534, 541, 545, 547, 552, 555, 558, 563, 566, 568, 573, 576, 578, 586, 589, 591, 600, 604, 607, 609,
      615, 618, 620, 629, 633, 636, 638, 645, 648, 650, 655, 658, 660, 666, 669, 673, 684, 695, 712, 719,
      722, 734, 749, 755, 758, 760, 764, 770, 773, 787, 793, 799, 802, 818, 821, 826, 843, 858, 872, 894,
      905, 920, 927, 931, 938, 941, 956, 959, 970, 981, 984, 994, 999, 1003, 1006, 1016, 1021, 1025,
      1028, 1038, 1043, 1047, 1050, 1060, 1065, 1069, 1072, 1079, 1083, 1086, 1093, 1097, 1100, 1116,
      1132, 1140, 1148, 1151, 1156, 1163, 1167, 1170, 1172, 1176, 1179, 1182, 1184, 1186, 1190, 1193,
      1195, 1199, 1202, 1205, 1207, 1211, 1214, 1216, 1233, 1239, 1242, 1244, 1246, 1258, 1262, 1265,
      1277, 1281, 1284, 1293, 1296, 1298, 1307, 1310, 1315, 1326, 1330, 1333, 1335, 1345, 1348, 1362,
      1368, 1373, 1376, 1390, 1396, 1401, 1404, 1412, 1416, 1419, 1422, 1430, 1434, 1437, 1440, 1445,
      1448, 1450, 1458, 1464, 1467, 1471, 1478, 1481, 1486, 1489, 1491, 1494, 1500, 1504, 1507, 1510,
      1512, 1518, 1522, 1525, 1528, 1530, 1535, 1539, 1542, 1545, 1550, 1554, 1557, 1560, 1565, 1569,
      1572, 1575, 1579, 1582, 1584, 1594, 1605, 1608, 1610, 1612];
   ALIAS_LENGTH : constant array (Alias_Index) of Key_Length :=
     [7, 3, 2, 2, 6, 3, 2, 4, 9, 3, 2, 2, 7, 3, 2, 2, 2, 11, 4, 3, 2, 2, 6, 4, 3, 3, 6, 4, 3, 2, 4, 4, 3,
      2, 3, 7, 4, 3, 3, 2, 7, 4, 3, 3, 2, 6, 4, 3, 3, 2, 6, 4, 3, 3, 2, 11, 6, 4, 3, 11, 6, 4, 3, 4, 3,
      8, 3, 2, 6, 4, 3, 2, 3, 2, 6, 5, 2, 3, 3, 3, 8, 4, 3, 3, 2, 12, 6, 4, 3, 2, 3, 8, 13, 11, 4, 3, 3,
      9, 4, 19, 6, 3, 2, 8, 3, 2, 2, 12, 3, 2, 7, 4, 3, 3, 6, 3, 2, 2, 5, 3, 2, 4, 2, 4, 2, 7, 4, 2, 5,
      3, 3, 5, 3, 2, 5, 3, 2, 8, 3, 2, 9, 4, 3, 2, 6, 3, 2, 9, 4, 3, 2, 7, 3, 2, 5, 3, 2, 6, 3, 4, 11,
      11, 17, 7, 3, 12, 15, 6, 3, 2, 4, 6, 3, 14, 6, 6, 3, 16, 3, 5, 17, 15, 14, 22, 11, 15, 7, 4, 7, 3,
      15, 3, 11, 11, 3, 10, 5, 4, 3, 10, 5, 4, 3, 10, 5, 4, 3, 10, 5, 4, 3, 7, 4, 3, 7, 4, 3, 16, 16, 8,
      8, 3, 5, 7, 4, 3, 2, 4, 3, 3, 2, 2, 4, 3, 2, 4, 3, 3, 2, 4, 3, 2, 17, 6, 3, 2, 2, 12, 4, 3, 12, 4,
      3, 9, 3, 2, 9, 3, 5, 11, 4, 3, 2, 10, 3, 14, 6, 5, 3, 14, 6, 5, 3, 8, 4, 3, 3, 8, 4, 3, 3, 5, 3, 2,
      8, 6, 3, 4, 7, 3, 5, 3, 2, 3, 6, 4, 3, 3, 2, 6, 4, 3, 3, 2, 5, 4, 3, 3, 5, 4, 3, 3, 5, 4, 3, 3, 4,
      3, 2, 10, 11, 3, 2, 2, 10];

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
