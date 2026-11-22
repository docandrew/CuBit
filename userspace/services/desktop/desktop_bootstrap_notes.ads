pragma Ada_2022;
--  What the Desktop bootstrap screen shows besides its fixed text (CAP-001
--  NUC diagnostics): the latest Warning-or-worse log records and the
--  kernel's physical-map records, bounded, so one photo explains a stall.
package Desktop_Bootstrap_Notes with SPARK_Mode is
   Line_Bytes      : constant := 150;
   Recent_Capacity : constant := 8;
   Map_Capacity    : constant := 24;
   Map_Prefix      : constant String := "kernel: physical map";
   --  Logstore numbers each kernel denial record: "...denied #<N> ...".
   Denial_Prefix   : constant String := "kernel: device-memory denied #";
   --  And each process the kernel killed: "kernel: process killed #<N> ...".
   Kill_Prefix     : constant String := "kernel: process killed #";
   --  The latest kill records, shown apart from the other warnings.
   Kill_Capacity   : constant := 4;
   --  The render driver's records: the latest one is shown (its startup
   --  steps carry t=..., so a stalled step is visible).
   Driver_Prefix   : constant String := "intel-gpu:";
   --  Record numbers beyond this are kept as this (never wrap).
   Number_Limit    : constant := 1_000_000_000;
   subtype Record_Number is Natural range 0 .. Number_Limit;

   subtype Line_Length is Natural range 0 .. Line_Bytes;
   type Line is record
      Text   : String (1 .. Line_Bytes) := [others => ' '];
      Length : Line_Length := 0;
   end record;
   subtype Recent_Count is Natural range 0 .. Recent_Capacity;
   subtype Map_Count is Natural range 0 .. Map_Capacity;
   type Recent_Lines is array (1 .. Recent_Capacity) of Line;
   type Map_Lines is array (1 .. Map_Capacity) of Line;
   subtype Kill_Count is Natural range 0 .. Kill_Capacity;
   type Kill_Lines is array (1 .. Kill_Capacity) of Line;

   type Notes is record
      Recent      : Recent_Lines;
      Recent_Used : Recent_Count := 0;
      Map         : Map_Lines;
      Map_Used    : Map_Count := 0;
      Driver      : Line;           --  latest Driver_Prefix record (Length 0: none)
      Shown_Total : Natural := 0;   --  important records seen, for "n more"
      --  The highest kernel denial record number seen (0: none).
      Highest_Denial : Record_Number := 0;
      --  The highest kernel kill record number seen, and the latest kills.
      Highest_Kill : Record_Number := 0;
      Kills        : Kill_Lines;
      Kills_Used   : Kill_Count := 0;
   end record;

   function Is_Map_Record (Text : String) return Boolean is
     (Text'Length >= Map_Prefix'Length
      and then Text (Text'First .. Text'First + (Map_Prefix'Length - 1)) = Map_Prefix);

   function Is_Driver_Record (Text : String) return Boolean is
     (Text'Length >= Driver_Prefix'Length
      and then Text (Text'First .. Text'First + (Driver_Prefix'Length - 1)) = Driver_Prefix);

   --  The number after Prefix at the start of Text, or 0 (saturating at
   --  Number_Limit).
   function Number_After (Prefix, Text : String) return Record_Number
     with Pre => Text'Last < Positive'Last and then Prefix'Length > 0
                 and then Prefix'Last < Positive'Last;

   --  The kernel record number in a denial record's text, or 0.
   function Denial_Number (Text : String) return Record_Number is
     (Number_After (Denial_Prefix, Text))
     with Pre => Text'Last < Positive'Last;

   --  The kernel record number in a kill record's text, or 0.
   function Kill_Number (Text : String) return Record_Number is
     (Number_After (Kill_Prefix, Text))
     with Pre => Text'Last < Positive'Last;

   --  One record. Important: Warning or worse. Keeps the latest
   --  Recent_Capacity important records (oldest gives way) and the first
   --  Map_Capacity physical-map records, and the latest driver record;
   --  text is cut at Line_Bytes.
   procedure Note (N : in out Notes; Important : Boolean; Text : String)
     with Pre  => Text'Last < Positive'Last,
          Post => N.Recent_Used >= N.Recent_Used'Old
                  and then N.Map_Used >= N.Map_Used'Old
                  and then N.Highest_Denial >= N.Highest_Denial'Old
                  and then N.Highest_Kill >= N.Highest_Kill'Old;

   function Text_Of (Item : Line) return String is
     (Item.Text (1 .. Item.Length));
end Desktop_Bootstrap_Notes;
