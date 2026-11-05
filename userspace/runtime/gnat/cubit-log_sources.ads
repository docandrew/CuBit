------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Who a log source was (LOG-003): the package identity and program name of
--  each process that publishes records, kept by logstore past its exit.
--
--  @description
--  logstore keys records by the publishing process. A name looked up from
--  the live process is gone once the process exits, and with it the way to
--  the records that matter most: a failed service's last lines. So the
--  launchers, which start every process and know what they started (devmgr
--  and procmgr), tell logstore each process's identity as they start it,
--  and its exit, as notices on a channel each opens to logstore under the
--  registrar authority (Registrar_Tag). A process never names itself:
--  logstore takes a name only from a registrar.
--
--  Readers ask logstore which sources it knows (CuBit.Log_Protocol's
--  List_Sources, log-observer): the live ones, and the exited ones while
--  logstore still keeps their records (its per-source retention, LOG-002).
--
--  An entry, little-endian (a notice and a listing entry alike):
--     0  u8  kind (Started or Exited; listed: whether it has exited)
--     1  u8  identity length     2  u8  name length     3..7  zero
--     8  u64 source (the process word CuBit.Log_Protocol carries)
--    16  u64 when logstore learned of it (monotonic ms; zero in a notice)
--    24  identity (Identity_Bytes)    88  name (Name_Bytes)   120..127 zero
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Contracts;
with CuBit.Protocols;

package CuBit.Log_Sources with Pure, SPARK_Mode is

   --  The registrar role: a launcher's channel of notices to logstore. Only
   --  a holder of logstore's untagged endpoint (its launcher, and devmgr,
   --  which procmgr offers it to) can stamp this tag. "LOR" in the high
   --  half; the low half names the launcher.
   Registrar_Tag_Base : constant Unsigned_64 := 16#4C4F_5200_0000_0000#;
   subtype Registrar_Issuance is Unsigned_64 range 1 .. 16#FFFF_FFFF#;
   Device_Manager_Issuance  : constant Registrar_Issuance := 1;
   Process_Manager_Issuance : constant Registrar_Issuance := 2;
   function Registrar_Tag (Issuance : Registrar_Issuance) return Unsigned_64 is
     (Registrar_Tag_Base + Issuance);
   --  Tags are kernel-stamped; knowing these numeric values grants nothing.
   function Is_Registrar (Tag : Unsigned_64) return Boolean is
     ((Tag and 16#FFFF_FFFF_0000_0000#) = Registrar_Tag_Base and then
      (Tag and 16#FFFF_FFFF#) /= 0);

   --  No process: a source word never names this.
   No_Source : constant Unsigned_64 := 0;

   --  A package identity (com.cubit.acpi, as CuBit.Process_Observer carries
   --  it) and a program's file name (acpi.svc).
   Identity_Bytes : constant := 64;
   Name_Bytes     : constant := 32;
   subtype Identity_Length is Natural range 0 .. Identity_Bytes;
   subtype Name_Length is Natural range 0 .. Name_Bytes;
   subtype Identity_Text is String (1 .. Identity_Bytes);
   subtype Name_Text is String (1 .. Name_Bytes);

   --  Printable ASCII other than space.
   function Text_Character (C : Character) return Boolean is (C in '!' .. '~');
   function Valid_Text (Text : String) return Boolean is
     (for all C of Text => Text_Character (C));

   type Source_Identity is record
      Source        : Unsigned_64 := No_Source;
      Identity      : Identity_Text := [others => ' '];
      Identity_Used : Identity_Length := 0;
      Name          : Name_Text := [others => ' '];
      Name_Used     : Name_Length := 0;
   end record;

   --  It is about a process, and its text is printable.
   function Valid (Item : Source_Identity) return Boolean is
     (Item.Source /= No_Source
      and then Valid_Text (Item.Identity (1 .. Item.Identity_Used))
      and then Valid_Text (Item.Name (1 .. Item.Name_Used)));
   --  It says who the process is: an identity or a name or both.
   function Named (Item : Source_Identity) return Boolean is
     (Item.Identity_Used > 0 or else Item.Name_Used > 0);

   function Identity_Of (Item : Source_Identity) return String is
     (Item.Identity (1 .. Item.Identity_Used));
   function Name_Of (Item : Source_Identity) return String is
     (Item.Name (1 .. Item.Name_Used));

   --  What a launcher knows of a process it started: its identity and its
   --  program's name (the file name, any directory left off). Each is cut
   --  to its field, and anything but printable non-space text is left out.
   function Make (Source : Unsigned_64; Identity, Name : String) return Source_Identity
   with Pre  => Source /= No_Source and then Name'Last < Positive'Last,
        Post => Make'Result.Source = Source and then Valid (Make'Result);

   --  The short name a person types for it: its program name before the
   --  first '.' (acpi for acpi.svc), else its identity's last part (acpi for
   --  com.cubit.acpi); empty when it is not Named.
   function Label (Item : Source_Identity) return String
   with Post => Label'Result'Length <= Identity_Bytes;

   --  Whether Wanted names it: its program name, its label, its identity, or
   --  the end of its identity after a '.' (cubit.acpi, acpi).
   function Answers_To (Item : Source_Identity; Wanted : String) return Boolean;

   --  A notice from a registrar (Started, Exited), or a listing entry
   --  (Started: still running; Exited: it has exited).
   type Entry_Kind is (Started, Exited);
   for Entry_Kind use (Started => 1, Exited => 2);

   type Source_Entry is record
      Kind    : Entry_Kind := Started;
      Who     : Source_Identity;
      Seen_Ms : Unsigned_64 := 0;
   end record;

   Entry_Bytes : constant := 128;
   subtype Entry_Position is Natural range 0 .. Entry_Bytes - 1;
   type Entry_Buffer is array (Entry_Position) of Unsigned_8;

   procedure Encode (Item : Source_Entry; Into : out Entry_Buffer)
   with Pre => Valid (Item.Who);

   --  The entry in From, which is untrusted: Accepted only when it decodes
   --  exactly as Encode writes a valid entry, and a Started one is Named.
   procedure Decode (From : Entry_Buffer; Item : out Source_Entry; Accepted : out Boolean)
   with Post => (if Accepted then Valid (Item.Who)
                   and then (if Item.Kind = Started then Named (Item.Who)));

   --  A registrar's channel: notices, in the order the launcher saw them.
   --  Lossless: a launcher keeps what does not fit and puts it later.
   SOURCE_NOTICE_SCHEMA : constant CuBit.Protocols.Schema_Id := 16#4C4F_4753_5243_0001#;
   NOTICE_PAGES : constant := 1;
   CONTRACT : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => SOURCE_NOTICE_SCHEMA, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Entry_Bytes),
      Kind    => CuBit.Channel_Contracts.Queue,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => NOTICE_PAGES,
      Buffers => 1,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

   --  List_Sources fills one page the reader lends with entries, newest
   --  source first.
   Page_Bytes : constant := 4_096;
   Page_Entries : constant := Page_Bytes / Entry_Bytes;
   subtype Page_Count is Natural range 0 .. Page_Entries;
   subtype Page_Position is Positive range 1 .. Page_Entries;
   function Entry_Offset (Position : Page_Position) return Natural is
     ((Position - 1) * Entry_Bytes);

end CuBit.Log_Sources;
