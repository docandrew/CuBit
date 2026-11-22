------------------------------------------------------------------------------
--  The lexicon service's channel protocol (docs/lexicon.md, "The lexicon
--  service"): one request is one Lexicon_Answers operation on one text (a
--  word, or a paragraph to check), its answer one page of Lexicon_Wire
--  items. Shared by lexicon.svc and every client: the Dictionary app, the
--  CCL hosts, and any app its manifest grants the lexicon service.
--
--  Calls are coarse by design (APP-003's cost classes): a whole paragraph
--  per Check, a whole entry page per Define or Thesaurus, so a spell check
--  of a page of text is a handful of channel requests, not one per word.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Contracts;
with CuBit.Messages;
with CuBit.Protocols;
with CuBit.Request_Channels;
with Lexicon_Wire;

package Lexicon_Channel is

   --  The registration role (userspace/ccl/catalogs/native-runtime-services.ccl,
   --  "lexicon"); clients name the service, never this number.
   Service_Role : constant Unsigned_64 := CuBit.Messages.DRIVER_LEXICON;

   --  The channel offer's contract name in lexicon.svc's manifest.
   Contract_Name : constant String := "com.cubit.lexicon";

   --  One request: 8 bytes of fields, then the text.
   REQUEST_BYTES : constant := 4_088;
   REQUEST_FIELD_BYTES : constant := 8;
   REQUEST_TEXT_BYTES : constant := REQUEST_BYTES - REQUEST_FIELD_BYTES;
   subtype Request_Text_Length is Natural range 0 .. REQUEST_TEXT_BYTES;
   type Request_Text is array (1 .. REQUEST_TEXT_BYTES) of Character
     with Component_Size => 8;

   --  Fields as plain words: the server copies a request out of the
   --  client's region, then validates every field (docs/data-plane.md).
   type Request is record
      Operation   : Unsigned_8 := 0;    --  Lexicon_Answers.Operation'Pos
      Suggestions : Unsigned_8 := 0;    --  near spellings per unknown word
      Length      : Unsigned_16 := 0;   --  bytes of Text used
      Resume      : Unsigned_32 := 0;   --  item number or byte offset
      Text        : Request_Text := [others => ' '];
   end record
     with Size => REQUEST_BYTES * 8;
   for Request use record
      Operation   at 0 range 0 .. 7;
      Suggestions at 1 range 0 .. 7;
      Length      at 2 range 0 .. 15;
      Resume      at 4 range 0 .. 31;
      Text        at 8 range 0 .. REQUEST_TEXT_BYTES * 8 - 1;
   end record;

   type Answer_Status is (Answered, Malformed, No_Lexicon);
   for Answer_Status use (Answered => 1, Malformed => 2, No_Lexicon => 3);

   --  One answer: 4 bytes of fields, then one Lexicon_Wire page.
   RESULT_BYTES : constant := 4_088;
   RESULT_FIELD_BYTES : constant := 4;
   PAGE_BYTES : constant := RESULT_BYTES - RESULT_FIELD_BYTES;
   pragma Compile_Time_Error (PAGE_BYTES < Lexicon_Wire.MINIMUM_PAGE_BYTES,
                              "an answer page must hold the largest item");
   subtype Page_Length is Natural range 0 .. PAGE_BYTES;
   subtype Page is Lexicon_Wire.Answer_Bytes (0 .. PAGE_BYTES - 1);

   type Result is record
      Status : Unsigned_8 := 0;    --  Answer_Status'Enum_Rep
      Spare  : Unsigned_8 := 0;    --  always 0
      Length : Unsigned_16 := 0;   --  bytes of Bytes used
      Bytes  : Page := [others => 0];
   end record
     with Size => RESULT_BYTES * 8;
   for Result use record
      Status at 0 range 0 .. 7;
      Spare  at 1 range 0 .. 7;
      Length at 2 range 0 .. 15;
      Bytes  at 4 range 0 .. PAGE_BYTES * 8 - 1;
   end record;

   --  Eight requests in flight per channel, eight answers waiting.
   package RC is new CuBit.Request_Channels
     (Request, Result, Request_Bits => 3, Completion_Bits => 3);

   Schema : constant CuBit.Protocols.Schema_Id := 16#1E71_C0A0_D1C7_0001#;
   TOKEN_BYTES : constant := 8;
   Submission_Bytes : constant := TOKEN_BYTES + REQUEST_BYTES;

   Element : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => Schema, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Submission_Bytes),
      Kind    => CuBit.Channel_Contracts.Duplex,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => RC.Client_Pages,
      Buffers => RC.Server_Pages,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

end Lexicon_Channel;
