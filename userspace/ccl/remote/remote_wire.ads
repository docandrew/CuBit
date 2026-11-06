------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The LAN remote-session wire profile (ccl-remote.app, tools/cubit-remote):
--  frames, the pairing token, names inside a place, and which operation
--  each session grant admits. Pure and total; it grants nothing itself.
--
--  @description
--  One request per TCP connection. A request is a 28-byte header and a
--  body; a response a 12-byte header and a body. Integers are big-endian.
--
--    request:  magic "CBRM" | version | operation | place | 0 |
--              body length (4) | token (16) | body
--    response: magic "CBRM" | version | status | 0 | 0 |
--              body length (4) | body
--
--  Every request carries the pairing token; the endpoint compares it in
--  constant time (Same_Token) before it decodes the body. The token is
--  never echoed. Bodies by operation (Decode_Body):
--
--    see Operation below.
--
--  The token is 16 random bytes, shown to the person at the device as 26
--  Crockford base32 characters (Token_Text); the host client decodes what
--  the person types (Decode_Token_Text).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Remote_Wire with SPARK_Mode is

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   ---------------------------------------------------------------------------
   --  Frames
   ---------------------------------------------------------------------------
   Magic_Length : constant := 4;
   --  "CBRM": CuBit remote management.
   Magic : constant Byte_Array (1 .. Magic_Length) :=
     [Character'Pos ('C'), Character'Pos ('B'), Character'Pos ('R'), Character'Pos ('M')];
   Protocol_Version : constant := 1;

   Token_Bytes : constant := 16;
   subtype Token_Index is Positive range 1 .. Token_Bytes;
   type Token is array (Token_Index) of Unsigned_8;

   Request_Header_Bytes  : constant := 28;
   Response_Header_Bytes : constant := 12;

   --  Header byte positions (1-based).
   Version_At    : constant := 5;
   Operation_At  : constant := 6;
   Place_At      : constant := 7;
   Status_At     : constant := 6;
   Length_At     : constant := 9;    --  4 bytes, both headers
   Token_At      : constant := 13;   --  16 bytes, requests only

   --  Named limits. A put or get moves at most one chunk; a file is at
   --  most Maximum_File_Bytes; CCL source is at most Maximum_Source_Bytes.
   Maximum_Chunk_Bytes  : constant := 32_768;
   Maximum_Name_Bytes   : constant := 240;
   Maximum_Source_Bytes : constant := 4_096;
   Maximum_File_Bytes   : constant := 268_435_456;   --  256 MiB
   Digest_Bytes         : constant := 32;            --  SHA-256

   Name_Length_Bytes : constant := 2;
   Offset_Bytes      : constant := 8;
   Job_Bytes         : constant := 4;

   Maximum_Request_Body : constant :=
     Name_Length_Bytes + Maximum_Name_Bytes + Offset_Bytes + Maximum_Chunk_Bytes;
   --  An evaluation's text, a listing, or one chunk of output with its
   --  fixed fields.
   Maximum_Response_Body : constant := 65_536;

   subtype Request_Body_Length  is Natural range 0 .. Maximum_Request_Body;
   subtype Response_Body_Length is Natural range 0 .. Maximum_Response_Body;
   subtype Name_Length   is Natural range 0 .. Maximum_Name_Bytes;
   subtype Chunk_Length  is Natural range 0 .. Maximum_Chunk_Bytes;
   subtype File_Offset   is Unsigned_64 range 0 .. Maximum_File_Bytes;
   subtype Digest_Index  is Positive range 1 .. Digest_Bytes;
   type Digest is array (Digest_Index) of Unsigned_8;

   --  Job identities: nonzero, per endpoint lifetime.
   subtype Job_Id is Unsigned_32 range 1 .. Unsigned_32'Last;

   ---------------------------------------------------------------------------
   --  Operations, places, grants, statuses
   ---------------------------------------------------------------------------
   --  Hello         body: empty. Response: grants (1), chunk limit (4),
   --                then the endpoint's own name text.
   --  Evaluate      body: CCL source. Response: ok (1), then the result's
   --                "Type: value" text (or the diagnostic).
   --  List          body: name (may be empty: the place itself). Response:
   --                one line per entry: "kind size name".
   --  Stat          body: name. Response: size (8), SHA-256 (32).
   --  Get           body: name, offset (8). Response: up to one chunk from
   --                offset (short at end of file).
   --  Put           body: name, offset (8), data (at most one chunk): into
   --                the name's staging file (offset 0 starts it afresh).
   --  Commit        body: name, size (8), SHA-256 (32): the staging file
   --                becomes the name if both match; Integrity_Mismatch if not.
   --  Remove        body: name.
   --  Build_Start   body: component name (in the workspace). Response: job (4).
   --  Build_Poll    body: job (4), output offset (8). Response: state (1),
   --                exit code (4), output total (8), first kept offset (8),
   --                then output from offset (at most one chunk).
   --  Build_Cancel  body: job (4). Response: state (1).
   --  Capture       body: output (1), then the area x, y, width, height (2
   --                each; all zero: the whole output). One frame of that
   --                output, as the device composed it, saved as a PNG in
   --                the Captures place (Capture_Name). Response: result (1,
   --                Capture_Result), then when Captured: width (4), height
   --                (4), PNG size (8), its SHA-256 (32) and its name (the
   --                rest). The host takes it with Get on Captures.
   type Operation is
     (Hello, Evaluate, List, Stat, Get, Put, Commit, Remove,
      Build_Start, Build_Poll, Build_Cancel, Capture);
   for Operation use
     (Hello => 1, Evaluate => 2, List => 3, Stat => 4, Get => 5, Put => 6,
      Commit => 7, Remove => 8, Build_Start => 9, Build_Poll => 10, Build_Cancel => 11,
      Capture => 12);

   --  Where a file operation acts. None for the others. Captures holds the
   --  device's screenshots (read-only to the host).
   type Place is (None, Workspace, Mail_To_Device, Mail_To_Host, Captures);
   for Place use
     (None => 0, Workspace => 1, Mail_To_Device => 2, Mail_To_Host => 3, Captures => 4);

   --  What the paired principal may do (CTX-001 direction): Diagnose is
   --  always granted; the others only when the device's launch parameters
   --  say so. Capture: the device's startup entry granted ccl-remote the
   --  display-capture capability (the person at the device must still
   --  allow each session's captures; Capture_Off otherwise).
   type Grant is (Diagnose, Files, Builds, Mail, Capture);
   type Grant_Set is array (Grant) of Boolean;
   Diagnose_Only : constant Grant_Set := [Diagnose => True, others => False];
   function Grant_Bits (Item : Grant_Set) return Unsigned_8 is
     ((if Item (Diagnose) then 1 else 0) + (if Item (Files) then 2 else 0) +
      (if Item (Builds) then 4 else 0) + (if Item (Mail) then 8 else 0) +
      (if Item (Capture) then 16 else 0));

   --  How a capture ended (the Capture response's first byte).
   type Capture_Result is
     (Captured, No_Capability, Capture_Off, Rate_Limited, No_Session, No_Frame,
      Bad_Output, Bad_Area, Capture_Failed);
   for Capture_Result use
     (Captured => 0, No_Capability => 1, Capture_Off => 2, Rate_Limited => 3,
      No_Session => 4, No_Frame => 5, Bad_Output => 6, Bad_Area => 7, Capture_Failed => 8);
   Capture_Request_Bytes : constant := 1 + 4 * 2;
   --  The fixed fields of a Captured response before the name.
   Capture_Fixed_Bytes : constant := 1 + 4 + 4 + Offset_Bytes + Digest_Bytes;
   --  Outputs are numbered from 0; areas in the output's pixels.
   Maximum_Output : constant := 255;
   subtype Output_Number is Natural range 0 .. Maximum_Output;
   subtype Area_Field is Natural range 0 .. 65_535;
   --  The PNG of output N in the Captures place: the newest capture of it
   --  replaces the one before.
   function Capture_Name (Output : Output_Number) return String is
     ("output-" & (if Output >= 100 then [Character'Val (Character'Pos ('0') + Output / 100)] else "") &
      (if Output >= 10 then [Character'Val (Character'Pos ('0') + (Output / 10) mod 10)] else "") &
      [Character'Val (Character'Pos ('0') + Output mod 10)] & ".png");

   type Status is
     (OK, Malformed, Unauthorized, Not_Granted, Not_Found, Too_Large, Conflict,
      Busy, Failed, Integrity_Mismatch, Unavailable, Unsupported_Version, Locked);
   for Status use
     (OK => 0, Malformed => 1, Unauthorized => 2, Not_Granted => 3, Not_Found => 4,
      Too_Large => 5, Conflict => 6, Busy => 7, Failed => 8, Integrity_Mismatch => 9,
      Unavailable => 10, Unsupported_Version => 11, Locked => 12);

   type Job_State is (Running, Exited, Failed_To_Start, Cancelled, Unknown_Job);
   for Job_State use
     (Running => 1, Exited => 2, Failed_To_Start => 3, Cancelled => 4, Unknown_Job => 5);

   --  Whether Grants admit Op on Where. Diagnose reads (the workspace,
   --  evaluation); Files writes the workspace; Mail reads and writes the
   --  mailbox (to-device: the host writes; to-host: the host reads and
   --  removes); Builds starts and follows builds of workspace components.
   function Admits (Grants : Grant_Set; Op : Operation; Where : Place) return Boolean is
     (case Op is
         when Hello | Evaluate => Grants (Diagnose) and then Where = None,
         when List | Stat | Get =>
           (case Where is
               when Workspace => Grants (Diagnose),
               when Mail_To_Device | Mail_To_Host => Grants (Mail),
               when Captures => Grants (Capture),
               when None => False),
         when Put | Commit =>
           (case Where is
               when Workspace => Grants (Files),
               when Mail_To_Device => Grants (Mail),
               when Mail_To_Host | Captures | None => False),
         when Remove =>
           (case Where is
               when Workspace => Grants (Files),
               when Mail_To_Device | Mail_To_Host => Grants (Mail),
               when Captures | None => False),
         when Build_Start => Grants (Builds) and then Where = Workspace,
         when Build_Poll | Build_Cancel => Grants (Builds) and then Where = None,
         when Capture => Grants (Capture) and then Where = None);

   --  Whether Op writes (a write needs a grant beyond Diagnose).
   function Writes (Op : Operation) return Boolean is
     (Op in Put | Commit | Remove | Build_Start | Build_Cancel);

   ---------------------------------------------------------------------------
   --  The token
   ---------------------------------------------------------------------------
   --  Equal tokens, in time independent of where they differ.
   function Same_Token (Left, Right : Token) return Boolean
     with Post => Same_Token'Result = (Left = Right);

   --  Crockford base32, no padding: 26 characters for 128 bits, the last
   --  carrying 3 bits (its 2 low bits zero).
   Token_Text_Length : constant := 26;
   subtype Token_Text is String (1 .. Token_Text_Length);
   function Token_Image (Item : Token) return Token_Text;
   --  What a person typed: case-insensitive, '-' and ' ' ignored, I and L
   --  read as 1, O as 0. Valid only for exactly 26 symbols whose padding
   --  bits are zero, so each token has exactly one decoding.
   procedure Decode_Token_Text (Text : String; Item : out Token; Valid : out Boolean)
     with Pre => Text'Length <= 256;

   ---------------------------------------------------------------------------
   --  Names inside a place
   ---------------------------------------------------------------------------
   --  A relative name: components separated by single '/', each made of
   --  letters, digits, '.', '_', '-' and '+', none "." or "..", none ending
   --  in the staging suffix, and nothing else (no scheme, no '@', no ':',
   --  no leading or trailing '/').
   Staging_Suffix : constant String := ".cubit-remote-part";
   function Name_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' | '+');
   function Valid_Name (Name : String) return Boolean
     with Pre => Name'Length <= Maximum_Name_Bytes and then Name'Last < Positive'Last;

   ---------------------------------------------------------------------------
   --  Decoding requests
   ---------------------------------------------------------------------------
   type Request_Header is record
      Op         : Operation := Hello;
      Where      : Place := None;
      Length     : Request_Body_Length := 0;
      Credential : Token := [others => 0];
   end record;

   --  Header checks the magic, version, operation and place codes and the
   --  body length's bound. Result: OK, Malformed, Unsupported_Version or
   --  Too_Large.
   procedure Decode_Header
     (Data : Byte_Array; Header : out Request_Header; Result : out Status)
     with Pre  => Data'Length = Request_Header_Bytes and then Data'First = 1,
          Post => (if Result /= OK then Header.Length = 0);

   type Request is record
      Op     : Operation := Hello;
      Where  : Place := None;
      Name   : String (1 .. Maximum_Name_Bytes) := [others => ' '];
      Name_Last : Name_Length := 0;
      Offset : File_Offset := 0;     --  Get, Put; Build_Poll's output offset
      Size   : File_Offset := 0;     --  Commit
      Hash   : Digest := [others => 0];
      Job    : Unsigned_32 := 0;
      --  Capture: the output and the area (all zero: the whole output).
      Output : Output_Number := 0;
      Area_X, Area_Y, Area_Width, Area_Height : Area_Field := 0;
      --  Evaluate's source, Put's data: Body (Data_First .. Data_First +
      --  Data_Length - 1) of the body Decode_Body was given.
      Data_First  : Positive := 1;
      Data_Length : Natural := 0;
   end record;

   --  The body for Header (its token already compared). Result: OK, or
   --  Malformed for a body of the wrong shape, an invalid name, a zero
   --  job, or an offset or size beyond Maximum_File_Bytes; Too_Large for
   --  a put whose end passes Maximum_File_Bytes.
   procedure Decode_Body
     (Header : Request_Header; Data : Byte_Array; Item : out Request; Result : out Status)
     with Pre  => Data'First = 1 and then Data'Length = Header.Length,
          Post => (if Result = OK then
                     Item.Op = Header.Op and then Item.Where = Header.Where and then
                     Item.Name_Last <= Maximum_Name_Bytes and then
                     Item.Data_Length <= Data'Length and then
                     (if Item.Data_Length > 0 then
                        Item.Data_First >= Data'First and then
                        Item.Data_First - 1 + Item.Data_Length <= Data'Last) and then
                     (if Item.Op = Put then Item.Data_Length <= Maximum_Chunk_Bytes) and then
                     (if Item.Op = Evaluate then Item.Data_Length <= Maximum_Source_Bytes) and then
                     (if Item.Op in Build_Poll | Build_Cancel then Item.Job /= 0));

   ---------------------------------------------------------------------------
   --  Encoding responses
   ---------------------------------------------------------------------------
   subtype Response_Header is Byte_Array (1 .. Response_Header_Bytes);
   function Encode_Response_Header (Result : Status; Length : Response_Body_Length)
     return Response_Header;

   --  Big-endian fields, for bodies.
   procedure Put_U32 (Data : in out Byte_Array; At_Index : Positive; Value : Unsigned_32)
     with Pre => At_Index >= Data'First and then At_Index <= Positive'Last - 3 and then
                 At_Index + 3 <= Data'Last;
   procedure Put_U64 (Data : in out Byte_Array; At_Index : Positive; Value : Unsigned_64)
     with Pre => At_Index >= Data'First and then At_Index <= Positive'Last - 7 and then
                 At_Index + 7 <= Data'Last;
   function Get_U32 (Data : Byte_Array; At_Index : Positive) return Unsigned_32
     with Pre => At_Index >= Data'First and then At_Index <= Positive'Last - 3 and then
                 At_Index + 3 <= Data'Last;
   function Get_U64 (Data : Byte_Array; At_Index : Positive) return Unsigned_64
     with Pre => At_Index >= Data'First and then At_Index <= Positive'Last - 7 and then
                 At_Index + 7 <= Data'Last;

end Remote_Wire;
