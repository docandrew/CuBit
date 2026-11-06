--  Remote_Wire (userspace/ccl/remote): the LAN remote-session profile's
--  frames, token, names and grants, hosted with run-time checks on.
--  The token vectors are also checked by tools/cubit-remote's own tests,
--  so the device's display and the host client agree on every symbol.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Remote_Wire; use Remote_Wire;

procedure Remote_Wire_Tests is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & What);
         Failures := Failures + 1;
      end if;
   end Check;

   function Counting return Token is
      T : Token;
   begin
      for I in Token_Index loop
         T (I) := Unsigned_8 (I - 1);
      end loop;
      return T;
   end Counting;

   --  A deterministic generator for round trips (xorshift64).
   State : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next return Unsigned_64 is
   begin
      State := State xor Shift_Left (State, 13);
      State := State xor Shift_Right (State, 7);
      State := State xor Shift_Left (State, 17);
      return State;
   end Next;

   function Header
     (Op : Unsigned_8; Where : Unsigned_8; Length : Unsigned_32; Credential : Token)
      return Byte_Array
   is
      H : Byte_Array (1 .. Request_Header_Bytes) := [others => 0];
   begin
      H (1 .. 4) := Magic;
      H (Version_At) := Protocol_Version;
      H (Operation_At) := Op;
      H (Place_At) := Where;
      Put_U32 (H, Length_At, Length);
      for I in Token_Index loop
         H (Token_At + I - 1) := Credential (I);
      end loop;
      return H;
   end Header;

   function Name_Field (Name : String) return Byte_Array is
      B : Byte_Array (1 .. 2 + Name'Length);
   begin
      B (1) := Unsigned_8 (Name'Length / 256);
      B (2) := Unsigned_8 (Name'Length mod 256);
      for I in Name'Range loop
         B (2 + I - Name'First + 1) := Character'Pos (Name (I));
      end loop;
      return B;
   end Name_Field;

   function U64_Field (Value : Unsigned_64) return Byte_Array is
      B : Byte_Array (1 .. 8) := [others => 0];
   begin
      Put_U64 (B, 1, Value);
      return B;
   end U64_Field;

   function U32_Field (Value : Unsigned_32) return Byte_Array is
      B : Byte_Array (1 .. 4) := [others => 0];
   begin
      Put_U32 (B, 1, Value);
      return B;
   end U32_Field;

   procedure Decode
     (Op : Operation; Where : Place; Body_Bytes : Byte_Array;
      Item : out Request; Result : out Status)
   is
      H : Request_Header;
      Data : constant Byte_Array (1 .. Body_Bytes'Length) := Body_Bytes;
   begin
      H := (Op => Op, Where => Where, Length => Body_Bytes'Length, Credential => [others => 0]);
      Decode_Body (H, Data, Item, Result);
   end Decode;

   Item : Request;
   Result : Status;
   H : Request_Header;
   T : Token;
   Valid : Boolean;
begin
   --  Token text vectors (tools/cubit-remote/test_cubit_remote.py has the same).
   Check (Token_Image (Counting) = "000G40R40M30E209185GR38E1W", "counting token image");
   Check (Token_Image ([others => 16#FF#]) = "ZZZZZZZZZZZZZZZZZZZZZZZZZW", "all-ones token image");
   Check (Token_Image ([16#DE#, 16#AD#, 16#BE#, 16#EF#, 16#DE#, 16#AD#, 16#BE#, 16#EF#,
                        16#DE#, 16#AD#, 16#BE#, 16#EF#, 16#DE#, 16#AD#, 16#BE#, 16#EF#])
          = "VTPVXVYYNPZEZQNDQVQXXBDYXW", "deadbeef token image");
   Decode_Token_Text ("000g4-0r40m-30e20-9185g-r38e1-w", T, Valid);
   Check (Valid and then T = Counting, "lower case, dashes");
   Decode_Token_Text ("OOOG40R40M30E2O9185GR38E1W", T, Valid);
   Check (Valid and then T = Counting, "O read as 0");
   Decode_Token_Text ("ZZZZZZZZZZZZZZZZZZZZZZZZZX", T, Valid);
   Check (not Valid and then T = [Token_Index => 0], "padding bits set refused");
   Decode_Token_Text ("ZZZZZZZZZZZZZZZZZZZZZZZZZ", T, Valid);
   Check (not Valid, "25 symbols refused");
   Decode_Token_Text ("ZZZZZZZZZZZZZZZZZZZZZZZZZW0", T, Valid);
   Check (not Valid, "27 symbols refused");
   Decode_Token_Text ("000G40R40M30E209185GR38E1U", T, Valid);
   Check (not Valid, "U is not a symbol");
   Decode_Token_Text ("", T, Valid);
   Check (not Valid, "empty refused");
   for Round in 1 .. 2_000 loop
      declare
         A, B : Unsigned_64;
         Original, Back : Token;
      begin
         A := Next;
         B := Next;
         for I in 1 .. 8 loop
            Original (I) := Unsigned_8 (Shift_Right (A, 8 * (I - 1)) and 16#FF#);
            Original (I + 8) := Unsigned_8 (Shift_Right (B, 8 * (I - 1)) and 16#FF#);
         end loop;
         Decode_Token_Text (Token_Image (Original), Back, Valid);
         Check (Valid and then Back = Original, "round trip" & Round'Image);
         Check (Same_Token (Original, Back), "same token" & Round'Image);
         Back (Token_Index'First + Round mod Token_Bytes) :=
           Back (Token_Index'First + Round mod Token_Bytes) xor 16#01#;
         Check (not Same_Token (Original, Back), "one bit differs" & Round'Image);
      end;
   end loop;

   --  Names.
   Check (Valid_Name ("a"), "one letter");
   Check (Valid_Name ("src/ps2/component.ccl"), "nested");
   Check (Valid_Name ("a-b_c.d+e"), "punctuation");
   Check (Valid_Name ("..a"), "dot-dot prefix is a name");
   Check (not Valid_Name (""), "empty");
   Check (not Valid_Name ("/a"), "leading slash");
   Check (not Valid_Name ("a/"), "trailing slash");
   Check (not Valid_Name ("a//b"), "double slash");
   Check (not Valid_Name ("../a"), "parent first");
   Check (not Valid_Name ("a/.."), "parent last");
   Check (not Valid_Name ("a/./b"), "dot inside");
   Check (not Valid_Name ("."), "dot");
   Check (not Valid_Name ("@nvme:0/x"), "scheme");
   Check (not Valid_Name ("a b"), "space");
   Check (not Valid_Name ("a\b"), "backslash");
   Check (not Valid_Name ("x" & Staging_Suffix), "staging suffix reserved");

   --  Headers.
   Decode_Header (Header (Operation'Enum_Rep (Hello), 0, 0, Counting), H, Result);
   Check (Result = OK and then H.Op = Hello and then H.Where = None and then H.Credential = Counting,
          "hello header");
   Decode_Header (Header (Operation'Enum_Rep (Put), 1, 100, Counting), H, Result);
   Check (Result = OK and then H.Op = Put and then H.Where = Workspace and then H.Length = 100,
          "put header");
   declare
      Bad : Byte_Array := Header (1, 0, 0, Counting);
   begin
      Bad (1) := Character'Pos ('X');
      Decode_Header (Bad, H, Result);
      Check (Result = Malformed, "bad magic");
      Bad := Header (1, 0, 0, Counting);
      Bad (Version_At) := 2;
      Decode_Header (Bad, H, Result);
      Check (Result = Unsupported_Version, "version 2");
      Bad := Header (1, 0, 0, Counting);
      Bad (Place_At + 1) := 1;
      Decode_Header (Bad, H, Result);
      Check (Result = Malformed, "reserved byte");
   end;
   Decode_Header (Header (0, 0, 0, Counting), H, Result);
   Check (Result = Malformed, "operation 0");
   Decode_Header (Header (12, 0, 0, Counting), H, Result);
   Check (Result = Malformed, "operation 12");
   Decode_Header (Header (1, 4, 0, Counting), H, Result);
   Check (Result = Malformed, "place 4");
   Decode_Header (Header (6, 1, Maximum_Request_Body + 1, Counting), H, Result);
   Check (Result = Too_Large and then H.Length = 0, "body too large");
   Decode_Header (Header (6, 1, Maximum_Request_Body, Counting), H, Result);
   Check (Result = OK, "largest body");

   --  Bodies.
   Decode (Hello, None, [1 .. 0 => 0], Item, Result);
   Check (Result = OK, "hello body");
   Decode (Hello, None, [0], Item, Result);
   Check (Result = Malformed, "hello with a body");
   Decode (Evaluate, None, [Character'Pos ('('), Character'Pos ('+'), Character'Pos (' '),
                            Character'Pos ('1'), Character'Pos (')')], Item, Result);
   Check (Result = OK and then Item.Data_First = 1 and then Item.Data_Length = 5, "evaluate");
   Decode (Evaluate, None, [16#00#], Item, Result);
   Check (Result = Malformed, "evaluate NUL");
   Decode (Evaluate, None, [1 .. 0 => 0], Item, Result);
   Check (Result = Malformed, "evaluate empty");
   Decode (List, Workspace, Name_Field (""), Item, Result);
   Check (Result = OK and then Item.Name_Last = 0, "list the place");
   Decode (Stat, Workspace, Name_Field (""), Item, Result);
   Check (Result = Malformed, "stat needs a name");
   Decode (Stat, Workspace, Name_Field ("out/ps2.drv"), Item, Result);
   Check (Result = OK and then Item.Name (1 .. Item.Name_Last) = "out/ps2.drv", "stat");
   Decode (Stat, Workspace, Name_Field ("../etc"), Item, Result);
   Check (Result = Malformed, "stat outside the place");
   Decode (Stat, Workspace, Name_Field ("a") & [0], Item, Result);
   Check (Result = Malformed, "stat trailing byte");
   Decode (Get, Workspace, Name_Field ("a") & U64_Field (4096), Item, Result);
   Check (Result = OK and then Item.Offset = 4096, "get");
   Decode (Get, Workspace, Name_Field ("a") & U64_Field (Maximum_File_Bytes + 1), Item, Result);
   Check (Result = Malformed, "get beyond the file limit");
   declare
      Chunk : constant Byte_Array (1 .. Maximum_Chunk_Bytes) := [others => 16#5A#];
   begin
      Decode (Put, Workspace, Name_Field ("big.bin") & U64_Field (0) & Chunk, Item, Result);
      Check (Result = OK and then Item.Data_Length = Maximum_Chunk_Bytes
             and then Item.Data_First = 2 + 7 + 8 + 1, "put a full chunk");
      Decode (Put, Workspace, Name_Field ("big.bin") & U64_Field (0) & Chunk & [0], Item, Result);
      Check (Result = Malformed, "put more than a chunk");
      Decode (Put, Workspace,
              Name_Field ("big.bin") & U64_Field (Maximum_File_Bytes - Maximum_Chunk_Bytes + 1) & Chunk,
              Item, Result);
      Check (Result = Too_Large, "put past the file limit");
      Decode (Put, Workspace,
              Name_Field ("big.bin") & U64_Field (Maximum_File_Bytes - Maximum_Chunk_Bytes) & Chunk,
              Item, Result);
      Check (Result = OK, "put up to the file limit");
   end;
   Decode (Put, Workspace, Name_Field ("empty") & U64_Field (0), Item, Result);
   Check (Result = OK and then Item.Data_Length = 0, "put nothing");
   Decode (Commit, Workspace, Name_Field ("a") & U64_Field (3) & [1 .. Digest_Bytes => 7], Item, Result);
   Check (Result = OK and then Item.Size = 3 and then Item.Hash = [Digest_Index => 7], "commit");
   Decode (Commit, Workspace, Name_Field ("a") & U64_Field (3) & [1 .. Digest_Bytes - 1 => 7], Item, Result);
   Check (Result = Malformed, "commit short digest");
   Decode (Build_Poll, None, U32_Field (3) & U64_Field (17), Item, Result);
   Check (Result = OK and then Item.Job = 3 and then Item.Offset = 17, "poll");
   Decode (Build_Poll, None, U32_Field (0) & U64_Field (17), Item, Result);
   Check (Result = Malformed and then Item.Job = 0, "poll job 0");
   Decode (Build_Cancel, None, U32_Field (9), Item, Result);
   Check (Result = OK and then Item.Job = 9, "cancel");
   Decode (Build_Start, Workspace, Name_Field ("userspace/services/ps2/component.ccl"), Item, Result);
   Check (Result = OK, "build start");
   declare
      Long : constant String (1 .. Maximum_Name_Bytes + 1) := [others => 'a'];
   begin
      Decode (Stat, Workspace, Name_Field (Long), Item, Result);
      Check (Result = Malformed, "name too long");
      Decode (Stat, Workspace, Name_Field (Long (1 .. Maximum_Name_Bytes)), Item, Result);
      Check (Result = OK, "longest name");
   end;
   --  A declared name length beyond the body.
   Decode (Stat, Workspace, [0, 9, Character'Pos ('a')], Item, Result);
   Check (Result = Malformed, "name length beyond the body");

   --  Grants: Diagnose reads; writing needs more.
   Check (Admits (Diagnose_Only, Evaluate, None), "diagnose evaluates");
   Check (Admits (Diagnose_Only, Get, Workspace), "diagnose reads the workspace");
   Check (not Admits (Diagnose_Only, Put, Workspace), "diagnose cannot write");
   Check (not Admits (Diagnose_Only, Get, Mail_To_Host), "diagnose cannot read mail");
   Check (not Admits (Diagnose_Only, Build_Start, Workspace), "diagnose cannot build");
   Check (not Admits (Diagnose_Only, Evaluate, Workspace), "evaluate takes no place");
   Check (Admits ([Diagnose | Files => True, others => False], Commit, Workspace), "files commits");
   Check (not Admits ([others => True], Put, Mail_To_Host), "nobody writes to-host from the host");
   Check (Admits ([Diagnose | Mail => True, others => False], Put, Mail_To_Device), "mail sends");
   Check (Admits ([Diagnose | Mail => True, others => False], Remove, Mail_To_Host), "mail takes");
   Check (Admits ([Diagnose | Builds => True, others => False], Build_Poll, None), "builds poll");
   Check (not Admits ([Diagnose | Builds => True, others => False], Build_Start, Mail_To_Device),
          "builds only from the workspace");
   for Op in Operation loop
      for P in Place loop
         Check (not Admits ([others => False], Op, P), "nothing granted admits nothing");
         if Writes (Op) then
            Check (not Admits (Diagnose_Only, Op, P), "diagnose admits no write");
         end if;
      end loop;
   end loop;
   Check (Grant_Bits (Diagnose_Only) = 1 and then Grant_Bits ([others => True]) = 15, "grant bits");

   --  Responses.
   declare
      R : constant Response_Header := Encode_Response_Header (Integrity_Mismatch, 300);
   begin
      Check (R (1 .. 4) = Magic and then R (Version_At) = Protocol_Version
             and then R (Status_At) = 9 and then Get_U32 (R, Length_At) = 300, "response header");
   end;

   if Failures = 0 then
      Put_Line ("PASS: remote wire frames, token text, names, grants and bounds");
   else
      Put_Line ("FAIL:" & Failures'Image & " remote wire checks");
      raise Program_Error;
   end if;
end Remote_Wire_Tests;
