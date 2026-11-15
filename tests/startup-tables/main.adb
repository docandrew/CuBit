--  Hosted tests for CuBit.Startup_Tables: encode/decode round trips, every
--  refusal reason, Find, and a differential check of Decode against an
--  independent reference on random tables, random bytes and single-byte
--  mutations of valid encodings. With a scenario argument, loads one table
--  into CuBit.Startup_State (once per process, as at startup) and checks
--  the generated Startup package (generated/startup.ads) against it.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with CuBit.Startup_Tables; use CuBit.Startup_Tables;
with CuBit.Startup_State;
with CuBit.Handles;
with CuBit.Launch_Envelopes;
with CuBit.Handles.Startup;
with CuBit.Handles.Test_Words;
with CuBit.Startup_Builder; use CuBit.Startup_Builder;
with Startup;

procedure Main is
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   subtype Byte is Unsigned_8;
   package Random_Bytes is new Ada.Numerics.Discrete_Random (Byte);
   Gen : Random_Bytes.Generator;
   function Draw (Low, High : Natural) return Natural is
     (Low + Natural (Random_Bytes.Random (Gen)) mod (High - Low + 1));

   ---------------------------------------------------------------------------
   --  Independent reference: the layout read straight from the bytes with
   --  plain integer arithmetic, written from the format comment.
   ---------------------------------------------------------------------------
   function Reference_Valid (Raw : Bytes) return Boolean is
      function U (Offset, Width : Natural) return Long_Long_Integer is
         Value : Long_Long_Integer := 0;
      begin
         for K in reverse 0 .. Width - 1 loop
            Value := Value * 256 + Long_Long_Integer (Raw (Raw'First + Offset + K));
         end loop;
         return Value;
      end U;
      Count : Long_Long_Integer;
   begin
      if Raw'Length < 16 then return False; end if;
      if U (0, 4) /= 16#5453_4243# or else U (4, 2) /= 1 then return False; end if;
      Count := U (6, 2);
      if Count > 40 or else Long_Long_Integer (Raw'Length) /= 16 + Count * 80 then return False; end if;
      if U (9, 3) /= 0 then return False; end if;
      case U (8, 1) is
         when 1 | 3 => if U (12, 4) /= 0 then return False; end if;
         when 2 => if U (12, 4) not in 1 .. 32_768 then return False; end if;
         when others => return False;
      end case;
      for I in 0 .. Natural (Count) - 1 loop
         declare
            B : constant Natural := 16 + I * 80;
            Len : constant Natural := Natural (U (B + 3, 1));
            State : constant Long_Long_Integer := U (B + 2, 1);
            Need_Code : constant Long_Long_Integer := U (B + 1, 1);
            Handle_Zero : constant Boolean := U (B + 8, 4) = 0 and then U (B + 12, 4) = 0;
         begin
            if U (B, 1) not in 1 .. 12 or else Need_Code not in 1 .. 2
              or else State not in 1 .. 3 or else U (B + 4, 4) /= 0
            then
               return False;
            end if;
            if (State = 1 and then not Handle_Zero) or else (State = 2 and then Handle_Zero)
              or else (State = 3 and then (not Handle_Zero or else Need_Code /= 2))
            then
               return False;
            end if;
            if Len not in 1 .. 64 then return False; end if;
            for K in 0 .. 63 loop
               declare
                  C : constant Character := Character'Val (Raw (Raw'First + B + 16 + K));
               begin
                  if K < Len then
                     if C not in 'a' .. 'z' | '0' .. '9' | '-' then return False; end if;
                     if K = 0 and then C not in 'a' .. 'z' then return False; end if;
                     if K = Len - 1 and then C = '-' then return False; end if;
                     if K > 0 and then C = '-'
                       and then Character'Val (Raw (Raw'First + B + 16 + K - 1)) = '-'
                     then
                        return False;
                     end if;
                  elsif Character'Pos (C) /= 0 then
                     return False;
                  end if;
               end;
            end loop;
            for J in 0 .. I - 1 loop
               declare
                  Other : constant Natural := 16 + J * 80;
                  Same : Boolean := U (Other + 3, 1) = U (B + 3, 1);
               begin
                  for K in 0 .. 63 loop
                     Same := Same and then Raw (Raw'First + Other + 16 + K) = Raw (Raw'First + B + 16 + K);
                  end loop;
                  if Same then return False; end if;
               end;
            end loop;
         end;
      end loop;
      return True;
   end Reference_Valid;

   function Entry_Of
     (Text : String; Kind : Capability_Kind := Endpoint; Requirement : Need := Required;
      State : Entry_State := Unassigned; Handle : Unsigned_64 := No_Handle) return Startup_Entry is
     (Label => To_Name (Text), Kind => Kind, Requirement => Requirement, State => State,
      Handle => Handle);

   function Encoded (Item : Table) return Bytes is
      Output : Bytes (1 .. Maximum_Table_Bytes);
      Length : Table_Length;
   begin
      Encode (Item, Output, Length);
      return Output (1 .. Length);
   end Encoded;

   function Sample return Table is
      T : Table := Empty_Table;
   begin
      T.Limit := (Kind => At_Most, Count => 128);
      T.Count := 4;
      T.Entries (1) := Entry_Of ("registers", Device_Memory, Required, Granted, 16#0000_0001_0000_0004#);
      T.Entries (2) := Entry_Of ("device", Interrupt, Required, Granted, 16#0000_0001_0000_0005#);
      T.Entries (3) := Entry_Of ("logstore", Endpoint, Optional, Absent);
      T.Entries (4) := Entry_Of ("pointer-events", Notification, Optional, Granted, 16#0000_0002_0000_0007#);
      return T;
   end Sample;

   procedure Expect (Raw : Bytes; Want : Decode_Result; What : String) is
      Result : Table;
      Outcome : Decode_Result;
   begin
      Decode (Raw, Result, Outcome);
      Check (Outcome = Want, What & ": got " & Decode_Result'Image (Outcome));
      Check ((Outcome = Decoded) = Reference_Valid (Raw), What & ": reference disagrees");
      if Outcome /= Decoded then
         Check (Result = Empty_Table, What & ": refused table not empty");
      end if;
   end Expect;

   --  A copy of the sample encoding with one byte replaced.
   function Mutated (Offset : Natural; Value : Byte) return Bytes is
      Raw : Bytes := Encoded (Sample);
   begin
      Raw (Raw'First + Offset) := Value;
      return Raw;
   end Mutated;

   ---------------------------------------------------------------------------
   procedure Test_Round_Trip is
      Result : Table;
      Outcome : Decode_Result;
      Raw : constant Bytes := Encoded (Sample);
   begin
      Check (Raw'Length = Header_Bytes + 4 * Entry_Bytes, "sample length");
      Decode (Raw, Result, Outcome);
      Check (Outcome = Decoded, "sample decodes");
      Check (Result = Sample, "sample round trip");
      Check (Encoded (Result) = Raw, "re-encoding is identical");
      Check (Find (Result, To_Name ("device")) = 2, "find device");
      Check (Find (Result, To_Name ("pointer-events")) = 4, "find hyphenated");
      Check (Find (Result, To_Name ("missing")) = 0, "find missing");
      Check (Find (Result, To_Name ("devic")) = 0, "find prefix is not a match");
      Check (Raw (1 .. 4) = [16#43#, 16#42#, 16#53#, 16#54#], "magic spells CBST");

      --  Empty table, each limit kind, a declaration (all Unassigned).
      declare
         T : Table := Empty_Table;
      begin
         Expect (Encoded (T), Decoded, "empty standard");
         T.Limit := (Kind => Kernel_Limit);
         Expect (Encoded (T), Decoded, "empty unlimited");
         T.Limit := (Kind => At_Most, Count => Maximum_Handle_Limit);
         Expect (Encoded (T), Decoded, "at most maximum");
         T.Count := 2;
         T.Entries (1) := Entry_Of ("config");
         T.Entries (2) := Entry_Of ("clock", Endpoint, Optional);
         Check (Declaration (T) and then not Complete (T), "declaration predicate");
         Decode (Encoded (T), Result, Outcome);
         Check (Outcome = Decoded and then Result = T, "declaration round trip");
      end;

      --  Full table with maximum-length names.
      declare
         T : Table := Empty_Table;
         Text : String (1 .. Maximum_Name_Bytes) := [others => 'a'];
      begin
         T.Count := Maximum_Entries;
         for I in 1 .. Maximum_Entries loop
            Text (Maximum_Name_Bytes - 1) := Character'Val (Character'Pos ('a') + (I - 1) / 26);
            Text (Maximum_Name_Bytes) := Character'Val (Character'Pos ('a') + (I - 1) mod 26);
            T.Entries (I) := Entry_Of (Text, Process, Required, Granted, Unsigned_64 (I));
         end loop;
         Check (Encoded (T)'Length = Maximum_Table_Bytes, "full table length");
         Decode (Encoded (T), Result, Outcome);
         Check (Outcome = Decoded and then Result = T, "full table round trip");
         Check (Complete (T), "full table complete");
      end;
   end Test_Round_Trip;

   procedure Test_Refusals is
      Raw : constant Bytes := Encoded (Sample);
      First : constant Natural := Header_Bytes;  --  first entry's offset
   begin
      Expect (Raw (1 .. Header_Bytes - 1), Wrong_Length, "short header");
      Expect (Raw (1 .. Raw'Last - 1), Wrong_Length, "truncated entry");
      declare
         Longer : constant Bytes := Raw & [0];
      begin
         Expect (Longer, Wrong_Length, "trailing byte");
      end;
      Expect (Mutated (Magic_Offset, 0), Bad_Magic, "magic");
      Expect (Mutated (Version_Offset, 2), Unknown_Version, "version");
      Expect (Mutated (Count_Offset, 41), Too_Many_Entries, "count above maximum");
      Expect (Mutated (Count_Offset, 3), Wrong_Length, "count below length");
      Expect (Mutated (Header_Reserved_First + 1, 1), Bad_Reserved, "header reserved");
      Expect (Mutated (Limit_Kind_Offset, 0), Bad_Limit, "limit kind 0");
      Expect (Mutated (Limit_Kind_Offset, 4), Bad_Limit, "limit kind 4");
      Expect (Mutated (Limit_Kind_Offset, 1), Bad_Limit, "standard limit with a count");
      declare
         Zero_Count : Bytes := Raw;
      begin
         Zero_Count (Limit_Count_Offset + 1) := 0;
         Expect (Zero_Count, Bad_Limit, "at most zero");
         Zero_Count (Limit_Count_Offset + 2) := 16#81#;  --  33024
         Expect (Zero_Count, Bad_Limit, "at most above maximum");
      end;
      Expect (Mutated (First + Kind_Offset, 0), Bad_Kind, "kind 0");
      Expect (Mutated (First + Kind_Offset, 13), Bad_Kind, "kind 13");
      Expect (Mutated (First + Need_Offset, 3), Bad_Need, "need 3");
      Expect (Mutated (First + State_Offset, 4), Bad_State, "state 4");
      Expect (Mutated (First + Entry_Reserved_First + 3, 9), Bad_Reserved, "entry reserved");
      --  Granted with a zero handle; Unassigned with a handle; Absent but
      --  required; Absent with a handle.
      declare
         Zeroed : Bytes := Raw;
      begin
         Zeroed (First + Handle_Offset + 1 .. First + Handle_Offset + 8) := [others => 0];
         Expect (Zeroed, Bad_Handle, "granted without handle");
      end;
      Expect (Mutated (First + State_Offset, 1), Bad_Handle, "unassigned with handle");
      Expect (Mutated (First + 2 * Entry_Bytes + Need_Offset, 1), Bad_Handle, "absent but required");
      Expect (Mutated (First + 2 * Entry_Bytes + Handle_Offset, 1), Bad_Handle, "absent with handle");
      --  Names.
      Expect (Mutated (First + Name_Length_Offset, 0), Bad_Name, "empty name");
      Expect (Mutated (First + Name_Length_Offset, 65), Bad_Name, "name too long");
      Expect (Mutated (First + Name_Length_Offset, 10), Bad_Name, "length past text");
      Expect (Mutated (First + Name_Length_Offset, 8), Bad_Name, "length short of text");
      Expect (Mutated (First + Name_Offset, Character'Pos ('R')), Bad_Name, "upper case");
      Expect (Mutated (First + Name_Offset, Character'Pos ('1')), Bad_Name, "leading digit");
      Expect (Mutated (First + Name_Offset, Character'Pos ('-')), Bad_Name, "leading hyphen");
      Expect (Mutated (First + Name_Offset + 8, Character'Pos ('-')), Bad_Name, "trailing hyphen");
      Expect (Mutated (First + Name_Offset + 4, Character'Pos ('_')), Bad_Name, "underscore");
      Expect (Mutated (First + Name_Offset + 20, Character'Pos ('a')), Bad_Name, "padding not zero");
      Expect (Mutated (First + 3 * Entry_Bytes + Name_Offset + 8, Character'Pos ('-')),
              Bad_Name, "doubled hyphen");
      --  Duplicate: make entry 2's name "registers".
      declare
         Dup : Bytes := Raw;
         Second : constant Natural := First + Entry_Bytes;
         Text : constant String := "registers";
      begin
         Dup (Second + Name_Length_Offset + 1) := Text'Length;
         for K in Text'Range loop
            Dup (Second + Name_Offset + K) := Character'Pos (Text (K));
         end loop;
         Expect (Dup, Duplicate_Name, "duplicate name");
      end;
   end Test_Refusals;

   --  Random valid tables round trip; random bytes and mutations agree
   --  with the reference.
   procedure Test_Random is
      Kinds : constant array (0 .. 11) of Capability_Kind :=
        [Endpoint, Notification, Framebuffer, Render, Network, Device_Memory,
         IO_Ports, Interrupt, DMA, Scheduling, Process, Resource];
      Alphabet : constant String := "abcdefghijklmnopqrstuvwxyz0123456789-";
      Accepted, Refused : Natural := 0;
   begin
      for Round in 1 .. 2_000 loop
         declare
            T : Table := Empty_Table;
            Result : Table;
            Outcome : Decode_Result;
         begin
            T.Limit := (case Draw (0, 2) is
                          when 0 => (Kind => Standard_Limit),
                          when 1 => (Kind => Kernel_Limit),
                          when others => (Kind => At_Most, Count => Unsigned_32 (Draw (1, 255)) * 128));
            for I in 1 .. Draw (0, Maximum_Entries) loop
               declare
                  Length : constant Positive := Draw (1, 12);
                  Text : String (1 .. Length);
                  E : Startup_Entry;
               begin
                  Text (1) := Character'Val (Character'Pos ('a') + Draw (0, 25));
                  for K in 2 .. Length loop
                     Text (K) := Alphabet (Draw (1, Alphabet'Length));
                     if Text (K) = '-' and then (Text (K - 1) = '-' or else K = Length) then
                        Text (K) := 'x';
                     end if;
                  end loop;
                  E := Entry_Of (Text, Kinds (Draw (0, 11)),
                                 (if Draw (0, 1) = 0 then Required else Optional));
                  case Draw (0, 2) is
                     when 0 => null;
                     when 1 => E.State := Granted; E.Handle := Unsigned_64 (Draw (1, 255)) * 16#1_0000_0001#;
                     when others =>
                        if E.Requirement = Optional then E.State := Absent; end if;
                  end case;
                  if Find (T, E.Label) = 0 then
                     T.Count := T.Count + 1;
                     T.Entries (T.Count) := E;
                  end if;
               end;
            end loop;
            Check (Valid_Table (T), "random table valid");
            declare
               Raw : constant Bytes := Encoded (T);
            begin
               Decode (Raw, Result, Outcome);
               Check (Outcome = Decoded and then Result = T, "random round trip");
               Check (Reference_Valid (Raw), "reference accepts random table");
               --  Mutate one byte; the decoder and the reference must agree,
               --  and an accepted mutation must re-encode to itself.
               if Raw'Length > 0 then
                  declare
                     M : Bytes := Raw;
                     At_Offset : constant Natural := Draw (0, Raw'Length - 1);
                  begin
                     M (M'First + At_Offset) := Random_Bytes.Random (Gen);
                     Decode (M, Result, Outcome);
                     Check ((Outcome = Decoded) = Reference_Valid (M), "mutation agrees with reference");
                     if Outcome = Decoded then
                        Accepted := Accepted + 1;
                        Check (Encoded (Result) = M, "accepted mutation is canonical");
                     else
                        Refused := Refused + 1;
                        Check (Result = Empty_Table, "refused mutation leaves empty table");
                     end if;
                  end;
               end if;
            end;
         end;
      end loop;
      Check (Accepted > 0 and then Refused > 0, "mutations exercised both outcomes");

      --  Pure noise: random lengths and bytes (with the magic and version
      --  kept half the time so the decoder gets past the header).
      for Round in 1 .. 20_000 loop
         declare
            Length : constant Natural := Draw (0, 6) * 80 + 16 + Draw (0, 2) - 1;
            Raw : Bytes (1 .. Natural'Min (Length, Maximum_Table_Bytes));
            Result : Table;
            Outcome : Decode_Result;
         begin
            for B of Raw loop B := Random_Bytes.Random (Gen); end loop;
            if Raw'Length >= 16 and then Draw (0, 1) = 0 then
               Raw (1 .. 8) := [16#43#, 16#42#, 16#53#, 16#54#, 1, 0, Byte ((Raw'Length - 16) / 80), 0];
               Raw (9) := Byte (Draw (1, 3));
               Raw (10 .. 12) := [0, 0, 0];
            end if;
            Decode (Raw, Result, Outcome);
            Check ((Outcome = Decoded) = Reference_Valid (Raw), "noise agrees with reference");
            if Outcome = Decoded then
               Check (Encoded (Result) = Raw, "accepted noise is canonical");
            end if;
         end;
      end loop;
   end Test_Random;

   ---------------------------------------------------------------------------
   --  Scenarios for the loaded table and the generated binding.
   ---------------------------------------------------------------------------
   package SS renames CuBit.Startup_State;
   package HS renames CuBit.Handles.Startup;
   package TW renames CuBit.Handles.Test_Words;
   use type SS.Load_Result, SS.Verification_Status, SS.Verification;

   --  The table a launcher builds for the fixture manifest
   --  (tests/ccl-manifests/fixtures/startup/manifest.ccl).
   function Launched (Pointer_Granted : Boolean := True) return Table is
      T : Table := Empty_Table;
   begin
      T.Limit := (Kind => At_Most, Count => 512);
      T.Count := 7;
      T.Entries (1) := Entry_Of ("registers", Device_Memory, Required, Granted, 16#1_0000_0010#);
      T.Entries (2) := Entry_Of ("device", Interrupt, Required, Granted, 16#1_0000_0011#);
      T.Entries (3) := Entry_Of ("devmgr", Endpoint, Required, Granted, 16#1_0000_0012#);
      T.Entries (4) := (if Pointer_Granted
                        then Entry_Of ("pointer-events", Notification, Optional, Granted, 16#1_0000_0013#)
                        else Entry_Of ("pointer-events", Notification, Optional, Absent));
      T.Entries (5) := Entry_Of ("real-time", Scheduling, Required, Granted, 16#1_0000_0014#);
      T.Entries (6) := Entry_Of (Self_Text, Endpoint, Required, Granted, 16#1_0000_0000#);
      T.Entries (7) := Entry_Of (Self_Process_Text, Process, Required, Granted, 16#1_0000_0001#);
      return T;
   end Launched;


   ---------------------------------------------------------------------------
   --  The launcher's builder, with a scripted minter. Assertions are on, so
   --  Build's postcondition is also checked on every call.
   ---------------------------------------------------------------------------
   type Script is array (Entry_Index) of Mint_Outcome;
   type Entry_Index_List is array (Positive range <>) of Entry_Index;
   type Report_Log is array (1 .. Maximum_Entries) of Mint_Outcome;
   type Scripted is record
      Answers   : Script := [others => Granted];
      Zero_At   : Entry_Count := 0;   --  answer Granted with handle 0 here
      Minted    : Natural := 0;
      Reports   : Natural := 0;
      Reported  : Report_Log := [others => Granted];
   end record;

   procedure Scripted_Mint
     (Into : in out Scripted; Item : Startup_Entry; Index : Entry_Index;
      Handle : out Unsigned_64; Outcome : out Mint_Outcome)
   is
      pragma Unreferenced (Item);
   begin
      Into.Minted := Into.Minted + 1;
      Outcome := Into.Answers (Index);
      Handle := (if Outcome = Granted and then Index /= Into.Zero_At
                 then 16#1_0000_0000# + Unsigned_64 (Index) else No_Handle);
   end Scripted_Mint;

   procedure Scripted_Report
     (Into : in out Scripted; Item : Startup_Entry; Index : Entry_Index; Outcome : Mint_Outcome)
   is
      pragma Unreferenced (Item);
   begin
      Into.Reports := Into.Reports + 1;
      Check (Index = Into.Reports, "reports arrive in order");
      Into.Reported (Into.Reports) := Outcome;
   end Scripted_Report;

   procedure Build_Startup is new Build (Scripted, Scripted_Mint, Scripted_Report);

   Self_Word         : constant Unsigned_64 := 16#1_0000_0000#;
   Self_Process_Word : constant Unsigned_64 := 16#1_0000_0001#;

   --  The fixture manifest's declaration (its .cubit.startup section).
   function Fixture_Declaration return Table is
      T : Table := Launched;
   begin
      T.Count := 5;
      for I in 1 .. T.Count loop
         T.Entries (I).State := Unassigned;
         T.Entries (I).Handle := No_Handle;
      end loop;
      return T;
   end Fixture_Declaration;

   procedure Test_Builder is
      Declared : constant Table := Fixture_Declaration;
      Built : Table;
      Result : Build_Result;
   begin
      Check (Valid_Table (Declared) and then Declaration (Declared), "fixture declaration");
      --  Everything granted.
      declare
         Source : Scripted;
      begin
         Build_Startup (Source, Declared, Self_Word, Self_Process_Word, Built, Result);
         Check (Result = (Status => Ready, Index => 0, Why => Granted), "all granted");
         Check (Built.Count = 7 and then Complete (Built) and then Follows (Declared, Built), "built table");
         Check (Source.Minted = 5 and then Source.Reports = 5, "one mint and one report each");
         Check (Find (Built, To_Name (Self_Text)) = 6 and then Built.Entries (6).Handle = Self_Word,
                "self entry");
         Check (Built.Entries (7).Handle = Self_Process_Word, "self-process entry");
         Check (Built.Entries (4).State = Granted, "optional granted");
         Check (Built.Limit = (Kind => At_Most, Count => 512), "limit kept");
         Check (Encoded (Built)'Length = Header_Bytes + 7 * Entry_Bytes, "built table encodes");
      end;
      --  The optional entry refused: Absent, launch goes on.
      for Why in Mint_Refusal loop
         declare
            Source : Scripted;
         begin
            Source.Answers (4) := Why;
            Build_Startup (Source, Declared, Self_Word, Self_Process_Word, Built, Result);
            Check (Result.Status = Ready, "optional refused is not fatal: " & Mint_Outcome'Image (Why));
            Check (Built.Entries (4).State = Absent and then Built.Entries (4).Handle = No_Handle,
                   "optional refused is absent");
            Check (Source.Reported (4) = Why, "optional refusal reported");
         end;
      end loop;
      --  A required entry refused: the launch fails, naming it and why;
      --  nothing after it is minted.
      for Why in Mint_Refusal loop
         for At_Index of Entry_Index_List'[1, 3, 5] loop
            declare
               Source : Scripted;
            begin
               Source.Answers (At_Index) := Why;
               Build_Startup (Source, Declared, Self_Word, Self_Process_Word, Built, Result);
               Check (Result = (Status => Required_Refused, Index => At_Index, Why => Why),
                      "required refused at" & At_Index'Image & " " & Mint_Outcome'Image (Why));
               Check (Built = Empty_Table, "no table on refusal");
               Check (Source.Minted = At_Index and then Source.Reports = At_Index,
                      "minting stops at the refusal");
               Check (Source.Reported (At_Index) = Why, "required refusal reported");
            end;
         end loop;
      end loop;
      --  Granted without a handle.
      declare
         Source : Scripted;
      begin
         Source.Zero_At := 2;
         Build_Startup (Source, Declared, Self_Word, Self_Process_Word, Built, Result);
         Check (Result.Status = Minter_Fault and then Result.Index = 2, "zero handle is a fault");
         Check (Built = Empty_Table, "no table on fault");
      end;
      --  Refusals before any mint.
      declare
         Source : Scripted;
         Bad : Table := Declared;
      begin
         Bad.Entries (1).State := Granted;
         Bad.Entries (1).Handle := 7;
         Build_Startup (Source, Bad, Self_Word, Self_Process_Word, Built, Result);
         Check (Result.Status = Not_A_Declaration and then Source.Minted = 0, "not a declaration");
         Build_Startup (Source, Declared, No_Handle, Self_Process_Word, Built, Result);
         Check (Result.Status = Bad_Self_Handle and then Source.Minted = 0, "zero self handle");
         Build_Startup (Source, Declared, Self_Word, No_Handle, Built, Result);
         Check (Result.Status = Bad_Self_Handle and then Source.Minted = 0, "zero self-process handle");
         Bad := Declared;
         Bad.Entries (2).Label := To_Name (Self_Process_Text);
         Build_Startup (Source, Bad, Self_Word, Self_Process_Word, Built, Result);
         Check (Result.Status = Reserved_Name_Declared and then Source.Minted = 0, "reserved name");
         Bad := Empty_Table;
         Bad.Count := Maximum_Entries - 1;
         for I in 1 .. Bad.Count loop
            Bad.Entries (I) := Entry_Of ("e" & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Left));
         end loop;
         Build_Startup (Source, Bad, Self_Word, Self_Process_Word, Built, Result);
         Check (Result.Status = Too_Many_Entries and then Source.Minted = 0, "too many entries");
         Bad.Count := Maximum_Entries - 2;
         Build_Startup (Source, Bad, Self_Word, Self_Process_Word, Built, Result);
         Check (Result.Status = Ready and then Built.Count = Maximum_Entries, "full table builds");
      end;
      --  Random declarations and answers; Build's postcondition is checked
      --  on every call (assertions are on).
      for Round in 1 .. 3_000 loop
         declare
            Source : Scripted;
            T : Table := Empty_Table;
            Refused_Required : Entry_Count := 0;
         begin
            T.Count := Draw (0, Maximum_Entries - 2);
            for I in 1 .. T.Count loop
               T.Entries (I) := Entry_Of ("c" & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Left),
                                          Process, (if Draw (0, 1) = 0 then Required else Optional));
               Source.Answers (I) := (if Draw (0, 3) = 0 then Mint_Outcome'Val (Draw (1, 5)) else Granted);
               if Refused_Required = 0 and then Source.Answers (I) /= Granted
                 and then T.Entries (I).Requirement = Required
               then
                  Refused_Required := I;
               end if;
            end loop;
            Build_Startup (Source, T, Self_Word, Self_Process_Word, Built, Result);
            if Refused_Required = 0 then
               Check (Result.Status = Ready and then Follows (T, Built), "random build");
               declare
                  Loaded : Table;
                  Outcome : Decode_Result;
               begin
                  Decode (Encoded (Built), Loaded, Outcome);
                  Check (Outcome = Decoded and then Loaded = Built and then Complete (Loaded),
                         "random build round trips");
               end;
            else
               Check (Result.Status = Required_Refused and then Result.Index = Refused_Required,
                      "random refusal names the first required refusal");
            end if;
         end;
      end loop;
   end Test_Builder;

   procedure Load (Item : Table; Want : SS.Load_Result) is
      Outcome : SS.Load_Result;
   begin
      SS.Load (Encoded (Item), Outcome);
      Check (Outcome = Want, "load: got " & SS.Load_Result'Image (Outcome));
   end Load;

   procedure Scenario (Name : String) is
   begin
      if Name /= "not-loaded" then
         Check (SS.Verify (Startup.Expected).Status = SS.Not_Loaded, "verify before load");
         Check (not CuBit.Handles.Valid (Startup.Registers), "no handle before load");
      end if;
      if Name = "ready" or else Name = "absent" then
         Load (Launched (Pointer_Granted => Name = "ready"), SS.Loaded);
         Check (SS.Verify (Startup.Expected) = (Status => SS.Ready, Index => 0), "verify ready");
         Check (TW.Word (Startup.Registers) = 16#1_0000_0010#, "registers handle");
         Check (TW.Word (Startup.Device) = 16#1_0000_0011#, "device handle");
         Check (TW.Word (Startup.Devmgr) = 16#1_0000_0012#, "devmgr handle");
         Check (TW.Word (Startup.Real_Time) = 16#1_0000_0014#, "real-time handle");
         Check (Startup.Pointer_Events.Present = (Name = "ready"), "optional presence");
         if Startup.Pointer_Events.Present then
            Check (TW.Word (Startup.Pointer_Events.Value) = 16#1_0000_0013#,
                   "optional handle");
         end if;
         --  A name of another kind does not resolve: no cross-kind handles.
         Check (not CuBit.Handles.Valid (HS.Endpoint_Named ("registers")), "kind is checked");
         Check (not CuBit.Handles.Valid (HS.Endpoint_Named ("Registers")), "invalid name");
         Check (CuBit.Handles.Valid (HS.Endpoint_Named (Self_Text)), "self endpoint");
         Check (CuBit.Handles.Valid (HS.Process_Named (Self_Process_Text)), "self process");
         --  Loaded once: a second table is refused and changes nothing.
         Load (Empty_Table, SS.Already_Loaded);
         Check (TW.Word (Startup.Registers) = 16#1_0000_0010#, "second load ignored");
      elsif Name = "built" then
         declare
            Source : Scripted;
            Built : Table;
            Result : Build_Result;
         begin
            Source.Answers (4) := Not_Available;
            Build_Startup (Source, Fixture_Declaration, Self_Word, Self_Process_Word, Built, Result);
            Check (Result.Status = Ready, "builder ready");
            Load (Built, SS.Loaded);
            Check (SS.Verify (Startup.Expected) = (Status => SS.Ready, Index => 0), "builder output verifies");
            Check (not Startup.Pointer_Events.Present, "refused optional is absent");
            Check (TW.Word (Startup.Device) = 16#1_0000_0002#, "builder handle by name");
         end;
      elsif Name = "missing" then
         declare
            T : Table := Launched;
         begin
            T.Entries (3) := Entry_Of ("devmgr-x", Endpoint, Required, Granted, 16#1_0000_0012#);
            Load (T, SS.Loaded);
            Check (SS.Verify (Startup.Expected) = (Status => SS.Missing, Index => 3), "verify missing");
            Check (not CuBit.Handles.Valid (Startup.Devmgr), "missing required is null");
         end;
      elsif Name = "wrong-kind" then
         declare
            T : Table := Launched;
         begin
            T.Entries (2).Kind := Device_Memory;
            Load (T, SS.Loaded);
            Check (SS.Verify (Startup.Expected) = (Status => SS.Wrong_Kind, Index => 2), "verify wrong kind");
            Check (not CuBit.Handles.Valid (Startup.Device), "wrong kind is null");
         end;
      elsif Name = "wrong-need" then
         declare
            T : Table := Launched;
         begin
            T.Entries (5).Requirement := Optional;
            Load (T, SS.Loaded);
            Check (SS.Verify (Startup.Expected) = (Status => SS.Wrong_Need, Index => 5), "verify wrong need");
         end;
      elsif Name = "incomplete" then
         declare
            T : Table := Launched;
         begin
            T.Entries (1).State := Unassigned;
            T.Entries (1).Handle := No_Handle;
            Load (T, SS.Incomplete);
            Check (SS.Verify (Startup.Expected).Status = SS.Not_Loaded, "incomplete is not loaded");
            Load (Launched, SS.Loaded);
            Check (SS.Verify (Startup.Expected).Status = SS.Ready, "a later good load still works");
         end;
      elsif Name = "malformed" then
         declare
            Raw : Bytes := Encoded (Launched);
            Outcome : SS.Load_Result;
         begin
            Raw (Raw'Last) := 1;  --  padding after the last name
            SS.Load (Raw, Outcome);
            Check (Outcome = SS.Malformed, "malformed refused");
            Check (SS.Verify (Startup.Expected).Status = SS.Not_Loaded, "malformed is not loaded");
         end;
      elsif Name = "not-loaded" then
         Check (SS.Verify (Startup.Expected) = (Status => SS.Not_Loaded, Index => 0), "nothing loaded");
         Check (not Startup.Pointer_Events.Present, "optional absent before load");
      else
         Check (False, "unknown scenario " & Name);
      end if;
   end Scenario;

   --  Launch envelopes (CuBit.Launch_Envelopes): a table appended after a
   --  payload splits back out exactly; a block without a trailer is all
   --  payload; corrupt trailers and noise never split into anything that
   --  leaves the block.
   procedure Test_Envelopes is
      package LE renames CuBit.Launch_Envelopes;
      use type LE.Block;
      type Size_List is array (Positive range <>) of Table_Length;
      Sizes : constant Size_List := [0, 1, 7, 8, 9, 16, 300, Maximum_Table_Bytes];
      subtype Small is LE.Block (1 .. 8_192);
      Item : Small;
      Length : LE.Block_Length;
      Fits, Present : Boolean;
      Payload : LE.Block_Length;
      First : Positive;
      Count : LE.Table_Count;
      Encoded : Bytes (1 .. Maximum_Table_Bytes);
      Used : Table_Length;
   begin
      for Payload_Bytes in 0 .. 40 loop
         for Table_Bytes of Sizes loop
            Item := [others => 16#AA#];
            Length := Payload_Bytes;
            declare
               T : constant Bytes (1 .. Table_Bytes) :=
                 [for I in 1 .. Table_Bytes => Unsigned_8 (Natural (I) mod 251)];
            begin
               LE.Append (Item, Length, T, Fits);
               Check (Fits, "append fits");
               LE.Split (Item (1 .. Length), Payload, First, Count, Present);
               Check (Present and then Payload = Payload_Bytes and then Count = Table_Bytes
                      and then First = Payload_Bytes + 1, "split finds what append put");
               if Present and then Count = Table_Bytes then
                  Check (Item (First .. First + Count - 1) = LE.Block (T), "table bytes kept");
               end if;
               Check ((for all I in 1 .. Payload_Bytes => Item (I) = 16#AA#), "payload kept");
            end;
         end loop;
      end loop;
      --  No trailer: all payload.
      Item := [others => 0];
      LE.Split (Item (1 .. 100), Payload, First, Count, Present);
      Check (not Present and then Payload = 100 and then Count = 0, "no trailer");
      LE.Split (Item (1 .. 0), Payload, First, Count, Present);
      Check (not Present and then Payload = 0, "empty block");
      --  A real startup table round-trips through an envelope.
      Encode (Launched, Encoded, Used);
      Length := 24;
      Item := [others => 0];
      LE.Append (Item, Length, Encoded (1 .. Used), Fits);
      LE.Split (Item (1 .. Length), Payload, First, Count, Present);
      Check (Present and then Payload = 24 and then Count = Used, "startup table envelope");
      --  Corrupt trailers: a count past the block, or over the table limit.
      declare
         Bad : Small := Item;
      begin
         Bad (Length - 15) := 16#FF#;
         Bad (Length - 14) := 16#FF#;
         LE.Split (Bad (1 .. Length), Payload, First, Count, Present);
         Check (not Present and then Payload = Length, "oversized count refused");
      end;
      --  No room.
      declare
         Tight : LE.Block (1 .. 40) := [others => 0];
         L : LE.Block_Length := 30;
      begin
         LE.Append (Tight, L, Encoded (1 .. 9), Fits);
         Check (not Fits and then L = 30, "no room refused");
      end;
      --  Noise never splits outside the block.
      for Round in 1 .. 20_000 loop
         declare
            N : constant Natural := Natural (Random_Bytes.Random (Gen)) * 31 mod 4_000;
         begin
            for I in 1 .. N loop
               Item (I) := Random_Bytes.Random (Gen);
            end loop;
            if N >= 16 and then Round mod 3 = 0 then
               --  A valid magic with a random count.
               for I in 0 .. 7 loop
                  Item (N - 7 + I) := Unsigned_8 (Shift_Right (LE.Magic, 8 * I) and 16#FF#);
               end loop;
            end if;
            LE.Split (Item (1 .. N), Payload, First, Count, Present);
            Check (Payload <= N and then
                   (if Present then Payload + LE.Padded (Count) + LE.Trailer_Bytes = N
                    else Payload = N), "noise stays inside");
         end;
      end loop;
   end Test_Envelopes;

begin
   if Ada.Command_Line.Argument_Count = 1 then
      Scenario (Ada.Command_Line.Argument (1));
      Put_Line ("startup-tables " & Ada.Command_Line.Argument (1) & ":" & Natural'Image (Checks) &
                " checks," & Natural'Image (Failures) & " failures");
      if Failures /= 0 then
         raise Program_Error with "startup scenario failed";
      end if;
      return;
   end if;
   Random_Bytes.Reset (Gen, 20261013);
   Test_Builder;
   Test_Round_Trip;
   Test_Refusals;
   Test_Random;
   Test_Envelopes;
   Put_Line ("startup-tables:" & Natural'Image (Checks) & " checks," &
             Natural'Image (Failures) & " failures");
   if Failures /= 0 then
      raise Program_Error with "startup-tables tests failed";
   end if;
end Main;
