--  Hosted tests for CuBit.Launch_Arguments: the encoder round trip, every
--  rejection reason, limits, the C entry point, and a differential check of
--  Validate against an independent reference on random and mutated blocks.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with Interfaces.C;
with CuBit.Launch_Arguments; use CuBit.Launch_Arguments;
with CuBit.Launch_Arguments_C;
with CuBit.Child_Exits;
with Process_Launch;
with CuBit.Launch_Authority;

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

   --  Independent reference: decode the header, then split the string bytes
   --  at each NUL and require exactly the declared number of strings with
   --  nothing after the last terminator; the description (format 3) fills
   --  the rest and is not looked at here.
   function Reference_Valid (Item : Block) return Boolean is
      function U32 (Offset : Natural) return Long_Long_Integer is
        (Long_Long_Integer (Item (Offset + 1)) +
         256 * Long_Long_Integer (Item (Offset + 2)) +
         65536 * Long_Long_Integer (Item (Offset + 3)) +
         16777216 * Long_Long_Integer (Item (Offset + 4)));
      Arguments, Environment, Directory, Strings_Bytes : Long_Long_Integer;
      Strings : Natural := 0;
      In_String : Boolean := False;
   begin
      if Item'Length < 16 or else Item'Length > 65536 then
         return False;
      end if;
      if Item (1) /= 3 or else Item (2) /= 0 then
         return False;
      end if;
      Directory := Long_Long_Integer (Item (3)) + 256 * Long_Long_Integer (Item (4));
      if Directory > 1 then
         return False;
      end if;
      Arguments := U32 (4);
      Environment := U32 (8);
      Strings_Bytes := U32 (12);
      if Arguments > 4096 or else Environment > 4096
        or else Arguments + Environment + Directory > 4096
        or else Strings_Bytes > Long_Long_Integer (Item'Length - 16)
        or else Long_Long_Integer (Item'Length - 16) - Strings_Bytes > 4096
      then
         return False;
      end if;
      for I in 17 .. 16 + Natural (Strings_Bytes) loop
         In_String := True;
         if Item (I) = 0 then
            Strings := Strings + 1;
            In_String := False;
         end if;
      end loop;
      return not In_String
        and then Long_Long_Integer (Strings) = Arguments + Environment + Directory;
   end Reference_Valid;

   function Text (Item : Block; First : Positive; Last : Natural)
     return String is
      Result : String (1 .. Last - First + 1);
   begin
      for K in Result'Range loop
         Result (K) := Character'Val (Item (First + K - 1));
      end loop;
      return Result;
   end Text;

   type Text_Access is access constant String;
   type Text_List is array (Positive range <>) of Text_Access;

   Arguments_In : constant Text_List :=
     [new String'("args-check.app"), new String'("alpha"),
      new String'("two words"), new String'(""), new String'("--flag=x")];
   Environment_In : constant Text_List :=
     [new String'("CUBIT_TEST=1"), new String'("EMPTY=")];
   Directory_In : constant String := "@nvme:0/build/objects";

   B : Builder;
   Length : Present_Length;
   Accepted : Boolean;
begin
   ----------------------------------------------------------------- round trip
   Start (B);
   for A of Arguments_In loop
      Add_Argument (B, A.all, Accepted);
      Check (Accepted, "add argument " & A.all);
   end loop;
   for E of Environment_In loop
      Add_Environment (B, E.all, Accepted);
      Check (Accepted, "add environment " & E.all);
   end loop;
   Add_Argument (B, "late", Accepted);
   Check (not Accepted, "argument after environment rejected");
   Add_Directory (B, Directory_In, Accepted);
   Check (Accepted, "add the working directory");
   Add_Directory (B, "@nvme:0/second", Accepted);
   Check (not Accepted, "a second working directory rejected");
   Add_Environment (B, "LATE=1", Accepted);
   Check (not Accepted, "environment after the directory rejected");
   Add_Argument (B, "bad" & ASCII.NUL & "x", Accepted);
   Check (not Accepted, "NUL inside a value rejected");
   Finish (B, Length, Accepted);
   Check (Accepted, "finish accepts");
   declare
      Item : constant Block := B.Data (1 .. Length);
      First : Positive;
      Last : Natural;
      Found : Boolean;
   begin
      Check (Validate (Item) = Valid, "round trip validates");
      Check (Reference_Valid (Item), "reference agrees on round trip");
      Check (Arguments_Declared (Item) = Arguments_In'Length,
             "argument count");
      Check (Environment_Declared (Item) = Environment_In'Length,
             "environment count");
      Check (Directory_Declared (Item) = 1, "directory count");
      for K in Arguments_In'Range loop
         Locate (Item, K, First, Last, Found);
         Check (Found and then Text (Item, First, Last) = Arguments_In (K).all,
                "argument" & K'Image);
      end loop;
      for K in Environment_In'Range loop
         Locate (Item, Arguments_In'Length + K, First, Last, Found);
         Check (Found and then
                Text (Item, First, Last) = Environment_In (K).all,
                "environment" & K'Image);
      end loop;
      Locate (Item, Arguments_In'Length + Environment_In'Length + 1,
              First, Last, Found);
      Check (Found and then Text (Item, First, Last) = Directory_In,
             "the working directory follows the environment");
      Locate (Item, Arguments_In'Length + Environment_In'Length + 2,
              First, Last, Found);
      Check (not Found, "no string past the declared ones");

      ---------------------------------------------------------- C entry point
      declare
         use type Interfaces.C.int;
         Arguments, Environment, Directory : aliased Unsigned_32 := 99;
      begin
         Check (CuBit.Launch_Arguments_C.Validate
                  (Item'Address, Unsigned_32 (Item'Length),
                   Arguments'Access, Environment'Access, Directory'Access) = 1
                and then Arguments = Arguments_In'Length
                and then Environment = Environment_In'Length
                and then Directory = 1,
                "C validate accepts with counts");
         Arguments := 99;
         Check (CuBit.Launch_Arguments_C.Validate
                  (Item'Address, Unsigned_32 (Item'Length - 1),
                   Arguments'Access, Environment'Access, Directory'Access) = 0
                and then Arguments = 99,
                "C validate rejects a truncated block, counts untouched");
         Check (CuBit.Launch_Arguments_C.Validate
                  (Item'Address, Maximum_Block_Bytes + 1,
                   Arguments'Access, Environment'Access, Directory'Access) = 0,
                "C validate rejects an oversized length");
      end;

      ---------------------------------------------- each rejection, by reason
      declare
         M : Block := Item;
      begin
         M (1) := 1;
         Check (Validate (M) = Unknown_Format, "version 1 rejected");
         M := Item;
         M (3) := 2;
         Check (Validate (M) = Too_Many_Directories, "two directories rejected");
         M := Item;
         M (4) := 1;
         Check (Validate (M) = Too_Many_Directories, "directory count 257 rejected");
         M := Item;
         M (3) := 0;
         Check (Validate (M) = Count_Mismatch, "directory string undeclared");
         M := Item;
         M (5) := M (5) + 1;
         Check (Validate (M) = Count_Mismatch, "one more argument declared");
         M := Item;
         M (9) := M (9) - 1;
         Check (Validate (M) = Count_Mismatch, "one less environment entry");
         M := Item;
         M (8) := 1;
         Check (Validate (M) = Too_Many_Strings, "huge argument count");
         M := Item;
         M (13) := M (13) + 1;
         Check (Validate (M) = Length_Mismatch, "string bytes mismatch");
         M := Item;
         M (M'Last) := Character'Pos ('x');
         Check (Validate (M) = Unterminated, "last string unterminated");
         M := Item;
         M (Header_Bytes + 4) := 0;
         Check (Validate (M) = Count_Mismatch, "extra terminator inside");
      end;
      Check (Validate (Item (1 .. 15)) = Wrong_Length, "short header");
      Check (Validate (Item (1 .. 0)) = Wrong_Length, "empty block");
      Check (Validate (Item (1 .. Item'Last - 1)) = Length_Mismatch,
             "truncated block");
   end;

   ------------------------------------------------------------------- limits
   Start (B);
   Finish (B, Length, Accepted);
   Check (Accepted and then Length = Header_Bytes,
          "an empty block (no strings) is well formed");
   Start (B);
   for K in 1 .. Maximum_Strings loop
      Add_Argument (B, "", Accepted);
      exit when not Accepted;
   end loop;
   Check (B.Arguments = Maximum_Strings, "maximum string count reached");
   Add_Environment (B, "X=1", Accepted);
   Check (not Accepted, "string count limit enforced");
   Finish (B, Length, Accepted);
   Check (Accepted and then Validate (B.Data (1 .. Length)) = Valid,
          "maximum count block validates");
   Start (B);
   declare
      Big : constant String (1 .. 1000) := [others => 'a'];
   begin
      loop
         Add_Argument (B, Big, Accepted);
         exit when not Accepted;
      end loop;
      Check (B.Used + Big'Length + 1 > Maximum_Block_Bytes,
             "byte limit stops the encoder");
      Add_Argument (B, Big (1 .. Maximum_Block_Bytes - B.Used - 1), Accepted);
      Check (Accepted and then B.Used = Maximum_Block_Bytes,
             "a block can fill the maximum exactly");
      Finish (B, Length, Accepted);
      Check (Accepted and then Length = Maximum_Block_Bytes
             and then Validate (B.Data (1 .. Length)) = Valid,
             "maximum-size block validates");
   end;
   --------------------------------------------- an attached description
   declare
      B : Builder;
      Length : Present_Length;
      Accepted : Boolean;
      Description : constant Block := [16#50#, 16#44#, 16#53#, 16#43#, 0, 0, 7];
      First : Positive;
      Last : Natural;
      Found : Boolean;
   begin
      Start (B);
      Add_Argument (B, "ld.app", Accepted);
      Add_Argument (B, "-o", Accepted);
      Finish (B, Length, Accepted);
      Attach_Description (B.Data, Length, Description, Accepted);
      Check (Accepted and then Length = Header_Bytes + 10 + Description'Length
             and then Validate (B.Data (1 .. Length)) = Valid
             and then Reference_Valid (B.Data (1 .. Length)),
             "a description attaches after the strings");
      Check (B.Data (Length - Description'Length + 1 .. Length) = Description
             and then Strings_Last (B.Data (1 .. Length)) = Header_Bytes + 10,
             "the description follows the strings exactly");
      Locate (B.Data (1 .. Length), 2, First, Last, Found);
      Check (Found and then Text (B.Data (1 .. Length), First, Last) = "-o",
             "strings still locate");
      Locate (B.Data (1 .. Length), 3, First, Last, Found);
      Check (not Found, "the description holds no strings");
      declare
         Before : constant Present_Length := Length;
      begin
         Attach_Description (B.Data, Length, Description, Accepted);
         Check (not Accepted and then Length = Before, "a second description is refused");
      end;
      --  The block does not record the description's length: a truncated
      --  one is still a valid block, and its decoder refuses it
      --  (CuBit.Program_Descriptions.Decode, tests/program-descriptions).
      Check (Validate (B.Data (1 .. Length - 1)) = Valid
             and then Strings_Last (B.Data (1 .. Length - 1)) = Header_Bytes + 10,
             "a truncated description leaves the strings intact");
   end;
   Check (Request_Valid (1, 0) and then Request_Valid (255, 16)
          and then Request_Valid (10, Maximum_Block_Bytes)
          and then not Request_Valid (0, 0) and then not Request_Valid (256, 0)
          and then not Request_Valid (10, 15)
          and then not Request_Valid (10, Maximum_Block_Bytes + 1),
          "request field bounds");

   --------------------------------------------------- differential, random
   declare
      subtype Small_Length is Natural range 0 .. 48;
      package Lengths is new Ada.Numerics.Discrete_Random (Small_Length);
      package Bytes is new Ada.Numerics.Discrete_Random (Unsigned_8);
      subtype Choice is Natural range 0 .. 9;
      package Choices is new Ada.Numerics.Discrete_Random (Choice);
      L : Lengths.Generator;
      G : Bytes.Generator;
      C : Choices.Generator;
      Agree, Valid_Seen : Natural := 0;
      Trials : constant := 400_000;
   begin
      Lengths.Reset (L, 7);
      Bytes.Reset (G, 11);
      Choices.Reset (C, 13);
      for T in 1 .. Trials loop
         declare
            N : constant Small_Length := Lengths.Random (L);
            Item : Block (1 .. N);
            Strings : Natural := 0;
            Described : Natural;
         begin
            --  Mostly plausible blocks: a correct header, string bytes drawn
            --  from {0, 'a'} so terminator counts often match.
            for K in Item'Range loop
               Item (K) := (if Choices.Random (C) < 4 then 0
                            else Character'Pos ('a'));
            end loop;
            --  Sometimes a description after the strings.
            Described := (if N >= 16 and then Choices.Random (C) < 3
                          then Natural (Bytes.Random (G)) mod (N - 15) else 0);
            for K in 17 .. N - Described loop
               if Item (K) = 0 then
                  Strings := Strings + 1;
               end if;
            end loop;
            if N >= 16 then
               Item (1 .. 16) := [3, 0, 0, 0, others => 0];
               declare
                  D : constant Natural :=
                    (if Strings > 0 and then Choices.Random (C) < 5 then 1 else 0);
                  A : constant Natural := (Strings - D) / 2;
                  E : constant Natural := Strings - D - A;
               begin
                  Item (3) := Unsigned_8 (D);
                  Item (5) := Unsigned_8 (A);
                  Item (9) := Unsigned_8 (E);
                  Item (13) := Unsigned_8 (N - 16 - Described);
               end;
               --  Sometimes break one header or string byte at random.
               if Choices.Random (C) < 3 then
                  Item (1 + Natural (Bytes.Random (G)) mod N) :=
                    Bytes.Random (G);
               end if;
            end if;
            if (Validate (Item) = Valid) = Reference_Valid (Item) then
               Agree := Agree + 1;
            end if;
            if Validate (Item) = Valid then
               Valid_Seen := Valid_Seen + 1;
            end if;
         end;
      end loop;
      Check (Agree = Trials, "random blocks: Validate agrees with reference");
      Check (Valid_Seen > Trials / 10, "random blocks include valid ones");
      Put_Line ("launch-arguments: random trials" & Trials'Image &
                ", valid" & Valid_Seen'Image);
   end;

   ------------------------------------------- exit reports, kernel and user
   declare
      use CuBit.Child_Exits;
      Exited_Wire : constant Unsigned_64 :=
        Process_Launch.Termination_Kind'Enum_Rep (Process_Launch.Exited);
      Stopped_Wire : constant Unsigned_64 :=
        Process_Launch.Termination_Kind'Enum_Rep (Process_Launch.Stopped);
   begin
      Check (Exited_Wire = Termination_Kind'Enum_Rep (Exited)
             and then Stopped_Wire = Termination_Kind'Enum_Rep (Stopped)
             and then Process_Launch.Child_Exit_Words = Event_Words
             and then Process_Launch.Arguments_Base = Block_Address
             and then Process_Launch.Maximum_Bytes = Maximum_Block_Bytes,
             "kernel and runtime agree on the wire values");
      Check (Process_Launch.To_Exit_Code (300) = 44
             and then Process_Launch.To_Exit_Code (255) = 255,
             "exit codes keep the low 8 bits");
      Check (Valid (4, 7, Exited_Wire, 42)
             and then Decode (7, Exited_Wire, 42, 9) = (7, Exited, 42, 9)
             and then Valid (4, 7, Stopped_Wire, 0)
             and then not Valid (3, 7, Exited_Wire, 0)
             and then not Valid (4, 0, Exited_Wire, 0)
             and then not Valid (4, 7, 3, 0)
             and then not Valid (4, 7, Exited_Wire, 256)
             and then not Valid (4, 7, Stopped_Wire, 1),
             "child exit event validation");
      Check (Process_Launch.Page_Count (1) = 1
             and then Process_Launch.Page_Count (4096) = 1
             and then Process_Launch.Page_Count (4097) = 2
             and then Process_Launch.Page_Count (65536) = 16,
             "argument page counts");
   end;

   ------------------------------------------------ launch tables, attenuation
   declare
      package LT renames CuBit.Launch_Authority;
      use type LT.Table_Bytes;
      function Name_Entry (Name : String) return LT.Table_Bytes is
         Result : LT.Table_Bytes (1 .. Name'Length + 1);
      begin
         Result (1) := Unsigned_8 (Name'Length);
         for K in Name'Range loop
            Result (K - Name'First + 2) := Character'Pos (Name (K));
         end loop;
         return Result;
      end Name_Entry;
      Header : constant LT.Table_Bytes :=
        [16#4C#, 16#4E#, 16#43#, 16#48#, 1, 0, 3, 0];
      Table : constant LT.Table_Bytes :=
        Header & Name_Entry ("as") & Name_Entry ("ld") &
        Name_Entry ("args-check.app");
      M : LT.Table_Bytes (1 .. Table'Length);
   begin
      Check (LT.Valid (Table), "launch table validates");
      Check (LT.Contains (Table, "as") and then LT.Contains (Table, "ld")
             and then LT.Contains (Table, "args-check.app")
             and then not LT.Contains (Table, "a")
             and then not LT.Contains (Table, "cc1")
             and then not LT.Contains (Table, "args-check.ap")
             and then not LT.Contains (Table, "/as"),
             "launch table names match exactly");
      Check (LT.Valid ([16#4C#, 16#4E#, 16#43#, 16#48#, 1, 0, 0, 0]),
             "an empty launch table is valid");
      M := Table;
      M (7) := 4;
      Check (not LT.Valid (M), "launch table with too many names");
      M := Table;
      M (7) := 2;
      Check (not LT.Valid (M), "launch table with trailing bytes");
      M := Table;
      M (10) := 0;
      Check (not LT.Valid (M), "NUL inside a launch name");
      M := Table;
      M (9) := 0;
      Check (not LT.Valid (M), "empty launch name");
      M := Table;
      M (1) := 0;
      Check (not LT.Valid (M), "launch table magic");
      M := Table;
      M (5) := 2;
      Check (not LT.Valid (M), "launch table version");
      Check (not LT.Valid (Table (1 .. Table'Last - 1)),
             "truncated launch table");
      for Cut in 1 .. Table'Length - 1 loop
         if LT.Valid (Table (1 .. Cut)) then
            Check (False, "no prefix of a table is valid");
         end if;
      end loop;
      Check (LT.Scope_Covered (0, 3, "@nvme:0/build", 0, 1,
                               "@nvme:0/build/obj")
             and then LT.Scope_Covered (0, 3, "@nvme:0/build", 0, 3,
                                        "@nvme:0/build")
             and then LT.Scope_Covered (0, 1, "", 0, 1, "@nvme:0/x")
             and then not LT.Scope_Covered (0, 1, "@nvme:0/build", 0, 3,
                                            "@nvme:0/build")
             and then not LT.Scope_Covered (0, 3, "@nvme:0/build", 0, 1,
                                            "@nvme:0/buildx")
             and then not LT.Scope_Covered (0, 3, "@nvme:0/build", 0, 1, "")
             and then not LT.Scope_Covered (0, 3, "@nvme:0/build", 1, 1,
                                            "@nvme:0/build")
             and then LT.Scope_Covered (2, 1, "example.org:443", 2, 1,
                                        "example.org:443")
             and then not LT.Scope_Covered (2, 1, "example.org:443", 2, 1,
                                            "example.org:4433"),
             "child scopes must be covered by the launcher's");
   end;

   if Failures = 0 then
      Put_Line ("launch-arguments:" & Checks'Image & " checks PASS");
   else
      Put_Line ("launch-arguments:" & Failures'Image & " of" & Checks'Image &
                " checks FAILED");
      raise Program_Error;
   end if;
end Main;
