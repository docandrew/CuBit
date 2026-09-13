with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with User_Buffer_Copy;
with User_Page_Walk; use User_Page_Walk;

procedure Main is
   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;
   Source, Length, Copied : Unsigned_64 := 0;
   Calls, Fail_At : Natural := 0;
   Success : Boolean;
   procedure Read_Chunk
     (Page : Unsigned_64; Within_Page : Natural; Destination_Offset : Unsigned_64;
      Count : Positive; Success : out Boolean)
   is
   begin
      Calls := Calls + 1;
      pragma Assert (Page mod 4096 = 0);
      pragma Assert (Within_Page < 4096 and then Count <= 4096 - Within_Page);
      pragma Assert (Page + Unsigned_64 (Within_Page) = Source + Copied);
      pragma Assert (Destination_Offset = Copied and then Unsigned_64 (Count) <= Length - Copied);
      Success := Calls /= Fail_At;
      if Success then Copied := Copied + Unsigned_64 (Count); end if;
   end Read_Chunk;
   procedure Copy is new User_Buffer_Copy.Copy (Read_Chunk);
   procedure Try_Copy (S, N : Unsigned_64; Failure : Natural := 0) is
   begin
      Source := S;
      Length := N;
      Copied := 0;
      Calls := 0;
      Fail_At := Failure;
      Copy (Source, Length, Success);
   end Try_Copy;

   type Words_Array is array (Level) of Unsigned_64;
   Baseline : constant Words_Array := [16#2005#, 16#3005#, 16#4005#, 16#9005#];
   Words : Words_Array := Baseline;
   Reads : Natural := 0;
   Address : constant Unsigned_64 := 16#1234_5678_9ABC#;
   procedure Read_Entry (Table_Frame : Unsigned_64; Index : Table_Index; Word : out Unsigned_64) is
      L : Level;
      Shift : Natural;
   begin
      Reads := Reads + 1;
      case Table_Frame is
         when 16#1000# => L := P4_Level; Shift := 39;
         when 16#2000# => L := P3_Level; Shift := 30;
         when 16#3000# => L := P2_Level; Shift := 21;
         when 16#4000# => L := P1_Level; Shift := 12;
         when others => raise Program_Error with "walk followed a data/invalid page as a table";
      end case;
      pragma Assert (Index = Natural (Shift_Right (Address, Shift) and 511));
      Word := Words (L);
   end Read_Entry;
   function Walk is new User_Page_Walk.Readable_Frame (Read_Entry);
   Frame : Unsigned_64;
   Name : String (1 .. 16);
   Name_Source : Unsigned_64;
   Available, Terminate_At, Name_Reads : Natural;
   procedure Read_Byte
     (Address : Unsigned_64; Value : out Character; Success : out Boolean) is
   begin
      Name_Reads := Name_Reads + 1;
      pragma Assert (Address = Name_Source + Unsigned_64 (Name_Reads - 1));
      Success := Name_Reads <= Available;
      Value := (if Name_Reads = Terminate_At then ASCII.NUL else 'x');
   end Read_Byte;
   procedure Copy_Name is new User_Buffer_Copy.Copy_Name (Read_Byte);
begin
   for Offset in Unsigned_64 range 0 .. 4095 loop
      for N of Unsigned_64_Array'[0, 1, 31, 64, 4095, 4096, 4097, 8192] loop
         Try_Copy (16#10000# + Offset, N);
         pragma Assert (Success and then Copied = N);
      end loop;
   end loop;
   Try_Copy (16#10FFF#, 8192, 2);
   pragma Assert (not Success and then Calls = 2 and then Copied = 1);
   Try_Copy (16#10FFF#, 8192, 3);
   pragma Assert (not Success and then Calls = 3 and then Copied = 4097);
   Try_Copy (16#10FFF#, 8192, 1);
   pragma Assert (not Success and then Calls = 1 and then Copied = 0);
   Try_Copy (0, 64);
   pragma Assert (not Success and then Calls = 0);
   Try_Copy (Unsigned_64'Last, 64);
   pragma Assert (not Success and then Calls = 0);
   Try_Copy (16#10000#, Unsigned_64'Last);
   pragma Assert (not Success and then Calls = 0);
   Try_Copy (User_Buffer_Copy.User_Limit - 1, 2);
   pragma Assert (not Success and then Calls = 0);
   Try_Copy (User_Buffer_Copy.User_Limit - 1, 1);
   pragma Assert (Success and then Copied = 1);
   Put_Line ("PASS copy chunks: every page offset, exact coverage, failure prefixes, null/wrap/canonical boundaries");

   Frame := Walk (16#1000#, Address, 16#FFFF#);
   pragma Assert (Frame = 16#9000# and then Reads = 4);
   for L in Level loop
      for Clear_Bit of Unsigned_64_Array'[Present_Bit, User_Bit] loop
         Words := Baseline;
         Words (L) := Words (L) and not Clear_Bit;
         Reads := 0;
         Frame := Walk (16#1000#, Address, 16#FFFF#);
         pragma Assert (Frame = 0 and then Reads = Level'Pos (L) + 1);
      end loop;
      Words := Baseline;
      Words (L) := Words (L) or Large_Page_Bit;
      Reads := 0;
      Frame := Walk (16#1000#, Address, 16#FFFF#);
      pragma Assert (Frame = (if L = P1_Level then 16#9000# else 0));
      pragma Assert (Reads = Level'Pos (L) + 1);
   end loop;
   Words := Baseline;
   Words (P1_Level) := 5;
   Frame := Walk (16#1000#, Address, 16#FFFF#);
   pragma Assert (Frame = 0);
   Words (P1_Level) := Frame_Mask or 5;
   Frame := Walk (16#1000#, Address, 16#FFFF#);
   pragma Assert (Frame = 0);
   Reads := 0;
   Frame := Walk (0, Address, 16#FFFF#);
   pragma Assert (Frame = 0 and then Reads = 0);
   Frame := Walk (16#1000#, User_Limit, 16#FFFF#);
   pragma Assert (Frame = 0 and then Reads = 0);
   Frame := Walk (16#1001#, Address, 16#FFFF#);
   pragma Assert (Frame = 0 and then Reads = 0);
   Put_Line ("PASS actual walker: inherited user/present bits, large-page rejection, leaf PAT, invalid frames");

   Name_Source := 16#10FFF#;
   Available := 1;
   Terminate_At := 1;
   Name_Reads := 0;
   Copy_Name (Name_Source, Name, Success);
   pragma Assert (Success and then Name_Reads = 1 and then Name (1) = ASCII.NUL);
   Terminate_At := 0;
   Name_Reads := 0;
   Copy_Name (Name_Source, Name, Success);
   pragma Assert (not Success and then Name_Reads = 2);
   Available := 16;
   Name_Reads := 0;
   Copy_Name (Name_Source, Name, Success);
   pragma Assert (Success and then Name_Reads = 16 and then Name = "xxxxxxxxxxxxxxxx");
   Name_Source := User_Buffer_Copy.User_Limit - 1;
   Name_Reads := 0;
   Copy_Name (Name_Source, Name, Success);
   pragma Assert (not Success and then Name_Reads = 1);
   Terminate_At := 1;
   Name_Reads := 0;
   Copy_Name (Name_Source, Name, Success);
   pragma Assert (Success and then Name_Reads = 1);
   Name_Reads := 0;
   Copy_Name (Unsigned_64'Last, Name, Success);
   pragma Assert (not Success and then Name_Reads = 0);
   Put_Line ("PASS actual name copy: terminator at mapped/canonical edge, unmapped continuation, exact truncation");
end Main;
