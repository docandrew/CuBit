with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with ISO_Records; use ISO_Records;
procedure ISO_Tests is
   Data : Sector := [others => 0];
   Item : File_Record;
   Consumed : Natural;
   Blocks : Unsigned_64;
   Valid : Boolean;
   procedure Put32 (P : Natural; V : Unsigned_32) is
   begin
      for I in 0 .. 3 loop
         Data (P + I) := Unsigned_8 (Shift_Right (V, I * 8) and 255);
         Data (P + 7 - I) := Data (P + I);
      end loop;
   end Put32;
   procedure Record_At (P : Natural) is
   begin
      Data (P) := 34;
      Put32 (P + 2, 20); Put32 (P + 10, 2048);
      Data (P + 25) := 2;
      Data (P + 28 .. P + 31) := [1, 0, 0, 1];
      Data (P + 32) := 1;
   end Record_At;
begin
   Data (0 .. 6) := [1, 67, 68, 48, 48, 49, 1];
   Put32 (80, 100);
   Data (120 .. 127) := [1, 0, 0, 1, 1, 0, 0, 1];
   Data (128 .. 131) := [0, 8, 8, 0];
   Data (881) := 1;
   Record_At (156);
   Decode_Volume (Data, 100, Blocks, Item, Valid);
   pragma Assert (Valid and Blocks = 100 and Item.Extent = 20);
   Decode_Volume (Data, 99, Blocks, Item, Valid);
   pragma Assert (not Valid and Blocks = 0);
   Data (84) := 1;
   Decode_Volume (Data, 100, Blocks, Item, Valid);
   pragma Assert (not Valid);
   Data := [others => 0];
   Record_At (0);
   Decode_Record (Data, 0, 21, Item, Consumed, Valid);
   pragma Assert (Valid and Consumed = 34);
   Decode_Record (Data, 0, 20, Item, Consumed, Valid);
   pragma Assert (not Valid);
   for Offset in 2015 .. 2050 loop
      Decode_Record (Data, Offset, 100, Item, Consumed, Valid);
      pragma Assert (not Valid);
   end loop;
   for Flag in Unsigned_8 range 4 .. 255 loop
      Data (25) := Flag;
      Decode_Record (Data, 0, 100, Item, Consumed, Valid);
      pragma Assert (not Valid);
   end loop;
   Data (25) := 0;
   for Length in Unsigned_8 range 0 .. 33 loop
      Data (0) := Length;
      Decode_Record (Data, 0, 100, Item, Consumed, Valid);
      pragma Assert (not Valid);
   end loop;
   Data (0) := 52; Data (32) := 19;
   declare
      Name : constant String := "ccl-workbench.app;1";
   begin
      Data (32) := Name'Length;
      for I in Name'Range loop Data (32 + I) := Character'Pos (Name (I)); end loop;
   end;
   Decode_Record (Data, 0, 100, Item, Consumed, Valid);
   pragma Assert (Valid and Matches (Item, "ccl-workbench.app"));
   pragma Assert (Matches (Item, "CCL-WORKBENCH.APP"));
   pragma Assert (not Matches (Item, "ccl_workbench.app"));
   Data (26) := 1;
   Decode_Record (Data, 0, 100, Item, Consumed, Valid);
   pragma Assert (not Valid);
   Put_Line ("ISO RECORDS PASS");
end ISO_Tests;
