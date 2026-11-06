with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with Firmware_Tables.MCFG;
procedure Parser_Tests is
   package F renames Firmware_Tables;
   package M renames Firmware_Tables.MCFG;
   Extent : constant Positive := M.Fixed_Size + M.Allocation_Size;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Seal (Data : in out F.Bytes) is
      Sum : F.Byte := 0;
   begin
      Data (Data'First + 9) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (Data'First + 9) := 0 - Sum;
   end Seal;
   function Fixture (First : Positive) return F.Bytes
     with Pre => First <= Positive'Last - (Extent - 1)
   is
      Data : F.Bytes (First .. First + (Extent - 1)) := [others => 0];
      Payload : constant F.Bytes :=
        [16#F0#,16#DE#,16#BC#,16#9A#,16#78#,16#56#,16#34#,16#12#,
         16#34#,16#12#,16#40#,16#20#,16#EF#,16#CD#,16#AB#,16#89#];
   begin
      Data (First .. First + 3) := [Character'Pos ('M'), Character'Pos ('C'),
                                   Character'Pos ('F'), Character'Pos ('G')];
      Data (First + 4) := F.Byte (Extent);
      Data (First + 8) := 1;
      Data (First + M.Fixed_Size .. Data'Last) := Payload;
      Seal (Data); return Data;
   end Fixture;
   type Origins is array (Positive range <>) of Positive;
   Starts : constant Origins := [1, 17, Positive'Last - (Extent - 1)];
begin
   for First of Starts loop
      declare
         Data : F.Bytes := Fixture (First);
         Table : constant M.Table_Metadata := M.Decode (Data);
         Value : constant M.Allocation_Result := M.Read_Allocation (Data, 1);
      begin
         Check (Table.Valid and then Table.Count = 1 and then Table.Revision = 1);
         Check (Value.Valid and then Value.Value.Base = 16#1234_5678_9ABC_DEF0#);
         Check (Value.Value.Segment = 16#1234# and then Value.Value.Reserved = 16#89AB_CDEF#);
         -- Structural parsing retains policy-relevant values, including a
         -- reversed bus range; it does not grant mapping authority.
         Check (Value.Value.First_Bus = 16#40# and then Value.Value.Last_Bus = 16#20#);
         Check (not M.Read_Allocation (Data, 0).Valid);
         Check (not M.Read_Allocation (Data, 2).Valid);
         Check (not M.Read_Allocation (Data, Natural'Last).Valid);
         for Length in 0 .. Extent - 1 loop
            Check (not M.Decode (Data (First .. First + Length - 1)).Valid);
         end loop;
         Data (First + 10) := Data (First + 10) xor 1;
         Check (not M.Decode (Data).Valid);
      end;
   end loop;
   declare
      Data : constant F.Bytes := Fixture (1);
      Extra : F.Bytes (1 .. Extent + 1) := [others => 0];
      Empty : F.Bytes (1 .. M.Fixed_Size) := Data (1 .. M.Fixed_Size);
      Partial : F.Bytes (1 .. Extent - 1) := Data (1 .. Extent - 1);
   begin
      Extra (1 .. Extent) := Data;
      Check (not M.Decode (Extra).Valid);
      Empty (5) := F.Byte (Empty'Length); Seal (Empty);
      Check (M.Decode (Empty).Valid and then M.Decode (Empty).Count = 0);
      Check (not M.Read_Allocation (Empty, 1).Valid);
      Partial (5) := F.Byte (Partial'Length); Seal (Partial);
      Check (not M.Decode (Partial).Valid);
   end;
   if Ada.Command_Line.Argument_Count = 1 then
      declare
         package IO renames Ada.Streams.Stream_IO;
         File : IO.File_Type;
         Data : constant F.Bytes := Fixture (1);
         Wire : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Extent));
      begin
         for I in Data'Range loop
            Wire (Ada.Streams.Stream_Element_Offset (I)) := Ada.Streams.Stream_Element (Data (I));
         end loop;
         IO.Create (File, IO.Out_File, Ada.Command_Line.Argument (1));
         IO.Write (File, Wire); IO.Close (File);
      end;
   end if;
   Ada.Text_IO.Put_Line ("MCFG parser checks" & Checks'Image);
end Parser_Tests;
