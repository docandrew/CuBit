with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Native_RCS_Start;
procedure Native_RCS_Start_Tests is
   Owned : Boolean := False;
   GPU : Unsigned_64 := 16#200000#;
   function Owner return Boolean is (Owned);
   function Status_GPU return Unsigned_64 is (GPU);
   package IO is new Intel_GPU_Native_RCS_Start (Owner, Status_GPU);
   type Words is array (Positive range <>) of Unsigned_32;
   Offsets : constant Words := [16#2098#, 16#2080#, 16#229C#, 16#209C#];
   Values : constant Words := [16#FFFFFFFF#, 16#200000#, 16#80008#, 16#1000000#];
   OK : Boolean;
begin
   for I in Offsets'Range loop
      pragma Assert (IO.Write_Allowed (Offsets (I), Values (I)));
      --  A valid request without ownership must not dereference MMIO.
      IO.Write32 (Offsets (I), Values (I), OK);
      pragma Assert (not OK);
      for Bit in 0 .. 31 loop
         declare Wrong : constant Unsigned_32 := Values (I) xor Shift_Left (1, Bit); begin
            pragma Assert (not IO.Write_Allowed (Offsets (I), Wrong));
            Owned := True;
            IO.Write32 (Offsets (I), Wrong, OK);
            pragma Assert (not OK);
            Owned := False;
         end;
      end loop;
   end loop;
   pragma Assert (IO.Read32 (16#209C#) = Unsigned_32'Last);
   Owned := True;
   for Offset in Unsigned_32 range 0 .. 16#FFFF# loop
      if Offset /= 16#2098# and Offset /= 16#2080# and
        Offset /= 16#229C# and Offset /= 16#209C#
      then
         pragma Assert (not IO.Write_Allowed (Offset, 0));
         IO.Write32 (Offset, 0, OK);
         pragma Assert (not OK);
         pragma Assert (IO.Read32 (Offset) = Unsigned_32'Last);
      end if;
   end loop;
   for Invalid of Words'(0, 1, 4095, 4097, 16#FEE00000#, 16#FFFFFFFF#) loop
      GPU := Unsigned_64 (Invalid);
      for I in Offsets'Range loop
         pragma Assert (not IO.Write_Allowed (Offsets (I), Values (I)));
         IO.Write32 (Offsets (I), Values (I), OK);
         pragma Assert (not OK);
      end loop;
   end loop;
   GPU := Unsigned_64'Last;
   pragma Assert (not IO.Write_Allowed (16#2080#, 16#FFFFFFFF#));
   GPU := 16#FEDFF000#;
   pragma Assert (IO.Write_Allowed (16#2080#, 16#FEDFF000#));
   pragma Assert (not IO.Write_Allowed (16#2080#, 16#200000#));
   GPU := 4096;
   pragma Assert (IO.Write_Allowed (16#2080#, 4096));
   Ada.Text_IO.Put_Line ("Native RCS adapter PASS: exact values, address bounds, rejected requests do no MMIO");
end Native_RCS_Start_Tests;
