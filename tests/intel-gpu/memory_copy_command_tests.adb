with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_Memory_Copy_Command;
with Intel_GPU_VA_Encoding;
procedure Memory_Copy_Command_Tests is
   package C renames Intel_GPU_Memory_Copy_Command;
   function Raw is new Ada.Unchecked_Conversion (C.Header, Unsigned_32);
   function Decode is new Ada.Unchecked_Conversion (Unsigned_32, C.Header);
   Values : constant array (Positive range <>) of Unsigned_64 :=
     [0, 1, 2, 3, 4, 16#209080#, 2 ** 32 - 4, 2 ** 32,
      2 ** 47 - 4, 2 ** 47, 2 ** 48 - 4, 2 ** 48 - 1,
      2 ** 48, 16#FFFF800000000000#, Unsigned_64'Last];
begin
   pragma Assert (Raw (C.Header'(others => <>)) = 16#17000003#);
   for Bit in 0 .. 31 loop
      declare
         V : constant Unsigned_32 := Shift_Left (Unsigned_32'(1), Bit);
      begin
         pragma Assert (C.Encode (Decode (V)) = V);
      end;
   end loop;
   for S of Values loop
      for D of Values loop
         declare
            R : constant C.Command := C.Build (S, D);
         begin
            pragma Assert (R.Valid = (C.Address_Valid (S) and C.Address_Valid (D)));
            if R.Valid then
               pragma Assert (R.Words (0) = 16#17000003#);
               pragma Assert ((Unsigned_64 (R.Words (1)) or
                 Shift_Left (Unsigned_64 (R.Words (2)), 32)) =
                 Intel_GPU_VA_Encoding.Canonical (D));
               pragma Assert ((Unsigned_64 (R.Words (3)) or
                 Shift_Left (Unsigned_64 (R.Words (4)), 32)) =
                 Intel_GPU_VA_Encoding.Canonical (S));
            else
               pragma Assert (for all W of R.Words => W = 0);
            end if;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Memory copy command PASS: 225 address pairs, 32 header bits; encoding only");
end Memory_Copy_Command_Tests;
