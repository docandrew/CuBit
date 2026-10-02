with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_TGL_PTE_Registers;
with Intel_GPU_ADLN_PPGTT;
procedure TGL_PTE_Registers_Tests is
   package R renames Intel_GPU_TGL_PTE_Registers;
   package P renames Intel_GPU_ADLN_PPGTT;
   use type R.Bits_1;
   Value : R.Leaf;
begin
   for Bit in 0 .. 63 loop
      declare Raw : constant Unsigned_64 := Shift_Left (1, Bit); begin
         Value := R.Decode (Raw);
         pragma Assert (R.Encode (Value) = Raw);
         pragma Assert ((Value.Null_Page = 1) = (Bit = 9));
         pragma Assert ((Value.Present = 1) = (Bit = 0));
         pragma Assert ((Value.Writable = 1) = (Bit = 1));
         pragma Assert (Unsigned_64 (Value.Address_Page) =
           (if Bit in 12 .. 38 then Shift_Left (1, Bit - 12) else 0));
      end;
   end loop;
   for Policy in P.Cache_Policy loop
      for Page in Unsigned_64 range 1 .. 65536 loop
         declare
            Raw : constant Unsigned_64 := P.Encode_Leaf (Page * 4096, Policy, P.Read_Write);
         begin
            Value := R.Decode (Raw);
            pragma Assert (Value.Present = 1 and Value.Writable = 1 and Value.Null_Page = 0);
            pragma Assert (Unsigned_64 (Value.Address_Page) = Page);
            pragma Assert (Unsigned_64 (Value.Write_Through) * 8 +
              Unsigned_64 (Value.Cache_Disable) * 16 = P.Cache_Bits (Policy));
            pragma Assert (Value.PAT = 0 and R.Encode (Value) = Raw);
         end;
      end loop;
   end loop;
   Value := (Present => 1, Writable => 1, Null_Page => 1, others => <>);
   pragma Assert (R.Encode (Value) = 16#203#);
   -- Representation only: no native caller publishes this null entry.
   Ada.Text_IO.Put_Line ("TGL PTE layout PASS:64 bits,262144 existing leaves, null field (NOT GPU semantics)");
end TGL_PTE_Registers_Tests;
