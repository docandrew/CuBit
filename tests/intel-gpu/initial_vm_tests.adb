with Interfaces; use Interfaces;
with Ada.Text_IO;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Initial_VM; use Intel_GPU_Initial_VM;
procedure Initial_VM_Tests is
   Tables : constant Table_Pages := [16#1000#, 16#2000#, 16#3000#, 16#4000#];
   Data : Data_Pages := [others => 0];
   P : Plan;
   type Addresses is array (Positive range <>) of Unsigned_64;
   procedure Check_Leaves is
   begin
      pragma Assert (P.Valid and P.Root_DMA = Tables (Root));
      for I in Table_Index loop
         pragma Assert (P.Entries (Leaves) (I) =
           (if Data (I) = 0 then 0 else Data (I) + 3));
      end loop;
   end Check_Leaves;
begin
   -- Dense mappings remain byte-for-byte compatible at every length.
   for Count in 1 .. 512 loop
      Data (Count - 1) := Unsigned_64 (Count + 10) * 4096;
      P := Build (2 ** 48 - 2 ** 21, Tables, Data);
      Check_Leaves;
      for I in Table_Index loop
         pragma Assert (P.Entries (Root) (I) = (if I = 511 then 16#2003# else 0));
         pragma Assert (P.Entries (Pointer_Directory) (I) = (if I = 511 then 16#3003# else 0));
         pragma Assert (P.Entries (Directory) (I) = (if I = 511 then 16#4003# else 0));
      end loop;
   end loop;
   -- Every leaf position can be mapped independently; all holes stay zero.
   for Slot in Table_Index loop
      Data := [others => 0]; Data (Slot) := 16#B000#;
      P := Build (16#200000#, Tables, Data); Check_Leaves;
      for L in Level loop
         Data (Slot) := Tables (L);
         P := Build (0, Tables, Data);
         pragma Assert (not P.Valid and P.Root_DMA = 0);
         pragma Assert (for all Kind in Level =>
           (for all I in Table_Index => P.Entries (Kind) (I) = 0));
      end loop;
   end loop;
   -- Separated command/data/target regions with unmapped guard gaps.
   Data := [0 => 16#B000#, 16 => 16#C000#, 511 => 16#D000#, others => 0];
   P := Build (16#200000#, Tables, Data); Check_Leaves;
   for L in Level loop
      for Other in Level loop
         declare Bad : Table_Pages := Tables; begin
            if L /= Other then
               Bad (L) := Bad (Other);
               pragma Assert (not Build (0, Bad, Data).Valid);
            end if;
         end;
      end loop;
   end loop;
   pragma Assert (not Build (4096, Tables, Data).Valid);
   pragma Assert (not Build (2 ** 48, Tables, Data).Valid);
   pragma Assert (not Build (0, Tables, Data, Access_Mode => Read_Only).Valid);
   pragma Assert (not Build (0, Tables, [others => 0]).Valid);
   for Invalid of Addresses'[1, 4095, 2 ** 32, Unsigned_64'Last] loop
      Data := [511 => Invalid, others => 0];
      pragma Assert (not Build (0, Tables, Data).Valid);
   end loop;
   Ada.Text_IO.Put_Line ("initial VM PASS: dense compatibility, every sparse leaf, unmapped gaps, table/data alias rejection (offline only)");
end Initial_VM_Tests;
