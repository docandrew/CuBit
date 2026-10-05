with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_VM_Table_Store;
with Intel_GPU_ADLN_PPGTT;
procedure VM_Table_Store_Tests is
   package S is new Intel_GPU_VM_Table_Store (4, 132);
   type Pages is array (1 .. 128) of S.Page;
   RAM : Pages := [others => [others => 16#DEAD#]] with Alignment => 4096;
   Copy_RAM : Pages := [others => [others => 16#BEEF#]] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (RAM'Address));
   Other : constant Unsigned_64 := Unsigned_64 (To_Integer (Copy_RAM'Address));
   Object, Target : S.Store;
   OK : Boolean;
begin
   pragma Assert (S.Capacity (Object) = 4 and S.Store'Object_Size < 20 * 4096 * 8);
   S.Set_Word (Object, 1, 0, 123);
   for Batch in 1 .. 8 loop
      S.Extend (Object, Base, Unsigned_64 (Batch * 65536), OK);
      pragma Assert (OK and S.Capacity (Object) = 4 + Batch * 16);
      for T in 5 + (Batch - 1) * 16 .. 4 + Batch * 16 loop
         for I in Intel_GPU_ADLN_PPGTT.Table_Index loop
            pragma Assert (S.Word (Object, T, I) = 0);
            S.Set_Word (Object, T, I, Unsigned_64 (T) * 512 + Unsigned_64 (I));
         end loop;
      end loop;
      pragma Assert (S.Word (Object, 1, 0) = 123);
      for T in 5 .. S.Capacity (Object) loop
         for I in Intel_GPU_ADLN_PPGTT.Table_Index loop
            pragma Assert (S.Word (Object, T, I) = Unsigned_64 (T) * 512 + Unsigned_64 (I));
         end loop;
      end loop;
      if Batch < 8 then pragma Assert (RAM (Batch * 16 + 1) (0) = 16#DEAD#); end if;
   end loop;
   S.Extend (Object, Base, 9 * 65536, OK); pragma Assert (not OK);
   S.Extend (Object, Other, 8 * 65536, OK); pragma Assert (not OK);
   S.Extend (Object, Base + 1, 8 * 65536, OK); pragma Assert (not OK);
   S.Extend (Object, Base, 8 * 65536, OK); pragma Assert (not OK);
   S.Extend (Target, Other, 2 * 65536, OK); pragma Assert (not OK);
   S.Extend (Target, Unsigned_64'Last - 4095, 4096, OK); pragma Assert (not OK);
   S.Extend (Target, Other, 4096, OK); pragma Assert (OK);
   S.Copy_Page (Target, 5, Object, 132);
   for I in Intel_GPU_ADLN_PPGTT.Table_Index loop
      pragma Assert (S.Word (Target, 5, I) = S.Word (Object, 132, I));
   end loop;
   S.Clear (Target, 5);
   pragma Assert (S.Word (Target, 5, 0) = 0 and S.Word (Object, 132, 0) = 132 * 512);
   pragma Assert (Copy_RAM (2) (0) = 16#BEEF#);
   Ada.Text_IO.Put_Line ("VM table metadata PASS: 4->132 stable tables, bounded extension, quota/alias geometry rejection, independent copy/clear (host RAM)");
end VM_Table_Store_Tests;
