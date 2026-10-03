with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Record_Store;
procedure Record_Store_Tests is
   type Entry_Record is record
      Identity : Unsigned_64 := 73;
      Owner : Unsigned_64 := 99;
      Retained : Boolean := True;
   end record;
   Empty : constant Entry_Record := (others => <>);
   package R is new Intel_GPU_Record_Store (Entry_Record, Empty);
   type RAM is array (Natural range 0 .. 8191) of Unsigned_64;
   Memory : RAM := [others => 16#CAFE#] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory'Address));
   Object : R.Store;
   Before : Positive;
   OK : Boolean;
begin
   for I in 1 .. R.Capacity (Object) loop
      pragma Assert (R.Get (Object, I) = Empty);
      R.Put (Object, I, (Unsigned_64 (I), Unsigned_64 (I) + 100, False));
   end loop;
   for Page in Unsigned_64 range 1 .. 8 loop
      Before := R.Capacity (Object);
      R.Extend (Object, Base, Page * 4096, OK); pragma Assert (OK);
      pragma Assert (R.Capacity (Object) > Before);
      for I in 1 .. Before loop
         pragma Assert (R.Get (Object, I) = (Unsigned_64 (I), Unsigned_64 (I) + 100, False));
      end loop;
      for I in Before + 1 .. R.Capacity (Object) loop
         -- Defaults are typed values, not an assumption that zero is valid.
         pragma Assert (R.Get (Object, I) = Empty);
         R.Put (Object, I, (Unsigned_64 (I), Unsigned_64 (I) + 100, False));
      end loop;
      Before := R.Capacity (Object);
      R.Extend (Object, Base, Page * 4096, OK); pragma Assert (not OK);
      R.Extend (Object, Base + 4096, (Page + 1) * 4096, OK); pragma Assert (not OK);
      R.Extend (Object, Base, (Page + 1) * 4096 + 1, OK); pragma Assert (not OK);
      R.Extend (Object, Base, Page * 4096 + 65536 + 4096, OK); pragma Assert (not OK);
      pragma Assert (R.Capacity (Object) = Before);
   end loop;
   pragma Assert (R.Capacity (Object) > 1024);
   for I in 4096 .. Memory'Last loop pragma Assert (Memory (I) = 16#CAFE#); end loop;
   Ada.Text_IO.Put_Line ("Record store PASS: eight extensions, >1024 records, typed defaults, stable old data, rejected relocation/replay/oversized growth, untouched tail");
end Record_Store_Tests;
