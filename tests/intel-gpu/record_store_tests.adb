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
   type RAM is array (Natural range 0 .. 16383) of Unsigned_64;
   Memory : RAM := [others => 16#CAFE#] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory'Address));
   Object : R.Store;
   Before : Positive;
   OK : Boolean;
   Stride_Words : constant Positive := Entry_Record'Object_Size / 64;

   procedure Check_Tail (Published_Bytes : Unsigned_64) is
      First_Untouched : constant Natural :=
        Natural (Published_Bytes / Unsigned_64 (Entry_Record'Object_Size / 8)) *
          Stride_Words;
   begin
      -- Includes an incomplete trailing record: publishing a page must not
      -- initialize a record that straddles the committed boundary.
      for I in First_Untouched .. Memory'Last loop
         pragma Assert (Memory (I) = 16#CAFE#);
      end loop;
   end Check_Tail;
begin
   R.Extend (Object, 0, 4096, OK); pragma Assert (not OK);
   R.Extend (Object, Base + 1, 4096, OK); pragma Assert (not OK);
   R.Extend (Object, Unsigned_64'Last - 4095, 4096, OK);
   pragma Assert (not OK);
   R.Extend (Object, Base, 0, OK); pragma Assert (not OK);
   pragma Assert (R.Capacity (Object) = 16);
   Check_Tail (0);
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
      Check_Tail (Page * 4096);
   end loop;
   pragma Assert (R.Capacity (Object) > 1024);
   Before := R.Capacity (Object);
   R.Extend (Object, Base, 32768 + 65536, OK); pragma Assert (OK);
   for I in 1 .. Before loop
      pragma Assert (R.Get (Object, I) =
        (Unsigned_64 (I), Unsigned_64 (I) + 100, False));
   end loop;
   for I in Before + 1 .. R.Capacity (Object) loop
      pragma Assert (R.Get (Object, I) = Empty);
   end loop;
   Check_Tail (32768 + 65536);
   Ada.Text_IO.Put_Line ("Record store PASS: page and exact64KiB growth, typed defaults, stable old data, rejection of overflow/relocation/replay/oversized growth, per-step incomplete-record and tail guards");
end Record_Store_Tests;
