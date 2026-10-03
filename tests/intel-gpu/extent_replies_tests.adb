with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Extent_Replies;
with Intel_GPU_Extent_Directory;
with System.Storage_Elements; use System.Storage_Elements;
procedure Extent_Replies_Tests is
   package R renames Intel_GPU_Extent_Replies;
   package E renames R.E;
   package D renames Intel_GPU_Extent_Directory;
   CPU : constant Unsigned_64 := 16#7000_0000_0000#;
   function Reply (I : Natural) return R.Words is
     ([Unsigned_64 (I), 16#8000_0000# - Unsigned_64 (I) * 2 * E.Block_Bytes,
       CPU + Unsigned_64 (I) * E.Block_Bytes, 17]);
begin
   declare
      Object : R.Assembly;
      OK : Boolean;
      type RAM is array (1 .. 16384) of Unsigned_8 with Alignment => 4096;
      Metadata : RAM := [others => 16#A5#];
      Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
      Old : D.Borrowed_View;
      function Large_Reply (Index : Natural) return R.Words is
        ([Unsigned_64 (Index), 2 ** 40 + Unsigned_64 (Index) * 2 * E.Block_Bytes,
          CPU + Unsigned_64 (Index) * E.Block_Bytes, 17]);
   begin
      R.Start (Object, CPU, 17, OK, 16, 24 * 1024 ** 3, 2 ** 48);
      pragma Assert (OK and R.Metadata_Capacity (Object) = 16);
      for Index in 0 .. 15 loop
         R.Accept_Reply (Object, Large_Reply (Index), OK); pragma Assert (OK);
      end loop;
      Old := R.Result (Object);
      R.Extend (Object, 600, OK); pragma Assert (OK);
      pragma Assert (D.Valid (Old) and not D.Valid (R.Result (Object)));
      R.Extend_Metadata (Object, Base + 1, 4096, OK); pragma Assert (not OK);
      R.Extend_Metadata (Object, Base, 8192, OK); pragma Assert (OK);
      pragma Assert (R.Metadata_Capacity (Object) = 528);
      for Index in 16 .. 527 loop
         R.Accept_Reply (Object, Large_Reply (Index), OK); pragma Assert (OK);
         pragma Assert (not D.Valid (R.Result (Object)));
      end loop;
      R.Extend_Metadata (Object, Base, 16384, OK); pragma Assert (OK);
      for Index in 528 .. 599 loop
         R.Accept_Reply (Object, Large_Reply (Index), OK); pragma Assert (OK);
      end loop;
      pragma Assert (D.Byte_Count (R.Result (Object)) = 600 * E.Block_Bytes);
      pragma Assert (D.Byte_Count (Old) = 16 * E.Block_Bytes);
      pragma Assert (not D.Resolve (Old, 16 * E.Block_Bytes, 4096).Valid);
      for Index in 0 .. 599 loop
         pragma Assert (D.Resolve (R.Result (Object), Unsigned_64 (Index) * E.Block_Bytes,
           4096).Address = Large_Reply (Index)(1));
      end loop;
      R.Extend (Object, 12289, OK);
      pragma Assert (not OK and not D.Valid (Old));
      R.Extend_Metadata (Object, Base, 8192, OK); pragma Assert (not OK);
   end;
   -- An above-default address is never accepted without trusted policy.
   declare
      Object : R.Assembly;
      OK : Boolean;
      Data : R.Words := Reply (0);
   begin
      R.Start (Object, CPU, 17, OK, 1); pragma Assert (OK);
      Data (1) := 2 ** 40;
      R.Accept_Reply (Object, Data, OK);
      pragma Assert (not OK and not D.Valid (R.Result (Object)));
   end;
   declare
      Object : R.Assembly;
      OK : Boolean;
      Previous : D.Borrowed_View;
   begin
      R.Start (Object, CPU, 17, OK, 1); pragma Assert (OK);
      for Count in 1 .. E.Addresses'Length loop
         if Count > 1 then
            Previous := R.Result (Object);
            R.Extend (Object, Count, OK); pragma Assert (OK);
         end if;
         pragma Assert (not D.Valid (R.Result (Object)));
         R.Accept_Reply (Object, Reply (Count - 1), OK);
         pragma Assert (OK and D.Byte_Count (R.Result (Object)) = Unsigned_64 (Count) * E.Block_Bytes);
         if Count > 1 then
            pragma Assert (D.Same_Owner (Previous, R.Result (Object)));
            pragma Assert (D.Byte_Count (Previous) = Unsigned_64 (Count - 1) * E.Block_Bytes);
            pragma Assert (not D.Resolve (Previous, D.Byte_Count (Previous), 1).Valid);
            for I in 0 .. Count - 2 loop
               pragma Assert (D.Resolve (Previous, Unsigned_64 (I) * E.Block_Bytes,
                 E.Block_Bytes).Address = Reply (I)(1));
            end loop;
         end if;
      end loop;
      R.Extend (Object, 17, OK);
      pragma Assert (not D.Valid (Previous));
      pragma Assert (not OK and not D.Valid (R.Result (Object)));
   end;
   for Fault in 0 .. 3 loop
      declare
         Object : R.Assembly;
         OK : Boolean;
         Data : R.Words;
      begin
         R.Start (Object, CPU, 17, OK, 1); pragma Assert (OK);
         R.Accept_Reply (Object, Reply (0), OK); pragma Assert (OK);
         if Fault = 0 then
            R.Extend (Object, 1, OK); pragma Assert (not OK);
         else
            R.Extend (Object, 3, OK); pragma Assert (OK);
            Data := Reply (1);
            if Fault = 1 then Data (1) := Reply (0)(1); end if;
            if Fault = 2 then Data (0) := 2; end if;
            if Fault = 3 then R.Cancel (Object); end if;
            R.Accept_Reply (Object, Data, OK); pragma Assert (not OK);
         end if;
         pragma Assert (not D.Valid (R.Result (Object)));
         R.Extend (Object, 2, OK); pragma Assert (not OK);
      end;
   end loop;
   for Bad in -1 .. 15 loop
      for Field in 0 .. 4 loop
         declare
            Object : R.Assembly;
            OK : Boolean;
            Data : R.Words;
         begin
            R.Start (Object, CPU, 17, OK); pragma Assert (OK);
            for I in 0 .. 15 loop
               pragma Assert (not D.Valid (R.Result (Object)));
               Data := Reply (I);
               if I = Bad then
                  if Field < 4 then Data (Field) := Data (Field) + 1;
                  else R.Cancel (Object); end if;
               end if;
               R.Accept_Reply (Object, Data, OK);
               if I = Bad then
                  pragma Assert (not OK and not D.Valid (R.Result (Object)));
                  R.Start (Object, CPU, 17, OK); pragma Assert (not OK);
                  R.Accept_Reply (Object, Reply (I), OK); pragma Assert (not OK);
                  exit;
               end if;
               pragma Assert (OK);
            end loop;
            if Bad = -1 then
               pragma Assert (D.Valid (R.Result (Object)));
               for I in 0 .. 15 loop
                  pragma Assert (D.Resolve (R.Result (Object),
                    Unsigned_64 (I) * E.Block_Bytes, E.Block_Bytes).Address = Reply (I)(1));
               end loop;
               R.Accept_Reply (Object, Reply (15), OK);
               pragma Assert (not OK and not D.Valid (R.Result (Object)));
            end if;
         end;
      end loop;
   end loop;
   for Duplicate in 1 .. 15 loop
      declare
         Object : R.Assembly;
         OK : Boolean;
         Data : R.Words;
      begin
         R.Start (Object, CPU, 17, OK); pragma Assert (OK);
         for I in 0 .. Duplicate loop
            Data := Reply (I);
            if I = Duplicate then Data (1) := Reply (0)(1); end if;
            R.Accept_Reply (Object, Data, OK);
            pragma Assert (OK = (I /= Duplicate));
         end loop;
         pragma Assert (not D.Valid (R.Result (Object)));
      end;
   end loop;
   for Case_Number in 0 .. 3 loop
      declare
         Object : R.Assembly;
         OK : Boolean;
         Base : constant Unsigned_64 :=
           (case Case_Number is
              when 0 => CPU + 1,
              when 1 => 16#8000_0000_0000#,
              when 2 => 16#8000_0000_0000# - E.Block_Bytes,
              when others => CPU);
      begin
         R.Start (Object, Base, (if Case_Number = 3 then 0 else 17), OK);
         pragma Assert (not OK and not D.Valid (R.Result (Object)));
      end;
   end loop;
   Put_Line ("extent replies PASS: complete map, field/cancel boundaries, aliases, bounds, terminal failure");
end Extent_Replies_Tests;
