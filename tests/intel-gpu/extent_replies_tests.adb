with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Extent_Replies;
procedure Extent_Replies_Tests is
   package R renames Intel_GPU_Extent_Replies;
   package E renames R.E;
   CPU : constant Unsigned_64 := 16#7000_0000_0000#;
   function Reply (I : Natural) return R.Words is
     ([Unsigned_64 (I), 16#8000_0000# - Unsigned_64 (I) * 2 * E.Block_Bytes,
       CPU + Unsigned_64 (I) * E.Block_Bytes, 17]);
begin
   for Bad in -1 .. 15 loop
      for Field in 0 .. 4 loop
         declare
            Object : R.Assembly;
            OK : Boolean;
            Data : R.Words;
         begin
            R.Start (Object, CPU, 17, OK); pragma Assert (OK);
            for I in 0 .. 15 loop
               pragma Assert (not E.Ready (R.Result (Object)));
               Data := Reply (I);
               if I = Bad then
                  if Field < 4 then Data (Field) := Data (Field) + 1;
                  else R.Cancel (Object); end if;
               end if;
               R.Accept_Reply (Object, Data, OK);
               if I = Bad then
                  pragma Assert (not OK and not E.Ready (R.Result (Object)));
                  R.Start (Object, CPU, 17, OK); pragma Assert (not OK);
                  R.Accept_Reply (Object, Reply (I), OK); pragma Assert (not OK);
                  exit;
               end if;
               pragma Assert (OK);
            end loop;
            if Bad = -1 then
               pragma Assert (E.Ready (R.Result (Object)));
               for I in 0 .. 15 loop
                  pragma Assert (E.Resolve (R.Result (Object),
                    Unsigned_64 (I) * E.Block_Bytes, E.Block_Bytes).Address = Reply (I)(1));
               end loop;
               R.Accept_Reply (Object, Reply (15), OK);
               pragma Assert (not OK and not E.Ready (R.Result (Object)));
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
         pragma Assert (not E.Ready (R.Result (Object)));
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
         pragma Assert (not OK and not E.Ready (R.Result (Object)));
      end;
   end loop;
   Put_Line ("extent replies PASS: complete map, field/cancel boundaries, aliases, bounds, terminal failure");
end Extent_Replies_Tests;
