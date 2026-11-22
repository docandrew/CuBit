with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
with Intel_GPU_Native_Plane;
with Intel_GPU_Plane_Registers;
with Intel_GPU_Plane_Decode;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces.C;
procedure Native_Plane_Tests is
   Held : Boolean := False;
   function Power_Held return Boolean is (Held);
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   use type Interfaces.C.int;
   use type Intel_GPU_Plane_Decode.Status;
   Fixture_Base : constant Integer_Address := 16#60070000#;
   Mapping : System.Address;
   procedure Check (Item : Pipe) is
      package Plane is new Intel_GPU_Native_Plane (Item, Power_Held);
      Result : Plane.Observation;
      Sizes : constant array (1 .. 5) of Unsigned_64 := [0, 1, 2_097_152, 8_388_608, Unsigned_64'Last];
   begin
      for Power in Boolean loop
         Held := Power;
         for Owner in Boolean loop
            for Size of Sizes loop
               if not Held or else not Owner or else Size not in 2_097_152 | 8_388_608 then
                  for Number in Intel_GPU_Plane_Registers.Plane_Number loop
                  Result := Plane.Inspect (Owner, Size, Number);
                  pragma Assert (not Result.Collected and Result.Reads = 0 and not Result.Decoded.Memory.Valid);
                  if Owner and Size in 2_097_152 | 8_388_608 then
                     pragma Assert (Plane.Diagnostic (Result) = "power unavailable; no register sample");
                  else
                     pragma Assert (Plane.Diagnostic (Result) = "rejected prerequisites");
                  end if;
                  end loop;
               end if;
            end loop;
         end loop;
      end loop;
   end Check;
   procedure Check_Positive (Item : Pipe) is
      package Plane is new Intel_GPU_Native_Plane (Item, Power_Held);
      R : Plane.Observation;
      function Surface (Number : Positive) return Unsigned_32 is
        (Unsigned_32 (Number + 10 * Pipe'Pos (Item)) * 16#10000#);
      procedure Set_Field (Number : Positive; Offset : Integer_Address; Value : Unsigned_32) is
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Fixture_Base + 16#1000# * Pipe'Pos (Item) +
                                    16#100# * Integer_Address (Number) + Offset);
      begin Register_Value := Value; end Set_Field;
   begin
      Held := True;
      -- Initialize every plane before reading any: a wrong plane selector
      -- then sees plausible but distinguishable data, rather than a sentinel.
      for Number in Intel_GPU_Plane_Registers.Plane_Number loop
         Set_Field (Number, 16#80#, 16#84000000#);
         Set_Field (Number, 16#88#, Unsigned_32 (Number));
         Set_Field (Number, 16#90#, Unsigned_32 (Number - 1) * 16#10000# + 15);
         Set_Field (Number, 16#A4#, 0);
         Set_Field (Number, 16#9C#, Surface (Number));
         Set_Field (Number, 16#AC#, Surface (Number));
      end loop;
      for Number in Intel_GPU_Plane_Registers.Plane_Number loop
         R := Plane.Inspect (True, 8_388_608, Number);
         pragma Assert (R.Collected and R.Reads = 12 and R.Decoded.State = Intel_GPU_Plane_Decode.Linear_Ready);
         pragma Assert (Plane.Diagnostic (R) = "linear ready");
         pragma Assert (R.Before.Stride = Unsigned_32 (Number));
         pragma Assert (R.Before.Size = Unsigned_32 (Number - 1) * 16#10000# + 15);
         pragma Assert (R.Before.Surface = Surface (Number) and R.After.Live_Surface = Surface (Number));
         pragma Assert (R.Decoded.Memory.First = Unsigned_64 (Surface (Number)) and R.Decoded.Memory.Bytes = 4096);
         Set_Field (Number, 16#AC#, Surface (Number) + 4096);
         R := Plane.Inspect (True, 8_388_608, Number);
         pragma Assert (R.Collected and R.Decoded.State = Intel_GPU_Plane_Decode.Changing);
         pragma Assert (Plane.Diagnostic (R) = "changing");
         Set_Field (Number, 16#AC#, Surface (Number));
         Set_Field (Number, 16#A4#, Unsigned_32'Last);
         R := Plane.Inspect (True, 8_388_608, Number);
         pragma Assert (not R.Collected and R.Reads = 3 and not R.Decoded.Memory.Valid);
         pragma Assert (Plane.Diagnostic (R) = "register read failed after 3 reads");
         Set_Field (Number, 16#A4#, 0);
         R := Plane.Inspect (True, 8_388_608, Number);
         pragma Assert (R.Collected and R.Decoded.State = Intel_GPU_Plane_Decode.Linear_Ready);
      end loop;
   end Check_Positive;
begin
   for Item in Pipe loop Check (Item); end loop;
   -- Linux-only hosted fixture. Do not replace an existing mapping.
   Mapping := Mmap (To_Address (Fixture_Base), 16384, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (Fixture_Base) then
      if Mapping /= To_Address (Integer_Address'Last) then
         declare Ignored : constant Interfaces.C.int := Munmap (Mapping, 16384); begin null; end;
      end if;
      raise Program_Error with "cannot reserve isolated plane fixture pages";
   end if;
   for Item in Pipe loop Check_Positive (Item); end loop;
   pragma Assert (Munmap (Mapping, 16384) = 0);
   Ada.Text_IO.Put_Line ("native five-plane PASS: all twenty A/B/C/D planes in host fixture (NOT hardware)");
end Native_Plane_Tests;
