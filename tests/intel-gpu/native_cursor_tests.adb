with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
with Intel_GPU_Native_Cursor;
with Intel_GPU_Cursor_Decode;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces.C;
procedure Native_Cursor_Tests is
   Held : Boolean := False;
   function Power_Held return Boolean is (Held);
   -- Linux-hosted fixture only. Never replace an existing host mapping.
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   use type Interfaces.C.int;
   use type Intel_GPU_Cursor_Decode.Status;
   Fixture_Base : constant Integer_Address := 16#60070000#;
   Mapping : System.Address;
   procedure Check (Item : Pipe) is
      package Cursor is new Intel_GPU_Native_Cursor (Item, Power_Held);
      Result : Cursor.Observation;
      Sizes : constant array (1 .. 6) of Unsigned_64 :=
        [0, 1, 2_097_152, 4_194_304, 8_388_608, Unsigned_64'Last];
   begin
      for Power in Boolean loop
         Held := Power;
         for Owner in Boolean loop
            for Size of Sizes loop
               if not Held or else not Owner or else
                 Size not in 2_097_152 | 4_194_304 | 8_388_608
               then
                  Result := Cursor.Inspect (Owner, Size);
                  pragma Assert (not Result.Collected and Result.Reads = 0 and not Result.Decoded.Memory.Valid);
                  pragma Assert (Cursor.Diagnostic (Result) =
                    (if not Owner or Size not in 2_097_152 | 4_194_304 | 8_388_608
                     then "rejected prerequisites" else "power unavailable; no register sample"));
               end if;
            end loop;
         end loop;
      end loop;
   end Check;
   procedure Check_Positive (Item : Pipe) is
      package Cursor is new Intel_GPU_Native_Cursor (Item, Power_Held);
      R : Cursor.Observation;
      Expected_Base : constant Unsigned_32 := Unsigned_32 (1 + Pipe'Pos (Item)) * 4096;
      procedure Set_Field (Offset : Integer_Address; Value : Unsigned_32) is
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Fixture_Base + 16#1000# * Pipe'Pos (Item) + Offset);
      begin Register_Value := Value; end Set_Field;
   begin
      Held := True;
      -- Different per-pipe bases catch accidental A/B aliasing. Actual
      -- volatile adapter reads execute against these anonymous host pages.
      Set_Field (16#80#, 16#27#); Set_Field (16#84#, Expected_Base);
      Set_Field (16#AC#, Expected_Base); Set_Field (16#A0#, 0);
      R := Cursor.Inspect (True, 8_388_608);
      pragma Assert (R.Collected and R.Reads = 8 and R.Decoded.State = Intel_GPU_Cursor_Decode.Ready);
      pragma Assert (Cursor.Diagnostic (R) = "ready");
      pragma Assert (R.Before.Base = Expected_Base and R.After.Live_Base = Expected_Base);
      pragma Assert (R.Decoded.Memory.First = Unsigned_64 (Expected_Base) and R.Decoded.Memory.Bytes = 16_384);
      Set_Field (16#AC#, Expected_Base + 4096);
      R := Cursor.Inspect (True, 8_388_608);
      pragma Assert (R.Collected and R.Decoded.State = Intel_GPU_Cursor_Decode.Changing);
      pragma Assert (Cursor.Diagnostic (R) = "changing");
      Set_Field (16#AC#, Expected_Base);
      Set_Field (16#A0#, Unsigned_32'Last);
      R := Cursor.Inspect (True, 8_388_608);
      pragma Assert (not R.Collected and R.Reads = 3 and not R.Decoded.Memory.Valid);
      pragma Assert (Cursor.Diagnostic (R) = "register read failed after 3 reads");
      -- A failed collection must end its access scope, so another independent
      -- observation is possible after the register becomes readable again.
      Set_Field (16#A0#, 0); Set_Field (16#80#, 0);
      R := Cursor.Inspect (True, 8_388_608);
      pragma Assert (R.Collected and R.Reads = 8 and R.Decoded.State = Intel_GPU_Cursor_Decode.Disabled);
      pragma Assert (Cursor.Diagnostic (R) = "disabled");
      Held := False;
      R := Cursor.Inspect (True, 8_388_608);
      pragma Assert (not R.Collected and R.Reads = 0);
   end Check_Positive;
begin
   for Item in Pipe loop Check (Item); end loop;
   Mapping := Mmap (To_Address (Fixture_Base), 16384, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (Fixture_Base) then
      -- Older hosts may ignore MAP_FIXED_NOREPLACE and choose another address.
      if Mapping /= To_Address (Integer_Address'Last) then
         declare Ignored : constant Interfaces.C.int := Munmap (Mapping, 16384); begin null; end;
      end if;
      raise Program_Error with "cannot reserve isolated cursor fixture pages";
   end if;
   for Item in Pipe loop Check_Positive (Item); end loop;
   pragma Assert (Munmap (Mapping, 16384) = 0);
   Ada.Text_IO.Put_Line ("native cursor PASS: rejection and real adapter loads against host fixture (NOT hardware)");
end Native_Cursor_Tests;
