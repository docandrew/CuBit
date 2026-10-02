with Interfaces; use Interfaces;
with Intel_GPU_GuC_Status; use Intel_GPU_GuC_Status;
with Intel_GPU_GuC_Wait;
with Ada.Text_IO;
procedure GuC_Status_Tests is
   procedure Run (Mode : Natural) is
      Reads, Clocks, Pauses : Natural := 0;
      function Read_Status return Unsigned_32 is
      begin
         Reads := Reads + 1;
         return (if Mode = 1 then 16#40000000# elsif Mode = 2 then Unsigned_32'Last
                 elsif Mode = 3 then 0 else 16#8000F000#);
      end Read_Status;
      function Now return Unsigned_64 is
      begin
         Clocks := Clocks + 1;
         return (if Mode = 4 or (Mode = 5 and Clocks > 1) then Unsigned_64'Last
                 elsif Mode = 6 and Clocks > 1 then 99
                 elsif Mode = 7 and Clocks > 1 then 3000100 else 100);
      end Now;
      procedure Pause is
      begin Pauses := Pauses + 1; end Pause;
      package W is new Intel_GPU_GuC_Wait (Read_Status, Now, Pause);
      use type W.Result;
      Status : W.Result;
      Raw : Unsigned_32;
      Decoded : State;
   begin
      W.Execute (3, Status, Raw, Decoded);
      pragma Assert (Status = (case Mode is
        when 0 => W.Firmware_Ready,
        when 1 | 2 => W.Device_Failed,
        when 3 | 7 => W.Timed_Out,
        when others => W.Invalid_Clock));
      pragma Assert (Decoded = Decode (Raw));
      pragma Assert (Reads = (if Mode = 4 then 0 elsif Mode = 3 then 3 else 1));
      pragma Assert (Pauses = (if Mode = 3 then 2 else 0));
   end Run;
begin
   for B in Unsigned_32 range 0 .. 127 loop
      for F in Unsigned_32 range 0 .. 255 loop
         for A in Unsigned_32 range 0 .. 3 loop
            for Reset in Unsigned_32 range 0 .. 1 loop
               declare
                  Raw : constant Unsigned_32 := Shift_Left (A, 30) or
                    Shift_Left (F, 8) or Shift_Left (B, 1) or Reset;
                  Expected : constant State :=
                    (if A = 1 or B in 16#13# | 16#2B# | 16#50# then Authentication_Failed
                     elsif B in 16#73# .. 16#75# | 16#77# | 16#79# | 16#7A# | 16#7E# then Bootrom_Failed
                     elsif F in 2 .. 4 | 7 | 16#60# | 16#70# | 16#71# | 16#73# .. 16#75# then Firmware_Failed
                     elsif A = 2 and F = 16#F0# and Reset = 0 then Ready else Pending);
               begin pragma Assert (Decode (Raw) = Expected); end;
            end loop;
         end loop;
      end loop;
   end loop;
   pragma Assert (Decode (Unsigned_32'Last) = Invalid_MMIO);
   for Mode in 0 .. 7 loop Run (Mode); end loop;
   Ada.Text_IO.Put_Line ("PASS: 262144 GuC status combinations, all-ones and8 bounded wait cases");
end GuC_Status_Tests;
