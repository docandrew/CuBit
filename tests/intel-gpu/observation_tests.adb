with Interfaces; use Interfaces;
with Intel_GPU_Probe; use Intel_GPU_Probe;
with Intel_GPU_Observation; use Intel_GPU_Observation;
procedure Observation_Tests is
   Calls : Natural := 0;
   Base : constant Unsigned_64 := 16#6000_0000#;
   Result : Snapshot;
   function Read_Test (Address : Unsigned_64) return Unsigned_32 is
   begin
      Calls := Calls + 1;
      case Calls is
         when 1 =>
            pragma Assert (Address = Base + 16#45400#);
            return 16#FFFF_FFFF#;
         when 2 =>
            pragma Assert (Address = Base + 16#45404#);
            return 0;
         when others => raise Program_Error;
      end case;
   end Read_Test;
   procedure Read_Snapshot is new Capture (Read_Test);
begin
   for Hardware in Platform loop
      for D0 in Boolean loop
         Calls := 0;
         Read_Snapshot (Hardware, D0, Base, 16#20_0000#, Result);
         pragma Assert (Result.Captured = (Hardware = Alder_Lake_N and D0));
         if Result.Captured then
            pragma Assert (Calls = 2);
            pragma Assert (Result.Values = [16#FFFF_FFFF#, 0]);
         else
            pragma Assert (Calls = 0 and Result.Values = [0, 0]);
         end if;
      end loop;
   end loop;
   for Size in Unsigned_64 range 16#453FC# .. 16#4540C# loop
      Calls := 0;
      Read_Snapshot (Alder_Lake_N, True, Base, Size, Result);
      pragma Assert (Result.Captured = (Size >= 16#45408#));
      pragma Assert (Calls = (if Result.Captured then 2 else 0));
   end loop;
   Calls := 0;
   Read_Snapshot (Alder_Lake_N, True, Base + 1, 16#20_0000#, Result);
   pragma Assert (not Result.Captured and Calls = 0);
   Read_Snapshot (Alder_Lake_N, True, 0, 16#20_0000#, Result);
   pragma Assert (not Result.Captured and Calls = 0);
   Read_Snapshot
     (Alder_Lake_N, True, Unsigned_64'Last - 3, 16#20_0000#, Result);
   pragma Assert (not Result.Captured and Calls = 0);
end Observation_Tests;
