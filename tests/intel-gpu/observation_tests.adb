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
         when 3 =>
            pragma Assert (Address = Base + 16#45504#);
            return 16#0030_0002#;
         when 4 =>
            pragma Assert (Address = Base + 16#42000#);
            return 16#0C20_0000#;
         when others => raise Program_Error;
      end case;
   end Read_Test;
   procedure Read_Snapshot is new Capture (Read_Test);
begin
   for Pipe in Display_Pipe loop
      declare
         Index : constant Natural := 5 + Display_Pipe'Pos (Pipe);
         Requests : constant Unsigned_32 := 2 or
           (if Pipe = Pipe_A then 0 else 8) or Shift_Left (2, Index * 2);
         States : constant Unsigned_32 := Shift_Right (Requests, 1);
         Both : constant Unsigned_32 := Requests or States;
      begin
         pragma Assert (Pipe_Request_State_Mask (Pipe) = Both);
         pragma Assert (Pipe_Request_State_Set (Both, Pipe));
         pragma Assert (not Pipe_Request_State_Set (Requests, Pipe));
         pragma Assert (not Pipe_Request_State_Set (States, Pipe));
         pragma Assert (not Pipe_Request_State_Set (Unsigned_32'Last, Pipe));
         pragma Assert (Pipe_Request_State_Set (16#FC0F#, Pipe) = (Pipe /= Pipe_D));
         for Bit in 0 .. 31 loop
            pragma Assert
              (Pipe_Request_State_Set (Both xor Shift_Left (1, Bit), Pipe) =
               ((Both and Shift_Left (1, Bit)) = 0));
         end loop;
      end;
   end loop;
   for Hardware in Platform loop
      for D0 in Boolean loop
         Calls := 0;
         Read_Snapshot (Hardware, D0, Base, 16#20_0000#, Result);
         pragma Assert (Result.Captured = (Hardware = Alder_Lake_N and D0));
         if Result.Captured then
            pragma Assert (Calls = 4);
            pragma Assert (Result.Values = [16#FFFF_FFFF#, 0, 16#0030_0002#, 16#0C20_0000#]);
         else
            pragma Assert (Calls = 0 and Result.Values = Register_Values'[others => 0]);
         end if;
      end loop;
   end loop;
   for Size in Unsigned_64 range 16#453FC# .. 16#4550C# loop
      Calls := 0;
      Read_Snapshot (Alder_Lake_N, True, Base, Size, Result);
      pragma Assert (Result.Captured = (Size >= 16#45508#));
      pragma Assert (Calls = (if Result.Captured then 4 else 0));
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
