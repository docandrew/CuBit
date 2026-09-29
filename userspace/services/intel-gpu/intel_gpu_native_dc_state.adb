with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Native_DC_State is
   use Intel_GPU_DC_State;
   Active : Boolean := False;
   function Capture (Owner : Boolean) return Observation is
      Samples : array (1 .. 2) of Snapshot := [others => [others => 0]];
      Result : Observation;
   begin
      if Active or else not Owner or else not PW1_Held then return Result; end if;
      Active := True; Result.Status := Read_Failed;
      for Pass in Samples'Range loop
         for F in Field loop
            if not PW1_Held then Active := False; return Result; end if;
            declare
               Value : Unsigned_32 with Import, Volatile_Full_Access,
                 Address => To_Address (16#60000000# + Integer_Address (Register_Offset (F)));
               Copied : constant Unsigned_32 := Value;
            begin
               if Copied = Unsigned_32'Last then Active := False; return Result; end if;
               Samples (Pass) (F) := Copied; Result.Reads := Result.Reads + 1;
            end;
         end loop;
      end loop;
      if not PW1_Held then Active := False; return Result; end if;
      Active := False;
      if Samples (1) /= Samples (2) then Result.Status := Changing; return Result; end if;
      Result.Status := Collected; Result.Values := Samples (1);
      return Result;
   end Capture;
end Intel_GPU_Native_DC_State;
