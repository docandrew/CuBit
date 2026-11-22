with Intel_GPU_Timestamp_Clock;
package body Intel_GPU_Timestamp_Observe with SPARK_Mode is
   procedure Sample (Hz : out Interfaces.Unsigned_32; Status : out Outcome) is
      use Interfaces;
      use Intel_GPU_Timestamp_Clock;
      Modes : array (1 .. 2) of CTC_Mode;
      Configs : array (1 .. 2) of RPM_CONFIG0 := [others => (others => <>)];
      Dividers : array (1 .. 2) of Timestamp_Override := [others => (others => <>)];
      Raw : Unsigned_32;
      OK : Boolean;
   begin
      Hz := 0;
      Status := Read_Failed;
      for Pass in 1 .. 2 loop
         Read (CTC_Mode_Offset, Raw, OK);
         if not OK then return; end if;
         Modes (Pass) := Decode_Mode (Raw);
         Read ((if Modes (Pass).Divide_Logic = 0 then CONFIG0_Offset
                else Override_Offset), Raw, OK);
         if not OK then return; end if;
         if Modes (Pass).Divide_Logic = 0 then
            Configs (Pass) := Decode (Raw);
         else
            Dividers (Pass) := Decode_Override (Raw);
         end if;
      end loop;
      if not Same_Clock (Modes (1), Modes (2), Configs (1), Configs (2),
                         Dividers (1), Dividers (2)) then
         Status := Changing;
         return;
      end if;
      Hz := Timestamp_Hz (Modes (2), Configs (2), Dividers (2));
      Status := (if Hz = 0 then Reserved_Selector else Ready);
   end Sample;
end Intel_GPU_Timestamp_Observe;
