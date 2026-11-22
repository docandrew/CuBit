with Ada.Text_IO;
with Interfaces; use Interfaces;
with PS2_Boot_Probe;
procedure Probe_Tests is
   type Device_Mode is (Missing, Normal, Stuck_Full, Disappearing);
   Mode : Device_Mode := Missing;
   Remaining, Status_Reads, Data_Reads : Natural := 0;
   function Read_Port (Port : Unsigned_16) return Unsigned_8 is
   begin
      if Port = 16#64# then
         Status_Reads := Status_Reads + 1;
         case Mode is
            when Missing => return 16#FF#;
            when Stuck_Full => return 1;
            when Disappearing =>
               return (if Data_Reads = 3 then 16#FF# else 1);
            when Normal => return (if Remaining = 0 then 0 else 1);
         end case;
      else
         pragma Assert (Port = 16#60#);
         Data_Reads := Data_Reads + 1;
         if Remaining > 0 then Remaining := Remaining - 1; end if;
         return 16#FA#;
      end if;
   end Read_Port;
   package Probe is new PS2_Boot_Probe (Read_Port);
   use type Probe.Probe_Result;
   Result : Probe.Probe_Result;
   procedure Reset (Kind : Device_Mode; Bytes : Natural := 0) is
   begin
      Mode := Kind;
      Remaining := Bytes;
      Status_Reads := 0;
      Data_Reads := 0;
   end Reset;
begin
   Reset (Missing);
   Probe.Drain (Result);
   pragma Assert (Result = Probe.Controller_Unavailable);
   pragma Assert (Status_Reads = 1 and Data_Reads = 0);
   for Stale in 0 .. Probe.Max_Stale_Bytes loop
      Reset (Normal, Stale);
      Probe.Drain (Result);
      pragma Assert (Result = Probe.Quiescent);
      pragma Assert (Status_Reads = Stale + 1 and Data_Reads = Stale);
   end loop;
   Reset (Normal, Probe.Max_Stale_Bytes + 1);
   Probe.Drain (Result);
   pragma Assert (Result = Probe.Drain_Limit and Remaining = 1);
   Reset (Stuck_Full);
   Probe.Drain (Result);
   pragma Assert (Result = Probe.Drain_Limit);
   pragma Assert (Data_Reads = Probe.Max_Stale_Bytes);
   pragma Assert (Status_Reads = Probe.Max_Stale_Bytes + 1);
   Reset (Disappearing);
   Probe.Drain (Result);
   pragma Assert (Result = Probe.Controller_Unavailable and Data_Reads = 3);
   Ada.Text_IO.Put_Line ("PS2 probe PASS: absent, 0..256 stale bytes, overflow, stuck, disappearance");
end Probe_Tests;
