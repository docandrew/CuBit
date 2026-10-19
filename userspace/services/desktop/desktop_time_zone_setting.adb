with System;
with CuBit.Config;
with Clock_Settings;

package body Desktop_Time_Zone_Setting is
   use type CuBit.Config.ConfigStatus;
   use type System.Address;

   procedure Load (Zone : out Time_Zones.Time_Zone; Known : out Boolean) is
      Value  : System.Address;
      Length : Natural;
      Status : CuBit.Config.ConfigStatus;
   begin
      Zone := Time_Zones.Zone_UTC;
      CuBit.Config.get (Clock_Settings.Time_Zone_Key, Value, Length, Status);
      if Status = CuBit.Config.NotFound then
         Known := True;
      elsif Status = CuBit.Config.OK and then Value /= System.Null_Address and then Length > 0 then
         declare
            Name : String (1 .. Length) with Import, Address => Value;
         begin
            Time_Zones.Find (Name, Zone, Known);
         end;
      else
         Known := False;
      end if;
   end Load;

   procedure Store (Zone : Time_Zones.Time_Zone; Saved : out Boolean) is
      Name : constant String := Time_Zones.Name (Zone);
      Status : CuBit.Config.ConfigStatus;
   begin
      CuBit.Config.set (Clock_Settings.Time_Zone_Key, Name'Address, Name'Length, Status);
      Saved := Status = CuBit.Config.OK;
   end Store;
end Desktop_Time_Zone_Setting;
