with CuBit.Settings;
with Startup;

package body Desktop_Time_Zone_Setting is
   use all type CuBit.Settings.Outcome;
   use all type CuBit.Settings.Change_Outcome;
   package Setting renames Startup.Settings.Clock_Time_Zone;

   procedure Load (Zone : out Time_Zones.Time_Zone; Known : out Boolean) is
      Stored : constant Setting.Reading := Setting.Read;
   begin
      Zone := Time_Zones.Zone_UTC;
      case Stored.Kind is
         when Absent => Known := True;
         when Present => Time_Zones.Find (CuBit.Settings.Image (Stored.Item), Zone, Known);
         when Denied | Invalid | Unavailable => Known := False;
      end case;
   end Load;

   procedure Store (Zone : Time_Zones.Time_Zone; Saved : out Boolean) is
      Status : CuBit.Settings.Change_Outcome;
   begin
      Setting.Write (CuBit.Settings.To_Text (Time_Zones.Name (Zone)), Status);
      Saved := Status = Done;
   end Store;
end Desktop_Time_Zone_Setting;
