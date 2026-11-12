pragma Ada_2022;

package body Platform_Quirks with SPARK_Mode is

   function Sideband_Of (Platform : Observed_Platform) return Sideband_Source is
   begin
      for Q in Quirk loop
         if Tables.Applies (The_Table, Q, Platform)
           and then The_Table (Q).Changes.Sideband /= No_Sideband
         then
            return The_Table (Q).Changes.Sideband;
         end if;
      end loop;
      return No_Sideband;
   end Sideband_Of;

   function Communities_Of (Platform : Observed_Platform) return Community_Layout is
   begin
      for Q in Quirk loop
         if Tables.Applies (The_Table, Q, Platform)
           and then The_Table (Q).Changes.Communities /= No_Communities
         then
            return The_Table (Q).Changes.Communities;
         end if;
      end loop;
      return No_Communities;
   end Communities_Of;

   function Controller_Of (Platform : Observed_Platform) return GPIO_Controller is
   begin
      for Q in Quirk loop
         if Tables.Applies (The_Table, Q, Platform)
           and then The_Table (Q).Changes.Controller /= No_Controller
         then
            return The_Table (Q).Changes.Controller;
         end if;
      end loop;
      return No_Controller;
   end Controller_Of;

end Platform_Quirks;
