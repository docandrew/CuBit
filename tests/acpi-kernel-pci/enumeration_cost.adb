pragma Ada_2022;
with Ada.Text_IO;
with Ada.Real_Time;
with Interfaces; use Interfaces;
with Firmware_Tables.MCFG;
with Physical_Ranges;
with Root_Export_Admission;
procedure Enumeration_Cost is
   package P renames Physical_Ranges;
   use type Ada.Real_Time.Time;
   use type P.Firmware_Kind;
   procedure Measure (Allocations : Positive;
                      Last_Bus : Firmware_Tables.MCFG.Bus_Number) is
      M : P.Map := P.Empty_Map;
      Cursor : P.Firmware_Cursor;
      Found : Boolean;
      Item : P.Firmware_Run;
      Count, Calls, Omitted : Natural := 0;
      Expected : constant Natural := Allocations * (Natural (Last_Bus) + 1);
      Started, Finished : Ada.Real_Time.Time;
      Pick : Root_Export_Admission.Selection;
   begin
      for I in 1 .. Allocations loop
         P.Add_PCI_Config (M,
           (16#1_0000_0000# + Unsigned_64 (I) * 256 * P.PCI_Bus_Bytes,
            Firmware_Tables.MCFG.Segment_Number (I), 0, Last_Bus, 0));
      end loop;
      P.Seal (M);
      Started := Ada.Real_Time.Clock;
      Cursor := P.First_Firmware_Cursor (M);
      loop
         Calls := Calls + 1;
         P.Next_Firmware_Run (M, Cursor, Found, Item, Omitted);
         exit when not Found;
         if Item.Kind /= P.PCI_Config_Memory then
            raise Program_Error with "unexpected benchmark root kind";
         end if;
         Count := Count + 1;
         if Count > Expected then raise Program_Error with "enumeration did not finish"; end if;
      end loop;
      Finished := Ada.Real_Time.Clock;
      if not M.Complete or Count /= Expected or Omitted /= 0 or Calls /= Count + 1 then
         raise Program_Error with "benchmark inventory mismatch";
      end if;
      Pick := Root_Export_Admission.Choose
        (0, Root_Export_Admission.Root_Demand (Count));
      Ada.Text_IO.Put_Line
        ("enumeration allocations=" & Allocations'Image & " roots=" & Count'Image &
         " calls=" & Calls'Image & " seconds=" &
         Duration'Image (Ada.Real_Time.To_Duration (Finished - Started)) &
         " selection=" & Pick.Status'Image);
   end Measure;
begin
   Measure (1, Firmware_Tables.MCFG.Bus_Number'Last);
   Measure (33, Firmware_Tables.MCFG.Bus_Number'Last);
   Measure (P.Max_PCI_Allocations, 0);
end Enumeration_Cost;
