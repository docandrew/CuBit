package body Intel_GPU_Port_Detection with SPARK_Mode is
   function Decode_Port (Words : Hotplug_Words; Item : Port) return Port_Sense is
   begin
      if Item in Combo_Port then
         if Words.South_Live = Unreadable_Word or else Words.South_DDI_Control = Unreadable_Word then
            return (Unreadable, No_Path);
         elsif not Bit (Words.South_DDI_Control, Enable_Bit (Item)) then
            return (Detection_Off, No_Path);
         elsif Bit (Words.South_Live, South_Live_Bit (Item)) then
            return (Sink, Native);
         else
            return (No_Sink, No_Path);
         end if;
      end if;
      if Words.South_Live = Unreadable_Word or else Words.South_TC_Control = Unreadable_Word or else
        Words.North_Live = Unreadable_Word or else Words.North_TC_Control = Unreadable_Word or else
        Words.North_TBT_Control = Unreadable_Word
      then
         return (Unreadable, No_Path);
      end if;
      declare
         Legacy_On : constant Boolean := Bit (Words.South_TC_Control, Enable_Bit (Item));
         Alt_On : constant Boolean := Bit (Words.North_TC_Control, Enable_Bit (Item));
         TBT_On : constant Boolean := Bit (Words.North_TBT_Control, Enable_Bit (Item));
      begin
         --  i915 reads all three; a legacy sink wins (a static port has no
         --  alternate mode).
         if Legacy_On and then Bit (Words.South_Live, South_Live_Bit (Item)) then
            return (Sink, Legacy);
         elsif Alt_On and then Bit (Words.North_Live, DP_Alt_Live_Bit (Item)) then
            return (Sink, DP_Alternate);
         elsif TBT_On and then Bit (Words.North_Live, TBT_Live_Bit (Item)) then
            return (Sink, Thunderbolt_Alternate);
         elsif not (Legacy_On or else Alt_On or else TBT_On) then
            return (Detection_Off, No_Path);
         else
            return (No_Sink, No_Path);
         end if;
      end;
   end Decode_Port;

   function Decode (Words : Hotplug_Words) return Sense_Set is
      Result : Sense_Set := [others => (Unreadable, No_Path)];
   begin
      for P in Port loop
         Result (P) := Decode_Port (Words, P);
         pragma Loop_Invariant (for all Q in Port'First .. P => Result (Q) = Decode_Port (Words, Q));
      end loop;
      return Result;
   end Decode;
end Intel_GPU_Port_Detection;
