with Intel_GPU_Display_Registers;
package body Intel_GPU_Port_Scan with SPARK_Mode is
   package R renames Intel_GPU_Display_Registers;

   procedure Capture (Words : out PD.Hotplug_Words) is
   begin
      Words := (South_Live => Read_MMIO (R.SDEISR),
                South_DDI_Control => Read_MMIO (R.SHOTPLUG_CTL_DDI),
                South_TC_Control => Read_MMIO (R.SHOTPLUG_CTL_TC),
                North_Live => Read_MMIO (R.GEN11_DE_HPD_ISR),
                North_TC_Control => Read_MMIO (R.GEN11_TC_HOTPLUG_CTL),
                North_TBT_Control => Read_MMIO (R.GEN11_TBT_HOTPLUG_CTL));
   end Capture;

   procedure Scan (Words : PD.Hotplug_Words; Result : out Results; Base : out Bases) is
      Item : Port_Result;
   begin
      Result := [others => <>];
      Base := [others => [others => 0]];
      for P in PD.Port loop
         Item.Sense := PD.Decode_Port (Words, P);
         Item.Channel := PD.Channel_For (Is_Lit (P), Item.Sense);
         case Item.Channel is
            when PD.DDC_Channel => Read_DDC_Base (P, Base (P), Item.Outcome);
            when PD.AUX_Channel => Item.Outcome := PD.Needs_AUX;
            when PD.No_Channel => Item.Outcome := PD.Not_Read;
         end case;
         Item.State := PD.Decide (Is_Lit (P), Item.Sense, Item.Outcome);
         Result (P) := Item;
         pragma Loop_Invariant
           (for all Q in PD.Port'First .. P =>
              Result (Q).Sense = PD.Decode_Port (Words, Q) and then
              Result (Q).Channel = PD.Channel_For (Is_Lit (Q), Result (Q).Sense) and then
              (if Result (Q).Channel /= PD.DDC_Channel then
                 Result (Q).Outcome = (if Result (Q).Channel = PD.AUX_Channel then PD.Needs_AUX
                                       else PD.Not_Read)) and then
              Result (Q).State = PD.Decide (Is_Lit (Q), Result (Q).Sense, Result (Q).Outcome));
      end loop;
   end Scan;
end Intel_GPU_Port_Scan;
