package body Intel_GPU_Hotplug_Debounce with SPARK_Mode is
   procedure Pulse (Item : in out State; Port : PD.Port; Now : Milliseconds; Masked : out Boolean) is
      Pin : Pin_State renames Item (Port);
   begin
      Masked := False;
      if Storming (Pin) then
         return;
      end if;
      if Now < Pin.Period_Start or else Now - Pin.Period_Start >= Storm_Period_Ms then
         Pin.Period_Start := Now;
         Pin.Pulses := 0;
      end if;
      if Pin.Pulses < Pulse_Count'Last then
         Pin.Pulses := Pin.Pulses + 1;
      end if;
      if Pin.Pulses > Storm_Threshold then
         Pin.Masked_Until := Now + Storm_Reenable_Ms;
         Pin.Due := Never;
         Pin.Pulses := 0;
         Masked := True;
      else
         Pin.Due := Now + Settle_Ms;
      end if;
   end Pulse;

   procedure Take_Due (Item : in out State; Now : Milliseconds; Due : out Port_Set) is
   begin
      Due := [others => False];
      for P in PD.Port loop
         if Item (P).Due /= Never and then Item (P).Due <= Now then
            Due (P) := True;
            Item (P).Due := Never;
         end if;
         pragma Loop_Invariant
           (for all Q in PD.Port =>
              (if Q <= P then
                 Due (Q) = (Item'Loop_Entry (Q).Due /= Never and then Item'Loop_Entry (Q).Due <= Now) and then
                 (if Due (Q) then Item (Q).Due = Never else Item (Q).Due = Item'Loop_Entry (Q).Due)
               else Item (Q) = Item'Loop_Entry (Q) and then not Due (Q)));
      end loop;
   end Take_Due;

   procedure Detected
     (Item : in out State; Port : PD.Port; Now : Milliseconds; Sink_Without_EDID : Boolean) is
      Pin : Pin_State renames Item (Port);
   begin
      if Sink_Without_EDID and then Pin.Retry_Left then
         Pin.Retry_Left := False;
         if Pin.Due = Never then
            Pin.Due := Now + Retry_Ms;
         end if;
      elsif not Sink_Without_EDID then
         Pin.Retry_Left := True;
      end if;
   end Detected;

   procedure Take_Reenabled (Item : in out State; Now : Milliseconds; Ready : out Port_Set) is
   begin
      Ready := [others => False];
      for P in PD.Port loop
         if Item (P).Masked_Until /= Never and then Item (P).Masked_Until <= Now then
            Ready (P) := True;
            Item (P).Masked_Until := Never;
         end if;
         pragma Loop_Invariant
           (for all Q in PD.Port =>
              (if Q <= P then
                 Ready (Q) = (Item'Loop_Entry (Q).Masked_Until /= Never and then
                              Item'Loop_Entry (Q).Masked_Until <= Now)
               else not Ready (Q)));
      end loop;
   end Take_Reenabled;

   function Next_Wake (Item : State) return Milliseconds is
      Result : Milliseconds := Never;
   begin
      for P in PD.Port loop
         Result := Milliseconds'Min (Result, Milliseconds'Min (Item (P).Due, Item (P).Masked_Until));
      end loop;
      return Result;
   end Next_Wake;
end Intel_GPU_Hotplug_Debounce;
