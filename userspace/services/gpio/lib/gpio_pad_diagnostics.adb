pragma Ada_2022;
package body GPIO_Pad_Diagnostics with SPARK_Mode is
   use type G.Outcome, I.Access_Status;
   procedure Capture (Plan : G.Plan_Result; Idle : Boolean; Result : out Snapshot) is
      IDs : constant array (Register_Kind) of I.Read_ID :=
        [Host_Owner => I.Host_Owner, Pad_Configuration => I.Pad_Configuration,
         Group_Status => I.Group_Status, Group_Enable => I.Group_Enable];
   begin
      Result := [others => (others => <>)];
      if not Idle or else Plan.Status /= G.Ready then return; end if;
      for K in Register_Kind loop
         Result (K).Attempted := True;
         Read_Register (Plan, IDs (K), Result (K).Value, Result (K).Status);
         exit when Result (K).Status /= I.Access_OK;
      end loop;
   end Capture;
end GPIO_Pad_Diagnostics;
