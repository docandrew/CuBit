package body Fixture is
   procedure Reset (Kind : Positive) is
   begin
      Scenario := Kind; Turns := 0; Binds := 0; Prepares := 0; Arms := 0;
      Rejects := 0; Waits := 0; Poll_Sleeps := 0; Health_Sleeps := 0;
      Idle_Sleeps := 0; Open_Calls := 0; Reply_Calls := 0; Event_Calls := 0; Ready_Calls := 0;
      Polled := 0; Now := 1; Masked := False; Payload := False; Stop_Pending := False;
      Stopped := False; Retained := False; Supplied := False; Peer_Event_Sent := False;
      Clock_OK := Kind /= 14; Prepare_OK := Kind /= 13; Arm_OK := Kind /= 15;
      Deadline_Seen := 0; Reply_Label := 0; Ready_Label := 0;
      Delivery := Launch_Configuration.MSI; Vectors := 1;
      if Kind in 7 | 9 then Delivery := Launch_Configuration.Polled; Vectors := 0;
      elsif Kind = 12 then Delivery := Launch_Configuration.MSI_X; end if;
   end Reset;
end Fixture;
