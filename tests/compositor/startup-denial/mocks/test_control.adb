package body Test_Control is
 procedure Reset is begin Calls := (others => 0); Deny := False; Empty_Slot := False; Inspect_OK := True; Stops := 0; Stage_Logs := 0; Stage_Length := 0; Last_Stage := (others => ' '); end;
 procedure Attempt(S : Stage; OK : out Boolean) is
 begin Calls(S) := Calls(S)+1; OK := not (Deny and Failure=S); end;
end Test_Control;
