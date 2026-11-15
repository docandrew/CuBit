with Probe_Model;package body ACPI_Native_PCI_Windows is
 package Q renames ACPI_PCI_Protocol;package P renames Q.P;package R renames ACPI_Local_PCI_Results;
 use type Word,Q.Wire.Time_Instant,P.Segment_Number,P.Bus_Number,P.Device_Number,P.Function_Number,P.T.Access_Width,Probe_Model.Test_Mode;
 procedure Prepare_Local(Scope:Word;Key:P.Bus_Key;Setup_Deadline_Milliseconds:Q.Wire.Time_Instant;Allow_Setup:Boolean;Result:out Local_Window) is
 begin if Scope/=Probe_Model.Scope or Key.Segment/=0 or Key.Bus/=0 or Setup_Deadline_Milliseconds/=Q.Wire.No_Finite_Deadline or Allow_Setup then raise Program_Error with "unexpected setup or key";end if;
 Probe_Model.Headers:=Probe_Model.Headers+1;Result:=(others=><>);
 if Probe_Model.Selected=Probe_Model.No_Window then Result.Status:=Q.Not_Ready;else Result.Status:=Q.Ready;Result.Epoch:=Probe_Model.Epoch;Result.ID:=Probe_Model.ID;end if;end;
 procedure Read_Local(Scope,Epoch,ID:Word;Region:P.Region;Offset:Word;Width:P.T.Access_Width;Deadline_Microseconds:Word;Guard:not null access function return Boolean;Result:out R.Result) is
 begin if Scope/=Probe_Model.Scope or Epoch/=Probe_Model.Epoch or ID/=Probe_Model.ID or Region.Device.Segment/=0 or Region.Device.Bus/=0 or Region.Device.Device/=31 or Region.Device.Func/=0 or Region.Base/=0 or Region.Length/=256 or Width/=P.T.Dword_Access or Deadline_Microseconds/=1000 or not Guard.all then raise Program_Error with "unexpected read or narrowed identity/deadline";end if;
 Probe_Model.Reads:=Probe_Model.Reads+1;Result:=(Status=>R.Read_Complete,Value=>0);
 if Probe_Model.Selected=Probe_Model.Expired then Result.Status:=R.Deadline_Expired;return;end if;
 case Offset is
 when 0=>Result.Value:=(if Probe_Model.Selected=Probe_Model.No_Intel then 16#5481_1022# elsif Probe_Model.Selected=Probe_Model.Wide_Identity then Word'Last else 16#5481_8086#);
 when 8=>if Probe_Model.Selected=Probe_Model.Class_Refused then Result.Status:=R.Guard_Refused;else Result.Value:=(if Probe_Model.Selected=Probe_Model.No_Class then 16#0300_0000# else 16#0601_0000#);end if;
 when 16#98#=>if Probe_Model.Selected=Probe_Model.LGMR_Refused then Result.Status:=R.Deadline_Expired;else Result.Value:=(if Probe_Model.Selected=Probe_Model.Disabled then 16#FE0B_0000# else 16#FE0B_0001#);end if;
 when others=>raise Program_Error with "unrequested register";end case;end;
end ACPI_Native_PCI_Windows;