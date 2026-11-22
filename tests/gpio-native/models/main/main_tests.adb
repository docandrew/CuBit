pragma Ada_2022;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Main;
with Main_Model;
with GPIO_Control_Wire;
procedure Main_Tests is
   package B renames Main_Model;
   package G renames B.G;
   package W renames GPIO_Control_Wire;
   use type B.Scenario, B.I.Word, W.Completion_Status;
   Quarantined : Boolean := False;
   function Has (Text : String) return Boolean is
     (Ada.Strings.Fixed.Index (B.U.To_String (B.Logs), Text) /= 0);
begin
   B.Initialize;
   begin
      Main;
   exception when B.Test_Complete => Quarantined := True; end;
   B.Check (B.Ready_Reports + B.Decline_Reports = 1, "exactly one actual Main readiness attempt");
   B.Check (Has ("bring-up "), "actual bring-up logger emitted summary");
   if B.Current = B.Runtime_Write_Unknown then
      B.Check (Quarantined and B.Ready_Reports = 1 and B.Decline_Reports = 0,
         "runtime unknown write retains resources after initial readiness");
      B.Check (B.Runtime_Snapshots = 1 and B.Runtime_Lines = 7 and B.Post_Fault_Waits = 3,
         "one detached snapshot across repeated supervised waits");
      B.Check (Has ("controller fault=TRUE") and Has ("unresolved= 1") and
         Has ("admin phase=QUARANTINED status=UNCERTAIN"),
         "snapshot identifies controller, registry and admin uncertainty");
      B.Check (Has ("reason=REGISTER_INDETERMINATE") and Has ("write-id=WRITE_CONFIGURATION status=UNKNOWN_EFFECT") and
         Has ("effects possible=TRUE"), "snapshot retains exact engine operation and effect uncertainty");
      B.Check (B.Register_Reads = B.Reads_At_Fault and B.Register_Writes = B.Writes_At_Fault and
         B.Sink_Closes = 0 and B.Saved_Answers = 1 and B.Last_Admin = W.Uncertain,
         "logging and retained waits perform no further MMIO, cleanup or reply replay");
   elsif B.Current = B.No_Parent then
      B.Check (Quarantined and B.Decline_Reports = 1 and B.Ready_Reports = 0 and
         B.Role_Attempts = 0 and B.Control_Answers = 0 and not B.Offered,
         "production Main without a route declines before registration or sink admission");
      B.Check (Has ("parent line unavailable vector=0 reason=parent-line-required") and
         B.Parent_ACKs = 0 and B.Event_Deliveries = 0,
         "missing optional route reports once and never invents IRQ or polling events");
   elsif B.Current in B.Wrong_Parent_Vector | B.Initial_ACK_Refused then
      B.Check (Quarantined and B.Decline_Reports = 1 and B.Ready_Reports = 0 and B.Control_Answers = 0,
         "inconsistent/refused parent retains resources for supervision without serving events");
      B.Check (B.Parent_ACKs = (if B.Current = B.Initial_ACK_Refused then 1 else 0), "refused ACK is never retried");
   elsif B.Current in B.Happy | B.Stop_During_Admission then
      B.Check (not Quarantined and then B.Role_Attempts = 1 and then B.Ready_Reports = 1 and then B.Decline_Reports = 0,
         "actual Main reports Ready after parking/registration then terminates cleanly");
      B.Check (B.Sink_Closes = 1 and then not B.Saved and then B.Sink_Closed, "actual owner drain closes sink and consumes saved reply");
      if B.Current in B.Happy then
         B.Check (B.Saved_Answers = 2 and then B.Control_Answers = 2 and then B.Event_Deliveries = 1 and then B.Request_Stage = 6,
            "actual Main drives admit/arm/IRQ/event/ACK/retire/shutdown through actual stateful owners");
         B.Check (B.Parent_ACKs = 2 and Has ("parent line armed vector="), "initial quiet ACK plus actual IRQ ACK recorded");
      else
         B.Check (B.Saved_Answers = 1 and then B.Last_Admin = W.Refused and then B.Control_Answers = 0,
            "stop during runtime admission drains before refused reply, with no client arm");
      end if;
      B.Check (Has ("bring-up complete") and then Has ("gpio-addressable=251") and then Has ("nomap-untouched=6"),
         "success summary reports explicit polled GPIO geometry");
   elsif B.Current in B.Early_Stop | B.Parking_Unknown | B.Wait_Failure then
      B.Check (Quarantined and then B.Decline_Reports = 1 and then B.Ready_Reports = 0 and then B.Role_Attempts = 0,
         "partial/uncertain startup declines once and retains process for supervision");
      B.Check (B.Register_Reads = B.Reads_At_Decline and then B.Register_Writes = B.Writes_At_Decline,
         "quarantine does not resume MMIO after the declined startup");
      B.Check (not Has ("bring-up complete") and then B.Sink_Closes = 0, "partial startup never logs completion or invents client cleanup");
      if B.Current = B.Parking_Unknown then B.Check (B.Register_Writes = 1, "unknown mask write attempted exactly once");
      elsif B.Current = B.Wait_Failure then B.Check (B.Yields > 0, "failed wait falls back to yield without replay"); end if;
   else
      B.Check (not Quarantined and then not B.Saved and then B.Sink_Closes = 0, "definite startup/readiness refusal exits without client ownership");
      if B.Current = B.Readiness_Refused then
         B.Check (B.Ready_Reports = 1 and then B.Decline_Reports = 0 and then B.Role_Attempts = 1,
            "failed readiness ACK never replays as another ready or decline");
      else
         B.Check (B.Ready_Reports = 0 and then B.Decline_Reports = 1, "startup failure sends decline only");
      end if;
      if B.Current in B.Invalid_Launcher | B.Wrong_Launcher_Kind | B.Unreadable_Launcher | B.Zero_Launcher_ID then
         B.Check (B.Metadata_Inspections = 0 and then B.Register_Writes = 0, "invalid launcher/clock fails before mapping or MMIO");
      end if;
      B.Check (not Has ("bring-up complete"), "refusal summary cannot claim success");
   end if;
   if B.Current in B.Missing_Community | B.Missing_Configuration | B.Wrong_Controller | B.Invalid_Launch_Path | B.Wrong_Window_Count | B.Late_Window_Mismatch | B.Late_Read_Only | B.Overlapping_Community | B.Unaligned_Community | B.Zero_Launch_Epoch then
      B.Check(B.Binds=0 and B.Probes=0 and B.Register_Reads=0 and B.Register_Writes=0,
         "invalid launch/late authority refuses before ANY map, probe or register access");
      B.Check(B.Role_Attempts=0 and B.Decline_Reports=1 and B.Ready_Reports=0,
         "invalid complete binding never registers or announces ready");
   end if;
   for Pin in G.ACPI_Pin range 0 .. 359 loop
      declare L : constant G.Location := G.Locate (Pin); begin
         if L.Found then
            B.Check ((B.Memory (L.Resource) (Natural (L.Enable_Register) / 4) and 16#A500_0000#) = 16#A500_0000# and then
               (B.Memory (L.Resource) (Natural (L.Status_Register) / 4) and 16#A500_0000#) = 16#A500_0000#,
               "actual Main startup/client lifecycle preserves firmware-owned register bits");
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("GPIO actual Main hosted checks" & B.Checks'Image);
end Main_Tests;
