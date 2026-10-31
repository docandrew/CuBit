pragma Ada_2022;
with Main_Model;
package body GPIO_Native_Communities is
   package B renames Main_Model;
   use type P.Address, I.Word, I.Access_Status, G.Outcome, G.Plan_Result, G.Memory_Resource_Ordinal, G.Pad_Number, B.Scenario;
   Mapped, Probed : array (G.Memory_Resource_Ordinal) of Boolean := [others => False];
   Frozen : Boolean := False;
   Cache : array (G.Pad_Number) of G.Plan_Result;
   function Ready (Resource : G.Memory_Resource_Ordinal) return Boolean is (Frozen and then Probed (Resource));
   procedure Bind (Resource : G.Memory_Resource_Ordinal; Authority : CuBit.Handles.Device_Memory;
      Physical, Bytes, Virtual, Page_Limit : P.Address; Deadline : I.Tick; Result : out T.Map_Report) is
      pragma Unreferenced (Page_Limit, Deadline);
      use type CuBit.Handles.Device_Memory;
   begin
      B.Check (not Frozen and then Authority = CuBit.Handles.Device_Memory (Natural (Resource) + 1) and then
         Physical = 16#1000_0000# + P.Address (Authority) * 8192 and then Bytes = 8192 and then Virtual mod P.Page_Bytes = 0,
         "actual resource owner uses exact named metadata spans");
      B.Binds := B.Binds + 1;
      Mapped (Resource) := not (B.Current = B.Map_Refused and then Resource = 0);
      Result := (if Mapped (Resource) then (T.Bound, P.Allowed) else (T.Map_Refused, P.Allowed));
   end Bind;
   procedure Probe (Resource : G.Memory_Resource_Ordinal; Controller_HID : String;
      Deadline : I.Tick; Result : out T.Header_Report) is
      pragma Unreferenced (Deadline);
   begin
      B.Check (Mapped (Resource) and then Controller_HID = G.Target_HID and then not Frozen, "probe same mapped profile");
      B.Probes := B.Probes + 1; Probed (Resource) := True;
      Result := (T.Probed, 16#0094_0000#, 16#1000#, G.Ready);
   end Probe;
   procedure Freeze (Ready_Resources : out Resource_Count) is
   begin
      Frozen := True; Ready_Resources := 0;
      for Resource in G.Memory_Resource_Ordinal loop if Ready (Resource) then Ready_Resources := Ready_Resources + 1; end if; end loop;
   end Freeze;
   procedure Admit_Pin (Pin : G.ACPI_Pin; Result : out T.Pin_Admission; Plan : out G.Plan_Result) is
      L : constant G.Location := G.Locate (Pin);
   begin
      Plan := (Status => G.No_GPIO_Pin); Result := T.Unsupported_Pin;
      if not L.Found then return; end if;
      if not Ready (L.Resource) then Result := T.Header_Unavailable; return; end if;
      Plan := G.Plan (G.Target_HID, Pin, 16#0094_0000#, 16#1000#, 8192);
      Result := (if Cache (L.Pad).Status = G.Ready then T.Already_Admitted else T.Admitted);
      Cache (L.Pad) := Plan;
   end Admit_Pin;
   procedure Read (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status) is
   begin
      B.Check (Plan.Status = G.Ready and then Ready (Plan.Selected.Resource) and then Cache (Plan.Selected.Pad) = Plan,
         "only exact cached admitted plans read modeled registers");
      B.Register_Reads := B.Register_Reads + 1;
      Value := B.Memory (Plan.Selected.Resource) (Natural (I.Read_Offset (Plan, ID)) / 4); Status := I.Access_OK;
   end Read;
   procedure Write (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
      Offset : constant Natural := Natural (I.Write_Offset (Plan, ID)) / 4;
      Old : constant I.Word := B.Memory (Plan.Selected.Resource) (Offset);
   begin
      B.Check (Plan.Status = G.Ready and then Ready (Plan.Selected.Resource) and then Cache (Plan.Selected.Pad) = Plan,
         "only exact cached admitted plans write modeled registers");
      B.Check (G.Interrupt_Eligible (Plan.Selected,
         B.Memory (Plan.Selected.Resource) (Natural (Plan.Selected.Owner_Register) / 4),
         B.Memory (Plan.Selected.Resource) (Natural (Plan.Selected.Host_Register) / 4), 0),
         "all Main writes target observed host-owned pads");
      B.Register_Writes := B.Register_Writes + 1;
      case ID is
         when I.Clear_Status =>
            B.Check (Value = Plan.Selected.Interrupt_Mask, "W1C exact one-bit mask");
            B.Memory (Plan.Selected.Resource) (Offset) := Old and not Value;
         when I.Write_Enable =>
            B.Check (((Old xor Value) and not Plan.Selected.Interrupt_Mask) = 0, "neighbor enable bits preserved");
            B.Memory (Plan.Selected.Resource) (Offset) := Value;
         when I.Write_Configuration =>
            B.Check (B.Ready_ACK and then Plan.Selected.Pad = G.Locate (160).Pad,
               "pad configuration only after startup Ready and runtime admission");
            B.Memory (Plan.Selected.Resource) (Offset) := Value;
      end case;
      Status := (if B.Current = B.Parking_Unknown and then B.Register_Writes = 1 then I.Unknown_Effect else I.Access_OK);
   end Write;
end GPIO_Native_Communities;
