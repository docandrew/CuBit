pragma Ada_2022;
with System.Storage_Elements;
package body GPIO_Community_Window is
   use type P.Address;
   use type P.Outcome;
   use type G.Outcome;
   use type G.Memory_Resource_Ordinal;
   use type G.Plan_Result;
   use type I.Word;
   Attempted, Mapped, Probe_Attempted, Header_Ready : Boolean := False;
   Base, Extent : P.Address := 0;
   Resource_ID : G.Memory_Resource_Ordinal := 0;
   Revision_Value, PADBAR_Value : I.Word := 0;
   Known : array (G.Pad_Number) of Boolean := [others => False];
   Plans : array (G.Pad_Number) of G.Plan_Result;
   function Ready return Boolean is (Mapped and Header_Ready);
   function Load (Offset : G.Register_Offset) return I.Word is
      Cell : I.Word with Import, Volatile_Full_Access,
         Address => System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address (Base + P.Address (Offset)));
   begin return Cell; end Load;
   procedure Store (Offset : G.Register_Offset; Value : I.Word) is
      Cell : I.Word with Import, Volatile_Full_Access,
         Address => System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address (Base + P.Address (Offset)));
   begin Cell := Value; end Store;
   procedure Bind
     (Authority : Authority_Type; Resource : G.Memory_Resource_Ordinal;
      Physical, Bytes, Virtual, Page_Limit : P.Address; Result : out T.Map_Report) is
      Metadata : P.Description;
      OK : Boolean;
   begin
      Result := (others => <>);
      if Attempted then Result.Status := T.Already_Attempted; return; end if;
      Attempted := True; Inspect (Authority, Metadata);
      Result.Policy := P.Admit (Metadata, Physical, Bytes, Virtual, Page_Limit);
      if Result.Policy /= P.Allowed then return; end if;
      Map (Authority, Physical, Virtual, Bytes / P.Page_Bytes, OK);
      if not OK then Result.Status := T.Map_Refused; return; end if;
      Base := Virtual; Extent := Bytes; Resource_ID := Resource;
      Mapped := True; Result.Status := T.Bound;
   end Bind;
   procedure Probe (Controller_HID : String; Result : out T.Header_Report) is
      Representative : constant array (G.Memory_Resource_Ordinal) of G.ACPI_Pin := [0, 96, 256, 352];
      Plan : G.Plan_Result;
   begin
      Result := (others => <>);
      if not Mapped then return; end if;
      if Probe_Attempted then Result.Status := T.Already_Probed; return; end if;
      Probe_Attempted := True;
      if Controller_HID /= G.Target_HID then Result.Status := T.Wrong_Controller; return; end if;
      Revision_Value := Load (G.Revision_Register); PADBAR_Value := Load (G.PADBAR_Register);
      Result.Revision := Revision_Value; Result.PADBAR := PADBAR_Value;
      Plan := G.Plan (Controller_HID, Representative (Resource_ID), Revision_Value, PADBAR_Value, G.Span (Extent));
      Result.Geometry := Plan.Status;
      if Plan.Status /= G.Ready then Result.Status := T.Geometry_Refused; return; end if;
      Header_Ready := True; Result.Status := T.Probed;
   end Probe;
   procedure Admit_Pin (Pin : G.ACPI_Pin; Result : out T.Pin_Admission; Plan : out G.Plan_Result) is
      Where : constant G.Location := G.Locate (Pin);
   begin
      Plan := (Status => G.No_GPIO_Pin); Result := T.Header_Unavailable;
      if not Ready then return; end if;
      if not Where.Found then Result := T.Unsupported_Pin; return; end if;
      if Where.Resource /= Resource_ID then Result := T.Wrong_Resource; return; end if;
      if Known (Where.Pad) then Plan := Plans (Where.Pad); Result := T.Already_Admitted; return; end if;
      Plan := G.Plan (G.Target_HID, Pin, Revision_Value, PADBAR_Value, G.Span (Extent));
      if Plan.Status /= G.Ready then Result := T.Pin_Geometry_Refused; return; end if;
      Plans (Where.Pad) := Plan; Known (Where.Pad) := True; Result := T.Admitted;
   end Admit_Pin;
   function Allowed (Plan : G.Plan_Result) return Boolean is
     (Ready and then Plan.Status = G.Ready and then Known (Plan.Selected.Pad)
      and then Plan = Plans (Plan.Selected.Pad));
   procedure Read
     (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status) is
   begin
      Value := 0; Status := I.Unavailable;
      if not Ready then return; end if;
      if not Allowed (Plan) then Status := I.Denied; return; end if;
      Value := Load (I.Read_Offset (Plan, ID)); Status := I.Access_OK;
   end Read;
   procedure Write
     (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
      Offset : G.Register_Offset;
      Current, Updated : I.Word;
      RX_State : constant I.Word := 2;
   begin
      Status := I.Unavailable; if not Ready then return; end if;
      Status := I.Denied; if not Allowed (Plan) then return; end if;
      Offset := I.Write_Offset (Plan, ID);
      case ID is
         when I.Clear_Status =>
            if Value /= Plan.Selected.Interrupt_Mask then return; end if;
            Updated := Value;
         when I.Write_Enable =>
            Current := Load (Offset);
            if ((Current xor Value) and not Plan.Selected.Interrupt_Mask) /= 0 then return; end if;
            Updated := Value;
         when I.Write_Configuration =>
            Current := Load (Offset);
            if ((Current xor Value) and not (I.Changed_Fields or RX_State)) /= 0 then return; end if;
            Updated := (Current and not I.Changed_Fields) or (Value and I.Changed_Fields);
      end case;
      Store (Offset, Updated); Status := I.Access_OK;
   end Write;
end GPIO_Community_Window;
