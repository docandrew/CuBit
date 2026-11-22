pragma Ada_2022;
package body ACPI_Backend_PCI_Reads with SPARK_Mode is
   use type Word, Catalog.Revision_Number, Catalog.W.P.Address_Space, P.Outcome, Catalog.B.Tick;
   function Status (State : Context) return Phase is (State.Stage);
   function Live (State : Context) return Boolean is
      Header : constant Catalog.Inventory := Catalog.Header (Hardware, State.Scope);
   begin
      return Header.Available and then Header.Revision = State.Revision;
   end Live;
   function Live_Root (State : Context) return Boolean is
      Window : constant Catalog.W.Window_Result := Catalog.Describe_Window (Hardware, State.Window);
   begin
      return Window.Allowed and then Catalog.W.Valid (Window.Value)
        and then Window.Value.Space = Catalog.W.P.Memory_Space
        and then Window.Value.Readable and then not Window.Value.Writable
        and then not Window.Value.Delegable and then Window.Value.Base = State.Base
        and then Window.Value.Length = P.Bus_Bytes and then Is_PCI (State.Window);
   end Live_Root;
   procedure Configure
     (State : in out Context; Scope : Word; ID : Record_ID;
      Window : Catalog.W.Window_ID; Result : out Outcome) is
      Header : Catalog.Inventory;
      Selected : Catalog.W.Window_Result;
   begin
      if State.Working then Result := Reentered; return; end if;
      if State.Stage /= Fresh then Result := Already_Consumed; return; end if;
      State.Stage := Failed;
      if Scope = 0 then Result := Denied; return; end if;
      if ID = No_Record then Result := Invalid_Record; return; end if;
      Header := Catalog.Header (Hardware, Scope);
      if not Header.Available or else Header.Revision = Catalog.No_Revision then
         Result := Not_Ready; return;
      end if;
      State.Working := True;
      Selected := Catalog.Describe_Window (Hardware, Window);
      if not Selected.Allowed or else not Catalog.W.Valid (Selected.Value)
        or else Selected.Value.Space /= Catalog.W.P.Memory_Space
        or else not Selected.Value.Readable or else Selected.Value.Writable
        or else Selected.Value.Delegable or else Selected.Value.Length /= P.Bus_Bytes
        or else not Is_PCI (Window)
      then Result := Invalid_Record;
      else
         State.Scope := Scope; State.ID := ID; State.Revision := Header.Revision;
         State.Window := Window; State.Base := Selected.Value.Base;
         State.Stage := Serving; Result := Configured;
      end if;
      State.Working := False;
   end Configure;
   procedure Admit (State : in out Context; Scope : Word; Result : out Outcome) is
   begin
      if State.Working then Result := Reentered;
      elsif State.Stage /= Serving then Result := Not_Ready;
      elsif Scope = 0 or else Scope /= State.Scope then Result := Denied;
      elsif not Live (State) then State.Stage := Failed; Result := Catalog_Changed;
      else Result := Ready;
      end if;
   end Admit;
   procedure Header (State : in out Context; Scope : Word; Result : out Header_Report) is
      Code : Outcome;
   begin
      Result := (others => <>); Admit (State, Scope, Code); Result.Status := Code;
      if Code /= Ready then return; end if;
      State.Working := True;
      if not Live_Root (State) then State.Stage := Failed; Result.Status := Backing_Unavailable;
      else Result.Epoch := Epoch_Number (State.Revision); Result.ID := State.ID; Result.First := State.Base;
      end if;
      State.Working := False;
   end Header;
   procedure Read
     (State : in out Context; Scope : Word; Epoch : Epoch_Number; ID : Record_ID;
      Region : P.Region; Offset : Word; Width : P.T.Access_Width;
      Value : out Word; Result : out Outcome; Reason : out P.Outcome;
      Deadline : Catalog.B.Tick; For_Write : Boolean := False) is
      Planned : P.View;
      View : Catalog.Bindings.Access_View;
      Completed, Clock_OK : Boolean;
      Started, Current : Catalog.B.Tick;
   begin
      Value := 0; Reason := P.Invalid_Window;
      Admit (State, Scope, Result); if Result /= Ready then return; end if;
      if Epoch /= Epoch_Number (State.Revision) then Result := Stale; return; end if;
      if ID = No_Record or else ID /= State.ID then Result := Denied; return; end if;
      Planned := P.Plan (Region, Offset, Width, P.Bus_Bytes, For_Write);
      Reason := Planned.Status;
      if Planned.Status /= P.Read_Ready then Result := Policy_Refused; return; end if;
      State.Working := True;
      Read_Clock (Started, Clock_OK);
      if not Clock_OK then State.Working := False; Result := Clock_Unavailable; return; end if;
      if Deadline = Catalog.B.No_Finite_Deadline or else Started >= Deadline then
         State.Working := False; Result := Deadline_Expired; return;
      end if;
      if not Live_Root (State) then
         State.Stage := Failed; State.Working := False; Result := Backing_Unavailable; return;
      end if;
      View := Catalog.Select_Access (Hardware, State.Window, Catalog.W.P.Memory_Space,
        State.Base + Word (Planned.First), Width, False);
      if not View.Allowed or else View.Space /= Catalog.W.P.Memory_Space then
         State.Stage := Failed; State.Working := False; Result := Backing_Unavailable; return;
      end if;
      Read_Clock (Current, Clock_OK);
      if not Clock_OK then State.Working := False; Result := Clock_Unavailable; return; end if;
      if Current < Started then State.Working := False; Result := Clock_Reversed; return; end if;
      if Current >= Deadline then State.Working := False; Result := Deadline_Expired; return; end if;
      Transfer (View.Virtual_Address, Width, Value, Completed);
      if Completed and then Value <= Catalog.W.Width_Mask (Width) then Result := Done;
      else Value := 0; State.Stage := Failed; Result := Read_Failed;
      end if;
      State.Working := False;
   end Read;
end ACPI_Backend_PCI_Reads;
