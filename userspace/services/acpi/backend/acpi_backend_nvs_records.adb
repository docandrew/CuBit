pragma Ada_2022;
with ACPI_Inventory_Protocol;
package body ACPI_Backend_NVS_Records with SPARK_Mode is
   use type Word, Catalog.Revision_Number, Catalog.W.P.Address_Space;
   function Observe (State : Context) return Statistics is ((State.Stage, State.Count, State.Working));
   function Live (State : Context) return Boolean is
      Current : constant Catalog.Inventory := Catalog.Header (Hardware, State.Scope);
   begin
      return Current.Available and then Current.Revision = State.Revision;
   end Live;
   function Live_Row (State : Context; I : Row_Index) return Boolean is
      Item : constant Row := State.Items (I);
      Window : constant Catalog.W.Window_Result := Catalog.Describe_Window (Hardware, Item.Window);
   begin
      return Window.Allowed and then Window.Value.Space = Catalog.W.P.Memory_Space
        and then Window.Value.Readable and then Window.Value.Base = Item.Base
        and then Window.Value.Length = Item.Length and then Is_NVS (Item.Window);
   end Live_Row;
   procedure Configure (State : in out Context; Scope : Word; Result : out Outcome) is
      Current : Catalog.Inventory;
   begin
      if State.Working then Result := Reentered; return; end if;
      if State.Stage /= Fresh then Result := Already_Consumed; return; end if;
      State.Stage := Failed;
      if Scope = 0 then Result := Denied; return; end if;
      Current := Catalog.Header (Hardware, Scope);
      if not Current.Available then Result := Not_Ready; return; end if;
      State.Scope := Scope; State.Revision := Current.Revision;
      State.Stage := Building; Result := Configured;
   end Configure;
   procedure Add
     (State : in out Context; ID : Record_ID; Window : Catalog.W.Window_ID; Result : out Outcome) is
      Selected : Catalog.W.Window_Result;
   begin
      if State.Working then Result := Reentered; return; end if;
      if State.Stage /= Building then Result := Not_Ready; return; end if;
      if not Live (State) then State.Stage := Failed; Result := Catalog_Changed; return; end if;
      if ID = No_Record then State.Stage := Failed; Result := Invalid_Record; return; end if;
      for I in 1 .. State.Count loop
         if State.Items (I).ID = ID then State.Stage := Failed; Result := Duplicate_Record; return; end if;
      end loop;
      if State.Count = Maximum_Records then State.Stage := Failed; Result := Capacity_Exceeded; return; end if;
      State.Working := True;
      Selected := Catalog.Describe_Window (Hardware, Window);
      if not Selected.Allowed or else Selected.Value.Space /= Catalog.W.P.Memory_Space
        or else not Selected.Value.Readable or else not Is_NVS (Window)
      then State.Stage := Failed; Result := Invalid_Record;
      else
         State.Count := State.Count + 1;
         State.Items (State.Count) := (ID, Window, Selected.Value.Base, Selected.Value.Length);
         Result := Added;
      end if;
      State.Working := False;
   end Add;
   procedure Seal (State : in out Context; Result : out Outcome) is
   begin
      if State.Working then Result := Reentered; return; end if;
      if State.Stage /= Building then Result := Not_Ready; return; end if;
      if not Live (State) then State.Stage := Failed; Result := Catalog_Changed; return; end if;
      State.Working := True;
      for I in 1 .. State.Count loop
         if not Live_Row (State, I) then
            State.Stage := Failed; State.Working := False; Result := Backing_Unavailable; return;
         end if;
      end loop;
      State.Stage := Serving; State.Working := False; Result := Sealed;
   end Seal;
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
      Status : Outcome;
   begin
      Result := (others => <>); Admit (State, Scope, Status); Result.Status := Status;
      if Status = Ready then Result.Epoch := Epoch_Number (State.Revision); Result.Count := State.Count; end if;
   end Header;
   procedure Describe
     (State : in out Context; Scope : Word; Epoch : Epoch_Number;
      Ordinal : Natural; Result : out Description_Report) is
      Status : Outcome;
   begin
      Result := (others => <>); Admit (State, Scope, Status); Result.Status := Status;
      if Status /= Ready then return; end if;
      if Epoch /= Epoch_Number (State.Revision) then Result.Status := Stale; return; end if;
      if Ordinal not in 1 .. State.Count then Result.Status := Denied; return; end if;
      State.Working := True;
      if not Live_Row (State, Ordinal) then State.Stage := Failed; Result.Status := Backing_Unavailable;
      else
         Result.ID := State.Items (Ordinal).ID;
         Result.Base := State.Items (Ordinal).Base; Result.Length := State.Items (Ordinal).Length;
      end if;
      State.Working := False;
   end Describe;
   procedure Read
     (State : in out Context; Scope : Word; Epoch : Epoch_Number;
      ID : Record_ID; Index : Word; Width : T.Access_Width; Value : out Word; Result : out Outcome) is
      View : Catalog.Bindings.Access_View;
      Completed : Boolean;
      Bytes : constant Word := Word (T.Octets (Width));
   begin
      Value := 0; Admit (State, Scope, Result); if Result /= Ready then return; end if;
      if Epoch /= Epoch_Number (State.Revision) then Result := Stale; return; end if;
      for I in 1 .. State.Count loop
         if State.Items (I).ID = ID then
            if Index >= State.Items (I).Length or else Index mod Bytes /= 0
              or else Bytes > State.Items (I).Length - Index
            then Result := Denied; return; end if;
            State.Working := True;
            if not Live_Row (State, I) then
               State.Stage := Failed; State.Working := False; Result := Backing_Unavailable; return;
            end if;
            View := Catalog.Select_Access (Hardware, State.Items (I).Window, Catalog.W.P.Memory_Space,
              State.Items (I).Base + Index, Width, False);
            if not View.Allowed or else View.Space /= Catalog.W.P.Memory_Space then
               State.Stage := Failed; State.Working := False; Result := Backing_Unavailable; return;
            end if;
            Transfer (View.Virtual_Address, Width, Value, Completed);
            if Completed and then Value <= ACPI_Inventory_Protocol.Width_Mask (Width) then Result := Done;
            else Value := 0; State.Stage := Failed; Result := Read_Failed;
            end if;
            State.Working := False; return;
         end if;
      end loop;
      Result := Denied;
   end Read;
end ACPI_Backend_NVS_Records;
