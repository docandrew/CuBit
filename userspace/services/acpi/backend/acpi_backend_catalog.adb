pragma Ada_2022;
with Hardware_Authority;
with ACPI_Region_IO;
package body ACPI_Backend_Catalog is
   procedure Describe_Memory (Authority : Memory_Handle; Item : out B.Description) is
   begin Inspect_Memory (Authority, Item); end Describe_Memory;
   procedure Describe_Ports (Authority : Port_Handle; Item : out B.Description) is
   begin Inspect_Ports (Authority, Item); end Describe_Ports;
   use type W.Window_ID, W.Outcome, B.Outcome, P.Address_Space, P.Access_Width, W.Write_Safety;
   function Has_Scope (State : Context; Stamp : Unsigned_64) return Boolean is
   begin
      if Stamp = 0 then return False; end if;
      for I in Scope_Index loop
         if I <= State.Scope_Last and then State.Scopes (I) = Stamp then return True; end if;
      end loop;
      return False;
   end Has_Scope;
   procedure Bind_Memory
     (State : in out Context; Window : W.Granted_Window;
      Authority : Memory_Authority; Deadline : B.Tick; Result : out B.Report) is
   begin
      Result := (others => <>);
      if State.Working then Result.Status := B.Reentered; return; end if;
      if State.Sealed then return; end if;
      State.Working := True;
      Bindings.Bind_Memory (State.Windows, Window, Authority, Deadline, Result);
      State.Working := False;
   end Bind_Memory;
   procedure Bind_Ports
     (State : in out Context; Window : W.Granted_Window;
      Authority : Port_Authority; Deadline : B.Tick; Result : out B.Report) is
   begin
      Result := (others => <>);
      if State.Working then Result.Status := B.Reentered; return; end if;
      if State.Sealed then return; end if;
      State.Working := True;
      Bindings.Bind_Ports (State.Windows, Window, Authority, Deadline, Result);
      State.Working := False;
   end Bind_Ports;
   procedure Authorize_Scope (State : in out Context; Stamp : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.Working or else State.Sealed or else Stamp = 0
        or else Has_Scope (State, Stamp) or else State.Scope_Last = Scope_Count'Last then return; end if;
      State.Scope_Last := State.Scope_Last + 1; State.Scopes (State.Scope_Last) := Stamp;
      Accepted := True;
   end Authorize_Scope;
   function Same_Resource (Parent, Child : P.Configuration) return Boolean is
     (Parent.Space = Child.Space and then Parent.Base = Child.Base and then Parent.Length = Child.Length
      and then Parent.Register_Offset = Child.Register_Offset
      and then Parent.Register_Width = Child.Register_Width
      and then (not Child.Readable or Parent.Readable)
      and then (not Child.Writable or Parent.Writable)
      and then (for all Width in P.Access_Width => not Child.Widths (Width) or Parent.Widths (Width)));
   procedure Add
     (State : in out Context; Window : W.Window_ID;
      Item : P.Configuration; Writes : W.Write_Safety; Result : out Add_Report)
   is
      View : constant W.Window_Result := Bindings.Describe (State.Windows, Window);
      Root : Slot_Count := 0;
      Next : Slot_Index;
   begin
      Result := (others => <>);
      if State.Working or else State.Sealed then Result.Status := Setup_Closed; return; end if;
      if not Has_Scope (State, Item.Tag) then Result.Status := Scope_Unavailable; return; end if;
      if not View.Allowed then Result.Status := Window_Unavailable; return; end if;
      for I in Slot_Index loop
         if I <= State.Count then
            declare Prior : constant P.Configuration := P.Policy (State.Items (I).Region); begin
               if Prior.Register_Authority.Resource_ID = Item.Register_Authority.Resource_ID then
                  if Prior.Tag = Item.Tag then Result.Status := Duplicate_Register; return; end if;
                  if Root = 0 then Root := State.Items (I).Root; end if;
               end if;
            end;
         end if;
      end loop;
      if Root /= 0 then
         if not P.Active (State.Items (Root).Region) or else P.Busy (State.Items (Root).Region) then
            Result.Status := Derivation_Refused; return;
         end if;
         declare Parent : constant P.Configuration := P.Policy (State.Items (Root).Region); begin
            if State.Items (Root).Window /= Window or else not Same_Resource (Parent, Item) then
               Result.Status := Conflicting_Resource; return;
            end if;
            if not Hardware_Authority.Can_Derive (Parent.Register_Authority, Item.Register_Authority) then
               Result.Status := Derivation_Refused; return;
            end if;
         end;
      end if;
      if State.Count = Slot_Count'Last then Result.Status := Catalog_Full; return; end if;
      Next := State.Count + 1;
      W.Install (State.Items (Next).Region, View.Value, Item, Writes, Result.Admission);
      if Result.Admission /= W.Admitted then return; end if;
      State.Items (Next).Window := Window;
      State.Items (Next).Root := (if Root = 0 then Next else Root);
      State.Items (Next).Writes := Writes;
      State.Count := Next; Result.Status := Added;
   end Add;
   procedure Seal (State : in out Context; Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.Working or else State.Sealed then return; end if;
      State.Sealed := True; State.Revision := 1; Accepted := True;
   end Seal;
   procedure Quarantine (State : in out Context) is
   begin
      State.Quarantined := True;
      for I in Slot_Index loop
         if I <= State.Count then P.Revoke (State.Items (I).Region); end if;
      end loop;
   end Quarantine;
   procedure Changed (State : in out Context) is
   begin
      if State.Sealed then
         if State.Revision = Revision_Number'Last then Quarantine (State);
         else State.Revision := State.Revision + 1; end if;
      end if;
   end Changed;
   function Live (State : Context; I : Slot_Index) return Boolean is
     (I <= State.Count and then State.Items (I).Root /= 0
      and then P.Active (State.Items (I).Region)
      and then P.Active (State.Items (State.Items (I).Root).Region)
      and then Bindings.Describe (State.Windows, State.Items (I).Window).Allowed);
   procedure Dispatch
     (State : in out Context; Stamp : Unsigned_64; Item : Wire.Packet; Reply : out Wire.Packet)
   is
      I : Slot_Count := 0;
      Ticket : Unsigned_64;
      procedure Transact
        (Window : in out W.Window_ID; Space : P.Address_Space; Address : Unsigned_64;
         Width : P.Access_Width; For_Write : Boolean; Input : Unsigned_64;
         Output : out Unsigned_64; Completed : out Boolean)
      is
         View : constant Bindings.Access_View := Bindings.Select_Access
           (State.Windows, Window, Space, Address, Width, For_Write);
      begin
         Output := 0; Completed := False;
         if not View.Allowed then return; end if;
         case View.Space is
            when P.Memory_Space => Transfer_Memory (View.Virtual_Address, Width, For_Write, Input, Output, Completed);
            when P.IO_Space => Transfer_Ports (View.Ports, View.Port, Width, For_Write, Input, Output, Completed);
         end case;
      end Transact;
      package IO is new ACPI_Region_IO (W.Window_ID, Transact);
      Window : W.Window_ID;
   begin
      Reply := (Label => Wire.Operation_Reply, Data => [Wire.Outcome'Pos (Wire.Denied), 0, 0, 0], others => <>);
      if State.Working or else not State.Sealed or else State.Quarantined
        or else not Has_Scope (State, Stamp) then return; end if;
      if Item.Length /= 4 or else Item.Flags /= 0 or else Item.Reserved /= 0
        or else Item.Label not in Wire.Read_Operation | Wire.Write_Operation
        or else Item.Data (3) /= 0
        or else (Item.Label = Wire.Read_Operation and then Item.Data (2) /= 0)
      then Reply.Data (0) := Wire.Outcome'Pos (Wire.Malformed); return; end if;
      for J in Slot_Index loop
         if J <= State.Count then
            declare C : constant P.Configuration := P.Policy (State.Items (J).Region); begin
               if C.Tag = Stamp and then C.Register_Authority.Resource_ID = Item.Data (1) then I := J; exit; end if;
            end;
         end if;
      end loop;
      if I = 0 or else not Live (State, I) then return; end if;
      State.Working := True; Window := State.Items (I).Window;
      IO.Dispatch (State.Items (I).Region, Window, Stamp, Item, Reply, Ticket);
      pragma Assert (Ticket = 0 or else (P.Busy (State.Items (I).Region)
        and then Ticket = P.Receipt (State.Items (I).Region)));
      if Reply.Data (0) in Wire.Outcome'Pos (Wire.Indeterminate) | Wire.Outcome'Pos (Wire.Backend_Fault) then
         Quarantine (State); Changed (State);
      end if;
      State.Working := False;
   end Dispatch;
   function Header (State : Context; Stamp : Unsigned_64) return Inventory is
      Count : Natural := 0;
   begin
      if State.Working or else not State.Sealed or else State.Quarantined or else not Has_Scope (State, Stamp)
      then return (Available => False); end if;
      for I in Slot_Index loop
         if I <= State.Count and then Live (State, I) and then P.Policy (State.Items (I).Region).Tag = Stamp
         then Count := Count + 1; end if;
      end loop;
      return (Available => True, Revision => State.Revision, Count => Count);
   end Header;
   function Describe
     (State : Context; Stamp : Unsigned_64; Revision : Revision_Number; Ordinal : Natural) return Description
   is
      Info : constant Inventory := Header (State, Stamp);
      Count : Natural := 0;
   begin
      if not Info.Available or else Info.Revision /= Revision or else Ordinal = 0
        or else Ordinal > Info.Count then return (Available => False); end if;
      for I in Slot_Index loop
         if I <= State.Count and then Live (State, I) and then P.Policy (State.Items (I).Region).Tag = Stamp then
            Count := Count + 1;
            if Count = Ordinal then return (Available => True, Item => P.Policy (State.Items (I).Region),
               Epoch => P.Epoch (State.Items (I).Region), Exact_Write_Safe => State.Items (I).Writes = W.Exact_Writes_Admitted);
            end if;
         end if;
      end loop;
      return (Available => False);
   end Describe;
   procedure Disable_Window (State : in out Context; ID : W.Window_ID; Accepted : out Boolean) is
      Was_Ready : constant Boolean := Bindings.Describe (State.Windows, ID).Allowed;
   begin
      Accepted := False; if State.Working then return; end if;
      Bindings.Disable (State.Windows, ID, Accepted);
      for I in Slot_Index loop
         if I <= State.Count and then State.Items (I).Window = ID then P.Revoke (State.Items (I).Region); end if;
      end loop;
      if Was_Ready then Changed (State); end if;
   end Disable_Window;
   procedure Revoke_Resource (State : in out Context; ID : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False; if State.Working then return; end if;
      for I in Slot_Index loop
         if I <= State.Count and then P.Policy (State.Items (I).Region).Register_Authority.Resource_ID = ID then
            Accepted := True; P.Revoke (State.Items (I).Region);
         end if;
      end loop;
      if Accepted then Changed (State); end if;
   end Revoke_Resource;
   procedure Revoke_Scope (State : in out Context; Stamp : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False; if State.Working then return; end if;
      for I in Slot_Index loop
         if I <= State.Count and then P.Policy (State.Items (I).Region).Tag = Stamp then
            Accepted := True;
            if State.Items (I).Root = I then
               for J in Slot_Index loop
                  if J <= State.Count and then State.Items (J).Root = I then P.Revoke (State.Items (J).Region); end if;
               end loop;
            else P.Revoke (State.Items (I).Region); end if;
         end if;
      end loop;
      if Accepted then Changed (State); end if;
   end Revoke_Scope;
   function Observe (State : Context) return Statistics is
      Item : Statistics := (State.Sealed, State.Working, State.Quarantined, Natural (State.Count), 0, 0,
        Bindings.Observe (State.Windows).Stored);
   begin
      for I in Slot_Index loop
         if I <= State.Count then
            if P.Active (State.Items (I).Region) then Item.Active_Registers := Item.Active_Registers + 1; end if;
            if P.Busy (State.Items (I).Region) then Item.Pending := Item.Pending + 1; end if;
         end if;
      end loop;
      return Item;
   end Observe;
   function Describe_Window (State : Context; ID : W.Window_ID) return W.Window_Result is
     (Bindings.Describe (State.Windows, ID));
   function Select_Access
     (State : Context; ID : W.Window_ID; Space : P.Address_Space;
      Address : Unsigned_64; Width : W.T.Access_Width; For_Write : Boolean)
      return Bindings.Access_View is
     (Bindings.Select_Access (State.Windows, ID, Space, Address, Width, For_Write));
end ACPI_Backend_Catalog;
