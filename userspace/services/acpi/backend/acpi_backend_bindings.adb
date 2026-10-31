pragma Ada_2022;
package body ACPI_Backend_Bindings is
   use type W.Window_ID, P.Address_Space, T.Access_Width, B.Tick, B.Outcome, B.Resource_Kind;
   function Find (State : Context; ID : W.Window_ID) return Slot_Count is
   begin
      for I in Slot_Index loop
         if I <= State.Count and then State.Items (I).Window.ID = ID then return I; end if;
      end loop;
      return 0;
   end Find;
   function Overlaps (State : Context; Window : W.Granted_Window) return Boolean is
   begin
      for I in Slot_Index loop
         if I <= State.Count and then State.Items (I).Window.Space = Window.Space then
            declare Old : W.Granted_Window renames State.Items (I).Window; begin
               if (if Window.Base <= Old.Base then Old.Base - Window.Base < Window.Length
                   else Window.Base - Old.Base < Old.Length)
               then return True; end if;
            end;
         end if;
      end loop;
      return False;
   end Overlaps;
   function Observe (State : Context) return Statistics is
      Item : Statistics := (Stored => Natural (State.Count), Ready => 0, Map_Attempts => 0,
         Working => State.Working, Next_Virtual => State.Cursor);
   begin
      for I in Slot_Index loop
         if I <= State.Count then
            if State.Items (I).At_Phase = Ready then Item.Ready := Item.Ready + 1; end if;
            if State.Items (I).Map_Attempted then Item.Map_Attempts := Item.Map_Attempts + 1; end if;
         end if;
      end loop;
      return Item;
   end Observe;
   procedure Sample
     (State : in out Context; Deadline : B.Tick; Status : out B.Outcome) is
      Now : B.Tick;
      Available : Boolean;
   begin
      Read_Clock (Now, Available);
      if not Available or else Now = B.No_Finite_Deadline then Status := B.Clock_Unavailable;
      elsif State.Seen_Clock and then Now < State.Previous then Status := B.Clock_Reversed;
      else
         State.Seen_Clock := True; State.Previous := Now;
         Status := (if Now >= Deadline then B.Deadline_Expired else B.Bound);
      end if;
   end Sample;
   procedure Prepare
     (State : in out Context; Window : W.Granted_Window; Deadline : B.Tick;
      Index : out Slot_Count; Result : out B.Report) is
   begin
      Index := 0; Result := (others => <>);
      if State.Working then Result.Status := B.Reentered; return; end if;
      if not W.Valid (Window) then return; end if;
      if Find (State, Window.ID) /= 0 then Result.Status := B.Duplicate_ID; return; end if;
      if Overlaps (State, Window) then Result.Status := B.Overlapping_Window; return; end if;
      if State.Count = Slot_Count'Last then Result.Status := B.Capacity_Exceeded; return; end if;
      if Deadline = B.No_Finite_Deadline then Result.Status := B.Deadline_Expired; return; end if;
      State.Working := True;
      Sample (State, Deadline, Result.Status);
      if Result.Status /= B.Bound then State.Working := False; return; end if;
      State.Count := State.Count + 1; Index := State.Count;
      State.Items (Index).Window := Window;
      State.Items (Index).At_Phase := Inspecting;
      Result.Stored := True;
   end Prepare;
   function Match (Window : W.Granted_Window; Item : B.Description) return B.Outcome is
   begin
      if not Item.Current then return B.Not_Current; end if;
      if Item.Kind /= (if Window.Space = P.Memory_Space then B.Memory_Resource else B.Port_Resource)
      then return B.Wrong_Kind; end if;
      if Item.Base /= Window.Base or else Item.Length /= Window.Length then return B.Range_Mismatch; end if;
      if (Window.Readable and then not Item.Readable) or else
        (Window.Writable and then not Item.Writable) or else
        (Window.Delegable and then not Item.Delegable)
      then return B.Rights_Mismatch; end if;
      return B.Bound;
   end Match;
   procedure Bind_Memory
     (State : in out Context; Window : W.Granted_Window;
      Authority : Memory_Authority; Deadline : B.Tick; Result : out B.Report) is
      I : Slot_Count;
      Item : B.Description;
      Mapped : Boolean;
      Declared_Window : constant W.Granted_Window := Window;
      Held_Authority : constant Memory_Authority := Authority;
   begin
      Result := (others => <>);
      if State.Working then Result.Status := B.Reentered; return; end if;
      if Held_Authority = No_Memory then Result.Status := B.Missing_Handle; return; end if;
      if Declared_Window.Space /= P.Memory_Space or else not W.Valid (Declared_Window) then return; end if;
      if Find (State, Declared_Window.ID) /= 0 then Result.Status := B.Duplicate_ID; return; end if;
      -- x86 mappings cannot represent write-only memory authority.
      if not Declared_Window.Readable then Result.Status := B.Unsupported_Memory_Rights; return; end if;
      if Virtual_First = 0 or else Virtual_First >= Virtual_Limit
        or else Virtual_Limit > B.User_Virtual_Limit
        or else Virtual_First mod W.Map_Page_Bytes /= 0
        or else Virtual_Limit mod W.Map_Page_Bytes /= 0
      then Result.Status := B.Invalid_Arena; return; end if;
      if Declared_Window.Length / W.Map_Page_Bytes > Unsigned_64 (Mapping_Page_Limit)
      then Result.Status := B.Mapping_Limit; return; end if;
      if State.Cursor >= Virtual_Limit or else Declared_Window.Length > Virtual_Limit - State.Cursor
      then Result.Status := B.Arena_Exhausted; return; end if;
      Prepare (State, Declared_Window, Deadline, I, Result);
      if I = 0 then return; end if;
      State.Items (I).Memory := Held_Authority;
      Inspect_Memory (Held_Authority, Item);
      Result.Status := Match (Declared_Window, Item);
      if Result.Status = B.Bound then Sample (State, Deadline, Result.Status); end if;
      if Result.Status /= B.Bound then
         State.Items (I).At_Phase := Refused; State.Working := False; return;
      end if;
      -- Reserve the whole VA extent BEFORE the possibly partial mapping.
      State.Items (I).Virtual := State.Cursor;
      State.Cursor := State.Cursor + Declared_Window.Length;
      State.Items (I).At_Phase := Mapping;
      State.Items (I).Map_Attempted := True; Result.Map_Attempted := True;
      Map_Memory (Held_Authority, Declared_Window.Base, State.Items (I).Virtual,
         Declared_Window.Length / W.Map_Page_Bytes, not Declared_Window.Writable, Mapped);
      if not Mapped then
         State.Items (I).At_Phase := Mapping_Unknown;
         Result.Status := B.Mapping_Refused;
      else
         Sample (State, Deadline, Result.Status);
         State.Items (I).At_Phase := (if Result.Status = B.Bound then Ready else Mapping_Unknown);
      end if;
      State.Working := False;
   end Bind_Memory;
   procedure Bind_Ports
     (State : in out Context; Window : W.Granted_Window;
      Authority : Port_Authority; Deadline : B.Tick; Result : out B.Report) is
      I : Slot_Count;
      Item : B.Description;
      Declared_Window : constant W.Granted_Window := Window;
      Held_Authority : constant Port_Authority := Authority;
   begin
      Result := (others => <>);
      if State.Working then Result.Status := B.Reentered; return; end if;
      if Held_Authority = No_Ports then Result.Status := B.Missing_Handle; return; end if;
      if Declared_Window.Space /= P.IO_Space then return; end if;
      Prepare (State, Declared_Window, Deadline, I, Result);
      if I = 0 then return; end if;
      State.Items (I).Ports := Held_Authority;
      Inspect_Ports (Held_Authority, Item);
      Result.Status := Match (Declared_Window, Item);
      if Result.Status = B.Bound then Sample (State, Deadline, Result.Status); end if;
      State.Items (I).At_Phase := (if Result.Status = B.Bound then Ready else Refused);
      State.Working := False;
   end Bind_Ports;
   procedure Disable (State : in out Context; ID : W.Window_ID; Accepted : out Boolean) is
      I : constant Slot_Count := Find (State, ID);
   begin
      Accepted := False;
      if State.Working or else I = 0 then return; end if;
      if State.Items (I).At_Phase = Ready then State.Items (I).At_Phase := Disabled; end if;
      Accepted := True;
   end Disable;
   function Describe (State : Context; ID : W.Window_ID) return W.Window_Result is
      I : constant Slot_Count := Find (State, ID);
   begin
      if State.Working or else I = 0 or else State.Items (I).At_Phase /= Ready
      then return (Allowed => False); end if;
      return (Allowed => True, Value => State.Items (I).Window);
   end Describe;
   function Select_Access
     (State : Context; ID : W.Window_ID; Space : P.Address_Space;
      Address : Unsigned_64; Width : T.Access_Width; For_Write : Boolean)
      return Access_View
   is
      I : constant Slot_Count := Find (State, ID);
      Bytes : constant Unsigned_64 := Unsigned_64 (T.Octets (Width));
      Offset : Unsigned_64;
   begin
      if State.Working or else I = 0 or else State.Items (I).At_Phase /= Ready then
         return (Allowed => False, Space => P.Memory_Space);
      end if;
      declare Window : W.Granted_Window renames State.Items (I).Window; begin
         if Space /= Window.Space or else Address < Window.Base or else
           (if For_Write then not Window.Writable else not Window.Readable)
         then return (Allowed => False, Space => P.Memory_Space); end if;
         Offset := Address - Window.Base;
         if Offset >= Window.Length or else Bytes > Window.Length - Offset
           or else not T.Aligned (Address, Width)
         then return (Allowed => False, Space => P.Memory_Space); end if;
         if Space = P.IO_Space then
            if Width = T.Qword_Access then return (Allowed => False, Space => P.Memory_Space); end if;
            return (Allowed => True, Space => P.IO_Space,
              Ports => State.Items (I).Ports, Port => Unsigned_16 (Address));
         end if;
         return (Allowed => True, Space => P.Memory_Space,
           Virtual_Address => State.Items (I).Virtual + Offset);
      end;
   end Select_Access;
end ACPI_Backend_Bindings;
