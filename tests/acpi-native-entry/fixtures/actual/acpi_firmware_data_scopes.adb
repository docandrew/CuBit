pragma Ada_2022;
package body ACPI_Firmware_Data_Scopes with SPARK_Mode is
   use type Word, Identifier, IPC.Time_Instant, IPC.Outcome, F.T.Access_Width;
   use type Scope_Store.Outcome, Descriptor_Store.Outcome;
   function Observe (Owner : Context) return Statistics is
     ((Scope_Store.Length (Owner.Scopes), Descriptor_Store.Length (Owner.Descriptors),
       Scope_Store.Capacity (Owner.Scopes), Descriptor_Store.Capacity (Owner.Descriptors),
       Owner.Working, Owner.Active, Owner.Clock_Broken));
   function Observe_Scope_Capacity (Owner : Context) return Capacity_Report is
      R : constant Scope_Store.Report := Scope_Store.Observe (Owner.Scopes);
   begin
      return (Problem => (case R.Status is
         when Scope_Store.Stored => No_Capacity_Failure,
         when Scope_Store.Allocation_Refused => Process_Heap_Limit,
         when Scope_Store.Count_Representation_Exhausted => Entry_Limit,
         when Scope_Store.Byte_Representation_Exhausted => Byte_Limit),
         Demand => R.Demand, Capacity => Word (R.Capacity), Requested_Bytes => R.Requested_Bytes);
   end Observe_Scope_Capacity;
   function Observe_Descriptor_Capacity (Owner : Context) return Capacity_Report is
      R : constant Descriptor_Store.Report := Descriptor_Store.Observe (Owner.Descriptors);
   begin
      return (Problem => (case R.Status is
         when Descriptor_Store.Stored => No_Capacity_Failure,
         when Descriptor_Store.Allocation_Refused => Process_Heap_Limit,
         when Descriptor_Store.Count_Representation_Exhausted => Entry_Limit,
         when Descriptor_Store.Byte_Representation_Exhausted => Byte_Limit),
         Demand => R.Demand, Capacity => Word (R.Capacity), Requested_Bytes => R.Requested_Bytes);
   end Observe_Descriptor_Capacity;
   function Find_Client (Owner : Context; Caller : Word; ID : Identifier) return Natural is
   begin
      for I in 1 .. Scope_Store.Length (Owner.Scopes) loop
         declare Item : constant Scope_Record := Scope_Store.Read (Owner.Scopes,I); begin
            if Item.Caller = Caller and then Item.Client_ID = ID then return I; end if;
         end;
      end loop;
      return 0;
   end Find_Client;
   function Find_Scope (Owner : Context; Caller : Word; ID : Identifier) return Natural is
   begin
      for I in 1 .. Scope_Store.Length (Owner.Scopes) loop
         if Identifier (I) = ID and then Scope_Store.Read (Owner.Scopes,I).Caller = Caller
         then return I; end if;
      end loop;
      return 0;
   end Find_Scope;
   procedure Sample (Owner : in out Context; Deadline : IPC.Time_Instant;
      Result : out Outcome) is
      Now : IPC.Time_Instant;
      Available : Boolean;
   begin
      Result := Clock_Reversed;
      if Owner.Clock_Broken then return; end if;
      Clock (Now, Available);
      if not Available then Result := Clock_Unavailable; return;
      elsif Owner.Seen_Clock and then Now < Owner.Last_Clock then
         Owner.Clock_Broken := True; return;
      end if;
      Owner.Seen_Clock := True; Owner.Last_Clock := Now;
      Result := (if Now >= Deadline then Expired else Ready);
   end Sample;
   procedure Require_Live (Owner : in out Context; Item : in out Scope_Record;
      For_Write : Boolean) is
      Accepted : Boolean;
   begin
      if Item.Status /= Ready then return; end if;
      if not Authorized (Item.Caller, For_Write) then Item.Status := Denied; return; end if;
      Sample (Owner, Item.Deadline, Item.Status);
      if Item.Status /= Ready then return; end if;
      if Item.Domain /= 0 then
         Check_Lease (Item.Caller, Item.Domain, Item.Sequence, Accepted);
         if not Accepted then Item.Status := Lease_Refused; return; end if;
      end if;
      Sample (Owner, Item.Deadline, Item.Status);
   end Require_Live;
   function Describe_Scope (Owner : Context; Caller : Word; Client_ID : Identifier)
      return Reply is
      Index : constant Natural := Find_Client (Owner,Caller,Client_ID);
   begin
      if Caller = 0 or else Client_ID = F.No_ID then return (Invalid_Request,F.No_ID);
      elsif Index = 0 then return (others => <>); end if;
      return (Scope_Store.Read (Owner.Scopes,Index).Status,Identifier (Index));
   end Describe_Scope;
   procedure Begin_Scope (Owner : in out Context; Caller : Word;
      Client_ID : Identifier; Deadline : IPC.Time_Instant; Domain, Sequence : Word;
      Result : out Reply) is
      Item : Scope_Record;
      Index : Natural;
      Stored : Scope_Store.Outcome;
   begin
      Result := (Invalid_Request,F.No_ID);
      if Caller = 0 or else Client_ID = F.No_ID or else Deadline = 0
        or else Word (Deadline) > F.Maximum_Scope_Deadline
        or else ((Domain = 0) /= (Sequence = 0)) then return;
      elsif Owner.Working then Result.Status := Reentered; return; end if;
      Owner.Working := True;
      Index := Find_Client (Owner,Caller,Client_ID);
      if Index /= 0 then
         Item := Scope_Store.Read (Owner.Scopes,Index);
         if Item.Deadline /= Deadline or else Item.Domain /= Domain or else Item.Sequence /= Sequence then
            Result.Status := Request_Changed; Owner.Working := False; return;
         end if;
      else
         if not Authorized (Caller,False) then
            Result.Status := Denied; Owner.Working := False; return;
         end if;
         Item := (Caller,Client_ID,Deadline,Domain,Sequence,Stopped);
         Scope_Store.Append (Owner.Scopes,Item,Index,Stored);
         if Stored /= Scope_Store.Stored then
            Result.Status := Allocation_Refused; Owner.Working := False; return;
         end if;
         Item.Status := Ready;
      end if;
      Require_Live (Owner,Item,False);
      Scope_Store.Replace (Owner.Scopes,Index,Item);
      Result := (Item.Status, (if Item.Status = Ready then Identifier (Index) else F.No_ID));
      Owner.Working := False;
   end Begin_Scope;
   procedure Select_Datum (Owner : in out Context; Caller : Word;
      Scope_ID, Client_ID, Record_ID : Identifier; Offset : Word;
      Width : F.T.Access_Width; Result : out Reply) is
      Scope_Index, Descriptor_Index : Natural := 0;
      Item : Scope_Record;
      Selection : Descriptor_Record;
      Accepted : Boolean;
      Stored : Descriptor_Store.Outcome;
   begin
      Result := (Invalid_Request,F.No_ID);
      if Caller = 0 or else Scope_ID = F.No_ID or else Client_ID = F.No_ID
        or else Record_ID = F.No_ID or else Offset > F.Maximum_Offset then return;
      elsif Owner.Working then Result.Status := Reentered; return; end if;
      Owner.Working := True;
      for I in 1 .. Descriptor_Store.Length (Owner.Descriptors) loop
         Selection := Descriptor_Store.Read (Owner.Descriptors,I);
         if Selection.Caller = Caller and then Selection.Client_ID = Client_ID then
            Descriptor_Index := I; exit;
         end if;
      end loop;
      if Descriptor_Index /= 0 and then
        (Selection.Scope_ID /= Scope_ID or else Selection.Record_ID /= Record_ID
         or else Selection.Offset /= Offset or else Selection.Width /= Width)
      then Result.Status := Request_Changed; Owner.Working := False; return; end if;
      Scope_Index := Find_Scope (Owner,Caller,Scope_ID);
      if Scope_Index = 0 then Result.Status := Not_Found; Owner.Working := False; return; end if;
      Item := Scope_Store.Read (Owner.Scopes,Scope_Index);
      Require_Live (Owner,Item,False);
      if Item.Status = Ready then
         Check_Selection (Caller,Record_ID,Offset,Width,Accepted);
         if not Accepted then Result.Status := Selection_Refused;
         else
            Require_Live (Owner,Item,False);
            Result.Status := Item.Status;
            if Item.Status = Ready then
               if Descriptor_Index = 0 then
                  Selection := (Caller,Client_ID,Scope_ID,Record_ID,Offset,Width);
                  Descriptor_Store.Append (Owner.Descriptors,Selection,Descriptor_Index,Stored);
                  if Stored /= Descriptor_Store.Stored then Result.Status := Allocation_Refused; end if;
               end if;
               if Result.Status = Ready then Result.ID := Identifier (Descriptor_Index); end if;
            end if;
         end if;
      else Result.Status := Item.Status;
      end if;
      Scope_Store.Replace (Owner.Scopes,Scope_Index,Item);
      Owner.Working := False;
   end Select_Datum;
   procedure Close_Scope (Owner : in out Context; Caller : Word;
      Scope_ID : Identifier; Result : out Reply) is
      Index : Natural;
      Item : Scope_Record;
   begin
      Result := (Invalid_Request,F.No_ID);
      if Caller = 0 or else Scope_ID = F.No_ID then return;
      elsif Owner.Working then Result.Status := Reentered; return; end if;
      Owner.Working := True;
      Index := Find_Scope (Owner,Caller,Scope_ID);
      if Index = 0 then Result.Status := Not_Found;
      else
         Item := Scope_Store.Read (Owner.Scopes,Index); Item.Status := Closed;
         Scope_Store.Replace (Owner.Scopes,Index,Item); Result := (Closed,Scope_ID);
      end if;
      Owner.Working := False;
   end Close_Scope;
   procedure Check_Active (Owner : in out Context; Caller : Word;
      For_Write : Boolean; Accepted : out Boolean) is
      Item : Scope_Record;
   begin
      Accepted := False;
      if not Owner.Working or else not Owner.Active or else Owner.Checking
        or else Owner.Active_Failed or else Caller /= Owner.Active_Caller
        or else For_Write /= Owner.Active_Write then return; end if;
      Owner.Checking := True;
      Item := Scope_Store.Read (Owner.Scopes,Owner.Active_Scope);
      Require_Live (Owner,Item,For_Write);
      Scope_Store.Replace (Owner.Scopes,Owner.Active_Scope,Item);
      Accepted := Item.Status = Ready;
      if not Accepted then Owner.Active_Failed := True; end if;
      Owner.Checking := False;
   end Check_Active;
   procedure Perform (Owner : in out Context; Caller : Word;
      Descriptor : Identifier; For_Write : Boolean; Input : Word;
      Output : out Word; Status : out IPC.Outcome) is
      Selection : Descriptor_Record;
      Item : Scope_Record;
      Index : Natural := 0;
      Data : Word := 0;
      Accepted : Boolean;
   begin
      Output := 0; Status := IPC.Denied;
      if Caller = 0 or else Descriptor = F.No_ID or else (not For_Write and then Input /= 0)
        or else Owner.Working then return; end if;
      Owner.Working := True;
      for I in 1 .. Descriptor_Store.Length (Owner.Descriptors) loop
         if Identifier (I) = Descriptor then Index := I; exit; end if;
      end loop;
      if Index = 0 then Owner.Working := False; return; end if;
      Selection := Descriptor_Store.Read (Owner.Descriptors,Index);
      if Selection.Caller /= Caller then Owner.Working := False; return; end if;
      Index := Find_Scope (Owner,Caller,Selection.Scope_ID);
      if Index = 0 then Owner.Working := False; return; end if;
      Item := Scope_Store.Read (Owner.Scopes,Index);
      Require_Live (Owner,Item,For_Write);
      Scope_Store.Replace (Owner.Scopes,Index,Item);
      if Item.Status /= Ready then Owner.Working := False; return; end if;
      Owner.Active := True; Owner.Active_Failed := False;
      Owner.Active_Caller := Caller; Owner.Active_Scope := Index; Owner.Active_Write := For_Write;
      Transfer_Record (Caller,Selection.Record_ID,Selection.Offset,Selection.Width,
         For_Write,Input,Item.Deadline,Data,Status);
      Check_Active (Owner,Caller,For_Write,Accepted);
      if not Accepted then Status := IPC.Indeterminate;
      elsif Status = IPC.Done and then not For_Write then Output := Data;
      end if;
      Owner.Active := False; Owner.Active_Caller := 0; Owner.Active_Scope := 0;
      Owner.Working := False;
   end Perform;
end ACPI_Firmware_Data_Scopes;
