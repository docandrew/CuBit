pragma Ada_2022;
with ACPI_Firmware_Data_Identities;
package body ACPI_Firmware_Data_Field_Client is
   use type Word, Identifier, IPC.Time_Instant, F.T.Access_Width, F.L.Region, F.Operation;
   use type P.Outcome, P.Effect_Kind, RPC.Outcome;
   use type Regions.Outcome, Frames.Outcome, Descriptors.Outcome;
   function Observe_Publication (Owner : Context) return Publication_Diagnostic is
      (Owner.Publication);
   function Observe_Capacity (Owner : Context) return Capacity_Report is (Owner.Capacity);
   function Has_Frames (Owner : Context) return Boolean is
   begin
      for I in 1 .. Frames.Length (Owner.Frame_Rows) loop
         if Frames.Read (Owner.Frame_Rows,I).Stage /= Retired then return True; end if;
      end loop;
      return False;
   end Has_Frames;
   function Observe (Owner : Context) return Statistics is
      ((Regions.Length (Owner.Region_Rows),Frames.Length (Owner.Frame_Rows),
        Descriptors.Length (Owner.Descriptor_Rows),Owner.Depth,Owner.Working,Owner.Failed,
        Has_Frames (Owner) or else RPC.Observe (Owner.Transport).Cleanup_Due or else Owner.Uncertain));
   function No_Dispatch (Status : RPC.Outcome) return Boolean is
      (Status in RPC.Not_Sent | RPC.Invalid_Request | RPC.Invalid_Deadline | RPC.Expired);
   procedure Invoke (Owner : in out Context; Request : F.Request;
      Expected : P.Effect_Kind; Width : F.T.Access_Width;
      Deadline : IPC.Time_Instant; Reply : out RPC.Result) is
      Attempt : RPC.Attempt;
      Captured : constant Boolean := not Owner.Uncertain;
   begin
      if Captured then
         Owner.Uncertain := True; Owner.Request := Request;
         Owner.Expected := Expected; Owner.Width := Width;
      end if;
      RPC.Call (Owner.Transport,Connection,Attempt,Request,Expected,Width,Deadline,Reply);
      if Captured and then (Reply.Status = RPC.Completed or else No_Dispatch (Reply.Status)) then
         Owner.Uncertain := False;
      end if;
   end Invoke;
   procedure Publish (Owner : in out Context; Region : F.L.Region;
      Deadline : IPC.Time_Instant; Reply : out Result) is
      Item : Region_Record;
      Index : Natural;
      Stored : Regions.Outcome;
      Good : Boolean;
      Answer : RPC.Result;
   begin
      Reply := (Status => Invalid_Request,others => <>);
      if Owner.Working then Reply.Status := Reentered; return;
      elsif Owner.Failed then Reply.Status := Failed; return;
      elsif not F.L.Valid (Region) or else Deadline = 0 or else Deadline = IPC.No_Finite_Deadline then return; end if;
      Owner.Working := True;
      for I in 1 .. Regions.Length (Owner.Region_Rows) loop
         Item := Regions.Read (Owner.Region_Rows,I);
         if Item.Geometry = Region then
            Reply := (Status => (if Item.Stage = Published then Ready else Pending),ID => Item.Server,Value => 0);
            Owner.Working := False; return;
         end if;
      end loop;
      Item.Geometry := Region;
      ACPI_Firmware_Data_Identities.Issue (Item.Client,Good);
      if not Good then Reply.Status := Exhausted; Owner.Working := False; return; end if;
      Regions.Append (Owner.Region_Rows,Item,Index,Stored);
      if Stored /= Regions.Stored then
         declare R : constant Regions.Report := Regions.Observe (Owner.Region_Rows); begin
            Owner.Capacity := (Region_Cache,
               (case Stored is when Regions.Allocation_Refused => Process_Heap_Limit,
                when Regions.Count_Representation_Exhausted => Entry_Limit,
                when Regions.Byte_Representation_Exhausted => Byte_Limit,
                when Regions.Stored => No_Capacity_Failure),R.Demand,Word (R.Capacity),R.Requested_Bytes);
         end;
         Reply.Status := Allocation_Refused; Owner.Working := False; return;
      end if;
      Invoke (Owner,(F.Publish,Item.Client,Region,Deadline),P.No_Effect,F.T.Qword_Access,Deadline,Answer);
      Owner.Publication := (Present => True, Transport => Answer.Status,
         Has_Remote => Answer.Reply.Valid,
         Remote => (if Answer.Reply.Valid then Answer.Reply.Item.Status else P.Invalid_Request),
         Deadline => Deadline);
      if Answer.Status = RPC.Completed and then Answer.Reply.Item.Status = P.Ready then
         Item.Server := Identifier (Answer.Reply.Item.Value); Item.Stage := Published;
         Reply := (Ready,Item.Server,0);
      else
         Owner.Failed := True;
         Item.Stage := Region_Stopped;
         if Answer.Status = RPC.Completed and then Answer.Reply.Item.Status = P.Pending then
            Owner.Uncertain := True; Owner.Request := (F.Publish,Item.Client,Region,Deadline);
            Owner.Expected := P.No_Effect; Owner.Width := F.T.Qword_Access;
         end if;
         Reply.Status := (if Owner.Uncertain then Pending else Denied);
      end if;
      Regions.Replace (Owner.Region_Rows,Index,Item); Owner.Working := False;
   end Publish;
   procedure Begin_Field (Owner : in out Context; Requires_Lock : Boolean;
      Deadline : IPC.Time_Instant; Domain, Sequence : Word; Reply : out Result) is
      Item, Parent : Frame_Record;
      Index : Natural;
      Stored : Frames.Outcome;
      Good : Boolean;
      Answer : RPC.Result;
   begin
      Reply := (Status => Invalid_Request,others => <>);
      if Owner.Working then Reply.Status := Reentered; return;
      elsif Owner.Failed then Reply.Status := Failed; return;
      elsif Deadline = 0 or else Word (Deadline) > F.Maximum_Scope_Deadline
        or else ((Domain = 0) /= (Sequence = 0)) then return; end if;
      Owner.Working := True;
      Item.Parent := Owner.Active; Item.Deadline := Deadline;
      Item.Domain := Domain; Item.Sequence := Sequence;
      if Owner.Active /= 0 then
         Parent := Frames.Read (Owner.Frame_Rows,Owner.Active);
         Item.Deadline := IPC.Time_Instant'Min (Deadline,Parent.Deadline);
         if Parent.Domain /= 0 then
            if Domain /= 0 and then (Domain /= Parent.Domain or else Sequence /= Parent.Sequence) then
               Owner.Working := False; return;
            end if;
            Item.Domain := Parent.Domain; Item.Sequence := Parent.Sequence;
         end if;
      end if;
      if Requires_Lock and then Item.Domain = 0 then Owner.Working := False; return; end if;
      ACPI_Firmware_Data_Identities.Issue (Item.Client,Good);
      if not Good then Reply.Status := Exhausted; Owner.Working := False; return; end if;
      Frames.Append (Owner.Frame_Rows,Item,Index,Stored);
      if Stored /= Frames.Stored then
         declare R : constant Frames.Report := Frames.Observe (Owner.Frame_Rows); begin
            Owner.Capacity := (Frame_Cache,
               (case Stored is when Frames.Allocation_Refused => Process_Heap_Limit,
                when Frames.Count_Representation_Exhausted => Entry_Limit,
                when Frames.Byte_Representation_Exhausted => Byte_Limit,
                when Frames.Stored => No_Capacity_Failure),R.Demand,Word (R.Capacity),R.Requested_Bytes);
         end;
         Reply.Status := Allocation_Refused; Owner.Working := False; return;
      end if;
      Invoke (Owner,(F.Begin_Scope,Item.Client,Item.Deadline,Item.Domain,Item.Sequence),
         P.No_Effect,F.T.Qword_Access,Item.Deadline,Answer);
      if Answer.Status = RPC.Completed and then Answer.Reply.Item.Status = P.Ready then
         Item.Server := Identifier (Answer.Reply.Item.Value); Item.Stage := Live;
         Owner.Active := Index; Owner.Depth := Owner.Depth + 1; Reply := (Ready,Item.Server,0);
      else
         Owner.Failed := True;
         if Answer.Status = RPC.Completed or else No_Dispatch (Answer.Status) then Item.Stage := Retired; end if;
         Reply.Status := (if Owner.Uncertain then Pending else Denied);
      end if;
      Frames.Replace (Owner.Frame_Rows,Index,Item); Owner.Working := False;
   end Begin_Field;
   procedure Access_Datum (Owner : in out Context; Record_ID : Identifier;
      Offset : Word; Width : F.T.Access_Width; For_Write : Boolean; Input : Word;
      Reply : out Result) is
      Region : Region_Record;
      Found : Boolean := False;
      Item : Descriptor_Record;
      Frame : Frame_Record;
      Index : Natural := 0;
      Stored : Descriptors.Outcome;
      Op : Identifier;
      Good : Boolean;
      Answer : RPC.Result;
      Request : F.Request;
   begin
      Reply := (Status => Invalid_Request,others => <>);
      if Owner.Working then Reply.Status := Reentered; return;
      elsif Owner.Failed then Reply.Status := Failed; return;
      elsif Owner.Active = 0 then Reply.Status := Not_Ready; return; end if;
      for I in 1 .. Regions.Length (Owner.Region_Rows) loop
         Region := Regions.Read (Owner.Region_Rows,I);
         if Region.Stage = Published and then Region.Server = Record_ID then Found := True; exit; end if;
      end loop;
      if not Found or else Offset >= Region.Geometry.Length
        or else Word (F.T.Octets (Width)) > Region.Geometry.Length - Offset
        or else not F.T.Aligned (Region.Geometry.Address + Offset,Width)
        or else (if For_Write then Input > F.L.Mask (Width) else Input /= 0) then return; end if;
      Owner.Working := True;
      Frame := Frames.Read (Owner.Frame_Rows,Owner.Active);
      for I in 1 .. Descriptors.Length (Owner.Descriptor_Rows) loop
         Item := Descriptors.Read (Owner.Descriptor_Rows,I);
         if Item.Frame = Owner.Active and then Item.Record_ID = Record_ID
           and then Item.Offset = Offset and then Item.Width = Width then Index := I; exit; end if;
      end loop;
      if Index = 0 then
         Item := (Frame => Owner.Active,Record_ID => Record_ID,Offset => Offset,Width => Width,others => <>);
         ACPI_Firmware_Data_Identities.Issue (Item.Client,Good);
         if not Good then Reply.Status := Exhausted; Owner.Working := False; return; end if;
         Descriptors.Append (Owner.Descriptor_Rows,Item,Index,Stored);
         if Stored /= Descriptors.Stored then
            declare R : constant Descriptors.Report := Descriptors.Observe (Owner.Descriptor_Rows); begin
               Owner.Capacity := (Descriptor_Cache,
                  (case Stored is when Descriptors.Allocation_Refused => Process_Heap_Limit,
                   when Descriptors.Count_Representation_Exhausted => Entry_Limit,
                   when Descriptors.Byte_Representation_Exhausted => Byte_Limit,
                   when Descriptors.Stored => No_Capacity_Failure),R.Demand,Word (R.Capacity),R.Requested_Bytes);
            end;
            Reply.Status := Allocation_Refused; Owner.Working := False; return;
         end if;
         Invoke (Owner,(F.Select_Datum,Frame.Server,Record_ID,Item.Client,Offset,Width),
            P.No_Effect,Width,Frame.Deadline,Answer);
         if Answer.Status /= RPC.Completed or else Answer.Reply.Item.Status /= P.Ready then
            Owner.Failed := True; Reply.Status := (if Owner.Uncertain then Pending else Denied);
            Owner.Working := False; return;
         end if;
         Item.Server := Identifier (Answer.Reply.Item.Value);
         Descriptors.Replace (Owner.Descriptor_Rows,Index,Item);
      end if;
      ACPI_Firmware_Data_Identities.Issue (Op,Good);
      if not Good then Reply.Status := Exhausted; Owner.Working := False; return; end if;
      Request := (if For_Write then (F.Write_Datum,Item.Server,Op,Input) else (F.Read_Datum,Item.Server,Op));
      Invoke (Owner,Request,(if For_Write then P.Write_Effect else P.Read_Effect),Width,Frame.Deadline,Answer);
      if Answer.Status = RPC.Completed and then Answer.Reply.Item.Status = P.Done then
         Reply := (Completed,Op,Answer.Reply.Item.Value);
      else
         Owner.Failed := True; Reply.ID := Op;
         if Answer.Status = RPC.Completed and then Answer.Reply.Item.Status not in
            P.Denied | P.Capacity_Refused | P.Invalid_Request then
            -- A received Indeterminate/backend-fault/history result is not
            -- evidence that the data effect did not happen. Keep the original
            -- operation identity for passive cleanup, never retry the datum.
            Owner.Uncertain := True; Owner.Request := Request;
            Owner.Expected := (if For_Write then P.Write_Effect else P.Read_Effect);
            Owner.Width := Width;
         end if;
         Reply.Status := (if Owner.Uncertain then Pending else Denied);
      end if;
      Owner.Working := False;
   end Access_Datum;
   procedure Close_Frame (Owner : in out Context; Index : Positive;
      Deadline : IPC.Time_Instant; Done : out Boolean) is
      Item : Frame_Record := Frames.Read (Owner.Frame_Rows,Index);
      Answer : RPC.Result;
   begin
      Done := Item.Stage = Retired; if Done then return; end if;
      if Item.Server = F.No_ID then
         Invoke (Owner,(F.Describe_Scope,Item.Client),P.No_Effect,F.T.Qword_Access,Deadline,Answer);
         if Answer.Status /= RPC.Completed then return; end if;
         case Answer.Reply.Item.Status is
            when P.Ready => Item.Server := Identifier (Answer.Reply.Item.Value);
            when P.Closed | P.Stopped | P.Expired | P.Clock_Unavailable | P.Clock_Reversed | P.Lease_Refused =>
               Item.Stage := Retired;
            when others => return;
         end case;
      end if;
      if Item.Stage /= Retired then
         Item.Stage := Closing; Frames.Replace (Owner.Frame_Rows,Index,Item);
         Invoke (Owner,(F.Close_Scope,Item.Server),P.No_Effect,F.T.Qword_Access,Deadline,Answer);
         if Answer.Status = RPC.Completed and then Answer.Reply.Item.Status = P.Closed then Item.Stage := Retired; end if;
      end if;
      Frames.Replace (Owner.Frame_Rows,Index,Item); Done := Item.Stage = Retired;
   end Close_Frame;
   procedure End_Field (Owner : in out Context; Deadline : IPC.Time_Instant; Reply : out Result) is
      Item : Frame_Record;
      Done : Boolean;
   begin
      Reply := (Status => Not_Ready,others => <>);
      if Owner.Working then Reply.Status := Reentered; return;
      elsif Owner.Active = 0 then return; end if;
      Owner.Working := True; Item := Frames.Read (Owner.Frame_Rows,Owner.Active);
      Close_Frame (Owner,Owner.Active,Deadline,Done);
      Owner.Active := Item.Parent; Owner.Depth := Owner.Depth - 1;
      if not Done then Owner.Failed := True; end if;
      Reply.Status := (if Done then Closed else Pending); Owner.Working := False;
   end End_Field;
   procedure Cleanup (Owner : in out Context; Deadline : IPC.Time_Instant; Reply : out Result) is
      Done, Closed_All : Boolean := True;
      Answer : RPC.Result;
      Request : F.Request;
   begin
      Reply := (Status => Not_Ready,others => <>);
      if Owner.Working then Reply.Status := Reentered; return; end if;
      Owner.Failed := True;
      if Owner.Depth /= 0 then return; end if;
      Owner.Working := True; RPC.Recover_Cleanup (Owner.Transport,Done);
      if not Done then Reply.Status := Pending; Owner.Working := False; return; end if;
      for I in reverse 1 .. Frames.Length (Owner.Frame_Rows) loop
         Close_Frame (Owner,I,Deadline,Done); if not Done then Closed_All := False; end if;
      end loop;
      if Owner.Uncertain then
         if Owner.Request.Kind in F.Read_Datum | F.Write_Datum then
            Request := (F.Query_Operation,Owner.Request.ID,
               (if Owner.Request.Kind = F.Read_Datum then Owner.Request.Operation_ID else Owner.Request.Write_Operation_ID));
            Invoke (Owner,Request,Owner.Expected,Owner.Width,Deadline,Answer);
            if Answer.Status = RPC.Completed and then Answer.Reply.Item.Effect = Owner.Expected
              and then Answer.Reply.Item.Status in P.Done | P.Denied | P.Indeterminate | P.Backend_Fault
            then Owner.Uncertain := False; end if;
         elsif Owner.Request.Kind = F.Publish then
            Invoke (Owner,(F.Describe_Publication,Owner.Request.ID),P.No_Effect,F.T.Qword_Access,Deadline,Answer);
            if Answer.Status = RPC.Completed and then Answer.Reply.Item.Status in
               P.Ready | P.Refused | P.Stopped then Owner.Uncertain := False; end if;
         elsif Closed_All then Owner.Uncertain := False;
         end if;
      end if;
      Reply.Status := (if Closed_All and then not Owner.Uncertain then Closed else Pending);
      Owner.Working := False;
   end Cleanup;
end ACPI_Firmware_Data_Field_Client;
