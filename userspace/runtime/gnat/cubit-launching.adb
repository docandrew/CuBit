pragma Ada_2022;

with Wait_ABI;
with System.Storage_Elements;

with CuBit.Grant_References;
with CuBit.Process_Events;
with CuBit.Stream_Regions;
with CuBit.Stream_Rings;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Published_Clock;
with CuBit.Handles.IPC;

package body CuBit.Launching is

   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   package LH renames CuBit.Launch_Handles;
   use type LH.Outcome;

   Page_Bytes : constant := 4096;
   Request_Bytes : constant :=
     LA.Maximum_Name_Bytes + LA.Maximum_Block_Bytes + LG.Maximum_Bytes +
     CuBit.Outlet_Rings.Maximum_Bytes;
   Request_Pages : constant := (Request_Bytes + Page_Bytes - 1) / Page_Bytes;

   type Request_Area is array (1 .. Request_Pages * Page_Bytes) of Unsigned_8;
   --  Lent to procmgr read-only on first use.
   Request : Request_Area with Alignment => Page_Bytes;
   Lent : Boolean := False;
   Lent_Reference : CuBit.Memory_Grants.Grant_Reference;

   --  The Launch badge (startup binding "launch", CAP-012): OP_LAUNCH goes
   --  through it; a program without one cannot launch.
   Launch_Badge : constant CuBit.Handles.Services.Process_Manager.Optional_Endpoint :=
     CuBit.Handles.Services.Process_Manager.Optional_Named ("launch");
   --  The last Request_Withheld refusal's label.
   Last_Withheld : LA.Label_Words := [others => 0];

   function Withheld_Request return String is
      Text : LA.Label_Text;
      Length : LA.Label_Length;
   begin
      LA.Decode_Label (Last_Withheld, Text, Length);
      return Text (1 .. Length);
   end Withheld_Request;

   package PP renames CuBit.Program_Descriptions;
   Describe_Bytes : constant := LA.Maximum_Name_Bytes + PP.Maximum_Descriptor_Bytes;
   Describe_Pages : constant := (Describe_Bytes + Page_Bytes - 1) / Page_Bytes;
   type Describe_Area is array (1 .. Describe_Pages * Page_Bytes) of Unsigned_8;
   --  Lent to procmgr writable on first use: it writes the descriptor here.
   Answer : Describe_Area with Alignment => Page_Bytes;
   Answer_Lent : Boolean := False;
   Answer_Reference : CuBit.Memory_Grants.Grant_Reference;

   procedure Launch
     (Through   : Launcher;
      Program   : String;
      Arguments : CuBit.Launch_Arguments.Block;
      Grants    : CuBit.Launch_Grants.Bytes;
      Started   : out Child;
      Result    : out Launch_Result;
      Failure   : out CuBit.Launch_Arguments.Launch_Failure;
      Rings     : CuBit.Outlet_Rings.Table := (others => <>))
   is
      Service : CuBit.Handles.Endpoint renames Through.Service;
      Ring_Table : CuBit.Outlet_Rings.Bytes (1 .. CuBit.Outlet_Rings.Maximum_Bytes);
      Ring_Bytes : CuBit.Outlet_Rings.Table_Length;
      Msg : Message := NULL_MESSAGE;
      Reply : MessageTag;
      Next : Natural := 0;
      Success : Boolean;
   begin
      Started := (others => <>);
      Failure := LA.Malformed_Request;
      if not Lent then
         CuBit.Memory_Grants.Create_Via_Capability
           (Service, Request'Address, Request_Pages, False,
            Lent_Reference, Success);
         if not Success then
            Result := No_Process_Manager;
            return;
         end if;
         Lent := True;
      end if;
      for C of Program loop
         Next := Next + 1;
         Request (Next) := Character'Pos (C);
      end loop;
      for B of Arguments loop
         Next := Next + 1;
         Request (Next) := B;
      end loop;
      for B of Grants loop
         Next := Next + 1;
         Request (Next) := B;
      end loop;
      CuBit.Outlet_Rings.Encode (Rings, Ring_Table, Ring_Bytes);
      for B of Ring_Table (1 .. Ring_Bytes) loop
         Next := Next + 1;
         Request (Next) := B;
      end loop;
      Msg.tag := (label => LA.Launch_Operation, length => LA.Launch_Request_Words,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words :=
        [CuBit.Grant_References.Encode (Lent_Reference),
         Unsigned_64 (Program'Length), Unsigned_64 (Arguments'Length),
         LG.Request_Word (0, Grants'Length, Ring_Bytes)];
      declare
         Received : CuBit.Handles.IPC.Carried;
      begin
         Reply := CuBit.Handles.IPC.Call_Accepting
           ((if Launch_Badge.Present
             then CuBit.Handles.Services.Process_Manager.Handle (Launch_Badge.Value)
             else Service),
            Msg, CuBit.Messages.Wait_Forever, Received);
         if Reply.label = CuBit.Kernel_ABI.Reply_OK then
            Started := (Process => CuBit.Process_IDs.From_Word (Msg.words (0)), others => <>);
            declare
               Taken : constant CuBit.Handles.IPC.Accepted_Endpoint :=
                 CuBit.Handles.IPC.Accept_Endpoint (Received);
               Ignore : CuBit.Handles.IPC.Close_Result;
            begin
               if Taken.OK and then LH.Valid_Rights_Word (Msg.words (1)) then
                  Started.Handle := Taken.Value;
                  Started.Rights := LH.Rights_Of (Msg.words (1));
               else
                  Ignore := CuBit.Handles.IPC.Close (Received);
               end if;
            end;
            Result := Launched;
            return;
         end if;
      end;
      Result := Refused;
      for Reason in LA.Launch_Failure loop
         if Msg.words (0) = LA.Launch_Failure'Enum_Rep (Reason) then
            Failure := Reason;
         end if;
      end loop;
      Last_Withheld := [others => 0];
      if LA."=" (Failure, LA.Request_Withheld) then
         Last_Withheld := [Msg.words (1), Msg.words (2), Msg.words (3)];
      end if;
   end Launch;

   procedure Lend_Ring
     (Through   : Launcher;
      Pages : Positive;
      Entry_Type : CuBit.Streams.TypeTag;
      Base : in out Unsigned_64; Grant : out Unsigned_64;
      Reference : out CuBit.Memory_Grants.Grant_Reference; Success : out Boolean)
   is
      Service : CuBit.Handles.Endpoint renames Through.Service;
      --  The whole region: the control page and the data ring.
      Region : constant Positive :=
        CuBit.Stream_Rings.Region_Pages (CuBit.Stream_Regions.Declared (Pages));
   begin
      Grant := 0;
      Reference := (others => <>);
      if Base = 0 then
         --  The break is not page aligned and grants are whole pages: take
         --  one page more and start the ring on a page boundary.
         Base := syscall (SYSCALL_SBRK, Unsigned_64 (Region + 1) * Page_Bytes);
         Success := Base /= Unsigned_64'Last;
         if not Success then
            Base := 0;
            return;
         end if;
         Base := (Base + Page_Bytes - 1) / Page_Bytes * Page_Bytes;
      end if;
      CuBit.Stream_Regions.Initialize (Base, Unsigned_16 (Entry_Type));
      CuBit.Memory_Grants.Create_Forwardable_Via_Capability
        (Service, System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address (Base)),
         Region, True, Reference, Success, notify => True);
      if Success then
         Grant := CuBit.Grant_References.Encode (Reference);
      end if;
   end Lend_Ring;

   ---------------------------------------------------------------------------
   --  Through the launch handle
   ---------------------------------------------------------------------------
   --  One call on Started's handle: Op with W0, its answer in Msg (words
   --  (0) an outcome word); Received what came with it when Accepting.
   procedure Handle_Call
     (Started : Child; Op : LH.Operation; W0 : Unsigned_64; Until_Time : Unsigned_64;
      Accepting : Boolean; Msg : out Message; Outcome : out LH.Outcome;
      Received : out CuBit.Handles.IPC.Carried);

   procedure Handle_Call
     (Started : Child; Op : LH.Operation; W0 : Unsigned_64; Until_Time : Unsigned_64;
      Accepting : Boolean; Msg : out Message; Outcome : out LH.Outcome;
      Received : out CuBit.Handles.IPC.Carried)
   is
      Reply : MessageTag;
   begin
      Msg := NULL_MESSAGE;
      Received := (Kind => CuBit.Handles.IPC.Absent);
      if not CuBit.Handles.Valid (Started.Handle) then
         Outcome := LH.Not_A_Handle;
         return;
      end if;
      Msg.tag := (label => LH.Label_Of (Op), length => LH.Request_Words (Op),
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words (0) := W0;
      if Accepting then
         Reply := CuBit.Handles.IPC.Call_Accepting (Started.Handle, Msg, Until_Time, Received);
      else
         Reply := capCall (Started.Handle, Msg, Until_Time);
      end if;
      Outcome :=
        (if Reply.label /= CuBit.Kernel_ABI.Reply_OK then LH.Failed
         elsif LH.Is_Outcome_Word (Msg.words (0)) then LH.To_Outcome (Msg.words (0))
         else LH.Malformed);
   end Handle_Call;

   procedure Send_Control
     (Started : Child; Kind : CuBit.Control_Events.Control_Kind;
      Result : out Control_Result)
   is
      Msg : Message;
      Outcome : LH.Outcome;
      Ignore : CuBit.Handles.IPC.Carried;
      Request : constant LH.Control_Request :=
        (case Kind is
           when CuBit.Control_Events.Stop      => LH.Ask_Stop,
           when CuBit.Control_Events.Interrupt => LH.Ask_Interrupt,
           when CuBit.Control_Events.Reload    => LH.Ask_Reload);
   begin
      Handle_Call (Started, LH.Control_Op, LH.Control_Word (Request),
                   Deadline_After (Handle_Answer_Ms), False, Msg, Outcome, Ignore);
      Result := (case Outcome is
                   when LH.Done => Sent,
                   when LH.Busy => Busy,
                   when others  => Refused);
   end Send_Control;

   procedure End_Child (Started : Child; Result : out Launch_Result) is
      Msg : Message;
      Outcome : LH.Outcome;
      Ignore : CuBit.Handles.IPC.Carried;
   begin
      if not CuBit.Handles.Valid (Started.Handle) then
         Result := No_Process_Manager;
         return;
      end if;
      Handle_Call (Started, LH.Control_Op, LH.Control_Word (LH.End_Now),
                   Deadline_After (Handle_Answer_Ms), False, Msg, Outcome, Ignore);
      Result := (if Outcome = LH.Done then Launched else Refused);
   end End_Child;

   procedure Observe
     (Started : Child; State : out LH.Child_State; Code : out Unsigned_64;
      Outcome : out LH.Outcome)
   is
      Msg : Message;
      Ignore : CuBit.Handles.IPC.Carried;
   begin
      State := LH.Running;
      Code := 0;
      Handle_Call (Started, LH.Observe_Op, 0, Deadline_After (Handle_Answer_Ms), False,
                   Msg, Outcome, Ignore);
      if Outcome = LH.Done then
         State := (if Msg.words (2) = LH.State_Word (LH.Exited) then LH.Exited
                   elsif Msg.words (2) = LH.State_Word (LH.Stopped) then LH.Stopped
                   else LH.Running);
         Code := Msg.words (3);
      end if;
   end Observe;

   procedure Debug
     (Started : Child; Until_Time : Unsigned_64; Session : out CuBit.Handles.Debug.Session;
      Outcome : out LH.Outcome)
   is
      package DA renames CuBit.Handles.Debug.ABI;
      Msg : Message;
      Received : CuBit.Handles.IPC.Carried;
      Ignore : CuBit.Handles.IPC.Close_Result;
   begin
      Session := CuBit.Handles.Debug.From_Word (0);
      Handle_Call (Started, LH.Debug_Op, DA.Kind_Word (DA.Observe), Until_Time, True,
                   Msg, Outcome, Received);
      if Outcome = LH.Done then
         declare
            Taken : constant CuBit.Handles.Debug.Accepted_Session :=
              CuBit.Handles.Debug.Accept_Session (Received, DA.Observe);
         begin
            if Taken.OK then
               Session := Taken.Value;
            else
               Ignore := CuBit.Handles.IPC.Close (Received);
               Outcome := LH.Failed;
            end if;
         end;
      end if;
   end Debug;

   procedure Narrow
     (Started : Child; Rights : LH.Rights; Narrowed : out Child; Outcome : out LH.Outcome)
   is
      Msg : Message;
      Received : CuBit.Handles.IPC.Carried;
      Ignore : CuBit.Handles.IPC.Close_Result;
   begin
      Narrowed := (Process => Started.Process, others => <>);
      Handle_Call (Started, LH.Narrow_Op, LH.Word_Of (Rights), Deadline_After (Handle_Answer_Ms),
                   True, Msg, Outcome, Received);
      if Outcome = LH.Done then
         declare
            Taken : constant CuBit.Handles.IPC.Accepted_Endpoint :=
              CuBit.Handles.IPC.Accept_Endpoint (Received);
         begin
            if Taken.OK and then LH.Valid_Rights_Word (Msg.words (1)) then
               Narrowed.Handle := Taken.Value;
               Narrowed.Rights := LH.Rights_Of (Msg.words (1));
            else
               Ignore := CuBit.Handles.IPC.Close (Received);
               Outcome := LH.Failed;
            end if;
         end;
      end if;
   end Narrow;

   procedure Release (Started : Child) is
      Msg : Message;
      Outcome : LH.Outcome;
      Ignore : CuBit.Handles.IPC.Carried;
      Ignore_Closed : CuBit.Handles.IPC.Close_Result;
   begin
      if CuBit.Handles.Valid (Started.Handle) then
         Handle_Call (Started, LH.Revoke_Op, 0, Deadline_After (Handle_Answer_Ms), False,
                      Msg, Outcome, Ignore);
         Ignore_Closed := CuBit.Handles.IPC.Close (Started.Handle);
      end if;
   end Release;

   procedure Poll_Exit
     (Started : Child; Has_Ended : out Boolean; Ended : out CuBit.Child_Exits.Report) is
   begin
      CuBit.Process_Events.Take_Exit (Started.Process, Has_Ended, Ended);
      if Has_Ended then
         Release (Started);
      end if;
   end Poll_Exit;

   procedure Wait (Started : Child; Ended : out CuBit.Child_Exits.Report) is
      package KA renames CuBit.Kernel_ABI;
      package KC renames CuBit.Kernel_Calls;
      Idle_Pause_Microseconds : constant := 1_000;
      Has_Ended : Boolean;
      Ignore : Unsigned_64;
   begin
      loop
         Poll_Exit (Started, Has_Ended, Ended);
         exit when Has_Ended;
         --  Woken by any traffic, not only events: pause briefly rather
         --  than spin on someone else's.
         Ignore := KC.Call (KA.Wait, Wait_ABI.Ready_Sources, Wait_ABI.Forever);
         Ignore := KC.Call
           (KA.Wait, Wait_ABI.Timer_Sources, Wait_ABI.Forever,
            CuBit.Published_Clock.Microseconds_Now + Idle_Pause_Microseconds);
      end loop;
   end Wait;

   Table_Pages : constant := (CuBit.Launch_Authority.Maximum_Table_Bytes + Page_Bytes - 1) / Page_Bytes;
   type Table_Area is array (1 .. Table_Pages * Page_Bytes) of Unsigned_8;
   --  Lent to procmgr writable on first use: it writes the table here.
   Table_Answer : Table_Area with Alignment => Page_Bytes;
   Table_Lent : Boolean := False;
   Table_Reference : CuBit.Memory_Grants.Grant_Reference;

   function Set_Staged_Launches (Allowed : Boolean) return CuBit.Messages.Message is
      Msg : Message := NULL_MESSAGE;
   begin
      Msg.tag := (label => CuBit.Launch_Authority.Staged_Switch_Operation,
                  length => 1, protocol => 0, ipc => <>, reserved => 0);
      Msg.words (0) := (if Allowed then CuBit.Launch_Authority.Staged_Switch_On
                        else CuBit.Launch_Authority.Staged_Switch_Off);
      return Msg;
   end Set_Staged_Launches;

   function Staged_Launches_Set (Reply : CuBit.Messages.Message) return Boolean is
     (Reply.tag.label = CuBit.Kernel_ABI.Reply_OK);

   procedure Launch_Table
     (Through   : Launcher;
      Table  : out CuBit.Launch_Authority.Table_Bytes;
      Length : out CuBit.Launch_Authority.Table_Length;
      Result : out Launch_Result)
   is
      Service : CuBit.Handles.Endpoint renames Through.Service;
      Msg : Message := NULL_MESSAGE;
      Reply : MessageTag;
      Success : Boolean;
   begin
      Table := [others => 0];
      Length := 0;
      if not Table_Lent then
         CuBit.Memory_Grants.Create_Via_Capability
           (Service, Table_Answer'Address, Table_Pages, True,
            Table_Reference, Success);
         if not Success then
            Result := No_Process_Manager;
            return;
         end if;
         Table_Lent := True;
      end if;
      Msg.tag := (label => CuBit.Launch_Authority.Table_Operation,
                  length => CuBit.Launch_Authority.Table_Request_Words, protocol => 0, ipc => <>, reserved => 0);
      Msg.words (0) := CuBit.Grant_References.Encode (Table_Reference);
      Reply := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
      if Reply.label = CuBit.Kernel_ABI.Reply_OK
        and then Msg.words (0) <= CuBit.Launch_Authority.Maximum_Table_Bytes
      then
         Length := Natural (Msg.words (0));
         for I in 1 .. Length loop
            Table (I) := Table_Answer (I);
         end loop;
         Result := Launched;
      else
         Result := Refused;
      end if;
   end Launch_Table;

   Places_Pages : constant :=
     (CuBit.Launch_Grants.Maximum_Bytes + Page_Bytes - 1) / Page_Bytes;
   type Places_Area is array (1 .. Places_Pages * Page_Bytes) of Unsigned_8;
   Places_Answer : Places_Area with Alignment => Page_Bytes;
   Places_Lent : Boolean := False;
   Places_Reference : CuBit.Memory_Grants.Grant_Reference;

   procedure Delegated_Places
     (Through   : Launcher;
      Region : out CuBit.Launch_Grants.Bytes;
      Length : out CuBit.Launch_Grants.Byte_Count;
      Result : out Launch_Result)
   is
      Service : CuBit.Handles.Endpoint renames Through.Service;
      Msg : Message := NULL_MESSAGE;
      Reply : MessageTag;
      Success : Boolean;
   begin
      Region := [others => 0];
      Length := 0;
      if not Places_Lent then
         CuBit.Memory_Grants.Create_Via_Capability
           (Service, Places_Answer'Address, Places_Pages, True,
            Places_Reference, Success);
         if not Success then
            Result := No_Process_Manager;
            return;
         end if;
         Places_Lent := True;
      end if;
      Msg.tag := (label => CuBit.Launch_Grants.Places_Operation,
                  length => CuBit.Launch_Grants.Places_Request_Words, protocol => 0, ipc => <>, reserved => 0);
      Msg.words (0) := CuBit.Grant_References.Encode (Places_Reference);
      Reply := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
      if Reply.label = CuBit.Kernel_ABI.Reply_OK
        and then Msg.words (0) <= CuBit.Launch_Grants.Maximum_Bytes
      then
         Length := Natural (Msg.words (0));
         for I in 1 .. Length loop
            Region (I) := Places_Answer (I);
         end loop;
         Result := Launched;
      else
         Result := Refused;
      end if;
   end Delegated_Places;

   procedure Describe
     (Through   : Launcher;
      Program    : String;
      Descriptor : out CuBit.Program_Descriptions.Bytes;
      Length     : out CuBit.Program_Descriptions.Descriptor_Length;
      Result     : out Launch_Result;
      Failure    : out CuBit.Launch_Arguments.Launch_Failure)
   is
      Service : CuBit.Handles.Endpoint renames Through.Service;
      Msg : Message := NULL_MESSAGE;
      Reply : MessageTag;
      Success : Boolean;
   begin
      Descriptor := [others => 0];
      Length := 0;
      Failure := LA.Malformed_Request;
      if not Answer_Lent then
         CuBit.Memory_Grants.Create_Via_Capability
           (Service, Answer'Address, Describe_Pages, True,
            Answer_Reference, Success);
         if not Success then
            Result := No_Process_Manager;
            return;
         end if;
         Answer_Lent := True;
      end if;
      for I in Program'Range loop
         Answer (I - Program'First + 1) := Character'Pos (Program (I));
      end loop;
      Msg.tag := (label => PP.Description_Operation, length => PP.Description_Request_Words,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words (0) := CuBit.Grant_References.Encode (Answer_Reference);
      Msg.words (1) := Unsigned_64 (Program'Length);
      Reply := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
      if Reply.label = CuBit.Kernel_ABI.Reply_OK
        and then Msg.words (0) <= PP.Maximum_Descriptor_Bytes
      then
         Length := Natural (Msg.words (0));
         for I in 1 .. Length loop
            Descriptor (I) := Answer (Program'Length + I);
         end loop;
         Result := Launched;
         return;
      end if;
      Result := Refused;
      for Reason in LA.Launch_Failure loop
         if Msg.words (0) = LA.Launch_Failure'Enum_Rep (Reason) then
            Failure := Reason;
         end if;
      end loop;
   end Describe;

   --  The service's and the program's names into the lent request page,
   --  then the request: Label with the names' lengths and Last_Word.
   procedure Ask_Switch
     (Through : Launcher; Label : Unsigned_32; Service, Program : String;
      Last_Word, Within_Ms : Unsigned_64; Msg : out Message; Reply : out MessageTag;
      Sent : out Boolean);

   procedure Ask_Switch
     (Through : Launcher; Label : Unsigned_32; Service, Program : String;
      Last_Word, Within_Ms : Unsigned_64; Msg : out Message; Reply : out MessageTag;
      Sent : out Boolean)
   is
      Success : Boolean;
      Next : Natural := 0;
   begin
      Msg := NULL_MESSAGE;
      Reply := (label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0);
      Sent := False;
      if not CuBit.Handles.Valid (Through.Service) then
         return;
      end if;
      if not Lent then
         CuBit.Memory_Grants.Create_Via_Capability
           (Through.Service, Request'Address, Request_Pages, False, Lent_Reference, Success);
         if not Success then
            return;
         end if;
         Lent := True;
      end if;
      for C of Service loop
         Next := Next + 1;
         Request (Next) := Character'Pos (C);
      end loop;
      for C of Program loop
         Next := Next + 1;
         Request (Next) := Character'Pos (C);
      end loop;
      Msg.tag := (label => Label, length => CuBit.Service_Replacement.Request_Words,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words (0) := CuBit.Grant_References.Encode (Lent_Reference);
      Msg.words (1) := Unsigned_64 (Service'Length);
      Msg.words (2) := Unsigned_64 (Program'Length);
      Msg.words (3) := Last_Word;
      Reply := capCall (Through.Service, Msg, Deadline_After (Within_Ms));
      Sent := True;
   end Ask_Switch;

   procedure Switch_Service
     (Through : Launcher; Service : String; Program : String;
      Grace : CuBit.Service_Replacement.Stop_Grace_Ms;
      Health : CuBit.Service_Replacement.Health_Window_Ms;
      Result : out Switch_Result)
   is
      package SR renames CuBit.Service_Replacement;
      Msg : Message;
      Reply : MessageTag;
      Sent : Boolean;
   begin
      Result := (others => <>);
      Ask_Switch (Through, SR.Switch_Operation, Service, Program, SR.Deadlines_Word (Grace, Health),
                  SR.Answer_Bound_Ms (Grace, Health) + Answer_Margin_Ms, Msg, Reply, Sent);
      if not Sent then
         return;
      end if;
      if Reply.label = REPLY_TIMEOUT then
         Result.Answer := Timed_Out;
      elsif Reply.label = CuBit.Kernel_ABI.Reply_OK and then SR.Is_Outcome (Msg.words (0)) then
         Result.Answer := Answered;
         Result.Outcome := SR.Outcome_Of (Msg.words (0));
         Result.Failure := SR.Health_Of (Msg.words (1));
         Result.Running := CuBit.Process_IDs.From_Word (Msg.words (2));
      else
         Result.Answer := Refused_Switch;
         if SR.Is_Refusal (Msg.words (0)) then
            Result.Why := SR.Refusal_Of (Msg.words (0));
         end if;
      end if;
   end Switch_Service;

   procedure Stage_Service_For_Boot
     (Through : Launcher; Service : String; Program : String; Result : out Switch_Result)
   is
      package SR renames CuBit.Service_Replacement;
      Msg : Message;
      Reply : MessageTag;
      Sent : Boolean;
   begin
      Result := (others => <>);
      Ask_Switch (Through, SR.Boot_Stage_Operation, Service, Program, 0, Stage_Deadline_Ms,
                  Msg, Reply, Sent);
      if not Sent then
         return;
      end if;
      if Reply.label = REPLY_TIMEOUT then
         Result.Answer := Timed_Out;
      elsif Reply.label = CuBit.Kernel_ABI.Reply_OK then
         Result.Answer := Answered;
      else
         Result.Answer := Refused_Switch;
         if SR.Is_Refusal (Msg.words (0)) then
            Result.Why := SR.Refusal_Of (Msg.words (0));
         end if;
      end if;
   end Stage_Service_For_Boot;

end CuBit.Launching;
