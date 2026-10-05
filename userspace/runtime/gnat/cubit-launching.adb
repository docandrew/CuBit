pragma Ada_2022;

with System.Storage_Elements;

with CuBit.Grant_References;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages; use CuBit.Messages;

package body CuBit.Launching is

   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;

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

   package PP renames CuBit.Program_Descriptions;
   Describe_Bytes : constant := LA.Maximum_Name_Bytes + PP.Maximum_Descriptor_Bytes;
   Describe_Pages : constant := (Describe_Bytes + Page_Bytes - 1) / Page_Bytes;
   type Describe_Area is array (1 .. Describe_Pages * Page_Bytes) of Unsigned_8;
   --  Lent to procmgr writable on first use: it writes the descriptor here.
   Answer : Describe_Area with Alignment => Page_Bytes;
   Answer_Lent : Boolean := False;
   Answer_Reference : CuBit.Memory_Grants.Grant_Reference;

   procedure Launch
     (Program   : String;
      Arguments : CuBit.Launch_Arguments.Block;
      Grants    : CuBit.Launch_Grants.Bytes;
      Started   : out Child;
      Result    : out Launch_Result;
      Failure   : out CuBit.Launch_Arguments.Launch_Failure;
      Rings     : CuBit.Outlet_Rings.Table := (others => <>))
   is
      Ring_Table : CuBit.Outlet_Rings.Bytes (1 .. CuBit.Outlet_Rings.Maximum_Bytes);
      Ring_Bytes : CuBit.Outlet_Rings.Table_Length;
      Msg : Message := NULL_MESSAGE;
      Reply : MessageTag;
      Next : Natural := 0;
      Success : Boolean;
   begin
      Started := (others => 0);
      Failure := LA.Malformed_Request;
      if not Lent then
         CuBit.Memory_Grants.Create_Via_Capability
           (CAP_SLOT_PROCMGR, Request'Address, Request_Pages, False,
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
                  flags => 0, reserved => 0);
      Msg.words :=
        [CuBit.Grant_References.Encode (Lent_Reference),
         Unsigned_64 (Program'Length), Unsigned_64 (Arguments'Length),
         LG.Request_Word (0, Grants'Length, Ring_Bytes)];
      Reply := capCall (CAP_SLOT_PROCMGR, Msg);
      if Reply.label = CuBit.Kernel_ABI.Reply_OK then
         Started := (Process => Msg.words (0), Generation => Msg.words (1));
         Result := Launched;
         return;
      end if;
      Result := Refused;
      for Reason in LA.Launch_Failure loop
         if Msg.words (0) = LA.Launch_Failure'Enum_Rep (Reason) then
            Failure := Reason;
         end if;
      end loop;
   end Launch;

   procedure Lend_Ring
     (Outlet : CuBit.Program_Descriptions.Connector_Index; Pages : Positive;
      Entry_Type : CuBit.Streams.TypeTag;
      Base : in out Unsigned_64; Grant : out Unsigned_64;
      Reference : out CuBit.Memory_Grants.Grant_Reference; Success : out Boolean)
   is
   begin
      Grant := 0;
      Reference := (others => <>);
      if Base = 0 then
         --  The break is not page aligned and grants are whole pages: take
         --  one page more and start the ring on a page boundary.
         Base := syscall (SYSCALL_SBRK, Unsigned_64 (Pages + 1) * Page_Bytes);
         Success := Base /= Unsigned_64'Last;
         if not Success then
            Base := 0;
            return;
         end if;
         Base := (Base + Page_Bytes - 1) / Page_Bytes * Page_Bytes;
      end if;
      CuBit.Streams.Initialize_Ring
        (Base, Pages, CuBit.Streams.StreamId (CuBit.Program_Descriptions.Ring_Id (Outlet)),
         Entry_Type, Subscriber => Unsigned_32 (syscall (SYSCALL_GETPID)));
      CuBit.Memory_Grants.Create_Forwardable_Via_Capability
        (CAP_SLOT_PROCMGR, System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address (Base)),
         Pages, True, Reference, Success);
      if Success then
         Grant := CuBit.Grant_References.Encode (Reference);
      end if;
   end Lend_Ring;

   --  Exit reports drained from the event queue, until asked for.
   Kept_Exits : constant := 32;
   Exits : array (1 .. Kept_Exits) of CuBit.Child_Exits.Report;
   Exit_Used : array (1 .. Kept_Exits) of Boolean := [others => False];

   procedure Drain_Exits;
   procedure Drain_Exits is
      package CE renames CuBit.Child_Exits;
      package KA renames CuBit.Kernel_ABI;
      package KC renames CuBit.Kernel_Calls;
      M : aliased Message;
   begin
      while KC.Call (KA.Receive_Event_Nonblocking,
                     Unsigned_64 (System.Storage_Elements.To_Integer (M'Address)))
            = KA.Event_Received
      loop
         if M.tag.label = CE.Event_Label
           and then CE.Valid (M.tag.length, M.words (0), M.words (1), M.words (2))
         then
            for K in Exits'Range loop
               if not Exit_Used (K) then
                  Exits (K) := CE.Decode (M.words (0), M.words (1), M.words (2), M.words (3));
                  Exit_Used (K) := True;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
   end Drain_Exits;

   procedure Poll_Exit
     (Started : Child; Has_Ended : out Boolean; Ended : out CuBit.Child_Exits.Report) is
   begin
      Drain_Exits;
      Ended := (others => <>);
      Has_Ended := False;
      for K in Exits'Range loop
         if Exit_Used (K) and then Exits (K).Process = Started.Process
           and then Exits (K).Generation = Started.Generation
         then
            Ended := Exits (K);
            Exit_Used (K) := False;
            Has_Ended := True;
            return;
         end if;
      end loop;
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
         Ignore := KC.Call
           (KA.Wait_For_IPC_Or_Completion_Until_Monotonic_Millisecond, KA.Forever);
         Ignore := KC.Call
           (KA.Sleep_Until_Monotonic_Microsecond,
            KC.Call (KA.Read_Monotonic_Microseconds) + Idle_Pause_Microseconds);
      end loop;
   end Wait;

   Table_Pages : constant := (CuBit.Launch_Authority.Maximum_Table_Bytes + Page_Bytes - 1) / Page_Bytes;
   type Table_Area is array (1 .. Table_Pages * Page_Bytes) of Unsigned_8;
   --  Lent to procmgr writable on first use: it writes the table here.
   Table_Answer : Table_Area with Alignment => Page_Bytes;
   Table_Lent : Boolean := False;
   Table_Reference : CuBit.Memory_Grants.Grant_Reference;

   procedure Launch_Table
     (Table  : out CuBit.Launch_Authority.Table_Bytes;
      Length : out CuBit.Launch_Authority.Table_Length;
      Result : out Launch_Result)
   is
      Msg : Message := NULL_MESSAGE;
      Reply : MessageTag;
      Success : Boolean;
   begin
      Table := [others => 0];
      Length := 0;
      if not Table_Lent then
         CuBit.Memory_Grants.Create_Via_Capability
           (CAP_SLOT_PROCMGR, Table_Answer'Address, Table_Pages, True,
            Table_Reference, Success);
         if not Success then
            Result := No_Process_Manager;
            return;
         end if;
         Table_Lent := True;
      end if;
      Msg.tag := (label => CuBit.Launch_Authority.Table_Operation,
                  length => CuBit.Launch_Authority.Table_Request_Words, flags => 0, reserved => 0);
      Msg.words (0) := CuBit.Grant_References.Encode (Table_Reference);
      Reply := capCall (CAP_SLOT_PROCMGR, Msg);
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

   procedure Describe
     (Program    : String;
      Descriptor : out CuBit.Program_Descriptions.Bytes;
      Length     : out CuBit.Program_Descriptions.Descriptor_Length;
      Result     : out Launch_Result;
      Failure    : out CuBit.Launch_Arguments.Launch_Failure)
   is
      Msg : Message := NULL_MESSAGE;
      Reply : MessageTag;
      Success : Boolean;
   begin
      Descriptor := [others => 0];
      Length := 0;
      Failure := LA.Malformed_Request;
      if not Answer_Lent then
         CuBit.Memory_Grants.Create_Via_Capability
           (CAP_SLOT_PROCMGR, Answer'Address, Describe_Pages, True,
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
                  flags => 0, reserved => 0);
      Msg.words (0) := CuBit.Grant_References.Encode (Answer_Reference);
      Msg.words (1) := Unsigned_64 (Program'Length);
      Reply := capCall (CAP_SLOT_PROCMGR, Msg);
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

end CuBit.Launching;
