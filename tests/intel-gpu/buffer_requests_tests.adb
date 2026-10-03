with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Render_Sessions;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Materialize;
with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_DMA_Cache;
with System.Storage_Elements; use System.Storage_Elements;
procedure Buffer_Requests_Tests is
   package Sessions renames Intel_GPU_Render_Sessions;
   package Layout renames Intel_GPU_Buffer_Backing;
   Registry : Sessions.Registry;
   First, Second : Unsigned_64;
   Accepted : Boolean;
   Ready : Boolean := True;
   Calls : Natural := 0;
   Revoke_During_Allocation, Lose_Owner, Wrong_Size : Boolean := False;
   function Owner_Ready return Boolean is (Ready);
   function Resolve (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (Sessions.Resolve (Registry, Sender, Stamp));
   function Allocate (Index : Layout.Slot; Pages : Layout.Page_Count)
     return Intel_GPU_Buffer_Reply.Backing is
      Offset : constant Unsigned_64 := Unsigned_64 (Index - 1) * 4096;
   begin
      Calls := Calls + 1;
      if Revoke_During_Allocation then Sessions.Close (Registry, 42, First); end if;
      if Lose_Owner then Ready := False; end if;
      return Intel_GPU_Buffer_Reply.From_Linear (16#1000_0000# + Offset, Layout.CPU_Base + Offset,
              Unsigned_64 (Pages) * 4096 + (if Wrong_Size then 4096 else 0),
              16#1000_0000#);
   end Allocate;
   package Buffers is new Intel_GPU_Buffer_Requests (Resolve, Owner_Ready);
   package VM is new Intel_GPU_VM_Image (4);
   package Binding is new Buffers.Binding (VM);
   Object : Buffers.Service;
   Reply : Buffers.Words;
   ID : Unsigned_64;
   use type Buffers.Words;
   use type Buffers.Allocation_Outcome;
   procedure Call (Operation, Value : Unsigned_64;
                   Sender : Unsigned_64 := 42; Stamp : Unsigned_64 := First) is
      Deferred : Buffers.Ticket;
      Consumed : Boolean;
   begin
      Buffers.Handle (Object, Sender, Stamp, Buffers.Label, 4, 0, 0,
                      [Buffers.Version, Operation, Value, 0], Reply, Deferred);
      if Deferred /= 0 then
         Buffers.Complete (Object, Deferred,
           Allocate (Layout.Slot (Deferred), Layout.Page_Count (Value / 4096)),
           Reply, Consumed);
         pragma Assert (Consumed);
      end if;
   end Call;
begin
   pragma Assert (Buffers.Ticket_Generation (0) = 0);
   declare
      type Generations is array (Positive range <>) of Unsigned_64;
   begin
      for Index in 1 .. Layout.Bootstrap_Slots loop
         for Generation of Generations'(1, 2, 128, Unsigned_64 (Unsigned_32'Last)) loop
            declare
               ID : constant Buffers.Ticket :=
                 (Generation - 1) * Buffers.Ticket_Stride + Unsigned_64 (Index);
            begin
               pragma Assert (Buffers.Ticket_Slot (ID) = Index);
               pragma Assert (Buffers.Ticket_Generation (ID) = Unsigned_32 (Generation));
            end;
         end loop;
      end loop;
   end;
   Sessions.Reserve (Registry, 42, First);
   Call (Buffers.Create, 4096);
   pragma Assert (Reply (0) = Buffers.Denied and Calls = 0);
   Sessions.Finalize (Registry, 42, First, True, Accepted);
   pragma Assert (Accepted);
   Sessions.Reserve (Registry, 43, Second);
   Sessions.Finalize (Registry, 43, Second, True, Accepted);
   declare
      use Intel_GPU_Buffer_Handles;
      Pool : Buffers.Service;
      Deferred : Buffers.Ticket;
      Response : Buffers.Words;
      Consumed : Boolean;
      Name : Unsigned_64;
   begin
      pragma Assert (Buffers.Close_Diagnostic (Pool, 42, First + 1, 1) = Session_Unavailable);
      pragma Assert (Buffers.Close_Diagnostic (Pool, 42, First, 0) = Invalid_Handle);
      pragma Assert (Buffers.Close_Diagnostic (Pool, 42, First, 2 ** 32) = Invalid_Handle);
      Buffers.Handle (Pool, 42, First, Buffers.Label, 4, 0, 0,
        [1, Buffers.Create, 4096, 0], Response, Deferred);
      Buffers.Complete (Pool, Deferred, Allocate (1, 1), Response, Consumed);
      pragma Assert (Consumed and Response (0) = Buffers.OK);
      Name := Response (2);
      pragma Assert (Buffers.Close_Diagnostic (Pool, 42, First, Name) = Close_Ready);
      pragma Assert (Buffers.Close_Diagnostic (Pool, 43, Second, Name) = Foreign_Session);
      Buffers.Handle (Pool, 43, Second, Buffers.Label, 4, 0, 0,
        [1, Buffers.Close, Name, 0], Response, Deferred);
      pragma Assert (Response (0) = Buffers.Denied);
      Buffers.Handle (Pool, 42, First, Buffers.Label, 4, 0, 0,
        [1, Buffers.Close, Name, 0], Response, Deferred);
      pragma Assert (Response (0) = Buffers.OK);
      pragma Assert (Buffers.Close_Diagnostic (Pool, 42, First, Name) = Already_Closed);
      Buffers.Quarantine (Pool);
      pragma Assert (Buffers.Close_Diagnostic (Pool, 42, First, Name) = Registry_Quarantined);
   end;
   -- Preserve the allocation-call baseline used by the existing suite.
   Calls := 0;
   Ada.Text_IO.Put_Line ("Close request diagnostics PASS: authenticated envelope, full-width handle, foreign/closed/quarantine reasons");
   declare
      Pool : Buffers.Service;
      Original_ID, Deferred : Buffers.Ticket;
      Response : Buffers.Words;
      Consumed : Boolean;
   begin
      Ready := False;
      Buffers.Handle (Pool, 42, First, Buffers.Label, 4, 0, 0,
        [1, Buffers.Create, 4096, 0], Response, Deferred);
      pragma Assert (Deferred = 0 and Response (0) = Buffers.Unavailable);
      pragma Assert (Buffers.Last_Allocation (Pool) = Buffers.Owner_Unavailable);
      Ready := True;
      Buffers.Handle (Pool, 42, First, Buffers.Label, 4, 0, 0,
        [1, Buffers.Create, 4096, 0], Response, Original_ID);
      pragma Assert (Original_ID /= 0);
      pragma Assert (Buffers.Last_Allocation (Pool) = Buffers.Awaiting_Backing);
      Buffers.Handle (Pool, 42, First, Buffers.Label, 4, 0, 0,
        [1, Buffers.Create, 4096, 0], Response, Deferred);
      pragma Assert (Deferred = 0 and Response (0) = Buffers.Unavailable);
      pragma Assert (Buffers.Last_Allocation (Pool) = Buffers.Application_Pending);
      -- A stale completion must not replace the retained diagnostic or drain
      -- the real pending request. Its eventual success must replace it.
      Buffers.Complete (Pool, Original_ID + 1, (Ready => False), Response, Consumed);
      pragma Assert (not Consumed);
      pragma Assert (Buffers.Last_Allocation (Pool) = Buffers.Application_Pending);
      Buffers.Complete (Pool, Original_ID,
        Intel_GPU_Buffer_Reply.From_Linear
          (16#1000_0000#, Layout.CPU_Base, 4096, 16#1000_0000#), Response, Consumed);
      pragma Assert (Consumed and Response (0) = Buffers.OK);
      pragma Assert (Buffers.Last_Allocation (Pool) = Buffers.Allocation_Ready);
   end;
   declare
      Pool : Buffers.Service;
      Original, Partial, Empty_Image : VM.Image;
      Deferred : Buffers.Ticket;
      Response : Buffers.Words;
      Consumed : Boolean;
      Handle : Unsigned_64;
      DMA : constant Unsigned_64 := 16#1000_0000#;
   begin
      Buffers.Handle (Pool, 42, First, Buffers.Label, 4, 0, 0,
                      [1, Buffers.Create, 4096, 0], Response, Deferred);
      Buffers.Complete (Pool, Deferred,
        Intel_GPU_Buffer_Reply.From_Linear (DMA, Layout.CPU_Base, 4096, DMA),
        Response, Consumed);
      pragma Assert (Consumed and Response (0) = Buffers.OK);
      Handle := Response (2);
      VM.Initialize (Original, [16#2000000#, 16#2001000#, 16#2002000#, 16#2003000#], Accepted);
      pragma Assert (Accepted);
      for Address of VM.Data_Pages'[4096, 8192] loop
         VM.Map_Page (Original, Address, DMA, Intel_GPU_ADLN_PPGTT.Write_Back,
                      Intel_GPU_ADLN_PPGTT.Read_Write, Accepted);
         pragma Assert (Accepted);
      end loop;
      VM.Seal (Original, Accepted); pragma Assert (Accepted);
      pragma Assert (not Binding.Closed_Buffer_Disjoint (Pool, Original, First, Handle));
      Buffers.Handle (Pool, 42, First, Buffers.Label, 4, 0, 0,
                      [1, Buffers.Close, Handle, 0], Response, Deferred);
      pragma Assert (Response (0) = Buffers.OK);
      pragma Assert (not Binding.Closed_Buffer_Disjoint (Pool, Original, First, Handle));
      VM.Prepare_Update (Partial, Original,
        [16#3000000#, 16#3001000#, 16#3002000#, 16#3003000#], Accepted);
      pragma Assert (Accepted);
      VM.Unmap_Pages (Partial, 4096, [DMA], Accepted); pragma Assert (Accepted);
      VM.Seal_Update (Partial, Accepted); pragma Assert (Accepted);
      --  Removing one virtual alias must not hide the other physical alias.
      pragma Assert (not Binding.Closed_Buffer_Disjoint (Pool, Partial, First, Handle));
      VM.Prepare_Update (Empty_Image, Partial,
        [16#4000000#, 16#4001000#, 16#4002000#, 16#4003000#], Accepted);
      pragma Assert (Accepted);
      VM.Unmap_Pages (Empty_Image, 8192, [DMA], Accepted); pragma Assert (Accepted);
      pragma Assert (not Binding.Closed_Buffer_Disjoint (Pool, Empty_Image, First, Handle));
      VM.Seal_Update (Empty_Image, Accepted); pragma Assert (Accepted);
      pragma Assert (Binding.Closed_Buffer_Disjoint (Pool, Empty_Image, First, Handle));
      pragma Assert (not Binding.Closed_Buffer_Disjoint (Pool, Empty_Image, Second, Handle));
      pragma Assert (not Binding.Closed_Buffer_Disjoint (Pool, Empty_Image, First, 0));
      pragma Assert (not Binding.Closed_Buffer_Disjoint (Pool, Empty_Image, First, Handle + 1));
      Ready := False;
      pragma Assert (not Binding.Closed_Buffer_Disjoint (Pool, Empty_Image, First, Handle));
      Ready := True;
      Buffers.Quarantine (Pool);
      pragma Assert (not Binding.Closed_Buffer_Disjoint (Pool, Empty_Image, First, Handle));
   end;
   Ada.Text_IO.Put_Line ("Closed BO VM observation PASS: all aliases, sealed image, identity, quarantine");
   declare
      package Native_Buffers is new Intel_GPU_Buffer_Requests
        (Resolve, Owner_Ready, First_Slot => 2);
      Pool : Native_Buffers.Service;
      Private_ID : Native_Buffers.Ticket;
      Consumed : Boolean;
   begin
      Ready := False;
      Native_Buffers.Reserve_Private (Pool, First, Private_ID);
      pragma Assert (Private_ID = 0);
      Ready := True;
      Native_Buffers.Reserve_Private (Pool, First, Private_ID);
      pragma Assert (Private_ID = 2); -- bootstrap slot1 remains excluded
      pragma Assert (Native_Buffers.Ticket_Session (Pool, Private_ID) = First);
      pragma Assert (Native_Buffers.Pending_For (Pool, First));
      pragma Assert (not Native_Buffers.Pending_For (Pool, Second));
      Native_Buffers.Quarantine (Pool);
      Native_Buffers.Finish_Private (Pool, Private_ID, Consumed);
      pragma Assert (Consumed); -- terminal cleanup still drains after failure
      pragma Assert (not Native_Buffers.Pending_For (Pool, First));
      pragma Assert (Native_Buffers.Ticket_Session (Pool, Private_ID) = First);
      pragma Assert (Native_Buffers.Ticket_Session (Pool, 1) = 0);
      pragma Assert (Native_Buffers.Ticket_Session (Pool, 0) = 0);
      Native_Buffers.Reserve_Private (Pool, First, Private_ID);
      pragma Assert (Private_ID = 0);
   end;
   declare
      Pool : Buffers.Service;
      Private_ID, Deferred, Other : Buffers.Ticket;
      Consumed : Boolean;
      Response : Buffers.Words;
   begin
      Buffers.Reserve_Private (Pool, 0, Private_ID);
      pragma Assert (Private_ID = 1);
      pragma Assert (Buffers.Ticket_Session (Pool, Private_ID) = 0);
      Buffers.Reserve_Private (Pool, 0, Other); pragma Assert (Other = 0);
      Buffers.Handle (Pool, 42, First, Buffers.Label, 4, 0, 0,
        [Buffers.Version, Buffers.Create, 4096, 0], Response, Deferred);
      pragma Assert (Deferred = 0 and Response (0) = Buffers.Unavailable);
      pragma Assert (Buffers.Last_Allocation (Pool) = Buffers.Private_Pending);
      -- Application completion cannot convert private backing into a handle.
      Buffers.Complete (Pool, Private_ID, (Ready => False), Response, Consumed);
      pragma Assert (not Consumed);
      Buffers.Finish_Private (Pool, 2, Consumed); pragma Assert (not Consumed);
      Buffers.Finish_Private (Pool, Private_ID, Consumed); pragma Assert (Consumed);
      Buffers.Finish_Private (Pool, Private_ID, Consumed); pragma Assert (not Consumed);
      Buffers.Handle (Pool, 42, First, Buffers.Label, 4, 0, 0,
        [Buffers.Version, Buffers.Create, 4096, 0], Response, Deferred);
      pragma Assert (Deferred = 2);
      pragma Assert (Buffers.Ticket_Session (Pool, Deferred) = First);
      pragma Assert (Buffers.Pending_For (Pool, First));
      pragma Assert (not Buffers.Pending_For (Pool, Second));
      Buffers.Reserve_Private (Pool, 0, Other); pragma Assert (Other = 0);
      Buffers.Finish_Private (Pool, Deferred, Consumed); pragma Assert (not Consumed);
      Buffers.Complete (Pool, Deferred, (Ready => False), Response, Consumed);
      pragma Assert (Consumed);
      pragma Assert (Buffers.Last_Allocation (Pool) = Buffers.Backing_Unavailable);
      Buffers.Retire_Session (Pool, First);
      Buffers.Reject_Delivery (Pool, Deferred);
      pragma Assert (not Buffers.Pending_For (Pool, First));
      pragma Assert (Buffers.Ticket_Session (Pool, Deferred) = First);
      pragma Assert (Buffers.Ticket_Session (Pool, 3) = 0);
      for Expected in 3 .. Layout.Bootstrap_Slots loop
         Buffers.Reserve_Private (Pool, 0, Private_ID);
         pragma Assert (Private_ID = Unsigned_64 (Expected));
         Buffers.Finish_Private (Pool, Private_ID, Consumed); pragma Assert (Consumed);
      end loop;
      Buffers.Reserve_Private (Pool, 0, Private_ID); pragma Assert (Private_ID = 0);
      Buffers.Handle (Pool, 42, First, Buffers.Label, 4, 0, 0,
        [Buffers.Version, Buffers.Create, 4096, 0], Response, Deferred);
      pragma Assert (Deferred = 0 and Response (0) = Buffers.Unavailable);
      pragma Assert (Buffers.Last_Allocation (Pool) = Buffers.Slots_Exhausted);
   end;
   Call (Buffers.Create, 0); pragma Assert (Reply (0) = Buffers.Bad_Request);
   Call (Buffers.Create, 4095); pragma Assert (Reply (0) = Buffers.Bad_Request);
   Call (Buffers.Create, Unsigned_64'Last);
   pragma Assert (Reply (0) = Buffers.Bad_Request and Calls = 0);
   Call (Buffers.Create, 4096);
   pragma Assert (Reply = [Buffers.OK, Buffers.Version, 1, 4096] and Calls = 1);
   ID := Reply (2);
   declare
      Image : VM.Image;
      Response : Buffers.Words;
      procedure Bind_Call (Sender, Stamp, Version, GPU : Unsigned_64;
                           Flags : Unsigned_8 := 0) is
      begin
         Binding.Handle (Object, Image, First, Sender, Stamp, Binding.Bind_Label,
           4, Flags, 0, [Version, ID, GPU, 4096], Response);
      end Bind_Call;
   begin
      VM.Initialize (Image, [16#2000000#, 16#2001000#, 16#2002000#, 16#2003000#], Accepted);
      pragma Assert (Accepted);
      Bind_Call (43, Second, 1, 4096);
      pragma Assert (Response = [Buffers.Denied, 1, 0, 0] and VM.Lookup (Image, 4096) = 0);
      Bind_Call (42, First, 2, 4096);
      pragma Assert (Response = [Buffers.Bad_Request, 1, 0, 0]);
      Bind_Call (42, First, 1, 4096, 1);
      pragma Assert (Response = [Buffers.Bad_Request, 1, 0, 0]);
      Bind_Call (42, First, 1, 4097);
      pragma Assert (Response = [Buffers.Denied, 1, 0, 0]);
      -- A valid page offset is still bounded by the actual one-page BO.
      Bind_Call (42, First, 1 + 2 ** 32, 4096);
      pragma Assert (Response = [Buffers.Denied, 1, 0, 0] and VM.Lookup (Image, 4096) = 0);
      Bind_Call (42, First, 16#FFFF_FFFF_0000_0001#, 4096);
      pragma Assert (Response = [Buffers.Denied, 1, 0, 0] and VM.Lookup (Image, 4096) = 0);
      Bind_Call (42, First, 16#0000_0001_0000_0002#, 4096);
      pragma Assert (Response = [Buffers.Bad_Request, 1, 0, 0]);
      Bind_Call (42, First, 1, 4096);
      pragma Assert (Response = [Buffers.OK, 1, 4096, 4096] and VM.Lookup (Image, 4096) /= 0);
      Bind_Call (42, First, 1, 4096);
      pragma Assert (Response = [Buffers.Denied, 1, 0, 0]);
      for Operation in Unsigned_64 range 2 .. 65535 loop
         Bind_Call (42, First, 1 + Shift_Left (Operation, 16), 4096);
         pragma Assert (Response = [Buffers.Bad_Request, 1, 0, 0] and
           VM.Lookup (Image, 4096) /= 0);
      end loop;
      Bind_Call (43, Second, 16#10001#, 4096);
      pragma Assert (Response = [Buffers.Denied, 1, 0, 0] and VM.Lookup (Image, 4096) /= 0);
      Bind_Call (42, First, 16#10001# + 2 ** 32, 4096);
      pragma Assert (Response = [Buffers.Denied, 1, 0, 0] and VM.Lookup (Image, 4096) /= 0);
      Ready := False;
      Bind_Call (42, First, 16#10001#, 4096);
      pragma Assert (Response = [Buffers.Unavailable, 1, 0, 0] and VM.Lookup (Image, 4096) /= 0);
      Ready := True;
      Bind_Call (42, First, 16#10001#, 4096);
      pragma Assert (Response = [Buffers.OK, 1, 4096, 4096] and VM.Lookup (Image, 4096) = 0);
      Bind_Call (42, First, 16#10001#, 4096);
      pragma Assert (Response = [Buffers.Denied, 1, 0, 0]);
      Bind_Call (42, First, 1, 4096);
      pragma Assert (Response = [Buffers.OK, 1, 4096, 4096]);
      VM.Seal (Image, Accepted); pragma Assert (Accepted);
      Bind_Call (42, First, 16#10001#, 4096);
      pragma Assert (Response = [Buffers.Unavailable, 1, 0, 0] and VM.Lookup (Image, 4096) /= 0);
      Bind_Call (42, First, 1, 8192);
      pragma Assert (Response = [Buffers.Unavailable, 1, 0, 0] and VM.Lookup (Image, 8192) = 0);
   end;
   declare
      Image : VM.Image;
      type Page is array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64;
      type Storage is array (VM.Page_Number) of Page;
      RAM : Storage := [others => [others => 0]] with Alignment => 4096, Volatile;
      Lose_Device : Boolean := False;
      function Context_Ready return Boolean is
        (Ready and then Resolve (42, First) = First);
      function Flush (CPU : Unsigned_64) return Boolean is
      begin
         if Lose_Device then Ready := False; end if;
         return Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096);
      end Flush;
      package Writer is new Intel_GPU_VM_Materialize (VM, Context_Ready, Flush);
      Destinations : Writer.Mappings;
      Root : Unsigned_64;
      procedure Reject_Bind (Owner, Sender, Stamp, Handle, Offset, Bytes : Unsigned_64) is
      begin
         Binding.Bind (Object, Image, Owner, Sender, Stamp, Handle,
                       4096, Offset, Bytes, Accepted);
         pragma Assert (not Accepted and VM.Used (Image) = 1);
         pragma Assert (VM.Lookup (Image, 4096) = 0);
      end Reject_Bind;
   begin
      VM.Initialize (Image, [4096, 8192, 12288, 16384], Accepted);
      pragma Assert (Accepted);
      Reject_Bind (Second, 42, First, ID, 0, 4096);
      Reject_Bind (Second, 43, Second, ID, 0, 4096);
      Reject_Bind (First, 42, Second, ID, 0, 4096);
      Reject_Bind (First, 42, First, 2 ** 32, 0, 4096);
      Reject_Bind (First, 42, First, ID, 4096, 4096);
      Ready := False;
      Reject_Bind (First, 42, First, ID, 0, 4096);
      Ready := True;
      Binding.Bind (Object, Image, First, 42, First, ID, 4096, 0, 4096, Accepted);
      pragma Assert (Accepted and VM.Lookup (Image, 4096) = 16#1000_0003#);
      Binding.Unbind (Object, Image, First, 43, Second, ID, 4096, 0, 4096, Accepted);
      pragma Assert (not Accepted and VM.Lookup (Image, 4096) = 16#1000_0003#);
      Binding.Unbind (Object, Image, Second, 42, First, ID, 4096, 0, 4096, Accepted);
      pragma Assert (not Accepted and VM.Lookup (Image, 4096) = 16#1000_0003#);
      Binding.Unbind (Object, Image, First, 42, Second, ID, 4096, 0, 4096, Accepted);
      pragma Assert (not Accepted and VM.Lookup (Image, 4096) = 16#1000_0003#);
      Ready := False;
      Binding.Unbind (Object, Image, First, 42, First, ID, 4096, 0, 4096, Accepted);
      pragma Assert (not Accepted and VM.Lookup (Image, 4096) = 16#1000_0003#);
      Ready := True;
      Binding.Unbind (Object, Image, First, 42, First, ID, 4096, 0, 4096, Accepted);
      pragma Assert (Accepted and VM.Lookup (Image, 4096) = 0);
      Binding.Bind (Object, Image, First, 42, First, ID, 4096, 0, 4096, Accepted);
      pragma Assert (Accepted);
      VM.Seal (Image, Accepted); pragma Assert (Accepted);
      Binding.Unbind (Object, Image, First, 42, First, ID, 4096, 0, 4096, Accepted);
      pragma Assert (not Accepted and VM.Lookup (Image, 4096) = 16#1000_0003#);
      for P in VM.Page_Number loop
         Destinations (P) :=
           (Unsigned_64 (To_Integer (RAM (P)'Address)), Unsigned_64 (P) * 4096);
      end loop;
      declare State : Writer.State; begin
         Writer.Prepare (State, Image, Destinations, Root, Accepted);
         pragma Assert (Accepted and Root = 4096);
         for P in VM.Page_Number loop
            for I in Intel_GPU_ADLN_PPGTT.Table_Index loop
               pragma Assert (RAM (P) (I) = VM.Entry_Value (Image, P, I));
            end loop;
         end loop;
      end;
      -- Losing device ownership during cache visibility must suppress root.
      declare
         State : Writer.State;
      begin
         Lose_Device := True;
         Writer.Prepare (State, Image, Destinations, Root, Accepted);
         pragma Assert (not Accepted and Root = 0);
         Ready := True;
      end;
   end;
   declare
      Source, Added, Removed, Rejected : VM.Image;
      Fresh : constant VM.Backing_Pages := [16#5000#, 16#6000#, 16#7000#, 16#8000#];
   begin
      VM.Initialize (Source, [4096, 8192, 12288, 16384], Accepted);
      Binding.Bind (Object, Source, First, 42, First, ID, 4096, 0, 4096, Accepted);
      pragma Assert (Accepted);
      VM.Seal (Source, Accepted); pragma Assert (Accepted);
      Binding.Prepare_Change (Object, Source, Rejected, Fresh, First,
        43, Second, ID, 8192, 0, 4096, False, Accepted);
      pragma Assert (not Accepted and VM.Used (Rejected) = 0);
      Binding.Prepare_Change (Object, Source, Rejected, Fresh, Second,
        42, First, ID, 8192, 0, 4096, False, Accepted);
      pragma Assert (not Accepted and VM.Used (Rejected) = 0);
      Binding.Prepare_Change (Object, Source, Rejected, Fresh, First,
        42, First, 2 ** 32, 8192, 0, 4096, False, Accepted);
      pragma Assert (not Accepted and VM.Used (Rejected) = 0);
      Ready := False;
      Binding.Prepare_Change (Object, Source, Rejected, Fresh, First,
        42, First, ID, 8192, 0, 4096, False, Accepted);
      pragma Assert (not Accepted and VM.Used (Rejected) = 0);
      Ready := True;
      Binding.Prepare_Change (Object, Source, Added, Fresh, First,
        42, First, ID, 8192, 0, 4096, False, Accepted);
      pragma Assert (Accepted and VM.Sealed (Added));
      pragma Assert (VM.Lookup (Source, 8192) = 0 and VM.Sealed (Source));
      pragma Assert (VM.Lookup (Added, 8192) = VM.Lookup (Source, 4096));
      Binding.Prepare_Change (Object, Added, Removed,
        [16#9000#, 16#A000#, 16#B000#, 16#C000#], First,
        42, First, ID, 4096, 0, 4096, True, Accepted);
      pragma Assert (Accepted and VM.Sealed (Removed));
      pragma Assert (VM.Lookup (Removed, 4096) = 0 and VM.Lookup (Added, 4096) /= 0);
      pragma Assert (VM.Lookup (Removed, 8192) = VM.Lookup (Added, 8192));
      -- A source with a missing removal range is never partially changed.
      Binding.Prepare_Change (Object, Removed, Rejected, Fresh, First,
        42, First, ID, 12288, 0, 4096, True, Accepted);
      pragma Assert (not Accepted and not VM.Sealed (Rejected));
      pragma Assert (VM.Lookup (Removed, 8192) /= 0 and VM.Lookup (Removed, 4096) = 0);
      declare
         Wire_Added, Wire_Removed, Wire_Empty : VM.Image;
         Request : Buffers.Words;
         Status : Binding.Preparation_Result;
         use type Binding.Preparation_Result;
      begin
         for Check in 1 .. 3 loop
            Binding.Check_Update_Request (Object, Source, First, 7,
              42, First, Binding.Update_Label, 4, 0, 0,
              [1 + 7 * 2 ** 32, ID, 8192, 4096], Status);
            pragma Assert (Status = Binding.Eligible and Calls = 1);
         end loop;
         -- Geometrically valid wire ranges still must fit the owned BO;
         -- preflight must not burn the one-shot candidate or allocate RAM.
         for Bad_Range in 1 .. 3 loop
            Request := [1 + 7 * 2 ** 32, ID, 8192, 4096];
            case Bad_Range is
               when 1 => Request (1) := ID + 2 ** 32;
               when 2 => Request (3) := 8192;
               when 3 => Request (1) := 16#FFFF_FFFF#;
            end case;
            Binding.Prepare_Request (Object, Source, Wire_Added, Fresh, First, 7,
              42, First, Binding.Update_Label, 4, 0, 0, Request, Status);
            pragma Assert (Status = Binding.Not_Ready and Calls = 1 and
              VM.Used (Wire_Added) = 0);
         end loop;
         for Bad in 0 .. 17 loop
            Request := [1 + 7 * 2 ** 32, ID, 8192, 4096];
            case Bad is
               when 4 => Request (0) := Request (0) + 1;
               when 5 => Request (0) := Request (0) + 2 * 2 ** 16;
               when 6 => Request (1) := 0;
               when 7 => Request (1) := ID + 16#FFFF_FFFF# * 2 ** 32;
               when 8 => Request (2) := 0;
               when 9 => Request (2) := 8193;
               when 10 => Request (2) := 2 ** 48;
               when 11 => Request (2) := 2 ** 48 - 4096; Request (3) := 8192;
               when 12 => Request (3) := 0;
               when 13 => Request (3) := 4097;
               when 14 => Request (3) := 16 * 1024 * 1024 + 4096;
               when 15 => Request (0) := 1 + 6 * 2 ** 32;
               when others => null;
            end case;
            Binding.Prepare_Request (Object, Source, Wire_Added, Fresh, First,
              (if Bad = 16 then 2 ** 32 - 1 else 7),
              (if Bad = 17 then 43 else 42), (if Bad = 17 then Second else First),
              (if Bad = 0 then Binding.Update_Label + 1 else Binding.Update_Label),
              (if Bad = 1 then 3 else 4), (if Bad = 2 then 1 else 0),
              (if Bad = 3 then 1 else 0), Request, Status);
            pragma Assert (Status =
              (if Bad = 15 then Binding.Stale_Generation
               elsif Bad = 16 then Binding.Not_Ready
               elsif Bad = 17 then Binding.Request_Denied else Binding.Malformed));
            pragma Assert (VM.Used (Wire_Added) = 0 and VM.Lookup (Source, 8192) = 0);
         end loop;
         Binding.Prepare_Request (Object, Source, Wire_Added, Fresh, First, 7,
           42, First, Binding.Update_Label, 4, 0, 0,
           [1 + 7 * 2 ** 32, ID, 8192, 4096], Status);
         pragma Assert (Status = Binding.Prepared and VM.Sealed (Wire_Added));
         pragma Assert (VM.Lookup (Wire_Added, 8192) = VM.Lookup (Source, 4096));
         Binding.Prepare_Request (Object, Wire_Added, Wire_Removed,
           [16#9000#, 16#A000#, 16#B000#, 16#C000#], First, 8,
           42, First, Binding.Update_Label, 4, 0, 0,
           [1 + 2 ** 16 + 8 * 2 ** 32, ID, 8192, 4096], Status);
         pragma Assert (Status = Binding.Prepared and VM.Sealed (Wire_Removed));
         pragma Assert (VM.Lookup (Wire_Removed, 8192) = 0);
         pragma Assert (VM.Lookup (Wire_Added, 8192) /= 0 and VM.Sealed (Source));
         Binding.Prepare_Request (Object, Wire_Removed, Wire_Empty,
           [16#D000#, 16#E000#, 16#F000#, 16#10000#], First, 9,
           42, First, Binding.Update_Label, 4, 0, 0,
           [1 + 2 ** 16 + 9 * 2 ** 32, ID, 4096, 4096], Status);
         pragma Assert (Status = Binding.Prepared and VM.Sealed (Wire_Empty));
         pragma Assert (VM.Lookup (Wire_Empty, 4096) = 0 and
           VM.Lookup (Wire_Empty, 8192) = 0 and VM.Lookup (Wire_Removed, 4096) /= 0);
      end;
   end;
   Call (Buffers.Close, ID, Sender => 43, Stamp => Second);
   pragma Assert (Reply (0) = Buffers.Denied);
   Ready := False;
   Call (Buffers.Create, 4096);
   pragma Assert (Reply (0) = Buffers.Unavailable and Calls = 1);
   Call (Buffers.Close, ID); -- cleanup does not depend on device readiness
   pragma Assert (Reply = [Buffers.OK, Buffers.Version, 0, 0]);
   declare
      Image : VM.Image;
   begin
      VM.Initialize (Image, [4096, 8192, 12288, 16384], Accepted);
      Ready := True;
      Binding.Bind (Object, Image, First, 42, First, ID, 4096, 0, 4096, Accepted);
      pragma Assert (not Accepted and VM.Used (Image) = 1);
      Ready := False;
   end;
   Call (Buffers.Close, ID); pragma Assert (Reply (0) = Buffers.Denied);
   Ready := True;
   Revoke_During_Allocation := True;
   Call (Buffers.Create, 4096);
   pragma Assert (Reply = [Buffers.Denied, Buffers.Version, 0, 0] and Calls = 2);
   Revoke_During_Allocation := False;
   -- A different live session can allocate; the interrupted slot stays spent.
   Call (Buffers.Create, 4096, Sender => 43, Stamp => Second);
   pragma Assert (Reply = [Buffers.OK, Buffers.Version, 2, 4096] and Calls = 3);
   Buffers.Retire_Session (Object, Second);
   Call (Buffers.Close, 2, Sender => 43, Stamp => Second);
   pragma Assert (Reply (0) = Buffers.Denied);
   Wrong_Size := True;
   Call (Buffers.Create, 4096, Sender => 43, Stamp => Second);
   pragma Assert (Reply (0) = Buffers.Unavailable and Calls = 4);
   pragma Assert (Buffers.Last_Allocation (Object) = Buffers.Backing_Size_Mismatch);
   Wrong_Size := False;
   Call (Buffers.Create, 4096, Sender => 43, Stamp => Second);
   pragma Assert (Reply (0) = Buffers.Unavailable and Calls = 4);
   pragma Assert (Buffers.Last_Allocation (Object) = Buffers.Quarantined);
   declare
      Other : Buffers.Service;
      Deferred : Buffers.Ticket;
      Consumed : Boolean;
   begin
      Lose_Owner := True;
      Buffers.Handle (Other, 43, Second, Buffers.Label, 4, 0, 0,
                      [Buffers.Version, Buffers.Create, 4096, 0], Reply, Deferred);
      pragma Assert (Deferred /= 0);
      Buffers.Complete (Other, Deferred, Allocate (Layout.Slot (Deferred), 1),
                        Reply, Consumed);
      pragma Assert (Consumed);
      pragma Assert (Reply (0) = Buffers.Unavailable and Calls = 5);
      Lose_Owner := False; Ready := True;
      Buffers.Handle (Other, 43, Second, Buffers.Label, 4, 0, 0,
                      [Buffers.Version, Buffers.Create, 4096, 0], Reply, Deferred);
      pragma Assert (Deferred = 0);
      pragma Assert (Reply (0) = Buffers.Unavailable and Calls = 5);
   end;
   declare
      Async : Buffers.Service;
      Deferred, Saved : Buffers.Ticket;
      Consumed : Boolean;
      Before : constant Natural := Calls;
   begin
      Buffers.Handle (Async, 43, Second, Buffers.Label, 4, 0, 0,
                      [Buffers.Version, Buffers.Create, 4096, 0], Reply, Saved);
      pragma Assert (Saved = 1 and Calls = Before); -- no callback or wait
      Buffers.Handle (Async, 43, Second, Buffers.Label, 4, 0, 0,
                      [Buffers.Version, Buffers.Create, 4096, 0], Reply, Deferred);
      pragma Assert (Deferred = 0 and Reply (0) = Buffers.Unavailable);
      Buffers.Complete (Async, 2, (Ready => False), Reply, Consumed);
      pragma Assert (not Consumed); -- unrelated completion leaves request pending
      Buffers.Retire_Session (Async, Second);
      Buffers.Complete (Async, Saved, Allocate (Layout.Slot (Saved), 1), Reply, Consumed);
      pragma Assert (Consumed and Reply (0) = Buffers.Denied);
      Buffers.Complete (Async, Saved, (Ready => False), Reply, Consumed);
      pragma Assert (not Consumed); -- duplicate cannot produce another reply
      Buffers.Handle (Async, 43, Second, Buffers.Label, 4, 0, 0,
                      [Buffers.Version, Buffers.Create, 4096, 0], Reply, Saved);
      pragma Assert (Saved = 2); -- busy request did not consume a ticket
      Buffers.Complete (Async, Saved, (Ready => False), Reply, Consumed);
      pragma Assert (Consumed and Reply (0) = Buffers.Unavailable);
      Buffers.Handle (Async, 43, Second, Buffers.Label, 4, 0, 0,
                      [Buffers.Version, Buffers.Create, 4096, 0], Reply, Saved);
      pragma Assert (Saved = 3);
      Buffers.Quarantine (Async);
      Buffers.Complete (Async, Saved, (Ready => False), Reply, Consumed);
      pragma Assert (Consumed and Reply (0) = Buffers.Unavailable);
   end;
   declare
      package Last_Slot is new Intel_GPU_Buffer_Requests
        (Resolve, Owner_Ready, First_Slot => Layout.Bootstrap_Slots);
      Last_Object : Last_Slot.Service;
      Last_Reply : Last_Slot.Words;
      Deferred : Last_Slot.Ticket;
      Consumed : Boolean;
   begin
      Last_Slot.Handle (Last_Object, 43, Second, Last_Slot.Label, 4, 0, 0,
                       [Last_Slot.Version, Last_Slot.Create, 4096, 0], Last_Reply, Deferred);
      pragma Assert (Deferred = Unsigned_64 (Layout.Bootstrap_Slots));
      for Generation in Unsigned_64 range 1 .. 32 loop
         declare Forged : constant Last_Slot.Ticket := Deferred + Generation * Last_Slot.Ticket_Stride; begin
            pragma Assert (Last_Slot.Ticket_Slot (Forged) = Layout.Bootstrap_Slots);
            pragma Assert (Last_Slot.Ticket_Session (Last_Object, Forged) = 0);
            Last_Slot.Complete (Last_Object, Forged, (Ready => False), Last_Reply, Consumed);
            pragma Assert (not Consumed and Last_Slot.Pending_For (Last_Object, Second));
            Last_Slot.Reject_Delivery (Last_Object, Forged);
         end;
      end loop;
      Last_Slot.Complete (Last_Object, Deferred, Allocate (Layout.Slot (Deferred), 1),
                         Last_Reply, Consumed);
      pragma Assert (Consumed and Last_Reply (0) = Last_Slot.OK);
      Last_Slot.Reject_Delivery (Last_Object, Deferred);
      Last_Slot.Reject_Delivery (Last_Object, Deferred);
      Last_Slot.Handle (Last_Object, 43, Second, Last_Slot.Label, 4, 0, 0,
                       [Last_Slot.Version, Last_Slot.Close, 1, 0], Last_Reply, Deferred);
      pragma Assert (Deferred = 0 and Last_Reply (0) = Last_Slot.Denied);
      Last_Slot.Handle (Last_Object, 43, Second, Last_Slot.Label, 4, 0, 0,
                       [Last_Slot.Version, Last_Slot.Create, 4096, 0], Last_Reply, Deferred);
      pragma Assert (Deferred = 0 and Last_Reply (0) = Last_Slot.Unavailable);
   end;
   declare
      Pool : Buffers.Service;
      Deferred, Other : Buffers.Ticket;
      Consumed : Boolean;
      Response : Buffers.Words;
   begin
      Buffers.Handle (Pool, 43, Second, Buffers.Label, 4, 0, 0,
        [1, Buffers.Create, 4096, 0], Response, Deferred);
      Buffers.Complete (Pool, Deferred, Allocate (Buffers.Ticket_Slot (Deferred), 1),
        Response, Consumed);
      pragma Assert (Consumed and Response (0) = Buffers.OK);
      for Generation in Unsigned_64 range 1 .. 32 loop
         Buffers.Reject_Delivery (Pool, Deferred + Buffers.Ticket_Stride * Generation);
      end loop;
      Buffers.Handle (Pool, 43, Second, Buffers.Label, 4, 0, 0,
        [1, Buffers.Close, Response (2), 0], Response, Other);
      pragma Assert (Response (0) = Buffers.OK); -- forged callbacks did not close it
   end;
   Ada.Text_IO.Put_Line ("Ticket identity PASS: forged generations cannot complete or reject a current allocation");
   declare
      Pool : Buffers.Service;
      Current, Next_ID, Other : Buffers.Ticket;
      Consumed, Accepted : Boolean;
      Response : Buffers.Words;
      Handle : Unsigned_64;
   begin
      Buffers.Handle (Pool, 43, Second, Buffers.Label, 4, 0, 0,
        [1, Buffers.Create, 4096, 0], Response, Current);
      Buffers.Complete (Pool, Current, Allocate (Buffers.Ticket_Slot (Current), 1),
        Response, Consumed);
      pragma Assert (Consumed and Response (0) = Buffers.OK);
      Handle := Response (2);
      for Cycle in 1 .. 128 loop
         pragma Assert (not Buffers.Closed_At (Pool, Buffers.Ticket_Slot (Current)).Ready);
         pragma Assert (not Buffers.Can_Retire (Pool, Second, Current));
         Buffers.Acknowledge_Retirement (Pool, Second, Current, True, Accepted);
         pragma Assert (not Accepted); -- still open
         Buffers.Handle (Pool, 43, Second, Buffers.Label, 4, 0, 0,
           [1, Buffers.Close, Handle, 0], Response, Other);
         pragma Assert (Response (0) = Buffers.OK);
         declare
            Candidate : constant Buffers.Closed_Allocation :=
              Buffers.Closed_At (Pool, Buffers.Ticket_Slot (Current));
         begin
            pragma Assert (Candidate.Ready and then Candidate.ID = Current and then
              Candidate.Session = Second and then Unsigned_64 (Candidate.Handle) = Handle and then
              Candidate.Generation = Unsigned_32 (Cycle));
         end;
         Buffers.Acknowledge_Retirement (Pool, Second, Current, False, Accepted);
         pragma Assert (not Accepted);
         pragma Assert (Buffers.Can_Retire (Pool, Second, Current));
         pragma Assert (not Buffers.Can_Retire (Pool, First, Current));
         pragma Assert (not Buffers.Can_Retire (Pool, Second, 0));
         pragma Assert (not Buffers.Can_Retire
           (Pool, Second, Current + Buffers.Ticket_Stride));
         Buffers.Acknowledge_Retirement (Pool, First, Current, True, Accepted);
         pragma Assert (not Accepted);
         Buffers.Acknowledge_Retirement (Pool, Second, Current, True, Accepted);
         pragma Assert (Accepted);
         pragma Assert (not Buffers.Closed_At (Pool, Buffers.Ticket_Slot (Current)).Ready);
         pragma Assert (not Buffers.Can_Retire (Pool, Second, Current));
         Buffers.Acknowledge_Retirement (Pool, Second, Current, True, Accepted);
         pragma Assert (not Accepted);
         Buffers.Reject_Delivery (Pool, Current); -- cannot erase retirement tombstone
         Buffers.Handle (Pool, 43, Second, Buffers.Label, 4, 0, 0,
           [1, Buffers.Create, 4096, 0], Response, Next_ID);
         pragma Assert (Next_ID = Current + Buffers.Ticket_Stride);
         pragma Assert (not Buffers.Closed_At (Pool, Buffers.Ticket_Slot (Current)).Ready);
         Buffers.Complete (Pool, Current, (Ready => False), Response, Consumed);
         pragma Assert (not Consumed);
         Buffers.Reject_Delivery (Pool, Current);
         Buffers.Complete (Pool, Next_ID, Allocate (Buffers.Ticket_Slot (Next_ID), 1),
           Response, Consumed);
         pragma Assert (Consumed and Response (0) = Buffers.OK and Response (2) = Handle + 1);
         Handle := Response (2);
         Current := Next_ID;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Application ticket reuse PASS:128 acknowledged cycles, closed handles, stale callbacks");
   for Lost in 1 .. 4 loop
      declare
         Device_Live, Session_Live : Boolean := True;
         function Device_Ready return Boolean is (Device_Live);
         function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
           (if Session_Live and Sender = 42 and Stamp = 99 then 99 else 0);
         package Deferred_Buffers is new Intel_GPU_Buffer_Requests (Session_Of, Device_Ready);
         package Deferred_Binding is new Deferred_Buffers.Binding (VM);
         State : Deferred_Buffers.Service;
         Source, Candidate : VM.Image;
         Response : Deferred_Buffers.Words;
         Ticket, Private_Ticket : Deferred_Buffers.Ticket;
         Status : Deferred_Binding.Preparation_Result;
         use type Deferred_Binding.Preparation_Result;
         Handle, Epoch : Unsigned_64;
         Consumed : Boolean;
      begin
         Deferred_Buffers.Handle (State, 42, 99, Deferred_Buffers.Label, 4, 0, 0,
           [1, Deferred_Buffers.Create, 4096, 0], Response, Ticket);
         Deferred_Buffers.Complete (State, Ticket,
           Intel_GPU_Buffer_Reply.From_Linear
             (16#1000_0000#, Layout.CPU_Base, 4096, 16#1000_0000#), Response, Consumed);
         pragma Assert (Consumed and Response (0) = Deferred_Buffers.OK);
         Handle := Response (2); Epoch := 0;
         VM.Initialize (Source, [4096, 8192, 12288, 16384], Accepted);
         Deferred_Binding.Bind (State, Source, 99, 42, 99, Handle, 4096, 0, 4096, Accepted);
         pragma Assert (Accepted);
         VM.Seal (Source, Accepted); pragma Assert (Accepted);
         Deferred_Binding.Check_Update_Request (State, Source, 99, Epoch, 42, 99,
           Deferred_Binding.Update_Label, 4, 0, 0, [1, Handle, 8192, 4096], Status);
         pragma Assert (Status = Deferred_Binding.Eligible);
         Deferred_Buffers.Reserve_Private (State, 99, Private_Ticket);
         pragma Assert (Private_Ticket /= 0);
         pragma Assert (Deferred_Buffers.Ticket_Session (State, Private_Ticket) = 99);
         -- Model event-loop changes while the supervisor allocation is pending.
         case Lost is
            when 1 => Device_Live := False;
            when 2 => Session_Live := False;
            when 3 =>
               Deferred_Buffers.Handle (State, 42, 99, Deferred_Buffers.Label, 4, 0, 0,
                 [1, Deferred_Buffers.Close, Handle, 0], Response, Ticket);
               pragma Assert (Response (0) = Deferred_Buffers.OK);
            when 4 => Epoch := 1;
         end case;
         Deferred_Binding.Prepare_Request (State, Source, Candidate,
           [16#5000#, 16#6000#, 16#7000#, 16#8000#], 99, Epoch,
           42, 99, Deferred_Binding.Update_Label, 4, 0, 0,
           [1, Handle, 8192, 4096], Status);
         pragma Assert (Status = (case Lost is
           when 2 => Deferred_Binding.Request_Denied,
           when 4 => Deferred_Binding.Stale_Generation,
           when others => Deferred_Binding.Not_Ready));
         pragma Assert (VM.Used (Candidate) = 0 and VM.Lookup (Source, 8192) = 0);
         Deferred_Buffers.Finish_Private (State, Private_Ticket, Consumed);
         pragma Assert (Consumed);
         Deferred_Buffers.Finish_Private (State, Private_Ticket, Consumed);
         pragma Assert (not Consumed);
      end;
   end loop;
   declare
      function Test_Session (Sender, Stamp : Unsigned_64) return Unsigned_64 is
        (if Sender = 7 and Stamp = 9 then 99 else 0);
      package P is new Intel_GPU_Buffer_Requests (Test_Session, Owner_Ready);
      Pool : P.Service;
      ID, App_ID, Old_ID, Initial_ID, Other : P.Ticket;
      Response : P.Words;
      Consumed : Boolean;
   begin
      Ready := True;
      P.Reserve_Private (Pool, 0, ID, Reclaimable => True);
      pragma Assert (ID = 0);
      P.Reserve_Private (Pool, 99, ID); -- pinned context-style allocation
      pragma Assert (ID = 1);
      P.Finish_Private (Pool, ID, Consumed); pragma Assert (Consumed);
      P.Acknowledge_Private_Retirement (Pool, 99, ID, True, Accepted);
      pragma Assert (not Accepted);
      P.Reserve_Private (Pool, 0, ID); -- pinned bootstrap allocation
      pragma Assert (ID = 2);
      P.Finish_Private (Pool, ID, Consumed); pragma Assert (Consumed);
      P.Acknowledge_Private_Retirement (Pool, 0, ID, True, Accepted);
      pragma Assert (not Accepted);
      P.Handle (Pool, 7, 9, P.Label, 4, 0, 0, [1, P.Create, 4096, 0], Response, App_ID);
      pragma Assert (App_ID = 3);
      P.Complete (Pool, App_ID, (Ready => False), Response, Consumed);
      pragma Assert (Consumed);
      P.Acknowledge_Private_Retirement (Pool, 99, App_ID, True, Accepted);
      pragma Assert (not Accepted);
      Initial_ID := 4; Old_ID := 0;
      for Generation in Unsigned_64 range 1 .. 128 loop
         P.Reserve_Private (Pool, 99, ID, Reclaimable => True);
         pragma Assert (ID = Initial_ID + (Generation - 1) * P.Ticket_Stride);
         pragma Assert (P.Ticket_Session (Pool, ID) = 99);
         P.Acknowledge_Private_Retirement (Pool, 99, ID, True, Accepted);
         pragma Assert (not Accepted); -- allocator still in flight
         if Old_ID /= 0 then
            P.Finish_Private (Pool, Old_ID, Consumed); pragma Assert (not Consumed);
            pragma Assert (P.Ticket_Session (Pool, Old_ID) = 0);
         end if;
         P.Finish_Private (Pool, ID, Consumed); pragma Assert (Consumed);
         P.Acknowledge_Private_Retirement (Pool, 99, Old_ID, True, Accepted);
         pragma Assert (not Accepted);
         P.Acknowledge_Private_Retirement (Pool, 100, ID, True, Accepted);
         pragma Assert (not Accepted);
         P.Acknowledge_Private_Retirement (Pool, 99, ID, False, Accepted);
         pragma Assert (not Accepted);
         P.Acknowledge_Retirement (Pool, 99, ID, True, Accepted);
         pragma Assert (not Accepted); -- not an application buffer
         Ready := False;
         P.Acknowledge_Private_Retirement (Pool, 99, ID, True, Accepted);
         pragma Assert (not Accepted);
         Ready := True;
         P.Acknowledge_Private_Retirement (Pool, 99, ID, True, Accepted);
         pragma Assert (Accepted);
         P.Acknowledge_Private_Retirement (Pool, 99, ID, True, Accepted);
         pragma Assert (not Accepted); -- duplicate acknowledgement
         if Generation = 1 then
            P.Handle (Pool, 7, 9, P.Label, 4, 0, 0, [1, P.Create, 4096, 0], Response, App_ID);
            pragma Assert (App_ID = 5); -- must not consume reusable private slot4
            P.Complete (Pool, App_ID, (Ready => False), Response, Consumed);
            pragma Assert (Consumed);
            P.Reserve_Private (Pool, 100, Other);
            pragma Assert (Other = 6); -- pinned storage cannot consume slot4
            P.Finish_Private (Pool, Other, Consumed); pragma Assert (Consumed);
         end if;
         Old_ID := ID;
      end loop;
      P.Retire_Session (Pool, 99);
      P.Acknowledge_Private_Retirement (Pool, 99, ID, True, Accepted);
      pragma Assert (not Accepted);
      P.Reserve_Private (Pool, 99, Other, Reclaimable => True);
      pragma Assert (Other = ID + Buffers.Ticket_Stride); -- exact acknowledged slot survives close
      P.Finish_Private (Pool, Other, Consumed); pragma Assert (Consumed);
      P.Quarantine (Pool);
      P.Acknowledge_Private_Retirement (Pool, 99, Other, True, Accepted);
      pragma Assert (not Accepted);
   end;
   Ada.Text_IO.Put_Line ("Private table tickets PASS:128 acknowledged generations, pinned/bootstrap/app separation, stale and cross-session rejection");
   declare
      function Test_Session (Sender, Stamp : Unsigned_64) return Unsigned_64 is (0);
      package P is new Intel_GPU_Buffer_Requests (Test_Session, Owner_Ready);
      Pool : P.Service;
      ID, Previous, Denied_ID : P.Ticket := 0;
      Previous_Session : Unsigned_64 := 0;
      Consumed : Boolean;
   begin
      Ready := True;
      for Session in Unsigned_64 range 1001 .. 1128 loop
         P.Reserve_Private (Pool, Session, ID, Reclaimable => True);
         pragma Assert (ID = 1 + (Session - 1001) * P.Ticket_Stride);
         pragma Assert (P.Ticket_Session (Pool, ID) = Session);
         if Previous /= 0 then
            P.Retire_Session (Pool, Previous_Session);
            pragma Assert (P.Pending_For (Pool, Session));
            pragma Assert (not P.Pending_For (Pool, Previous_Session));
            pragma Assert (P.Ticket_Session (Pool, Previous) = 0);
            P.Finish_Private (Pool, Previous, Consumed);
            pragma Assert (not Consumed);
         end if;
         P.Finish_Private (Pool, ID, Consumed); pragma Assert (Consumed);
         P.Acknowledge_Private_Retirement (Pool, Previous_Session, ID, True, Accepted);
         pragma Assert (not Accepted);
         P.Acknowledge_Private_Retirement (Pool, Session, Previous, True, Accepted);
         pragma Assert (not Accepted);
         P.Acknowledge_Private_Retirement (Pool, Session, ID, False, Accepted);
         pragma Assert (not Accepted);
         P.Acknowledge_Private_Retirement (Pool, Session, ID, True, Accepted);
         pragma Assert (Accepted);
         P.Retire_Session (Pool, Session);
         P.Acknowledge_Private_Retirement (Pool, Session, ID, True, Accepted);
         pragma Assert (not Accepted); -- no duplicate acknowledgement
         Ready := False;
         P.Reserve_Private (Pool, Session + 1, Denied_ID, Reclaimable => True);
         pragma Assert (Denied_ID = 0);
         Ready := True;
         Previous := ID; Previous_Session := Session;
      end loop;
      -- Closing an unacknowledged replacement must not make it reusable.
      P.Reserve_Private (Pool, 2000, ID, Reclaimable => True);
      P.Retire_Session (Pool, 2000);
      P.Finish_Private (Pool, ID, Consumed); pragma Assert (Consumed);
      P.Acknowledge_Private_Retirement (Pool, 2000, ID, True, Accepted);
      pragma Assert (not Accepted);
      P.Reserve_Private (Pool, 2001, ID, Reclaimable => True);
      pragma Assert (ID = 2); -- uncertain slot1 retained, not recycled
      P.Finish_Private (Pool, ID, Consumed); pragma Assert (Consumed);
   end;
   Ada.Text_IO.Put_Line ("Private table cross-session PASS:128 owner transitions, exact acknowledgements, stale completions and closed pending allocation retention");
   -- Closing admission must drain the outstanding allocation exactly once,
   -- including a replacement generation, without poisoning another session.
   declare
      Revoked : Boolean := False;
      function Race_Owner return Boolean is (True);
      function Race_Session (Sender, Stamp : Unsigned_64) return Unsigned_64 is
        (if Sender = 42 and Stamp = 101 and not Revoked then 101
         elsif Sender = 43 and Stamp = 102 then 102 else 0);
      package Race is new Intel_GPU_Buffer_Requests (Race_Session, Race_Owner);
      use type Race.Words;
      function Backing (Ticket : Race.Ticket) return Intel_GPU_Buffer_Reply.Backing is
         Offset : constant Unsigned_64 :=
           Unsigned_64 (Race.Ticket_Slot (Ticket) - 1) * 4096;
      begin
         return Intel_GPU_Buffer_Reply.From_Linear
           (16#1000_0000# + Offset, Layout.CPU_Base + Offset, 4096, 16#1000_0000#);
      end Backing;
   begin
      for Generation in 1 .. 32 loop
         for Scenario in 0 .. 2 loop
            declare
               Pool : Race.Service;
               Pending, Next_ID, Ignored : Race.Ticket;
               Response : Race.Words;
               Consumed, Ack : Boolean;
               Handle : Unsigned_64 := 0;
            begin
               Revoked := False;
               for Prior in 1 .. Generation - 1 loop
                  Race.Handle (Pool, 42, 101, Race.Label, 4, 0, 0,
                    [1, Race.Create, 4096, 0], Response, Pending);
                  pragma Assert (Pending = 1 + Unsigned_64 (Prior - 1) * Race.Ticket_Stride);
                  Race.Complete (Pool, Pending, Backing (Pending), Response, Consumed);
                  pragma Assert (Consumed and Response (0) = Race.OK);
                  Handle := Response (2);
                  Race.Handle (Pool, 42, 101, Race.Label, 4, 0, 0,
                    [1, Race.Close, Handle, 0], Response, Ignored);
                  pragma Assert (Response (0) = Race.OK);
                  Race.Acknowledge_Retirement (Pool, 101, Pending, True, Ack);
                  pragma Assert (Ack);
               end loop;
               Race.Handle (Pool, 42, 101, Race.Label, 4, 0, 0,
                 [1, Race.Create, 4096, 0], Response, Pending);
               pragma Assert (Pending = 1 + Unsigned_64 (Generation - 1) * Race.Ticket_Stride);
               case Scenario is
                  when 0 => Race.Retire_Session (Pool, 101);
                  when 1 => Revoked := True;
                  when others => Race.Retire_Session (Pool, 102);
               end case;
               pragma Assert (Race.Pending_For (Pool, 101));
               Race.Complete (Pool, Pending + Race.Ticket_Stride, Backing (Pending), Response, Consumed);
               pragma Assert (not Consumed and Race.Pending_For (Pool, 101));
               Race.Complete (Pool, Pending, Backing (Pending), Response, Consumed);
               pragma Assert (Consumed and not Race.Pending_For (Pool, 101));
               if Scenario < 2 then
                  pragma Assert (Response = [Race.Denied, 1, 0, 0]);
                  pragma Assert (not Race.Closed_At (Pool, 1).Ready);
                  Race.Acknowledge_Retirement (Pool, 101, Pending, True, Ack);
                  pragma Assert (not Ack); -- cancelled allocation stays retained
               else
                  pragma Assert (Response (0) = Race.OK and Response (2) /= 0);
               end if;
               Race.Handle (Pool, 43, 102, Race.Label, 4, 0, 0,
                 [1, Race.Create, 4096, 0], Response, Next_ID);
               pragma Assert (Next_ID = 2 and Race.Pending_For (Pool, 102));
               Race.Complete (Pool, Pending, Backing (Pending), Response, Consumed);
               pragma Assert (not Consumed and Race.Pending_For (Pool, 102));
               Race.Reject_Delivery (Pool, Pending);
               Race.Complete (Pool, Next_ID, Backing (Next_ID), Response, Consumed);
               pragma Assert (Consumed and Response (0) = Race.OK);
               pragma Assert (not Race.Pending_For (Pool, 102));
            end;
         end loop;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Allocation close races PASS:96 fresh/reused cancellation, revocation, unrelated-close cases; next-session completion isolated");
   declare
      Authorized : Unsigned_64 := 1000;
      function Cross_Owner return Boolean is (True);
      function Cross_Session (Sender, Stamp : Unsigned_64) return Unsigned_64 is
        (if Sender = 42 and Stamp = Authorized then Authorized else 0);
      package Cross is new Intel_GPU_Buffer_Requests (Cross_Session, Cross_Owner);
      Pool : Cross.Service;
      Current, Old, Ignored : Cross.Ticket := 0;
      Response : Cross.Words;
      Handle, Old_Handle : Unsigned_64 := 0;
      Consumed, Ack : Boolean;
      B : constant Intel_GPU_Buffer_Reply.Backing :=
        Intel_GPU_Buffer_Reply.From_Linear (16#1000_0000#, Layout.CPU_Base, 4096, 16#1000_0000#);
   begin
      for Cycle in 1 .. 128 loop
         Cross.Handle (Pool, 42, Authorized, Cross.Label, 4, 0, 0,
           [1, Cross.Create, 4096, 0], Response, Current);
         pragma Assert (Current = 1 + Unsigned_64 (Cycle - 1) * Buffers.Ticket_Stride);
         pragma Assert (Cross.Ticket_Session (Pool, Current) = Authorized);
         if Old /= 0 then
            pragma Assert (Cross.Ticket_Session (Pool, Old) = 0);
            Cross.Complete (Pool, Old, B, Response, Consumed);
            pragma Assert (not Consumed and Cross.Pending_For (Pool, Authorized));
            Cross.Reject_Delivery (Pool, Old);
            Cross.Retire_Session (Pool, Authorized - 1);
            pragma Assert (Cross.Pending_For (Pool, Authorized));
         end if;
         Cross.Complete (Pool, Current, B, Response, Consumed);
         pragma Assert (Consumed and Response (0) = Cross.OK);
         Handle := Response (2);
         pragma Assert (Handle = Unsigned_64 (Cycle));
         if Old /= 0 then
            Cross.Handle (Pool, 42, Authorized - 1, Cross.Label, 4, 0, 0,
              [1, Cross.Close, Handle, 0], Response, Ignored);
            pragma Assert (Response (0) = Cross.Denied);
            Cross.Handle (Pool, 42, Authorized, Cross.Label, 4, 0, 0,
              [1, Cross.Close, Old_Handle, 0], Response, Ignored);
            pragma Assert (Response (0) = Cross.Denied);
         end if;
         Cross.Handle (Pool, 42, Authorized, Cross.Label, 4, 0, 0,
           [1, Cross.Close, Handle, 0], Response, Ignored);
         pragma Assert (Response (0) = Cross.OK);
         Cross.Acknowledge_Retirement (Pool, Authorized, Current, True, Ack);
         pragma Assert (Ack);
         Cross.Retire_Session (Pool, Authorized);
         Old := Current; Old_Handle := Handle;
         Authorized := Authorized + 1;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Cross-session app tickets PASS:128 owners reuse acknowledged slot; stale handle/completion/close rejected");
   declare
      Admission : Boolean := True;
      Device_Owned : Boolean := True;
      function Cleanup_Owner return Boolean is (Device_Owned);
      function Cleanup_Session (Sender, Stamp : Unsigned_64) return Unsigned_64 is
        (if Admission and Sender = 42 and Stamp = 1000 then 1000 else 0);
      package Cleanup is new Intel_GPU_Buffer_Requests (Cleanup_Session, Cleanup_Owner);
      Pool : Cleanup.Service;
      Current, Ignored : Cleanup.Ticket;
      Response : Cleanup.Words;
      Consumed, Ack : Boolean;
      Handle : Unsigned_64;
   begin
      Cleanup.Handle (Pool, 42, 1000, Cleanup.Label, 4, 0, 0,
        [1, Cleanup.Create, 4096, 0], Response, Current);
      Cleanup.Complete (Pool, Current, Allocate (Cleanup.Ticket_Slot (Current), 1),
        Response, Consumed);
      pragma Assert (Consumed and Response (0) = Cleanup.OK);
      Handle := Response (2);
      -- Teardown closes even an app that departed without a close RPC.
      Admission := False;
      Cleanup.Retire_Session (Pool, 1000);
      pragma Assert (Cleanup.Closed_At (Pool, Cleanup.Ticket_Slot (Current)).Ready);
      Cleanup.Handle (Pool, 42, 1000, Cleanup.Label, 4, 0, 0,
        [1, Cleanup.Create, 4096, 0], Response, Ignored);
      pragma Assert (Ignored = 0 and Response (0) = Cleanup.Denied);
      Cleanup.Handle (Pool, 42, 1000, Cleanup.Label, 4, 0, 0,
        [1, Cleanup.Close, Handle, 0], Response, Ignored);
      pragma Assert (Ignored = 0 and Response (0) = Cleanup.Denied);
      pragma Assert (Cleanup.Can_Retire (Pool, 1000, Current));
      Cleanup.Acknowledge_Retirement (Pool, 1000, Current, False, Ack);
      pragma Assert (not Ack);
      Cleanup.Acknowledge_Retirement (Pool, 1001, Current, True, Ack);
      pragma Assert (not Ack);
      Cleanup.Acknowledge_Retirement (Pool, 1000, Current + Cleanup.Ticket_Stride, True, Ack);
      pragma Assert (not Ack);
      Device_Owned := False;
      Cleanup.Acknowledge_Retirement (Pool, 1000, Current, True, Ack);
      pragma Assert (not Ack);
      Device_Owned := True;
      -- True is supplied by a trusted coordinator only after hardware/CPU and
      -- allocator receipts; this hosted test proves metadata gates, not DMA.
      Cleanup.Acknowledge_Retirement (Pool, 1000, Current, True, Ack);
      pragma Assert (Ack and not Cleanup.Can_Retire (Pool, 1000, Current));
      Cleanup.Acknowledge_Retirement (Pool, 1000, Current, True, Ack);
      pragma Assert (not Ack);
      pragma Assert (not Admission);
   end;
   Ada.Text_IO.Put_Line ("Revoked-session cleanup PASS: exact trusted receipt retires BO without restoring app admission");
   declare
      Authorized : Unsigned_64 := 1000;
      function Cancel_Owner return Boolean is (True);
      function Cancel_Session (Sender, Stamp : Unsigned_64) return Unsigned_64 is
        (if Sender = 42 and Stamp = Authorized then Authorized else 0);
      package Cross is new Intel_GPU_Buffer_Requests (Cancel_Session, Cancel_Owner);
      function B (Slot : Layout.Slot) return Intel_GPU_Buffer_Reply.Backing is
         Offset : constant Unsigned_64 := Unsigned_64 (Slot - 1) * 4096;
      begin
         return Intel_GPU_Buffer_Reply.From_Linear
           (16#1000_0000# + Offset, Layout.CPU_Base + Offset, 4096, 16#1000_0000#);
      end B;
   begin
      for Generation in 1 .. 32 loop
         for Scenario in 0 .. 3 loop
            declare
               Pool : Cross.Service;
               Old, Replacement, Next_ID, Ignored : Cross.Ticket;
               Response : Cross.Words;
               Old_Handle, New_Handle : Unsigned_64 := 0;
               Consumed, Ack : Boolean;
            begin
               Authorized := 1000;
               for Prior in 1 .. Generation loop
                  Cross.Handle (Pool, 42, 1000, Cross.Label, 4, 0, 0,
                    [1, Cross.Create, 4096, 0], Response, Old);
                  Cross.Complete (Pool, Old, B (1), Response, Consumed);
                  pragma Assert (Consumed and Response (0) = Cross.OK);
                  Old_Handle := Response (2);
                  Cross.Handle (Pool, 42, 1000, Cross.Label, 4, 0, 0,
                    [1, Cross.Close, Old_Handle, 0], Response, Ignored);
                  pragma Assert (Response (0) = Cross.OK);
                  Cross.Acknowledge_Retirement (Pool, 1000, Old, True, Ack);
                  pragma Assert (Ack);
               end loop;
               Cross.Retire_Session (Pool, 1000);
               Authorized := 1001;
               Cross.Handle (Pool, 42, 1001, Cross.Label, 4, 0, 0,
                 [1, Cross.Create, 4096, 0], Response, Replacement);
               pragma Assert (Replacement = Old + Buffers.Ticket_Stride);
               pragma Assert (Cross.Ticket_Session (Pool, Replacement) = 1001);
               Cross.Retire_Session (Pool, 1000);
               pragma Assert (Cross.Pending_For (Pool, 1001));
               case Scenario is
                  when 0 => Cross.Retire_Session (Pool, 1001);
                  when 1 => Authorized := 1002;
                  when others => null;
               end case;
               Cross.Complete (Pool, Replacement,
                 (if Scenario = 2 then (Ready => False) else B (1)), Response, Consumed);
               pragma Assert (Consumed and not Cross.Pending_For (Pool, 1001));
               if Scenario = 3 then
                  pragma Assert (Response (0) = Cross.OK);
                  New_Handle := Response (2);
                  Cross.Reject_Delivery (Pool, Replacement);
                  Cross.Handle (Pool, 42, 1001, Cross.Label, 4, 0, 0,
                    [1, Cross.Close, New_Handle, 0], Response, Ignored);
                  pragma Assert (Response (0) = Cross.Denied);
               else
                  pragma Assert (Response (0) =
                    (if Scenario = 2 then Cross.Unavailable else Cross.Denied));
                  pragma Assert (Response (2) = 0 and Response (3) = 0);
               end if;
               -- A retained/failed generation cannot return to the reuse pool
               -- merely because the next authenticated owner arrives.
               Authorized := 1002;
               Cross.Handle (Pool, 42, 1002, Cross.Label, 4, 0, 0,
                 [1, Cross.Create, 4096, 0], Response, Next_ID);
               pragma Assert (Next_ID = 2);
               Cross.Complete (Pool, Replacement, B (1), Response, Consumed);
               pragma Assert (not Consumed and Cross.Pending_For (Pool, 1002));
               Cross.Reject_Delivery (Pool, Old);
               Cross.Reject_Delivery (Pool, Replacement);
               Cross.Complete (Pool, Next_ID, B (2), Response, Consumed);
               pragma Assert (Consumed and Response (0) = Cross.OK);
            end;
         end loop;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Cross-session cancellation PASS:128 reused-slot cancellation/revocation/allocation-failure/reply-loss cases; failed backing retained");
   Ada.Text_IO.Put_Line ("GPU buffer requests PASS: sessions, opaque handles, retirement, allocation races, deferred-update revalidation");
   declare
      Available : Boolean := True;
      function Owner return Boolean is (Available);
      function Session (Sender, Stamp : Unsigned_64) return Unsigned_64 is
        (if Sender = 7 and Stamp = 8 then 99 else 0);
      package Growth is new Intel_GPU_Buffer_Requests (Session, Owner);
      type RAM is array (Natural range 0 .. 2047) of Unsigned_64;
      Tickets, Handles : RAM := [others => 16#CAFE#] with Alignment => 4096;
      Ticket_Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Tickets'Address));
      Handle_Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Handles'Address));
      Object : Growth.Service;
      ID, Other : Growth.Ticket;
      Response : Growth.Words;
      OK, Consumed : Boolean;
      Name : Unsigned_64;
      Before : Positive;
      Admitted : Positive;
   begin
      Growth.Reserve_Private (Object, 99, ID, Reclaimable => True);
      Growth.Extend_Tickets (Object, Ticket_Base, 4096, OK);
      pragma Assert (not OK and Growth.Record_Capacity (Object) = 16);
      Growth.Extend_Handles (Object, Handle_Base, 4096, OK);
      pragma Assert (not OK and Growth.Handle_Capacity (Object) = 16);
      Growth.Finish_Private (Object, ID, Consumed);
      pragma Assert (Consumed);
      Growth.Acknowledge_Private_Retirement (Object, 99, ID, True, OK);
      pragma Assert (OK);
      Growth.Extend_Tickets (Object, Ticket_Base, 4096, OK);
      pragma Assert (OK and Growth.Record_Capacity (Object) > 16);
      pragma Assert (Growth.Committed_Slots (Object) = 16);
      Growth.Admit_Slots (Object, 17, (others => 1000), OK);
      pragma Assert (not OK); -- handle storage has not grown yet
      Growth.Extend_Handles (Object, Handle_Base, 4096, OK);
      pragma Assert (OK and Growth.Handle_Capacity (Object) > 16);
      for Missing in 1 .. 4 loop
         declare
            Limits : Growth.Supporting_Capacities := (others => 1000);
         begin
            case Missing is
               when 1 => Limits.Backing := 16;
               when 2 => Limits.Replacements := 16;
               when 3 => Limits.Retirement := 16;
               when 4 => Limits.Update_Index := 16;
               when others => null;
            end case;
            Growth.Admit_Slots (Object, 17, Limits, OK);
            pragma Assert (not OK and Growth.Committed_Slots (Object) = 16);
         end;
      end loop;
      Growth.Admit_Slots (Object, 17, (others => 1000), OK);
      pragma Assert (OK and Growth.Committed_Slots (Object) = 17);
      Growth.Admit_Slots (Object, 16, (others => 1000), OK);
      pragma Assert (not OK and Growth.Committed_Slots (Object) = 17);
      Growth.Handle (Object, 7, 8, Growth.Label, 4, 0, 0,
        [1, Growth.Create, 4096, 0], Response, ID);
      pragma Assert (ID = 2);
      Growth.Admit_Slots (Object, 18, (others => 1000), OK);
      pragma Assert (not OK and Growth.Committed_Slots (Object) = 17);
      Before := Growth.Record_Capacity (Object);
      Growth.Extend_Tickets (Object, Ticket_Base, 8192, OK);
      pragma Assert (not OK and Growth.Record_Capacity (Object) = Before);
      Growth.Complete (Object, ID, Intel_GPU_Buffer_Reply.From_Linear
        (16#1000_1000#, Layout.CPU_Base + 4096, 4096, 16#1000_0000#), Response, Consumed);
      pragma Assert (Consumed and Response (0) = Growth.OK);
      Name := Response (2);
      Growth.Extend_Tickets (Object, Ticket_Base, 8192, OK);
      pragma Assert (OK and Growth.Ticket_Session (Object, ID) = 99);
      Growth.Extend_Handles (Object, Handle_Base, 8192, OK);
      pragma Assert (OK);
      Admitted := Positive'Min (64, Positive'Min
        (Growth.Record_Capacity (Object), Growth.Handle_Capacity (Object)));
      Growth.Admit_Slots (Object, Admitted, (others => 1000), OK);
      pragma Assert (OK and Growth.Committed_Slots (Object) = Admitted);
      -- Growth must preserve the retirement receipt of the original private
      -- slot, not just active application handles. Reuse is generation-tagged.
      Growth.Reserve_Private (Object, 99, Other, Reclaimable => True);
      pragma Assert (Other = 1 + Growth.Ticket_Stride);
      pragma Assert (Growth.Ticket_Session (Object, 1) = 0);
      pragma Assert (Growth.Ticket_Session (Object, Other) = 99);
      Growth.Finish_Private (Object, 1, Consumed);
      pragma Assert (not Consumed and Growth.Pending_For (Object, 99));
      Growth.Finish_Private (Object, Other, Consumed); pragma Assert (Consumed);
      Growth.Acknowledge_Private_Retirement (Object, 99, 1, True, OK);
      pragma Assert (not OK);
      Growth.Acknowledge_Private_Retirement (Object, 100, Other, True, OK);
      pragma Assert (not OK);
      Growth.Acknowledge_Private_Retirement (Object, 99, Other, True, OK);
      pragma Assert (OK);
      Growth.Handle (Object, 7, 8, Growth.Label, 4, 0, 0,
        [1, Growth.Close, Name, 0], Response, Other);
      pragma Assert (Response (0) = Growth.OK and Growth.Can_Retire (Object, 99, ID));
      for Index in 3 .. Positive'Min (64, Positive'Min
        (Growth.Record_Capacity (Object), Growth.Handle_Capacity (Object))) loop
         Growth.Handle (Object, 7, 8, Growth.Label, 4, 0, 0,
           [1, Growth.Create, 4096, 0], Response, Other);
         pragma Assert (Other = Unsigned_64 (Index));
         Growth.Complete (Object, Other, Intel_GPU_Buffer_Reply.From_Linear
           (16#1000_0000# + Unsigned_64 (Index - 1) * 4096,
            Layout.CPU_Base + Unsigned_64 (Index - 1) * 4096,
            4096, 16#1000_0000#), Response, Consumed);
         pragma Assert (Consumed and Response (0) = Growth.OK);
      end loop;
      pragma Assert (Other > 16);
      pragma Assert (Growth.Ticket_Session (Object, Unsigned_64 (Layout.Slot'Last)) = 0);
      pragma Assert (not Growth.Closed_At (Object, Layout.Slot'Last).Ready);
      pragma Assert (not Growth.Can_Retire (Object, 99, Unsigned_64 (Layout.Slot'Last)));
      Before := Growth.Record_Capacity (Object);
      Available := False;
      Growth.Admit_Slots (Object, Admitted, (others => 1000), OK);
      pragma Assert (not OK and Growth.Committed_Slots (Object) = Admitted);
      Growth.Extend_Tickets (Object, Ticket_Base, 12288, OK);
      pragma Assert (not OK and Growth.Record_Capacity (Object) = Before);
      Available := True;
      Growth.Quarantine (Object);
      Growth.Admit_Slots (Object, Admitted, (others => 1000), OK);
      pragma Assert (not OK and Growth.Committed_Slots (Object) = Admitted);
      Growth.Extend_Tickets (Object, Ticket_Base, 12288, OK);
      pragma Assert (not OK);
      Growth.Extend_Handles (Object, Handle_Base, 12288, OK);
      pragma Assert (not OK);
      for I in 1024 .. Tickets'Last loop
         pragma Assert (Tickets (I) = 16#CAFE# and Handles (I) = 16#CAFE#);
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Request growth PASS: allocations beyond bootstrap, pending/revoked/quarantined rejection, old identities retained, uncommitted max-index rejected");
end Buffer_Requests_Tests;
