with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Image.Snapshots;
with Intel_GPU_VM_Materialize;
with Intel_GPU_VM_Update;
with Intel_GPU_ADLN_TLB_Invalidate;
with Intel_GPU_Context_Table;
with Intel_GPU_GuC_Context_Session;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
procedure VM_Update_Pipeline_Tests is
   procedure Run (Fail_Second_Invalidation : Boolean; Revoke_On_Resume : Boolean := False;
                  Keep_Disabled : Boolean := False; Empty_Remapping : Boolean := False;
                  Sparse : Boolean := False; Streamed : Boolean := False) is
      package VM is new Intel_GPU_VM_Image (8);
      package Topology is new VM.Growth;
      package Snapshots is new VM.Snapshots;
      Current_Image : VM.Image;
      type Page is array (Table_Index) of Unsigned_64;
      type Storage is array (Natural range 0 .. 17) of Page;
      RAM : Storage := [others => [others => 0]] with Alignment => 4096, Volatile;
      Images : array (Natural range 0 .. 2) of VM.Image;
      DMA : VM.Backing_Pages;
      OK, Disabled, Visible : Boolean := False;
      Step, Flushes, Resumes, Register_Writes : Natural := 0;
      Tick : Unsigned_64 := 0;
      function CPU (P : Natural) return Unsigned_64 is
        (Unsigned_64 (To_Integer (RAM (P)'Address)));
      package Life renames Intel_GPU_GuC_Context_Lifecycle;
      package Events renames Intel_GPU_GuC_Context_Event;
      use type Life.Phase;
      Queue_Count : Natural := 0;
      function Device_Ready return Boolean is (True);
      procedure Queue (Payload : Events.Words;
                       Result : out Life.Send_Result) is
      begin
         pragma Assert (Payload'Length > 0);
         Queue_Count := Queue_Count + 1; Result := Life.Queued;
      end Queue;
      procedure Retain (Payload : Events.Words; Fence : Unsigned_16;
                        Success : out Boolean) is
         pragma Unreferenced (Payload, Fence);
      begin Success := True; end Retain;
      package Driver is new Intel_GPU_GuC_Context_Session
        (Device_Ready, Queue, Retain);
      package Pool is new Intel_GPU_Context_Table
        (1, Driver, Device_Ready, Retain);
      Contexts : Pool.Table;
      Context_ID : Unsigned_32;
      use type Driver.Result;
      use type Pool.Dispatch_Result;
      function Owner return Boolean is
        (not Pool.Failed (Contexts) and then
         Pool.State (Contexts, Context_ID) /= Life.Quarantined);
      Session_Live : Boolean := True;
      function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
        (if Session_Live and Sender = 42 and Stamp = 99 then 99 else 0);
      package Buffers is new Intel_GPU_Buffer_Requests (Session_Of, Owner);
      package Binding is new Buffers.Binding (VM);
      Buffers_State : Buffers.Service;
      Response, Request : Buffers.Words;
      Ticket : Buffers.Ticket;
      Table_Tickets : array (1 .. 2) of Buffers.Ticket;
      Consumed : Boolean;
      Handle : Unsigned_64;
      procedure Submit (Action : Life.Operation) is
         Result : Driver.Result;
      begin
         Pool.Submit (Contexts, Context_ID, Action, Result);
         pragma Assert (Result = Driver.Queued);
      end Submit;
      procedure Ack (Runnable : Unsigned_32) is
         Result : Pool.Dispatch_Result;
         ID : Unsigned_32;
      begin
         Pool.Dispatch (Contexts, [16#90001002#, Context_ID, Runnable], 0, ID, Result);
         pragma Assert (Result = Pool.Delivered and ID = Context_ID);
      end Ack;
      procedure Reject_Work is
         Before : constant Natural := Queue_Count;
         Result : Driver.Result;
      begin
         pragma Assert (not Pool.Work_Allowed (Contexts, Context_ID));
         Pool.Notify_Work (Contexts, Context_ID, True, Result);
         pragma Assert (Result = Driver.Rejected and Queue_Count = Before);
      end Reject_Work;
      procedure Drain (Success : out Boolean);
      procedure Publish (Success : out Boolean);
      procedure Invalidate (Success : out Boolean);
      procedure Resume (Success : out Boolean);
      package Update is new Intel_GPU_VM_Update (Owner, Drain, Publish, Invalidate, Resume);
      procedure Array_Update is new Binding.Handle_Update (Update);
      procedure Handle_Update
        (Object : Buffers.Service; Source : VM.Image; Candidate : in out VM.Image;
         Tables : VM.Backing_Pages; State : in out Update.State;
         VM_Session, Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
         Length, Flags : Unsigned_8; Reserved : Unsigned_16;
         Request : Buffers.Words; Response : out Buffers.Words) is
         Count, Reads : Natural := 0;
         function Read_Page (Page : VM.Page_Number) return Unsigned_64 is
         begin
            Reads := Reads + 1;
            pragma Assert (Page = Reads and Page <= Count);
            return Tables (Page);
         end;
         procedure Stream_Update is new Binding.Handle_Update_From_Pages (Read_Page, Update);
      begin
         if Streamed then
            for P in Tables'Range loop
               exit when Tables (P) = 0;
               Count := P;
            end loop;
            Stream_Update (Object, Source, Candidate, Count, State,
              VM_Session, Sender, Stamp, Request_Label, Length, Flags, Reserved, Request, Response);
            if Response (0) = Buffers.Denied then pragma Assert (Reads = 0); end if;
            if Response (0) = Buffers.OK then pragma Assert (Reads = Count); end if;
         else
            Array_Update (Object, Source, Candidate, Tables, State,
              VM_Session, Sender, Stamp, Request_Label, Length, Flags, Reserved, Request, Response);
         end if;
      end Handle_Update;
      State : Update.State;
      Status : Update.Result;
      use type Update.Result;
      function Exclusive return Boolean is
        (Disabled and then Pool.State (Contexts, Context_ID) = Life.Disabled
         and then not Pool.Work_Allowed (Contexts, Context_ID)
         and then not Update.Can_Submit (State));
      function Flush (Address : Unsigned_64) return Boolean is
      begin
         pragma Assert (Exclusive);
         Flushes := Flushes + 1;
         if Address = CPU (0) then
            pragma Assert (Flushes = VM.Used (Images (Step)) + 1);
            Visible := True;
         else
            pragma Assert (not Visible);
         end if;
         return True; -- host fixture, not a device cache-coherence claim
      end Flush;
      package Writer is new Intel_GPU_VM_Materialize (VM, Exclusive, Flush);
      Backing : Writer.Mappings;
      Publications : array (Positive range 1 .. 2) of Writer.State;
      function TLB_Gate return Boolean is (Exclusive and Visible);
      procedure Write_Register (Offset, Value : Unsigned_32; Success : out Boolean) is
      begin
         pragma Assert (TLB_Gate and Value = 1);
         Register_Writes := Register_Writes + 1;
         pragma Assert (Offset = (if Register_Writes mod 2 = 1 then 16#CED8# else 16#CEEC#));
         Success := True;
      end Write_Register;
      procedure Read_Register (Offset : Unsigned_32; Value : out Unsigned_32;
                               Success : out Boolean) is
      begin
         pragma Assert (TLB_Gate and Offset in 16#CED8# | 16#CEEC#);
         Value := (if Fail_Second_Invalidation and Step = 2 then 1 else 0);
         Success := True; -- simulated completion, or permanently busy
      end Read_Register;
      procedure Clock (Value : out Unsigned_64; Success : out Boolean) is
      begin Tick := Tick + 1; Value := Tick; Success := True; end Clock;
      package TLB is new Intel_GPU_ADLN_TLB_Invalidate
        (TLB_Gate, Write_Register, Read_Register, Clock);
      Attempts : array (Positive range 1 .. 2) of TLB.Attempt;
      use type TLB.Result;
      procedure Drain (Success : out Boolean) is
      begin
         pragma Assert (not Update.Can_Submit (State));
         -- GPU flush completion remains assumed; GuC scheduling transitions
         -- now use the production table/session decoder with simulated events.
         Pool.Hold_Work (Contexts, Context_ID, Success);
         pragma Assert (Success);
         Reject_Work;
         if not Disabled then
            Submit (Life.Disable);
            Reject_Work;
            Ack (0);
         end if;
         Reject_Work;
         Disabled := True; Visible := False; Flushes := 0;
         Success := True;
      end Drain;
      procedure Publish (Success : out Boolean) is
      begin
         Reject_Work;
         for P in VM.Page_Number loop
            Backing (P) := (CPU ((Step - 1) * 8 + P),
                            VM.Page_DMA (Images (Step), P));
         end loop;
         Writer.Publish_Update (Publications (Step), Current_Image, Images (Step),
                                Backing, (CPU (0), 4096), Success);
      end Publish;
      procedure Invalidate (Success : out Boolean) is
         Result : TLB.Result;
      begin
         Reject_Work;
         TLB.Execute (Attempts (Step), Result, Poll_Limit => 4);
         Success := Result = TLB.Complete;
      end Invalidate;
      procedure Resume (Success : out Boolean) is
      begin
         pragma Assert (TLB_Gate);
         Reject_Work;
         if not Keep_Disabled then
            Submit (Life.Enable);
            Reject_Work;
            Ack (1);
         end if;
         -- Scheduling enable is not permission for new application work
         -- until the coordinator has committed its new VM generation.
         Reject_Work;
         Resumes := Resumes + 1;
         Disabled := Keep_Disabled; Success := True;
         if Revoke_On_Resume and Step = 2 then Session_Live := False; end if;
      end Resume;
   begin
      Pool.Open (Contexts, 16#200000#, 4096, 1000, 500000,
                 False, Context_ID, OK, Session => 99);
      pragma Assert (OK);
      Submit (Life.Register_Context); Submit (Life.Set_Policy);
      Submit (Life.Enable); Ack (1);
      if Keep_Disabled then
         Submit (Life.Disable); Ack (0); Disabled := True;
      end if;
      Buffers.Handle (Buffers_State, 42, 99, Buffers.Label, 4, 0, 0,
        [1, Buffers.Create, 8192, 0], Response, Ticket);
      pragma Assert (Ticket /= 0);
      Buffers.Complete (Buffers_State, Ticket, Intel_GPU_Buffer_Reply.From_Linear
        (16#1000_0000#, Intel_GPU_Buffer_Backing.CPU_Base, 8192, 16#1000_0000#),
        Response, OK);
      pragma Assert (OK and Response (0) = Buffers.OK);
      Handle := Response (2);
      for P in VM.Page_Number loop
         DMA (P) := Unsigned_64 (P) * 4096;
      end loop;
      VM.Initialize (Images (0), DMA, OK); pragma Assert (OK);
      Binding.Bind (Buffers_State, Images (0), 99, 42, 99, Handle,
                    4096, 0, 4096, OK);
      pragma Assert (OK);
      VM.Seal (Images (0), OK); pragma Assert (OK);
      VM.Initialize (Current_Image, DMA, OK); pragma Assert (OK);
      Binding.Bind (Buffers_State, Current_Image, 99, 42, 99, Handle,
                    4096, 0, 4096, OK); pragma Assert (OK);
      VM.Seal (Current_Image, OK); pragma Assert (OK);
      for I in Table_Index loop RAM (0) (I) := VM.Entry_Value (Images (0), 1, I); end loop;
      for Generation in 1 .. 2 loop
         Step := Generation;
         Buffers.Reserve_Private (Buffers_State, 99, Table_Tickets (Generation), Reclaimable => True);
         pragma Assert (Table_Tickets (Generation) /= 0);
         for P in VM.Page_Number loop
            DMA (P) := Unsigned_64 (Generation) * 16#100000# + Unsigned_64 (P) * 4096;
         end loop;
         Request :=
           [1 + Unsigned_64 (Generation - 1) * 2 ** 32 +
              (if Generation = 2 then 2 ** 16 else 0),
            Handle + (if Generation = 1 then 2 ** 32 else 0),
            (if Generation = 1 then 2 ** 39 else 4096), 4096];
         if Empty_Remapping then
            Request :=
              [1 + Unsigned_64 (Generation - 1) * 2 ** 32 +
                 (if Generation = 1 then 2 ** 16 else 0),
               Handle + (if Generation = 2 then 2 ** 32 else 0),
               (if Generation = 1 then 4096 else 2 ** 39), 4096];
         end if;
         if Sparse then
            declare
               Required : Natural := VM.Used (Current_Image);
            begin
               if (Shift_Right (Request (0), 16) and 16#FFFF#) = 0 then
                  Required := Required + Topology.Inspect
                    (Current_Image, Request (2), Request (3)).Additional_Tables;
               end if;
               for P in Required + 1 .. VM.Page_Number'Last loop DMA (P) := 0; end loop;
            end;
         end if;
         -- Neither foreign identity nor stale epoch may consume the candidate
         -- or issue hardware/context callbacks.
         Handle_Update (Buffers_State, Current_Image, Images (Generation),
           DMA, State, 99, 43, 99, Binding.Update_Label, 4, 0, 0, Request, Response);
         pragma Assert (Response (0) = Buffers.Denied and Response (2) = 0);
         Request (0) := Request (0) + 2 ** 32;
         Handle_Update (Buffers_State, Current_Image, Images (Generation),
           DMA, State, 99, 42, 99, Binding.Update_Label, 4, 0, 0, Request, Response);
         pragma Assert (Response (0) = Buffers.Unavailable and Response (2) = 0 and
           VM.Used (Images (Generation)) = 0 and Register_Writes = (Generation - 1) * 2);
         Request (0) := Request (0) - 2 ** 32;
         Handle_Update (Buffers_State, Current_Image, Images (Generation),
           DMA, State, 99, 42, 99, Binding.Update_Label, 4, 0, 0, Request, Response);
         Buffers.Finish_Private (Buffers_State, Table_Tickets (Generation), Consumed);
         pragma Assert (Consumed);
         pragma Assert (VM.Sealed (Images (Generation)));
         pragma Assert (Register_Writes = Generation * 2);
         for I in Table_Index loop
            pragma Assert (RAM (0) (I) = VM.Entry_Value (Images (Generation), 1, I));
         end loop;
         if Generation = 2 and Revoke_On_Resume then
            pragma Assert (Response (0) = Buffers.Unavailable and Response (2) = 0);
            pragma Assert (Update.Generation (State) = 2 and not Update.Can_Submit (State));
            -- Hardware transaction completed, but authority disappeared. Its
            -- epoch is not rolled back and no successful reply is constructed.
            Reject_Work;
         elsif Generation = 2 and Fail_Second_Invalidation then
            pragma Assert (Response (0) = Buffers.Unavailable and Response (2) = 0 and
              Response (3) = 0 and Disabled and Resumes = 1);
            pragma Assert (Update.Generation (State) = 1 and not Update.Can_Submit (State));
            Update.Execute (State, 1, Status); pragma Assert (Status = Update.Rejected);
            Reject_Work;
            Pool.Release_Work (Contexts, Context_ID, OK); pragma Assert (not OK);
         else
            pragma Assert (Response (0) = Buffers.OK and
              Response (2) = Unsigned_64 (Generation) and Response (3) = 0 and
              Disabled = Keep_Disabled and Resumes = Generation);
            pragma Assert (Update.Generation (State) = Unsigned_64 (Generation));
            Reject_Work;
            Snapshots.Adopt_Committed (Current_Image, Images (Generation), OK);
            pragma Assert (OK);
            Pool.Release_Work (Contexts, Context_ID, OK, Keep_Disabled);
            pragma Assert (OK);
            pragma Assert (Pool.Work_Allowed (Contexts, Context_ID) = not Keep_Disabled);
         end if;
         pragma Assert (VM.Root_DMA (Current_Image) = VM.Root_DMA
           (Images ((if Response (0) = Buffers.OK then Generation else Generation - 1))));
      end loop;
      -- Candidate generation one remains retained byte-for-byte after the
      -- second update, even when its invalidation failed after publication.
      for P in 1 .. VM.Used (Images (1)) loop
         for I in Table_Index loop
            pragma Assert (RAM (P) (I) = VM.Entry_Value (Images (1), P, I));
         end loop;
      end loop;
      if Empty_Remapping then
         pragma Assert (VM.Lookup (Images (1), 4096) = 0 and
           VM.Lookup (Images (1), 2 ** 39) = 0 and VM.Lookup (Images (2), 2 ** 39) /= 0);
      else
         pragma Assert (VM.Lookup (Images (1), 4096) /= 0 and VM.Lookup (Images (2), 4096) = 0);
      end if;
      if Keep_Disabled and then not Fail_Second_Invalidation and then not Revoke_On_Resume then
         pragma Assert (Update.Can_Submit (State) and Pool.State (Contexts, Context_ID) = Life.Disabled);
         for P in VM.Page_Number loop
            -- Page_DMA intentionally returns zero for unused reserved pages;
            -- retirement covers the full known allocation, not only Used.
            DMA (P) := 16#100000# + Unsigned_64 (P) * 4096;
            pragma Assert (VM.DMA_Disjoint (Current_Image, DMA (P), 4096));
         end loop;
         -- Simulated exact supervisor acknowledgement after quiescence and
         -- invalidation. This test does not exercise the live IPC supervisor.
         Snapshots.Forget_Retired
           (Images (1), VM.Revision (Images (1)), VM.Root_DMA (Images (1)), True, OK);
         pragma Assert (OK);
         Buffers.Acknowledge_Private_Retirement
           (Buffers_State, 99, Table_Tickets (1), True, OK);
         pragma Assert (OK);
         Buffers.Reserve_Private (Buffers_State, 99, Ticket, Reclaimable => True);
         pragma Assert (Ticket = Table_Tickets (1) + Buffers.Ticket_Stride);
         VM.Prepare_Update (Images (1), Current_Image, DMA, OK); pragma Assert (OK);
         VM.Seal_Update (Images (1), OK); pragma Assert (OK);
         Buffers.Finish_Private (Buffers_State, Ticket, Consumed); pragma Assert (Consumed);
         Buffers.Acknowledge_Private_Retirement
           (Buffers_State, 99, Table_Tickets (1), True, OK);
         pragma Assert (not OK); -- stale allocator completion
      else
         -- No retirement acknowledgement after uncertainty or while runnable.
         -- Both descriptions remain retained, and reservations cannot recycle.
         pragma Assert (VM.Sealed (Images (1)) and VM.Sealed (Images (2)));
         Buffers.Reserve_Private (Buffers_State, 99, Ticket, Reclaimable => True);
         pragma Assert (Ticket /= Table_Tickets (1) + Buffers.Ticket_Stride and
                        Ticket /= Table_Tickets (2) + Buffers.Ticket_Stride);
         if Ticket /= 0 then Buffers.Finish_Private (Buffers_State, Ticket, Consumed); end if;
      end if;
   end Run;
begin
   for Streamed in Boolean loop
   for Sparse in Boolean loop
      Run (False, Sparse => Sparse, Streamed => Streamed); Run (True, Sparse => Sparse, Streamed => Streamed);
      Run (False, True, Sparse => Sparse, Streamed => Streamed);
      Run (False, Keep_Disabled => True, Sparse => Sparse, Streamed => Streamed);
      Run (True, Keep_Disabled => True, Sparse => Sparse, Streamed => Streamed);
      Run (False, True, Keep_Disabled => True, Sparse => Sparse, Streamed => Streamed);
      Run (False, Keep_Disabled => True, Empty_Remapping => True, Sparse => Sparse, Streamed => Streamed);
      Run (True, Keep_Disabled => True, Empty_Remapping => True, Sparse => Sparse, Streamed => Streamed);
      Run (False, True, Keep_Disabled => True, Empty_Remapping => True, Sparse => Sparse, Streamed => Streamed);
   end loop;
   end loop;
   Ada.Text_IO.Put_Line ("VM update pipeline PASS: GuC scheduling and holds, two stable-root generations, map/unmap, materialize then invalidate, failed invalidation retains backing and blocks resume (simulated GPU)");
   Ada.Text_IO.Put_Line ("Private table reuse pipeline PASS: disabled committed replacement +mock allocator ack permits metadata/ticket reuse; uncertainty retains both generations");
end VM_Update_Pipeline_Tests;
