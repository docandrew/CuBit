with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_VM_Update;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
procedure VM_Insertion_Binding_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   procedure Run (Mode : Natural; Async : Boolean := False) is
      Image : VM.Image;
      Tables : VM.Backing_Pages;
      Owner, Session_Live : Boolean := True;
      Invalidated : Boolean := False;
      OK : Boolean;
      Writes, Captures : Natural := 0;
      function Exclusive return Boolean is (Owner);
      function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
        (if Session_Live and Sender = 42 and Stamp = 99 then 99 else 0);
      procedure Write_Leaf
        (Table_DMA : Unsigned_64; Index : Table_Index;
         Expected, Replacement : Unsigned_64; Success : out Boolean) is
      begin
         pragma Assert (Table_DMA = Tables (4) and Index = 2 and Expected = 0);
         pragma Assert (Replacement = 16#900003# and VM.Lookup (Image, 8192) = 0);
         Writes := Writes + 1; Success := True;
      end Write_Leaf;
      procedure Invalidate (Success : out Boolean) is
      begin
         pragma Assert (VM.Lookup (Image, 8192) = 0);
         Invalidated := Mode /= 1; Success := Invalidated;
         if Mode = 2 then Session_Live := False; Owner := False; end if;
      end Invalidate;
      package Insert is new VM.Insertion (Exclusive, Write_Leaf, Invalidate);
      Receipt : Insert.Controller;
      procedure Drain (Success : out Boolean) is
      begin Success := Owner; end Drain;
      procedure Publish (Success : out Boolean) is
      begin
         Insert.Publish (Receipt, Image, VM.Revision (Image), 8192,
           [1 => 16#900000#], Write_Back, Read_Write, Success);
      end Publish;
      Publication_Started : Boolean := False;
      procedure Advance_Publication (Finished, Success : out Boolean) is
      begin
         if not Publication_Started then
            Publication_Started := True;
            Insert.Start (Receipt, Image, VM.Revision (Image), 8192,
              [1 => 16#900000#], Write_Back, Read_Write, Success);
            Finished := not Success;
         else
            Insert.Step (Receipt, Image);
            Finished := not Insert.Publishing (Receipt);
            Success := not Finished or else Insert.Published (Receipt);
         end if;
      end Advance_Publication;
      procedure Resume (Success : out Boolean) is
      begin Insert.Commit (Receipt, Image, Invalidated, Success); end Resume;
      package Coordinator is new Intel_GPU_VM_Update
        (Exclusive, Drain, Publish, Invalidate, Resume);
      State : Coordinator.State;
      procedure Advance is new Coordinator.Advance (Advance_Publication);
      package Buffers is new Intel_GPU_Buffer_Requests (Session_Of, Exclusive);
      package Binding is new Buffers.Binding (VM);
      Service : Buffers.Service;
      Ticket : Buffers.Ticket;
      Request, Reply : Buffers.Words;
      procedure Capture
        (Backing : Intel_GPU_Buffer_Reply.Backing;
         GPU, Offset, Bytes, Revision : Unsigned_64; Accepted : out Boolean) is
      begin
         Captures := Captures + 1;
         pragma Assert (Backing.Ready and GPU = 8192 and Offset = 0 and Bytes = 4096);
         pragma Assert (Revision = VM.Revision (Image) and
           Intel_GPU_Buffer_Reply.Page_Address (Backing, 0) = 16#900000#);
         Accepted := Mode /= 3;
      end Capture;
      procedure Handle is new Binding.Handle_In_Place (Coordinator, False, Capture);
      procedure Begin_Request is new Binding.Begin_In_Place (Coordinator, False, Capture);
      procedure Finish_Request is new Binding.Finish_In_Place (Coordinator);
      procedure Call (Sender : Unsigned_64 := 42) is
         Started, Finished : Boolean;
         Before : constant Unsigned_64 := Coordinator.Generation (State);
         Status : Coordinator.Result;
         Previous_Writes : Natural;
         use type Coordinator.Result;
      begin
         if not Async then
            Handle (Service, Image, State, 99, Sender, 99, Binding.Update_Label,
                    4, 0, 0, Request, Reply);
         else
            Begin_Request (Service, Image, State, 99, Sender, 99, Binding.Update_Label,
                           4, 0, 0, Request, Reply, Started);
            if not Started then return; end if;
            pragma Assert (Writes = 0 and Reply (0) /= Buffers.OK);
            pragma Assert (not Coordinator.Can_Submit (State));
            if Mode = 5 then
               Finish_Request (Service, State, 99, Sender, 99, Before, Reply);
               return;
            end if;
            loop
               Previous_Writes := Writes;
               Advance (State, Finished, Status);
               pragma Assert (Writes <= Previous_Writes + 1);
               exit when Finished;
               pragma Assert (not Coordinator.Can_Submit (State));
            end loop;
            if Status /= Coordinator.Complete then return; end if;
            if Mode = 4 then Session_Live := False; end if;
            Finish_Request (Service, State, 99, Sender, 99, Before, Reply);
         end if;
      end Call;
   begin
      for P in Tables'Range loop Tables (P) := Unsigned_64 (P) * 4096; end loop;
      VM.Initialize (Image, Tables, OK); pragma Assert (OK);
      VM.Map_Page (Image, 4096, 16#800000#, Write_Back, Read_Write, OK);
      pragma Assert (OK); VM.Seal (Image, OK); pragma Assert (OK);
      Buffers.Handle (Service, 42, 99, Buffers.Label, 4, 0, 0,
                      [1, Buffers.Create, 4096, 0], Reply, Ticket);
      pragma Assert (Ticket /= 0);
      Buffers.Complete (Service, Ticket, Intel_GPU_Buffer_Reply.From_Linear
        (16#900000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#900000#), Reply, OK);
      pragma Assert (OK and Reply (0) = Buffers.OK);
      Request := [1, Reply (2), 8192, 4096];
      Call (43); pragma Assert (Reply (0) = Buffers.Denied and Captures = 0);
      Request (0) := 16#10001#;
      Call; pragma Assert (Reply (0) = Buffers.Bad_Request and Captures = 0);
      Request (0) := 1 + 2 ** 32;
      Call; pragma Assert (Reply (0) = Buffers.Unavailable and Captures = 0);
      Request (0) := 1;
      pragma Assert (Writes = 0);
      Call;
      if Mode = 0 then
         pragma Assert (Reply (0) = Buffers.OK and Reply (2) = 1 and
           VM.Lookup (Image, 8192) = 16#900003# and VM.Used (Image) = 4);
         Call; pragma Assert (Reply (0) = Buffers.Unavailable and Writes = 1);
      elsif Mode = 4 then
         pragma Assert (Reply (0) = Buffers.Unavailable and Writes = 1 and
           VM.Lookup (Image, 8192) = 16#900003# and not Coordinator.Can_Submit (State));
      elsif Mode = 5 then
         pragma Assert (Reply (0) = Buffers.Unavailable and Writes = 0 and
           not Coordinator.Can_Submit (State));
      elsif Mode = 3 then
         pragma Assert (Reply (0) = Buffers.Unavailable and Writes = 0 and
           not Insert.Failed (Receipt) and Coordinator.Can_Submit (State));
      else
         pragma Assert (Reply (0) = Buffers.Unavailable and Writes = 1 and
           VM.Lookup (Image, 8192) = 0 and not Coordinator.Can_Submit (State));
         Insert.Commit (Receipt, Image, False, OK);
         pragma Assert (not OK and Insert.Failed (Receipt));
      end if;
   end Run;
begin
   for Mode in 0 .. 3 loop Run (Mode); end loop;
   for Mode in 0 .. 5 loop Run (Mode, True); end loop;
   Ada.Text_IO.Put_Line ("Authenticated in-place bind PASS10: synchronous/stepped, denied/stale/op mismatch, deferred reply, premature finish, late identity loss (mock GPU)");
end VM_Insertion_Binding_Tests;
