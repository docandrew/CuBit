with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Render_Control;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_VM_Image;
procedure Bind_Retirement_Tests is
   package Control renames Intel_GPU_Render_Control;
   Admission : Control.Controller;
   function Resolve (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (Control.Resolve (Admission, Sender, Stamp));
   function Ready return Boolean is (True);
   package Buffers is new Intel_GPU_Buffer_Requests (Resolve, Ready);
   package VM is new Intel_GPU_VM_Image (4);
   package Binding is new Buffers.Binding (VM);
   Object : Buffers.Service;
   Image : VM.Image;
   Reply : Buffers.Words;
   Control_Reply : Control.Words;
   Ticket : Buffers.Ticket;
   Accepted : Boolean;
   Identity : constant Unsigned_64 := 7 * 2 ** 32 + 42;
   Session, Handle, Captured, Leaf : Unsigned_64;
   use type Buffers.Words;
begin
   Control.Bind (Admission, 9, 77);
   Control.Handle (Admission, 9, 77, True, Control.Label, 4, 0, 0,
     [1, Identity, 0, Control.Reserve], Control_Reply);
   pragma Assert (Control_Reply (0) = Control.OK);
   Session := Control_Reply (2);
   Control.Handle (Admission, 9, 77, True, Control.Label, 4, 0, 0,
     [1, Identity, Session, Control.Activate], Control_Reply,
     Recipient_Ready => True); -- mocked sharing-endpoint check
   pragma Assert (Control_Reply (0) = Control.OK);
   Buffers.Handle (Object, 42, Session, Buffers.Label, 4, 0, 0,
     [1, Buffers.Create, 8192, 0], Reply, Ticket);
   pragma Assert (Ticket /= 0);
   Buffers.Complete (Object, Ticket,
     Intel_GPU_Buffer_Reply.From_Linear
       (16#1000_0000#, Intel_GPU_Buffer_Backing.CPU_Base, 8192, 16#1000_0000#),
     Reply, Accepted);
   pragma Assert (Accepted and Reply (0) = Buffers.OK);
   Handle := Reply (2);
   VM.Initialize (Image, [16#2000000#, 16#2001000#, 16#2002000#, 16#2003000#], Accepted);
   pragma Assert (Accepted);
   Captured := Control.Recipient_Identity (Admission, 42, Session);
   Binding.Handle (Object, Image, Session, 42, Session, Binding.Bind_Label,
     4, 0, 0, [1 + 2 ** 32, Handle, 4096, 4096], Reply);
   pragma Assert (Reply = [Buffers.OK, 1, 4096, 4096]);
   Leaf := VM.Lookup (Image, 4096);
   pragma Assert ((Leaf and 16#FFFF_F000#) = 16#1000_1000#);
   -- Submission preflight must distinguish mapped bytes from an authenticated
   -- batch extent, including holes and page crossings. No commands executed.
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 4096, 4096, 4));
   Binding.Bind (Object, Image, Session, 42, Session, Handle, 8192, 0, 4096, Accepted);
   pragma Assert (Accepted);
   Binding.Bind (Object, Image, Session, 42, Session, Handle, 16384, 0, 8192, Accepted);
   pragma Assert (Accepted);
   Binding.Bind (Object, Image, Session, 42, Session, Handle, 24576, 0, 4096, Accepted);
   pragma Assert (Accepted);
   Binding.Bind (Object, Image, Session, 42, Session, Handle, 28672, 0, 4096, Accepted);
   pragma Assert (Accepted);
   VM.Seal (Image, Accepted); pragma Assert (Accepted);
   pragma Assert (Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 4096, 4096, 4096));
   pragma Assert (Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 16384 + 4088, 4088, 16));
   pragma Assert (Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 16384, 0, 8192));
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session, 43, Session, Handle, 16384, 0, 8192));
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle + 1, 16384, 0, 8192));
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 4096, 0, 4));
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 8192, 0, 8192));
   -- Both pages present, but the second aliases BO page0 instead of page1.
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 24576, 0, 8192));
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session + 1, 42, Session, Handle, 16384, 0, 8192));
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 16384, 0, 8196));
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 16388, 4, 4));
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 16384, 0, Unsigned_64'Last - 3));
   -- The successful reply is lost. This is the service's ordered retirement
   -- sequence, not a mock successful rollback or a hardware fence.
   Control.Reject_Delivery (Admission, Captured, Session);
   Buffers.Retire_Session (Object, Session);
   pragma Assert (Resolve (42, Session) = 0);
   pragma Assert (not Binding.Batch_Mapped
     (Object, Image, Session, 42, Session, Handle, 16384, 0, 8192));
   pragma Assert (VM.Lookup (Image, 4096) = Leaf);
   -- Neither another allocation nor a replay can reuse the uncertain VA.
   Buffers.Handle (Object, 42, Session, Buffers.Label, 4, 0, 0,
     [1, Buffers.Create, 4096, 0], Reply, Ticket);
   pragma Assert (Reply (0) = Buffers.Denied and Ticket = 0);
   Binding.Handle (Object, Image, Session, 42, Session, Binding.Bind_Label,
     4, 0, 0, [1, Handle, 4096, 4096], Reply);
   pragma Assert (Reply = [Buffers.Denied, 1, 0, 0]);
   pragma Assert (VM.Lookup (Image, 4096) = Leaf);
   Binding.Handle (Object, Image, Session, 42, Session, Binding.Bind_Label,
     4, 0, 0, [1, Handle, 8192, 4096], Reply);
   pragma Assert (Reply = [Buffers.Denied, 1, 0, 0] and
     VM.Lookup (Image, 8192) = 16#10000003#);
   Control.Handle (Admission, 9, 77, True, Control.Label, 4, 0, 0,
     [1, Identity, Session, Control.Activate], Control_Reply,
     Recipient_Ready => True);
   pragma Assert (Control_Reply (0) = Control.Bad_State);
   Control.Reject_Delivery (Admission, Captured, Session);
   Buffers.Retire_Session (Object, Session);
   pragma Assert (VM.Lookup (Image, 4096) = Leaf);
   Ada.Text_IO.Put_Line ("Lost bind reply PASS: admission closed, allocation/replay denied, backing retained");
end Bind_Retirement_Tests;
