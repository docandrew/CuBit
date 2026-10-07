with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Images;
with Intel_GPU_Buffer_Requests.Sharing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Views;
with Intel_GPU_Image_Lease;
with Intel_GPU_Image_Layout;
with Intel_GPU_Image_Consumers;
procedure Image_Provider_Tests is
   package L renames Intel_GPU_Image_Lease;
   package G renames CuBit.Memory_Grants;
   package C renames Intel_GPU_Image_Consumers;
   Consumers, Cleanup_Consumers : C.Ledger;
   GPU_Read, CPU_Read, Display_Read : C.Obligation;
   Ready, Authorized, Quiescent, Drained : Boolean := True;
   Admission : Unsigned_64 := 7;
   Revoke : Boolean := False;
   function Owner_Ready return Boolean is (Ready);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = 42 and Stamp = 99 then Admission else 0);
   function Authorize (Key : L.Identity; Image : Intel_GPU_Image_Layout.Descriptor)
      return Boolean is
   begin
      if Revoke then Admission := 8; end if;
      return Authorized and Key.Adapter = 1 and Key.Output_Epoch = 1;
   end Authorize;
   function Producer_Drained (Session, Allocation : Unsigned_64) return Boolean;
   function Consumers_Drained (Key : L.Identity) return Boolean is
     (Drained and then
       (if Key.Serial = 1 then C.Drained (Consumers, Key)
        else C.Drained (Cleanup_Consumers, Key)));
   procedure Recipient (Sender, Stamp : Unsigned_64;
      Slot : out CuBit.Messages.CapabilitySlot; Identity : out Unsigned_64) is
   begin Slot := 7; Identity := 42; end Recipient;
   package B is new Intel_GPU_Buffer_Requests (Session_Of, Owner_Ready);
   package P is new B.Images (Authorize, Producer_Drained, Consumers_Drained);
   package M is new B.Sharing (Recipient);
   Object : B.Service;
   Lease : L.Lease;
   Key : L.Identity := (1, 7, 0, 1, 1, 101, 0, 102);
   Image : constant Intel_GPU_Image_Layout.Descriptor :=
     (Intel_GPU_Image_Layout.BGRA8_UNorm, Intel_GPU_Image_Layout.Linear, 16, 16, 64, 0);
   Reply : B.Words;
   Ticket : B.Ticket;
   Accepted : Boolean;
   Table : M.Mapping_Table;
   function Producer_Drained (Session, Allocation : Unsigned_64) return Boolean is
     (Quiescent and then not M.Writable_Buffer_Held (Table, Session, Allocation));
   Mapping : M.Mapping_ID;
   Wire : Unsigned_64;
   Before : Natural;
   use type L.State, M.Mapping_ID;
begin
   B.Handle (Object, 42, 99, B.Label, 4, 0, 0,
     [B.Version, B.Create, 4096, 0], Reply, Ticket);
   pragma Assert (Ticket /= 0);
   B.Complete (Object, Ticket, Intel_GPU_Buffer_Reply.From_Linear
     (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#), Reply, Accepted);
   pragma Assert (Accepted);
   Key.Allocation := Reply (2);
   C.Open (Consumers, Key, Accepted); pragma Assert (Accepted);
   C.Reserve (Consumers, Key, C.GPU, GPU_Read, Accepted); pragma Assert (Accepted);
   C.Reserve (Consumers, Key, C.CPU, CPU_Read, Accepted); pragma Assert (Accepted);
   C.Reserve (Consumers, Key, C.Display, Display_Read, Accepted); pragma Assert (Accepted);
   P.Prepare (Object, 41, 99, Key, Image, Lease, Accepted);
   pragma Assert (not Accepted and L.Current (Lease) = L.Empty);
   P.Prepare (Object, 42, 99, (Key with delta Session => 8), Image, Lease, Accepted);
   pragma Assert (not Accepted);
   P.Prepare (Object, 42, 99, (Key with delta Allocation => Key.Allocation + 1), Image, Lease, Accepted);
   pragma Assert (not Accepted);
   P.Prepare (Object, 42, 99, (Key with delta Output_Epoch => 2), Image, Lease, Accepted);
   pragma Assert (not Accepted);
   Authorized := False;
   P.Prepare (Object, 42, 99, Key, Image, Lease, Accepted);
   pragma Assert (not Accepted);
   Authorized := True; Quiescent := False;
   P.Prepare (Object, 42, 99, Key, Image, Lease, Accepted);
   pragma Assert (not Accepted);
   Quiescent := True; Revoke := True;
   P.Prepare (Object, 42, 99, Key, Image, Lease, Accepted);
   pragma Assert (not Accepted and not B.Image_Writes_Held (Object, 7));
   Revoke := False; Admission := 7;
   declare
      Writer : M.Mapping_ID;
      Writer_Wire : Unsigned_64;
      State : Intel_GPU_Buffer_Views.View_State;
   begin
      G.Gone := False; G.Completed_Wire := 0;
      M.Map (Object, Table, 42, 99, Key.Allocation, 0, 4096, True, Writer, Writer_Wire);
      pragma Assert (Writer /= 0);
      pragma Assert (not M.Writable_Buffer_Held (Table, 8, Key.Allocation));
      P.Prepare (Object, 42, 99, Key, Image, Lease, Accepted);
      pragma Assert (not Accepted);
      M.Retire (Object, Table, 42, 99, Writer, Accepted, State);
      pragma Assert (Accepted);
      P.Prepare (Object, 42, 99, Key, Image, Lease, Accepted);
      pragma Assert (not Accepted); -- Revocation alone is not a drain.
      G.Completed_Wire := Writer_Wire;
      M.Poll (Object, Table);
      pragma Assert (not M.Writable_Buffer_Held (Table, 7, Key.Allocation));
   end;
   P.Prepare (Object, 42, 99, Key, Image, Lease, Accepted);
   pragma Assert (Accepted and B.Image_Writes_Held (Object, 7));
   pragma Assert (P.Backing (Object, Lease, Key).Ready);
   Before := G.Creates;
   M.Map (Object, Table, 42, 99, Key.Allocation, 0, 4096, True, Mapping, Wire);
   pragma Assert (Mapping = 0 and Wire = 0 and G.Creates = Before);
   Drained := False;
   P.Retire (Object, Lease, Key, Accepted);
   pragma Assert (not Accepted and B.Image_Writes_Held (Object, 7));
   Drained := True;
   C.Stop (Consumers, Key, Accepted); pragma Assert (Accepted);
   C.Complete (Consumers, Key, GPU_Read, True, Accepted); pragma Assert (Accepted);
   C.Complete (Consumers, Key, CPU_Read, True, Accepted); pragma Assert (Accepted);
   P.Retire (Object, Lease, Key, Accepted);
   pragma Assert (not Accepted and B.Image_Writes_Held (Object, 7));
   C.Complete (Consumers, Key, Display_Read, True, Accepted); pragma Assert (Accepted);
   P.Retire (Object, Lease, (Key with delta Serial => 2), Accepted);
   pragma Assert (not Accepted);
   P.Retire (Object, Lease, Key, Accepted);
   pragma Assert (Accepted and not B.Image_Writes_Held (Object, 7));
   M.Map (Object, Table, 42, 99, Key.Allocation, 0, 4096, True, Mapping, Wire);
   pragma Assert (Mapping /= 0 and Wire /= 0 and G.Creates = Before + 1);
   declare
      Final_Lease : L.Lease;
      State : Intel_GPU_Buffer_Views.View_State;
   begin
      G.Gone := True;
      M.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
      pragma Assert (Accepted);
      M.Poll (Object, Table);
      Key.Serial := 2;
      C.Open (Cleanup_Consumers, Key, Accepted); pragma Assert (Accepted);
      C.Stop (Cleanup_Consumers, Key, Accepted); pragma Assert (Accepted);
      P.Prepare (Object, 42, 99, Key, Image, Final_Lease, Accepted);
      pragma Assert (Accepted);
      B.Retire_Session (Object, 7);
      Admission := 0;
      pragma Assert (B.Image_Writes_Held (Object, 7));
      P.Retire (Object, Final_Lease, Key, Accepted);
      pragma Assert (Accepted and not B.Image_Writes_Held (Object, 7));
   end;
   Ada.Text_IO.Put_Line ("Image provider PASS: authenticated owner, callback replacement, producer drain, service write denial and exact retirement");
end Image_Provider_Tests;
