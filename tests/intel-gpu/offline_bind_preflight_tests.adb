with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
procedure Offline_Bind_Preflight_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   Owner, Live : Boolean := True;
   function Owner_Ready return Boolean is (Owner);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Live and Sender = 42 and Stamp = 99 then 99 else 0);
   package Buffers is new Intel_GPU_Buffer_Requests (Session_Of, Owner_Ready);
   package Binding is new Buffers.Binding (VM);
   use type Binding.Preparation_Result;
begin
   for Fault in 0 .. 20 loop
      declare
         Image : VM.Image;
         Service : Buffers.Service;
         Ticket : Buffers.Ticket;
         Request, Response : Buffers.Words;
         Epoch, Captured, Sender, Session, ID : Unsigned_64;
         Label : Unsigned_32 := Binding.Bind_Label;
         Length : Unsigned_8 := 4;
         Flags : Unsigned_8 := 0;
         Reserved : Unsigned_16 := 0;
         Status : Binding.Preparation_Result;
         OK : Boolean;
         First : Natural;
      begin
         Owner := True; Live := True; Sender := 42; Session := 99;
         Buffers.Handle (Service, 42, 99, Buffers.Label, 4, 0, 0,
           [1, Buffers.Create, 8192, 0], Response, Ticket); pragma Assert (Ticket /= 0);
         Buffers.Complete (Service, Ticket, Intel_GPU_Buffer_Reply.From_Linear
           (16#900000#, Intel_GPU_Buffer_Backing.CPU_Base, 8192, 16#900000#), Response, OK);
         pragma Assert (OK); ID := Response (2);
         if Fault /= 18 then
            VM.Initialize (Image, [1 => 4096, 2 => 8192, 3 => 12288, 4 => 16384, others => 0],
              OK, Backing_Count => 4); pragma Assert (OK);
            VM.Map_Page (Image, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
         end if;
         Epoch := VM.Revision (Image); Captured := Epoch;
         Request := [1 + 2 ** 32, ID, 2 ** 39, 4096]; -- one-page BO offset
         case Fault is
            when 1 => Sender := 43;
            when 2 => Session := 0;
            when 3 => Label := Binding.Update_Label;
            when 4 => Length := 3;
            when 5 => Flags := 1;
            when 6 => Reserved := 1;
            when 7 => Request (0) := 2;
            when 8 => Request (0) := 16#10001#;
            when 9 => Request (1) := 0;
            when 10 => Request (1) := ID + 2 ** 32;
            when 11 => Request (2) := 3;
            when 12 => Request (2) := 2 ** 48 - 4096; Request (3) := 8192;
            when 13 => Request (3) := 0;
            when 14 => Request (0) := 1 + 2 * 2 ** 32;
            when 15 => Captured := Epoch + 1;
            when 16 => Owner := False;
            when 17 => VM.Seal (Image, OK); pragma Assert (OK);
            when 19 => Buffers.Handle (Service, 42, 99, Buffers.Label, 4, 0, 0,
               [1, Buffers.Close, ID, 0], Response, Ticket);
            when 20 => Live := False;
            when others => null;
         end case;
         Binding.Check_Offline_Bind_Request (Service, Image, Session, Captured,
           Sender, 99, Label, Length, Flags, Reserved, Request, Status);
         pragma Assert ((Status = Binding.Eligible) = (Fault = 0));
         pragma Assert (VM.Revision (Image) = Epoch);
         if Fault = 15 then pragma Assert (Status = Binding.Stale_Generation); end if;
         if Fault = 0 then
            -- Eligibility does not claim absent directory backing is ready.
            Binding.Handle (Service, Image, 99, 42, 99, Label, 4, 0, 0, Request, Response);
            pragma Assert (Response (0) = Buffers.Denied and VM.Lookup (Image, 2 ** 39) = 0);
            VM.Append_Offline_Backing (Image, [16#300000#, 16#301000#, 16#302000#], First, OK);
            pragma Assert (OK);
            Binding.Check_Offline_Bind_Request (Service, Image, 99, Epoch, 42, 99,
              Label, 4, 0, 0, Request, Status); pragma Assert (Status = Binding.Stale_Generation);
            Binding.Handle (Service, Image, 99, 42, 99, Label, 4, 0, 0, Request, Response);
            pragma Assert (Response (0) = Buffers.OK);
            pragma Assert (VM.Lookup (Image, 2 ** 39) = Encode_Leaf (16#901000#, Write_Back, Read_Write));
            Request (0) := Request (0) + 16#10000#;
            Binding.Handle (Service, Image, 99, 42, 99, Label, 4, 0, 0, Request, Response);
            pragma Assert (Response (0) = Buffers.OK and VM.Lookup (Image, 2 ** 39) = 0);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Offline bind preflight PASS21: ownership/envelope/extent/revision rejection, actual offset encoding, append+bind and unchanged unbind");
end Offline_Bind_Preflight_Tests;
