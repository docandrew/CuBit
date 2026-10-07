with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Sharing;
with Intel_GPU_Buffer_Views;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
procedure Presentation_Exclusion_Tests is
   Ready : Boolean := True;
   Admission : Unsigned_64 := 7;
   Revoke_In_Lookup : Natural := 0;
   function Owner_Ready return Boolean is (Ready);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = 42 and Stamp = 99 then Admission else 0);
   procedure Recipient_Of
     (Sender, Stamp : Unsigned_64; Slot : out CuBit.Messages.CapabilitySlot;
      Identity : out Unsigned_64) is
   begin
      Slot := 7;
      Identity := (if Session_Of (Sender, Stamp) /= 0 then 42 else 0);
      case Revoke_In_Lookup is
         when 1 => Ready := False;
         when 2 => Admission := 0;
         when 3 => Admission := 8;
         when others => null;
      end case;
   end Recipient_Of;
   package B is new Intel_GPU_Buffer_Requests (Session_Of, Owner_Ready);
   package M is new B.Sharing (Recipient_Of);
   package G renames CuBit.Memory_Grants;
   use type M.Mapping_ID, Intel_GPU_Buffer_Views.View_State;
   Object : B.Service;
   Table : M.Mapping_Table;
   Reply : B.Words;
   Ticket : B.Ticket;
   Accepted : Boolean;
   ID, Wire, Presentation_Wire : Unsigned_64;
   Writer, Presentation, Attempt : M.Mapping_ID;
   State : Intel_GPU_Buffer_Views.View_State;
begin
   B.Handle (Object, 42, 99, B.Label, 4, 0, 0,
             [B.Version, B.Create, 4096, 0], Reply, Ticket);
   pragma Assert (Ticket /= 0);
   B.Complete (Object, Ticket, Intel_GPU_Buffer_Reply.From_Linear
     (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#),
     Reply, Accepted);
   pragma Assert (Accepted and Reply (0) = B.OK);
   ID := Reply (2);
   G.Gone := False;
   M.Map (Object, Table, 42, 99, ID, 0, 4096, True, Writer, Wire);
   pragma Assert (Writer /= 0);
   M.Map (Object, Table, 42, 99, ID, 0, 4096, False, Attempt, Presentation_Wire, True);
   pragma Assert (Attempt = 0);
   M.Retire (Object, Table, 42, 99, Writer, Accepted, State);
   pragma Assert (Accepted and State = Intel_GPU_Buffer_Views.Retiring);
   M.Map (Object, Table, 42, 99, ID, 0, 4096, False, Attempt, Presentation_Wire, True);
   pragma Assert (Attempt = 0);
   G.Completed_Wire := Wire;
   M.Poll (Object, Table);
   M.Map (Object, Table, 42, 99, ID, 0, 4096, False, Presentation, Presentation_Wire, True);
   pragma Assert (Presentation /= 0 and M.Presentation_Held (Table, 7));
   M.Map (Object, Table, 42, 99, ID, 0, 4096, True, Attempt, Wire);
   pragma Assert (Attempt = 0);
   M.Retire (Object, Table, 42, 99, Presentation, Accepted, State);
   pragma Assert (Accepted and State = Intel_GPU_Buffer_Views.Retiring);
   M.Poll (Object, Table); -- old writer completion cannot retire presentation
   pragma Assert (M.Presentation_Held (Table, 7));
   M.Map (Object, Table, 42, 99, ID, 0, 4096, True, Attempt, Wire);
   pragma Assert (Attempt = 0);
   G.Completed_Wire := Presentation_Wire;
   M.Poll (Object, Table);
   pragma Assert (not M.Presentation_Held (Table, 7));
   M.Map (Object, Table, 42, 99, ID, 0, 4096, True, Attempt, Wire);
   pragma Assert (Attempt /= 0);
   Ready := False;
   pragma Assert (M.Presentation_Held (Table, 7));
   Ada.Text_IO.Put_Line ("Presentation exclusion PASS: live and retiring writers/readers, exact completion, ownership loss");
   for Scenario in 1 .. 3 loop
      declare
         Before : constant Natural := G.Creates;
      begin
         Ready := True; Admission := 7; Revoke_In_Lookup := Scenario;
         M.Map (Object, Table, 42, 99, ID, 0, 4096, False, Attempt, Wire);
         pragma Assert (Attempt = 0 and Wire = 0 and G.Creates = Before);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Recipient revalidation PASS: ownership loss, session closure and replacement deny before grant creation");
end Presentation_Exclusion_Tests;
