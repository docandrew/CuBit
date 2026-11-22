with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Table_Allocations;
with Intel_GPU_Table_Provenance.Backing;
procedure Table_Role_Resolution_Tests is
   Live : Boolean := True;
   Receipt : Unsigned_64 := 0;
   function Ready return Boolean is (Live);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = 42 then Stamp else 0);
   package Tickets is new Intel_GPU_Buffer_Requests (Session_Of, Ready);
   Pool : Tickets.Service;
   package B renames Intel_GPU_Buffer_Reply;
   function Admitted (Session, Ticket : Unsigned_64; Slot : Positive) return Boolean is
     (Live and then Tickets.Ticket_Slot (Ticket) = Slot and then
      (Tickets.Is_Table_Allocation (Pool, Session, Ticket, Tickets.Incremental_Tables) or else
       Tickets.Is_Table_Allocation (Pool, Session, Ticket, Tickets.Replacement_Tables)));
   function Confirmed (Session, Ticket : Unsigned_64) return Boolean is
     (Live and Session /= 0 and Ticket /= 0 and Ticket = Receipt and
      Tickets.Ticket_Session (Pool, Ticket) = Session);
   package Registry is new Intel_GPU_Table_Allocations (Admitted, Confirmed);
   Backings : Registry.Registry;
   function Ticket_Session (Ticket : Unsigned_64) return Unsigned_64 is
     (Tickets.Ticket_Session (Pool, Ticket));
   procedure Select_Slice
     (Session, Ticket : Unsigned_64; Selected : out B.Backing;
      Allocation_Offset : out Unsigned_64; Accepted : out Boolean) is
   begin
      Allocation_Offset := 0;
      Registry.Lookup (Backings, Tickets.Ticket_Slot (Ticket), Session, Ticket,
        (if Tickets.Is_Table_Allocation (Pool, Session, Ticket, Tickets.Incremental_Tables)
         then Registry.Incremental_Tables else Registry.Replacement_Image), Selected, Accepted);
   end Select_Slice;
   package Resolver is new Intel_GPU_Table_Provenance.Backing
     (Ready, Ticket_Session, Select_Slice);
   Backing : constant B.Backing := B.From_Linear
     (16#200000#, B.Layout.CPU_Base, 8192, 16#200000#);
   ID, Previous : Tickets.Ticket := 0;
   OK : Boolean;
   procedure Resolve (Session, Ticket : Unsigned_64; Expected : Boolean) is
      CPU, DMA : Unsigned_64;
      Accepted : Boolean;
   begin
      Resolver.Resolve_Owned_Page (Session, Ticket, 4096, CPU, DMA, Accepted);
      pragma Assert (Accepted = Expected);
      if Expected then
         pragma Assert (CPU = B.Layout.CPU_Base + 4096 and DMA = 16#201000#);
      else pragma Assert (CPU = 0 and DMA = 0); end if;
   end Resolve;
begin
   for Cycle in 1 .. 128 loop
      declare
         Session : constant Unsigned_64 := Unsigned_64 (Cycle) + 100;
         Kind : constant Tickets.Private_Table_Kind :=
           (if Cycle mod 2 = 0 then Tickets.Replacement_Tables else Tickets.Incremental_Tables);
         Role : constant Registry.Allocation_Role :=
           (if Cycle mod 2 = 0 then Registry.Replacement_Image else Registry.Incremental_Tables);
      begin
         Tickets.Reserve_Private (Pool, Session, ID, True, Kind, Pages => 2);
         pragma Assert (ID /= 0);
         pragma Assert (Tickets.Ticket_Bytes (Pool, ID) = Backing.Bytes);
         Resolve (Session, ID, False); -- ticket alone is not backing authority
         Registry.Install (Backings, Tickets.Ticket_Slot (ID), Session, ID, Role, Backing, OK);
         pragma Assert (OK);
         Tickets.Finish_Private (Pool, ID, OK); pragma Assert (OK);
         Resolve (Session, ID, True);
         Resolve (Session + 1, ID, False);
         Resolve (Session, Previous, False);
         Live := False; Resolve (Session, ID, False); Live := True;
         Registry.Retire (Backings, Tickets.Ticket_Slot (ID), Session, ID, OK);
         pragma Assert (not OK); Resolve (Session, ID, True);
         Registry.Revoke (Backings, Tickets.Ticket_Slot (ID), Session, ID, OK);
         pragma Assert (OK); Resolve (Session, ID, False);
         Receipt := ID; -- explicit fake supervisor acknowledgment, NOT hardware
         Registry.Retire (Backings, Tickets.Ticket_Slot (ID), Session, ID, OK);
         pragma Assert (OK);
         Tickets.Acknowledge_Private_Retirement (Pool, Session, ID, True, OK);
         pragma Assert (OK);
         Resolve (Session, ID, False);
         Previous := ID; Receipt := 0;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("table role resolution PASS128: real ticket/registry/provenance composition without replacement images; modeled receipts");
end Table_Role_Resolution_Tests;
