with Ada.Text_IO;
with Intel_GPU_Buffer_Reply;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Messages;
with CuBit.Memory_Grants;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Sharing;
with Intel_GPU_Buffer_Views;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Record_Growth;
procedure Sharing_Tests is
   Ready, Active : Boolean := True;
   Identity : Unsigned_64 := 7 * 2 ** 32 + 42;
   function Owner_Ready return Boolean is (Ready);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Active and Sender = 42 and Stamp = 99 then 99 else 0);
   procedure Recipient_Of
     (Sender, Stamp : Unsigned_64; Slot : out CuBit.Messages.CapabilitySlot;
      Target : out Unsigned_64) is
   begin
      pragma Assert (Sender = 42 and Stamp = 99);
      Slot := 7;
      Target := Identity;
   end Recipient_Of;
   package B is new Intel_GPU_Buffer_Requests (Session_Of, Owner_Ready);
   package Sharing is new B.Sharing (Recipient_Of);
   use type Sharing.Retirement_State;
   package V renames Intel_GPU_Buffer_Views;
   package G renames CuBit.Memory_Grants;
   Object : B.Service;
   Response : B.Words;
   Ticket : B.Ticket;
   Consumed, Accepted : Boolean;
   ID : Unsigned_64;
   Base : constant Unsigned_64 := Intel_GPU_Buffer_Backing.CPU_Base;
   use type V.View_State;
   use type B.Words;
   procedure Poll_Full_Pass (Table : in out Sharing.Mapping_Table) is
   begin
      for Pass in 1 .. (Sharing.Record_Capacity (Table) + Sharing.Poll_Budget - 1) /
        Sharing.Poll_Budget loop
         Sharing.Poll (Object, Table);
      end loop;
   end Poll_Full_Pass;
begin
   B.Handle (Object, 42, 99, B.Label, 4, 0, 0,
     [1, B.Create, 8192, 0], Response, Ticket);
   pragma Assert (Ticket /= 0);
   B.Complete (Object, Ticket, Intel_GPU_Buffer_Reply.From_Linear (16#1000_0000#, Base, 8192, 16#1000_0000#),
               Response, Consumed);
   pragma Assert (Consumed and Response (0) = B.OK);
   ID := Response (2);
   G.Expected_Slot := 7;
   G.Expected_Offset := Base + 4096;
   G.Expected_Bytes := 4096;
   G.Expected_Access := G.Write_Access;
   G.Expected_Reference := (slot => 8, generation => 9);
   declare
      Table, Foreign_Table : Sharing.Mapping_Table;
      Foreign_Object : B.Service;
      State : Sharing.Mapping_Retirement;
      Mapping : Sharing.Mapping_ID;
      Reference : Unsigned_64;
      Done, OK : Boolean;
      Before : constant Natural := G.Revokes;
   begin
      G.Gone := False;
      for I in 1 .. 49 loop
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
         pragma Assert (Mapping /= 0);
      end loop;
      -- External admission is closed before starting either teardown phase.
      Active := False;
      Sharing.Begin_Retire_Session (Object, Table, 99, State, OK); pragma Assert (OK);
      Sharing.Begin_Retire_Session (Object, Table, 100, State, OK); pragma Assert (not OK);
      Sharing.Retire_Session_Step (Foreign_Object, Table, State, Done);
      pragma Assert (not Done and G.Revokes = Before);
      Sharing.Retire_Session_Step (Object, Foreign_Table, State, Done);
      pragma Assert (not Done and G.Revokes = Before);
      for Turn in 1 .. 4 loop
         Sharing.Retire_Session_Step (Object, Table, State, Done);
         pragma Assert (G.Revokes - Before = Natural'Min (16 * Turn, 49));
         pragma Assert (Done = (Turn = 4));
         pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Outstanding);
      end loop;
      Sharing.Retire_Session_Step (Object, Table, State, Done);
      pragma Assert (Done and G.Revokes - Before = 49);
      G.Gone := True;
      for Turn in 1 .. 4 loop Sharing.Poll (Object, Table); end loop;
      pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Clear);
      Active := True; G.Gone := False;
   end;
   Ada.Text_IO.Put_Line ("Bounded mapping retirement PASS:49 views in16/16/16/1 visits, roots pinned, completion not grant retirement");
   for Case_ID in 0 .. 8 loop
      declare
         Table : Sharing.Mapping_Table;
         Mapping : Sharing.Mapping_ID;
         Reference : Unsigned_64;
         State : V.View_State;
         Before : constant Natural := G.Creates;
      begin
         Ready := Case_ID /= 3;
         Active := Case_ID /= 4;
         Identity := (if Case_ID = 5 then 8 * 2 ** 32 + 42 else 7 * 2 ** 32 + 42);
         Sharing.Map (Object, Table, (if Case_ID = 1 then 43 else 42),
           (if Case_ID = 2 then 100 else 99),
           (if Case_ID = 6 then 0 elsif Case_ID = 7 then 2 ** 32 else ID),
           4096, (if Case_ID = 8 then 8192 else 4096), True, Mapping, Reference);
         Accepted := Mapping /= 0;
         pragma Assert (Accepted = (Case_ID = 0));
         pragma Assert (G.Creates = Before + (if Case_ID = 0 then 1 else 0));
         if Accepted then
            Sharing.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
            G.Gone := True;
            Sharing.Poll (Object, Table);
            Sharing.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
            pragma Assert (State = V.Retired);
         end if;
      end;
   end loop;
   Ready := True;
   Active := True;
   Identity := 7 * 2 ** 32 + 42;
   -- Rejections before kernel grant creation must not exhaust table capacity.
   for Missing_Recipient in Boolean loop
      declare
         Table : Sharing.Mapping_Table;
         Mapping : Sharing.Mapping_ID;
         Reference : Unsigned_64;
         State : V.View_State;
         Before : constant Natural := G.Creates;
      begin
         Identity := (if Missing_Recipient then 0 else 7 * 2 ** 32 + 42);
         for Attempt in 1 .. Sharing.Initial_Capacity * 3 loop
            Sharing.Map (Object, Table, 42, 99,
              (if Missing_Recipient then ID else ID + 100),
              4096, 4096, True, Mapping, Reference);
            pragma Assert (Mapping = 0 and Reference = 0 and G.Creates = Before);
            pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Clear);
         end loop;
         Identity := 7 * 2 ** 32 + 42;
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
         pragma Assert (Mapping /= 0 and Reference /= 0 and G.Creates = Before + 1);
         Sharing.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
         G.Gone := True;
         Sharing.Poll (Object, Table);
         pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Clear);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Pre-grant rejection PASS: unknown BO and absent recipient cannot exhaust slots");
   -- Each lifecycle case gets a fresh table. Grant references are supplied by
   -- the mock; the actual table stores distinct view objects, never copies.
   for Lifecycle in 0 .. 2 loop
      declare
         Table : Sharing.Mapping_Table;
         Mapping, Rejected : Sharing.Mapping_ID;
         Reference : Unsigned_64;
         State : V.View_State;
         Before : Natural;
      begin
         G.Gone := False;
         pragma Assert (Sharing.Observe_Retirement (Table, 0) = Sharing.Uncertain);
         pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Clear);
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
         pragma Assert (Mapping = 1 and Reference /= 0);
         pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Outstanding);
         pragma Assert (Sharing.Observe_Retirement (Table, 100) = Sharing.Clear);
         Ready := False;
         pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Uncertain);
         Ready := True;
         Before := G.Revokes;
         Sharing.Retire (Object, Table, 43, 99, Mapping, Accepted, State);
         pragma Assert (not Accepted and G.Revokes = Before);
         Sharing.Retire (Object, Table, 42, 100, Mapping, Accepted, State);
         pragma Assert (not Accepted and G.Revokes = Before);
         Sharing.Retire (Object, Table, 42, 99, 0, Accepted, State);
         pragma Assert (not Accepted and G.Revokes = Before);
         case Lifecycle is
            when 0 => Sharing.Reject_Delivery (Object, Table, Mapping);
            when 1 => Sharing.Retire_Session (Object, Table, 99);
            when others => Sharing.Quarantine (Object, Table);
         end case;
         pragma Assert (G.Revokes = Before + 1);
         Sharing.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
         pragma Assert (Accepted and State = V.Retiring and G.Revokes = Before + 1);
         pragma Assert (Sharing.Observe_Retirement (Table, 99) =
           (if Lifecycle = 2 then Sharing.Uncertain else Sharing.Outstanding));
         G.Gone := True;
         Sharing.Poll (Object, Table);
         Sharing.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
         pragma Assert (Accepted and State = V.Retired and G.Revokes = Before + 1);
         pragma Assert (Sharing.Observe_Retirement (Table, 99) =
           (if Lifecycle = 2 then Sharing.Uncertain else Sharing.Clear));
         if Lifecycle = 2 then
            Before := G.Creates;
            Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Rejected, Reference);
            pragma Assert (Rejected = 0 and Reference = 0 and G.Creates = Before);
         else
            -- Bad ranges perform no grant creation and release their pin.
            -- Their confirmed-retired slots remain reusable.
            for Index in 2 .. Sharing.Initial_Capacity + 1 loop
               Sharing.Map (Object, Table, 42, 99, ID, 1, 4096, True, Rejected, Reference);
               pragma Assert (Rejected = 0 and Reference = 0);
               pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Clear);
            end loop;
            Before := G.Creates;
            Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Rejected, Reference);
            pragma Assert (Rejected /= 0 and Reference /= 0 and G.Creates = Before + 1);
            Sharing.Retire (Object, Table, 42, 99, Rejected, Accepted, State);
         end if;
      end;
   end loop;
   declare
      Table : Sharing.Mapping_Table;
      Mapping : Sharing.Mapping_ID;
      Reference : Unsigned_64;
      State : V.View_State;
      Before : Natural;
   begin
      Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
      pragma Assert (Mapping /= 0);
      G.Succeed := False;
      G.Gone := True;
      Sharing.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
      pragma Assert (Accepted and State = V.Failed);
      Before := G.Revokes;
      Sharing.Poll (Object, Table);
      Sharing.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
      pragma Assert (Accepted and State = V.Failed and G.Revokes = Before);
      G.Succeed := True;
   end;
   declare
      Table : Sharing.Mapping_Table;
      Created, Saved : Sharing.Mapping_ID;
      Request, Reply : B.Words;
      Before : Natural := G.Creates;
   begin
      for Bad in 0 .. 9 loop
         Request := [1, ID, 4096, 4096];
         case Bad is
            when 4 => Request (0) := 2;
            when 5 => Request (0) := 1 + 4 * 2 ** 32;
            when 6 => Request (1) := 2 ** 32;
            when 7 => Request (2) := 1;
            when 8 => Request (3) := 0;
            when 9 => Request (3) := Unsigned_64'Last;
            when others => null;
         end case;
         Sharing.Handle (Object, Table, 42, 99,
           (if Bad = 0 then Sharing.Map_Label + 1 else Sharing.Map_Label),
           (if Bad = 1 then 3 else 4), (if Bad = 2 then 1 else 0),
           (if Bad = 3 then 1 else 0), Request, Reply, Created);
         pragma Assert (Reply = [B.Bad_Request, 1, 0, 0] and Created = 0);
      end loop;
      Sharing.Handle (Object, Table, 43, 99, Sharing.Map_Label, 4, 0, 0,
        [1, ID, 4096, 4096], Reply, Created);
      pragma Assert (Reply = [B.Denied, 1, 0, 0] and Created = 0);
      pragma Assert (G.Creates = Before);
      -- Read-only map is distinct from writable map; malformed input has
      -- not consumed the first mapping ticket.
      G.Expected_Access := G.Read_Access;
      Sharing.Handle (Object, Table, 42, 99, Sharing.Map_Label, 4, 0, 0,
        [1, ID, 4096, 4096], Reply, Created);
      pragma Assert (Reply (0) = B.OK and Reply (2) = 1 and Reply (3) /= 0);
      pragma Assert (Created = 1 and G.Creates = Before + 1);
      Saved := Created;
      G.Gone := False;
      Before := G.Revokes;
      Sharing.Handle (Object, Table, 42, 99, Sharing.Map_Label, 4, 0, 0,
        [1 + 2 * 2 ** 32, Unsigned_64 (Saved), 1, 0], Reply, Created);
      pragma Assert (Reply = [B.Bad_Request, 1, 0, 0] and G.Revokes = Before);
      Sharing.Handle (Object, Table, 42, 99, Sharing.Map_Label, 4, 0, 0,
        [1 + 2 * 2 ** 32, Unsigned_64 (Saved), 0, 0], Reply, Created);
      pragma Assert (Reply = [Sharing.Pending_Retirement, 1, 0, 0] and Created = 0);
      G.Gone := True;
      Sharing.Handle (Object, Table, 42, 99, Sharing.Map_Label, 4, 0, 0,
        [1 + 2 * 2 ** 32, Unsigned_64 (Saved), 0, 0], Reply, Created);
      pragma Assert (Reply = [B.OK, 1, 0, 0] and G.Revokes = Before + 1);
      G.Expected_Access := G.Write_Access;
      Sharing.Handle (Object, Table, 42, 99, Sharing.Map_Label, 4, 0, 0,
        [1 + 2 ** 32, ID, 4096, 4096], Reply, Created);
      pragma Assert (Reply (0) = B.OK and Created = 2);
      Sharing.Reject_Delivery (Object, Table, Created);
   end;
   pragma Assert (G.Forwardable_Creates = 0);
   declare
      Table : Sharing.Mapping_Table;
      Created : Sharing.Mapping_ID;
      Reply : B.Words;
      Before : constant Natural := G.Creates;
      Reference : Unsigned_64;
   begin
      G.Expected_Access := G.Read_Access;
      Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True,
                   Created, Reference, Presentation => True);
      pragma Assert (Created = 0 and Reference = 0 and G.Creates = Before);
      Sharing.Handle (Object, Table, 43, 99, Sharing.Map_Label, 4, 0, 0,
        [1 + 3 * 2 ** 32, ID, 4096, 4096], Reply, Created);
      pragma Assert (Reply (0) = B.Denied and G.Forwardable_Creates = 0);
      Sharing.Handle (Object, Table, 42, 99, Sharing.Map_Label, 4, 0, 0,
        [1 + 4 * 2 ** 32, ID, 4096, 4096], Reply, Created);
      pragma Assert (Reply (0) = B.Bad_Request and G.Forwardable_Creates = 0);
      Sharing.Handle (Object, Table, 42, 99, Sharing.Map_Label, 4, 0, 0,
        [1 + 3 * 2 ** 32, ID, 4096, 4096], Reply, Created);
      pragma Assert (Reply (0) = B.OK and Created = 1);
      pragma Assert (G.Forwardable_Creates = 1 and G.Creates = Before + 1);
      Sharing.Reject_Delivery (Object, Table, Created);
   end;
   for Successful_Revoke in Boolean loop
      declare
         Table : Sharing.Mapping_Table;
         Writer, Presented, Rejected : Sharing.Mapping_ID;
         Reference : Unsigned_64;
         State : V.View_State;
         Before : Natural;
      begin
         G.Gone := False;
         G.Expected_Access := G.Write_Access;
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Writer, Reference);
         pragma Assert (Writer /= 0 and not Sharing.Presentation_Held (Table, 99));
         pragma Assert (Sharing.Observe_Buffer_Retirement (Table, 99, ID) = Sharing.Outstanding);
         pragma Assert (Sharing.Observe_Buffer_Retirement (Table, 99, ID + 1) = Sharing.Clear);
         pragma Assert (Sharing.Observe_Buffer_Retirement (Table, 100, ID) = Sharing.Clear);
         pragma Assert (Sharing.Observe_Buffer_Retirement (Table, 0, ID) = Sharing.Uncertain);
         pragma Assert (Sharing.Observe_Buffer_Retirement (Table, 99, 0) = Sharing.Uncertain);
         Before := G.Creates;
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, False,
                      Rejected, Reference, Presentation => True);
         pragma Assert (Rejected = 0 and G.Creates = Before);
         Sharing.Retire (Object, Table, 42, 99, Writer, Accepted, State);
         pragma Assert (Accepted and State = V.Retiring);
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, False,
                      Rejected, Reference, Presentation => True);
         pragma Assert (Rejected = 0 and G.Creates = Before);
         G.Gone := True;
         Sharing.Poll (Object, Table);
         G.Gone := False;
         G.Expected_Access := G.Read_Access;
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, False,
                      Presented, Reference, Presentation => True);
         pragma Assert (Presented /= 0 and Sharing.Presentation_Held (Table, 99));
         pragma Assert (Sharing.Observe_Buffer_Retirement (Table, 99, ID) = Sharing.Outstanding);
         pragma Assert (not Sharing.Presentation_Held (Table, 100));
         pragma Assert (Sharing.Presentation_Held (Table, 0));
         Ready := False;
         pragma Assert (Sharing.Presentation_Held (Table, 100));
         pragma Assert (Sharing.Observe_Buffer_Retirement (Table, 99, ID + 1) = Sharing.Uncertain);
         Ready := True;
         Before := G.Creates;
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Rejected, Reference);
         pragma Assert (Rejected = 0 and G.Creates = Before);
         G.Succeed := Successful_Revoke;
         Sharing.Retire (Object, Table, 42, 99, Presented, Accepted, State);
         pragma Assert (Accepted and Sharing.Presentation_Held (Table, 99));
         G.Succeed := True;
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Rejected, Reference);
         pragma Assert (Rejected = 0 and G.Creates = Before);
         G.Gone := True;
         Sharing.Poll (Object, Table);
         pragma Assert (Sharing.Presentation_Held (Table, 99) = not Successful_Revoke);
         pragma Assert (Sharing.Observe_Buffer_Retirement (Table, 99, ID) =
           (if Successful_Revoke then Sharing.Clear else Sharing.Uncertain));
         G.Expected_Access := G.Write_Access;
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Rejected, Reference);
         pragma Assert ((Rejected /= 0) = Successful_Revoke);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Presentation interlock PASS: writer exclusion, pending/failed retirement, session isolation");
   declare
      Table : Sharing.Mapping_Table;
      Mapping, Previous : Sharing.Mapping_ID := 0;
      Reference : Unsigned_64;
      State : V.View_State;
      Before : Natural;
   begin
      for Cycle in 1 .. 4096 loop
         G.Gone := False;
         G.Expected_Access :=
           (if Cycle mod 2 = 0 then G.Read_Access else G.Write_Access);
         pragma Assert (not Sharing.Presentation_Held (Table, 99));
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096,
                      Cycle mod 2 /= 0, Mapping, Reference,
                      Presentation => Cycle mod 2 = 0);
         pragma Assert (Mapping = Unsigned_32 (Cycle) and Reference /= 0);
         pragma Assert (Sharing.Presentation_Held (Table, 99) = (Cycle mod 2 = 0));
         Before := G.Revokes;
         -- At capacity the first retired slot is repeatedly recycled. Its
         -- previous identity must never revoke the new occupant.
         if Cycle > Sharing.Initial_Capacity + 1 then
            Sharing.Retire (Object, Table, 42, 99, Previous, Accepted, State);
            pragma Assert (not Accepted and G.Revokes = Before);
            Sharing.Reject_Delivery (Object, Table, Previous);
            pragma Assert (G.Revokes = Before);
            pragma Assert (Sharing.Presentation_Held (Table, 99) = (Cycle mod 2 = 0));
         end if;
         Sharing.Retire (Object, Table, 43, 99, Mapping, Accepted, State);
         pragma Assert (not Accepted and G.Revokes = Before);
         Sharing.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
         pragma Assert (Accepted and State = V.Retiring and G.Revokes = Before + 1);
         --  Pending revoke is not permission to resume GPU writing. Repeated
         --  observations neither reissue the revoke nor forget its purpose.
         Sharing.Poll (Object, Table);
         Sharing.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
         pragma Assert (Accepted and State = V.Retiring and G.Revokes = Before + 1);
         pragma Assert (Sharing.Presentation_Held (Table, 99) = (Cycle mod 2 = 0));
         G.Gone := True;
         -- Poll is round-robin and bounded, not a full-table sweep.
         Poll_Full_Pass (Table);
         pragma Assert (not Sharing.Presentation_Held (Table, 99));
         Previous := Mapping;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Presentation recycling PASS: 4096 alternating writer/reader cycles, stale IDs, delayed release");
   declare
      type Storage_Array is array (Natural range <>) of Unsigned_8;
      Storage : Storage_Array (0 .. 16383) := [others => 16#A5#]
        with Alignment => 4096;
      Address : constant Unsigned_64 := Unsigned_64
        (System.Storage_Elements.To_Integer (Storage'Address));
      Table : Sharing.Mapping_Table;
      Mapping : Sharing.Mapping_ID;
      Reference : Unsigned_64;
      State : V.View_State;
      Total : Natural := 0;
      Old_Capacity : Positive;
   begin
      G.Gone := False;
      G.Expected_Access := G.Write_Access;
      for Round in 0 .. 3 loop
         while Total < Sharing.Record_Capacity (Table) loop
            Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
            Total := Total + 1;
            pragma Assert (Mapping = Unsigned_32 (Total) and Reference /= 0);
         end loop;
         pragma Assert (Sharing.Needs_Growth (Table));
         if Round < 3 then
            Old_Capacity := Sharing.Record_Capacity (Table);
            Sharing.Extend_Storage (Table, Address, 4096 * 2 ** Round, Accepted);
            pragma Assert (Accepted and Sharing.Record_Capacity (Table) > Old_Capacity);
            pragma Assert (not Sharing.Needs_Growth (Table));
            -- Same-base extension only; a rejected rebase cannot alter tokens.
            Old_Capacity := Sharing.Record_Capacity (Table);
            Sharing.Extend_Storage (Table, Address + 4096, 16384, Accepted);
            pragma Assert (not Accepted and Sharing.Record_Capacity (Table) = Old_Capacity);
         end if;
      end loop;
      for Index in 1 .. Total loop
         Sharing.Retire (Object, Table, 43, 99, Unsigned_32 (Index), Accepted, State);
         pragma Assert (not Accepted);
         Sharing.Retire (Object, Table, 42, 99, Unsigned_32 (Index), Accepted, State);
         pragma Assert (Accepted and State = V.Retiring);
      end loop;
      pragma Assert (Sharing.Needs_Growth (Table));
      G.Gone := True;
      Sharing.Poll (Object, Table);
      pragma Assert (not Sharing.Needs_Growth (Table));
      pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Outstanding);
      Poll_Full_Pass (Table);
      pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Clear);
      Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
      pragma Assert (Mapping = Unsigned_32 (Total + 1));
      Sharing.Retire (Object, Table, 42, 99, 1, Accepted, State);
      pragma Assert (not Accepted);
      Sharing.Retire (Object, Table, 42, 99, Mapping, Accepted, State);
      Sharing.Poll (Object, Table);
      pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Clear);
   end;
   Ada.Text_IO.Put_Line ("Grant metadata growth PASS: live pins across three extensions, delayed retirement, stale IDs");
   for Fail_Commit in Boolean loop
      declare
         type Storage_Array is array (Natural range <>) of Unsigned_8;
         Bytes : Storage_Array (0 .. 65535) := [others => 16#A5#] with Alignment => 4096;
         Base_Address : constant Unsigned_64 := Unsigned_64
           (System.Storage_Elements.To_Integer (Bytes'Address));
         Table : Sharing.Mapping_Table;
         Commits : Natural := 0;
         function Reserve (Count : Unsigned_64) return Unsigned_64 is
           (if Count = Bytes'Length then Base_Address else 0);
         function Commit (Base, Offset, Count : Unsigned_64) return Boolean is
         begin
            Commits := Commits + 1;
            pragma Assert (Base = Base_Address and Offset = 0 and Count = Bytes'Length);
            return not Fail_Commit;
         end Commit;
         function Initialize (Address, Count : Unsigned_64) return Boolean is
           (Address = Base_Address and Count = Bytes'Length);
         package Storage is new Intel_GPU_Metadata_Arena (Reserve, Commit, Initialize);
         function Capacity return Positive is (Sharing.Record_Capacity (Table));
         procedure Publish (Base, Count : Unsigned_64; OK : out Boolean) is
         begin Sharing.Extend_Storage (Table, Base, Count, OK); end Publish;
         package Growth is new Intel_GPU_Record_Growth (Storage, Capacity, Publish);
         Controller : Growth.Controller;
         use type Growth.Phase;
         Mapping : Sharing.Mapping_ID;
         Reference : Unsigned_64;
         State : V.View_State;
         Previous_Commits, Before : Natural;
      begin
         G.Gone := False;
         G.Expected_Access := G.Write_Access;
         for Index in 1 .. Sharing.Initial_Capacity loop
            Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
            pragma Assert (Mapping = Unsigned_32 (Index));
         end loop;
         Growth.Configure (Controller, Bytes'Length, 4096, Accepted);
         pragma Assert (Accepted and Sharing.Needs_Growth (Table));
         Growth.Request (Controller, Sharing.Initial_Capacity * 2, Accepted);
         pragma Assert (Accepted);
         Before := G.Creates;
         for Step in 1 .. 12 loop
            exit when Growth.Snapshot (Controller).State in Growth.Idle | Growth.Failed;
            Previous_Commits := Commits;
            Growth.Step (Controller);
            pragma Assert (Commits <= Previous_Commits + 1 and G.Creates = Before);
         end loop;
         pragma Assert (Commits = 1);
         pragma Assert (Growth.Snapshot (Controller).State =
           (if Fail_Commit then Growth.Failed else Growth.Idle));
         if Fail_Commit then
            pragma Assert (Capacity = Sharing.Initial_Capacity);
            Sharing.Retire (Object, Table, 42, 99, 1, Accepted, State);
            G.Gone := True;
            Sharing.Poll (Object, Table);
         else
            pragma Assert (Capacity >= Sharing.Initial_Capacity * 2);
         end if;
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
         pragma Assert (Mapping = Sharing.Initial_Capacity + 1 and G.Creates = Before + 1);
         Sharing.Retire_Session (Object, Table, 99);
         G.Gone := True;
         Poll_Full_Pass (Table);
         pragma Assert (Sharing.Observe_Retirement (Table, 99) = Sharing.Clear);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Grant growth controller PASS: bounded steps, no MAP replay, failed growth permits retirement/reuse");
   declare
      Table : Sharing.Mapping_Table;
      Mapping : Sharing.Mapping_ID;
      Reference : Unsigned_64;
      State : V.View_State;
      Before : Natural;
   begin
      G.Gone := False;
      G.Expected_Access := G.Write_Access;
      for Index in 1 .. Sharing.Initial_Capacity loop
         Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
         pragma Assert (Mapping = Unsigned_32 (Index));
      end loop;
      Sharing.Retire (Object, Table, 42, 99, 1, Accepted, State);
      pragma Assert (Accepted and State = V.Retiring);
      Before := G.Creates;
      Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
      pragma Assert (Mapping = 0 and Reference = 0 and G.Creates = Before);
      G.Gone := True;
      Sharing.Poll (Object, Table);
      Sharing.Map (Object, Table, 42, 99, ID, 4096, 4096, True, Mapping, Reference);
      pragma Assert (Mapping = 65 and Reference /= 0 and G.Creates = Before + 1);
      Before := G.Revokes;
      Sharing.Retire (Object, Table, 42, 99, 1, Accepted, State);
      Sharing.Reject_Delivery (Object, Table, 1);
      pragma Assert (not Accepted and G.Revokes = Before);
      Sharing.Quarantine (Object, Table);
      pragma Assert (G.Revokes = Before + Sharing.Initial_Capacity);
   end;
   -- Closing a BO name or session admission is not confirmation that Desktop
   -- has released the backing. Keep the independent presentation interlock.
   for Action in 0 .. 2 loop
      declare
         Owner : B.Service;
         Table : Sharing.Mapping_Table;
         Mapping, Rejected : Sharing.Mapping_ID;
         Reference, Buffer_ID : Unsigned_64;
         State : V.View_State;
         Before : Natural;
      begin
         G.Gone := False;
         G.Succeed := True;
         G.Expected_Access := G.Read_Access;
         B.Handle (Owner, 42, 99, B.Label, 4, 0, 0,
                   [1, B.Create, 8192, 0], Response, Ticket);
         B.Complete (Owner, Ticket,
           Intel_GPU_Buffer_Reply.From_Linear (16#1000_0000#, Base, 8192, 16#1000_0000#),
           Response, Consumed);
         pragma Assert (Consumed and Response (0) = B.OK);
         Buffer_ID := Response (2);
         Sharing.Map (Owner, Table, 42, 99, Buffer_ID, 4096, 4096, False,
                      Mapping, Reference, Presentation => True);
         pragma Assert (Mapping /= 0 and Sharing.Presentation_Held (Table, 99));
         case Action is
            when 0 =>
               B.Handle (Owner, 42, 99, B.Label, 4, 0, 0,
                         [1, B.Close, Buffer_ID, 0], Response, Ticket);
               pragma Assert (Response (0) = B.OK);
               Sharing.Retire (Owner, Table, 42, 99, Mapping, Accepted, State);
               pragma Assert (Accepted and State = V.Retiring);
            when 1 =>
               B.Retire_Session (Owner, 99);
               Sharing.Retire_Session (Owner, Table, 99);
            when others =>
               B.Quarantine (Owner);
               Sharing.Quarantine (Owner, Table);
         end case;
         Before := G.Creates;
         Sharing.Map (Owner, Table, 42, 99, Buffer_ID, 4096, 4096, True,
                      Rejected, Reference);
         pragma Assert (Rejected = 0 and G.Creates = Before);
         Sharing.Poll (Owner, Table);
         pragma Assert (Sharing.Presentation_Held (Table, 99));
         pragma Assert (Sharing.Observe_Retirement (Table, 99) /= Sharing.Clear);
         pragma Assert (Sharing.Observe_Buffer_Retirement (Table, 99, Buffer_ID) /= Sharing.Clear);
         G.Gone := True;
         Sharing.Poll (Owner, Table);
         -- A globally quarantined table stays closed even after its grants
         -- retire. Ordinary name/session closure allows confirmed drainage.
         pragma Assert (Sharing.Presentation_Held (Table, 99) = (Action = 2));
         pragma Assert ((Sharing.Observe_Retirement (Table, 99) = Sharing.Clear) =
                        (Action /= 2));
         pragma Assert ((Sharing.Observe_Buffer_Retirement (Table, 99, Buffer_ID) = Sharing.Clear) =
                        (Action /= 2));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Presentation close PASS: BO/session close retain loans; quarantine stays closed");
   B.Retire_Session (Object, 99);
   declare
      View : V.View;
      Before : constant Natural := G.Creates;
   begin
      Sharing.Share (Object, 42, 99, ID, 4096, 4096, True, View, Accepted);
      pragma Assert (not Accepted and G.Creates = Before);
   end;
   Ada.Text_IO.Put_Line ("Request-backed sharing PASS: authentication, owner, identity, range, retirement");
end Sharing_Tests;
