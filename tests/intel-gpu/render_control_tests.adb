with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Render_Control; use Intel_GPU_Render_Control;
with Intel_GPU_Render_Sessions;
with Intel_GPU_Broker_Request;
with Intel_GPU_Broker_Launches;
with Intel_GPU_Boot;
procedure Render_Control_Tests is
   Object : Controller;
   Reply : Words;
   Identity : constant Unsigned_64 := 7 * 2 ** 32 + 42;
   Tag : Unsigned_64;
   procedure Call (Operation, Session : Unsigned_64; Ready : Boolean := True;
                   Who : Unsigned_64 := 42; Stamp : Unsigned_64 := 99;
                   Target : Unsigned_64 := Identity;
                   Recipient_Ready : Boolean := True) is
   begin
      Handle (Object, Who, Stamp, Ready, Label, 4, 0, 0,
              [Version, Target, Session, Operation], Reply, Recipient_Ready);
   end Call;
begin
   declare
      Bootstrap : Controller;
      Actual_Tag : constant Unsigned_64 := Intel_GPU_Boot.Broker_Tag;
      Answer : Words;
   begin
      -- Exercise the production bootstrap constant: a fixture tag alone
      -- misses collisions between broker and application authority ranges.
      pragma Assert (Actual_Tag /= 0);
      pragma Assert (Actual_Tag not in
        Intel_GPU_Render_Sessions.Tag_Base + 1 ..
        Intel_GPU_Render_Sessions.Tag_Last);
      Bind (Bootstrap, 42, Actual_Tag);
      pragma Assert (Is_Broker (Bootstrap, 42, Actual_Tag));
      Handle (Bootstrap, 42, Actual_Tag, True, Label, 4, 0, 0,
        [Version, Identity, 0, Reserve], Answer);
      pragma Assert (Answer (0) = OK);
      pragma Assert (Answer (2) in
        Intel_GPU_Render_Sessions.Tag_Base + 1 ..
        Intel_GPU_Render_Sessions.Tag_Last);
      pragma Assert (not Is_Broker (Bootstrap, 42, Answer (2)));
      pragma Assert (not Is_Broker (Bootstrap, 43, Actual_Tag));
   end;
   declare
      Fresh : Controller;
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
      Tags : array (1 .. Intel_GPU_Render_Sessions.Capacity) of Unsigned_64;
      procedure Check (Condition : Boolean) is
      begin
         if not Condition then
            raise Program_Error with "controller issued-record lookup";
         end if;
      end Check;
   begin
      Check (Storage_Index (Fresh, 0) = 0);
      Check (Stored_Recipient_Slot (Fresh, 0) = 0);
      Check (Stored_Recipient_Slot (Fresh, Unsigned_64'Last) = 0);
      Check (Issued_Tag (Fresh, 0) = 0);
      Check (Storage_Index (Fresh, Unsigned_64'Last) = 0);
      Bind (Fresh, 42, 99);
      for I in Tags'Range loop
         Check (Storage_Index (Fresh, Base + Unsigned_64 (I)) = 0);
         Check (Stored_Recipient_Slot (Fresh, Base + Unsigned_64 (I)) = 0);
         Check (Issued_Tag (Fresh, I) = 0);
         Handle (Fresh, 42, 99, True, Label, 4, 0, 0,
           [Version, Identity, 0, Reserve], Reply);
         Check (Reply (0) = OK);
         Tags (I) := Reply (2);
         Check (Reply (3) = Unsigned_64 (39 + I));
         Check (Stored_Recipient_Slot (Fresh, Tags (I)) = Reply (3));
         Check (Storage_Index (Fresh, Tags (I)) = I);
         Check (Issued_Tag (Fresh, I) = Tags (I));
         Check (Resolve (Fresh, 42, Tags (I)) = 0);
         Handle (Fresh, 42, 99, True, Label, 4, 0, 0,
           [Version, Identity, Tags (I), Activate], Reply, True);
         Check (Reply (0) = OK);
         Check (Resolve (Fresh, 42, Tags (I)) = Tags (I));
         Check (Resolve (Fresh, 43, Tags (I)) = 0);
         Reject_Delivery (Fresh, Identity, Tags (I));
         Check (Stored_Recipient_Slot (Fresh, Tags (I)) = Unsigned_64 (39 + I));
         Check (Storage_Index (Fresh, Tags (I)) = I);
         Check (Resolve (Fresh, 42, Tags (I)) = 0);
      end loop;
      Quarantine (Fresh);
      for I in Tags'Range loop
         Check (Storage_Index (Fresh, Tags (I)) = I);
         Check (Stored_Recipient_Slot (Fresh, Tags (I)) = Unsigned_64 (39 + I));
         Check (Issued_Tag (Fresh, I) = Tags (I));
         Check (Resolve (Fresh, 42, Tags (I)) = 0);
         Check (Resolve_Retired (Fresh, 42, Tags (I)) = 0);
      end loop;
      Check (Storage_Index (Fresh, Base + Tags'Length + 1) = 0);
   end;
   declare
      package L renames Intel_GPU_Broker_Launches;
      package B renames Intel_GPU_Broker_Request;
      use type L.Phase;
      use type L.Outcome;
      Book : L.Ledger;
      ID, Denied_ID : L.Ticket;
      Taken : Boolean;
      Request : B.Decoded := (True, 40, 16, 1);
   begin
      L.Reserve (Book, (Valid => False), Identity, ID);
      pragma Assert (ID = 0);
      L.Reserve (Book, Request, 42, ID);
      pragma Assert (ID = 0);
      L.Reserve (Book, Request, Identity, ID);
      pragma Assert (ID = 1 and L.State (Book, ID) = L.Pending);
      L.Reserve (Book, Request, Identity, Denied_ID);
      pragma Assert (Denied_ID = 0); -- exact replay
      Request.Nonce := 2;
      L.Reserve (Book, Request, Identity, Denied_ID);
      pragma Assert (Denied_ID = 0); -- destination alias
      Request.Destination := 17;
      L.Reserve (Book, Request, Identity + 2 ** 32, Denied_ID);
      pragma Assert (Denied_ID = 0); -- source repurposed for reused PID
      L.Take_Reply (Book, ID, Taken);
      pragma Assert (not Taken);
      L.Delivered (Book, ID, True);
      pragma Assert (L.State (Book, ID) = L.Pending);
      L.Finish (Book, ID, L.Admitted);
      L.Finish (Book, ID, L.Rejected); -- late duplicate cannot change outcome
      pragma Assert (L.Result (Book, ID) = L.Admitted);
      L.Take_Reply (Book, ID, Taken); pragma Assert (Taken);
      L.Take_Reply (Book, ID, Taken); pragma Assert (not Taken);
      L.Delivered (Book, ID, False);
      pragma Assert (L.State (Book, ID) = L.Retained and L.Abort_Required (Book, ID));
      L.Delivered (Book, ID, True); -- no retry can turn failure into active
      pragma Assert (L.State (Book, ID) = L.Retained);
      for I in 2 .. L.Capacity loop
         L.Reserve (Book, (True, 40, I + 16, Unsigned_64 (I)), Identity, ID);
         pragma Assert (ID = I);
      end loop;
      L.Reserve (Book, (True, 41, 60, 99), Identity, Denied_ID);
      pragma Assert (Denied_ID = 0); -- retained entries never recycled
      for Value in L.Outcome loop
         for Sent in Boolean loop
            declare
               Fresh : L.Ledger;
            begin
               L.Reserve (Fresh, (True, 40, 16, 1), Identity, ID);
               L.Finish (Fresh, ID, Value);
               L.Take_Reply (Fresh, ID, Taken); pragma Assert (Taken);
               L.Delivered (Fresh, ID, Sent);
               pragma Assert ((L.State (Fresh, ID) = L.Acknowledged) =
                 (Sent and Value = L.Admitted));
               pragma Assert (L.Abort_Required (Fresh, ID) =
                 (not Sent and Value = L.Admitted));
               pragma Assert (L.Identity (Fresh, ID) = Identity and L.Nonce (Fresh, ID) = 1);
            end;
         end loop;
      end loop;
   end;
   declare
      package B renames Intel_GPU_Broker_Request;
      D : B.Decoded;
   begin
      for Source in 0 .. 64 loop
         for Destination in 0 .. 64 loop
            D := B.Decode (17, 17, B.Authority_Tag, B.Label, 4, 0, 0,
              [B.Version, Unsigned_64 (Source), Unsigned_64 (Destination), 99]);
            pragma Assert (D.Valid = (Source in 40 .. 55 and Destination <= 63));
            if D.Valid then
               pragma Assert (D.Source = Source and D.Destination = Destination);
               pragma Assert (D.Nonce = 99);
            end if;
         end loop;
      end loop;
      for Fault in 0 .. 12 loop
         D := B.Decode
           ((if Fault = 0 then 0 elsif Fault = 1 then 2 ** 32 else 17),
            (if Fault = 2 then 18 else 17),
            (if Fault = 3 then B.Authority_Tag + 1 else B.Authority_Tag),
            (if Fault = 4 then B.Label + 1 else B.Label),
            (if Fault = 5 then 3 else 4),
            (if Fault = 6 then 1 else 0),
            (if Fault = 7 then 1 else 0),
            [(if Fault = 8 then B.Version + 1 else B.Version),
             (if Fault = 9 then Unsigned_64'Last else 40),
             (if Fault = 10 then Unsigned_64'Last else 16),
             (if Fault = 11 then 0 else 99)]);
         pragma Assert (D.Valid = (Fault = 12));
      end loop;
   end;
   for Uncertain in Boolean loop
      for Pending in Boolean loop
         for Stopped in Boolean loop
            for Retired in Boolean loop
               pragma Assert
                 (Drain_Status ((Uncertain, Pending, Stopped, Retired)) =
                  (if Uncertain then Unavailable
                   elsif Pending or not Stopped or not Retired then Retirement_Pending
                   else OK));
            end loop;
         end loop;
      end loop;
   end loop;
   Call (Reserve, 0); pragma Assert (Reply (0) = Denied);
   pragma Assert (not Is_Broker (Object, 42, 99));
   Bind (Object, 42, 99);
   Bind (Object, 43, 100); -- no authority replacement
   pragma Assert (Is_Broker (Object, 42, 99));
   pragma Assert (not Is_Broker (Object, 43, 99));
   pragma Assert (not Is_Broker (Object, 42, 100));
   pragma Assert (not Is_Broker (Object, 43, 100));
   Call (Reserve, 0, Who => 43); pragma Assert (Reply (0) = Denied);
   Call (Reserve, 0, Stamp => 100); pragma Assert (Reply (0) = Denied);
   Call (Reserve, 0, Ready => False); pragma Assert (Reply (0) = Unavailable);
   Call (Reserve, 0, Target => 42); pragma Assert (Reply (0) = Bad_Request);
   -- Malformed envelopes must neither allocate nor leak a session tag.
   for Bad_Field in 0 .. 4 loop
      Handle (Object, 42, 99, True,
              (if Bad_Field = 0 then Label + 1 else Label),
              (if Bad_Field = 1 then 3 else 4),
              (if Bad_Field = 2 then 1 else 0),
              (if Bad_Field = 3 then 1 else 0),
              [(if Bad_Field = 4 then Version + 1 else Version),
               Identity, 0, Reserve], Reply);
      pragma Assert (Reply = [Bad_Request, Version, 0, 0]);
   end loop;
   Call (Reserve, 0); pragma Assert (Reply (0) = OK); Tag := Reply (2);
   pragma Assert (Tag = Intel_GPU_Render_Sessions.Tag_Base + 1);
   pragma Assert (Resolve (Object, 42, Tag) = 0);
   pragma Assert (Recipient_Identity (Object, 42, Tag) = 0);
   Call (Activate, Tag, Target => Identity + 2 ** 32);
   pragma Assert (Reply (0) = Bad_State);
   Call (Activate, Tag, Ready => False); pragma Assert (Reply (0) = Unavailable);
   pragma Assert (Activation_Identity (Object, 42, 99, Label, 4, 0, 0,
     [Version, Identity, Tag, Activate]) = Identity);
   for Fault in 0 .. 8 loop
      pragma Assert (Activation_Identity (Object,
        (if Fault = 0 then 43 else 42), (if Fault = 1 then 100 else 99),
        (if Fault = 2 then Label + 1 else Label),
        (if Fault = 3 then 3 else 4), (if Fault = 4 then 1 else 0),
        (if Fault = 5 then 1 else 0),
        [(if Fault = 6 then Version + 1 else Version),
         (if Fault = 7 then Identity + 2 ** 32 else Identity), Tag,
         (if Fault = 8 then Reserve else Activate)]) = 0);
   end loop;
   for Invalid_Tag of Words'(0, Intel_GPU_Render_Sessions.Tag_Base,
       Intel_GPU_Render_Sessions.Tag_Base + Intel_GPU_Render_Sessions.Capacity + 1,
       Unsigned_64'Last) loop
      pragma Assert (Activation_Identity (Object, 42, 99, Label, 4, 0, 0,
        [Version, Identity, Invalid_Tag, Activate]) = 0);
   end loop;
   Call (Activate, Tag, Recipient_Ready => False);
   pragma Assert (Reply (0) = Unavailable and Resolve (Object, 42, Tag) = 0);
   Call (Activate, Tag); pragma Assert (Reply (0) = OK);
   pragma Assert (Resolve (Object, 42, Tag) = Tag);
   pragma Assert (Session_Status (Object, 42, Tag, True, Status_Label,
     4, 0, 0, [Version, 0, 0, 0]) = [OK, Version, 0, 0]);
   pragma Assert (Session_Status (Object, 42, Tag, False, Status_Label,
     4, 0, 0, [Version, 0, 0, 0]) = [Unavailable, Version, 0, 0]);
   pragma Assert (Session_Status (Object, 43, Tag, True, Status_Label,
     4, 0, 0, [Version, 0, 0, 0]) = [Denied, Version, 0, 0]);
   for Bad_Field in 0 .. 7 loop
      pragma Assert (Session_Status (Object, 42, Tag, True,
        (if Bad_Field = 0 then Status_Label + 1 else Status_Label),
        (if Bad_Field = 1 then 3 else 4),
        (if Bad_Field = 2 then 1 else 0),
        (if Bad_Field = 3 then 1 else 0),
        [(if Bad_Field = 4 then Version + 1 else Version),
         (if Bad_Field = 5 then 1 else 0),
         (if Bad_Field = 6 then 1 else 0),
         (if Bad_Field = 7 then 1 else 0)]) = [Bad_Request, Version, 0, 0]);
   end loop;
   pragma Assert (Resolve (Object, 43, Tag) = 0);
   pragma Assert (Recipient_Identity (Object, 42, Tag) = Identity);
   pragma Assert (Recipient_Identity (Object, 43, Tag) = 0);
   pragma Assert (Recipient_Identity (Object, 42, Tag + 1) = 0);
   pragma Assert (Recipient_Identity (Object, 0, Tag) = 0);
   pragma Assert (Recipient_Identity (Object, 42, Unsigned_64'Last) = 0);
   Call (Activate, Tag); pragma Assert (Reply (0) = Bad_State);
   Call (Abort_Session, Tag, Ready => False); pragma Assert (Reply (0) = OK);
   pragma Assert (Resolve (Object, 42, Tag) = 0);
   pragma Assert (Recipient_Identity (Object, 42, Tag) = 0);
   Call (Activate, Tag); pragma Assert (Reply (0) = Bad_State);
   Call (Reserve, 0); pragma Assert (Reply (0) = OK and Reply (2) /= Tag);
   Tag := Reply (2);
   Call (Abort_Session, Tag); pragma Assert (Reply (0) = OK);
   Call (Activate, Tag); pragma Assert (Reply (0) = Bad_State);
   -- Retired tags are never recycled, even when capacity is exhausted.
   for Index in 3 .. Intel_GPU_Render_Sessions.Capacity loop
      Call (Reserve, 0);
      pragma Assert (Reply (0) = OK);
      pragma Assert
        (Reply (2) = Intel_GPU_Render_Sessions.Tag_Base + Unsigned_64 (Index));
      Call (Abort_Session, Reply (2));
      pragma Assert (Reply (0) = OK);
   end loop;
   Call (Reserve, 0); pragma Assert (Reply = [Unavailable, Version, 0, 0]);
   Quarantine (Object);
   Call (Reserve, 0); pragma Assert (Reply (0) = Unavailable);
   for Reserved_Tag of Words'
     (Intel_GPU_Render_Sessions.Tag_Base + 1,
      Intel_GPU_Render_Sessions.Tag_Base + 17,
      Intel_GPU_Render_Sessions.Tag_Last - 1,
      Intel_GPU_Render_Sessions.Tag_Last)
   loop
      declare Collision : Controller; begin
         Bind (Collision, 42, Reserved_Tag);
         pragma Assert (not Is_Broker (Collision, 42, Reserved_Tag));
         Bind (Collision, 42, 99);
         pragma Assert (not Is_Broker (Collision, 42, 99));
      end;
   end loop;
   declare
      Fresh, Invalid_Binding : Controller;
      Fresh_Tag : Unsigned_64;
   begin
      -- Invalid bootstrap authority cannot be repaired by a later caller.
      Bind (Invalid_Binding, 42, Intel_GPU_Render_Sessions.Tag_Base + 1);
      Bind (Invalid_Binding, 42, 99);
      Handle (Invalid_Binding, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, 0, Reserve], Reply);
      pragma Assert (Reply = [Denied, Version, 0, 0]);
      -- Test quarantine independently of exhaustion: active and unused
      -- capacity both become inaccessible.
      Bind (Fresh, 42, 99);
      Handle (Fresh, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, 0, Reserve], Reply);
      pragma Assert (Reply (0) = OK);
      Fresh_Tag := Reply (2);
      Handle (Fresh, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, Fresh_Tag, Activate], Reply, True);
      pragma Assert (Reply (0) = OK);
      Quarantine (Fresh);
      pragma Assert (Resolve (Fresh, 42, Fresh_Tag) = 0);
      pragma Assert (Recipient_Identity (Fresh, 42, Fresh_Tag) = 0);
      Handle (Fresh, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, 0, Reserve], Reply);
      pragma Assert (Reply = [Unavailable, Version, 0, 0]);
      Handle (Fresh, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, Fresh_Tag, Activate], Reply, True);
      pragma Assert (Reply = [Bad_State, Version, 0, 0]);
   end;
   declare
      Reused_PID : Controller;
      Old_Tag, New_Tag : Unsigned_64;
      New_Identity : constant Unsigned_64 := Identity + 2 ** 32;
   begin
      Bind (Reused_PID, 42, 99);
      Handle (Reused_PID, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, 0, Reserve], Reply);
      Old_Tag := Reply (2);
      Handle (Reused_PID, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, Old_Tag, Activate], Reply, True);
      pragma Assert (Recipient_Identity (Reused_PID, 42, Old_Tag) = Identity);
      Handle (Reused_PID, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, Old_Tag, Abort_Session], Reply);
      Handle (Reused_PID, 42, 99, True, Label, 4, 0, 0,
              [Version, New_Identity, 0, Reserve], Reply);
      New_Tag := Reply (2);
      Handle (Reused_PID, 42, 99, True, Label, 4, 0, 0,
              [Version, New_Identity, New_Tag, Activate], Reply, True);
      pragma Assert (Reply (0) = OK and New_Tag /= Old_Tag);
      pragma Assert (Recipient_Identity (Reused_PID, 42, New_Tag) = New_Identity);
      pragma Assert (Recipient_Identity (Reused_PID, 42, Old_Tag) = 0);
   end;
   for Activated in Boolean loop
      declare
         Lost : Controller;
         Lost_Tag : Unsigned_64;
      begin
         Bind (Lost, 42, 99);
         Handle (Lost, 42, 99, True, Label, 4, 0, 0,
                 [Version, Identity, 0, Reserve], Reply);
         Lost_Tag := Reply (2);
         if Activated then
            Handle (Lost, 42, 99, True, Label, 4, 0, 0,
                    [Version, Identity, Lost_Tag, Activate], Reply, True);
         end if;
         Reject_Delivery (Lost, Identity + 2 ** 32, Lost_Tag);
         pragma Assert (Resolve (Lost, 42, Lost_Tag) = (if Activated then Lost_Tag else 0));
         Reject_Delivery (Lost, Identity, Lost_Tag);
         Reject_Delivery (Lost, Identity, Lost_Tag);
         Reject_Delivery (Lost, Identity, Unsigned_64'Last);
         pragma Assert (Resolve (Lost, 42, Lost_Tag) = 0);
         Handle (Lost, 42, 99, True, Label, 4, 0, 0,
                 [Version, Identity, Lost_Tag, Activate], Reply, True);
         pragma Assert (Reply (0) = Bad_State);
      end;
   end loop;
   declare
      Own : Controller;
      First, Other : Unsigned_64 := 0;
   begin
      Bind (Own, 42, 99);
      for I in 1 .. 2 loop
         Handle (Own, 42, 99, True, Label, 4, 0, 0,
           [Version, Identity, 0, Reserve], Reply);
         if I = 1 then First := Reply (2); else Other := Reply (2); end if;
         Handle (Own, 42, 99, True, Label, 4, 0, 0,
           [Version, Identity, Reply (2), Activate], Reply, True);
         pragma Assert (Reply (0) = OK);
      end loop;
      for Fault in 0 .. 9 loop
         Close_Own (Own, (if Fault = 0 then 43 else 42),
           (if Fault = 1 then 99 else First),
           (if Fault = 2 then Label else Close_Own_Label),
           (if Fault = 3 then 3 else 4),
           (if Fault = 4 then 1 else 0),
           (if Fault = 5 then 1 else 0),
           [(if Fault = 6 then 2 else Version),
            (if Fault = 7 then Identity else 0),
            (if Fault = 8 then Other else 0),
            (if Fault = 9 then Abort_Session else 0)], Reply);
         pragma Assert (Reply (0) /= OK and Reply (2) = 0);
         pragma Assert (Resolve (Own, 42, First) = First);
         pragma Assert (Resolve (Own, 42, Other) = Other);
      end loop;
      Close_Own (Own, 42, First, Close_Own_Label, 4, 0, 0,
        [Version, 0, 0, 0], Reply);
      pragma Assert (Reply = [OK, Version, First, 0]);
      pragma Assert (Resolve (Own, 42, First) = 0);
      pragma Assert (Resolve (Own, 42, Other) = Other);
      Close_Own (Own, 42, First, Close_Own_Label, 4, 0, 0,
        [Version, 0, 0, 0], Reply);
      pragma Assert (Reply = [Denied, Version, 0, 0]);
   end;
   Ada.Text_IO.Put_Line ("GPU render control PASS: authority, incarnation, activation, late reply, own-session retirement");
end Render_Control_Tests;
