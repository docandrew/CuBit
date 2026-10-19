with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Render_Control; use Intel_GPU_Render_Control;
with Intel_GPU_Render_Sessions;
with Intel_GPU_Broker_Request;
with Intel_GPU_Broker_Launches;
with Intel_GPU_Boot;
procedure Render_Control_Tests is
   package RS renames Intel_GPU_Render_Sessions;
   -- Controllers hold a slot for every GuC context ID: keep them off the stack.
   type Controller_Access is access Controller;
   Object_Storage : constant Controller_Access := new Controller;
   Object : Controller renames Object_Storage.all;
   -- The first tag of a fresh registry's slot I.
   function First_Tag (I : RS.Slot) return Unsigned_64 is
     (RS.Tag_Base + RS.Slot_Span + Unsigned_64 (I));
   All_Released : constant RS.Release_Facts := (others => True);
   Reply : Words;
   Identity : constant Unsigned_64 := 16#0700_002A#;
   Tag : Unsigned_64;
   procedure Call (Operation, Session : Unsigned_64; Ready : Boolean := True;
                   Who : Unsigned_64 := 42; Stamp : Unsigned_64 := 99;
                   Target : Unsigned_64 := Identity) is
   begin
      Handle (Object, Who, Stamp, Ready, Label, 4, 0, 0,
              [Version, Target, Session, Operation], Reply);
   end Call;
begin
   declare
      Bootstrap_Storage : constant Controller_Access := new Controller;
      Bootstrap : Controller renames Bootstrap_Storage.all;
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
      Grow (Bootstrap, 1);
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
      Fresh_Storage : constant Controller_Access := new Controller;
      Fresh : Controller renames Fresh_Storage.all;
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
      Tags : array (1 .. 40) of Unsigned_64;
      procedure Check (Condition : Boolean) is
      begin
         if not Condition then
            raise Program_Error with "controller issued-record lookup";
         end if;
      end Check;
   begin
      Check (Storage_Index (Fresh, 0) = 0);
      Check (not Retired_Admission (Fresh, 0));
      Check (not Retired_Admission (Fresh, Unsigned_64'Last));
      Check (Issued_Tag (Fresh, 0) = 0);
      Check (Storage_Index (Fresh, Unsigned_64'Last) = 0);
      Bind (Fresh, 42, 99);
      Grow (Fresh, Tags'Length);
      for I in Tags'Range loop
         Check (Storage_Index (Fresh, First_Tag (I)) = 0);
         Check (Issued_Tag (Fresh, I) = 0);
         Handle (Fresh, 42, 99, True, Label, 4, 0, 0,
           [Version, Identity, 0, Reserve], Reply);
         Check (Reply (0) = OK);
         Tags (I) := Reply (2);
         --  No recipient capability: the reply's last word is always 0.
         Check (Reply (3) = 0);
         Check (Storage_Index (Fresh, Tags (I)) = I);
         Check (Issued_Tag (Fresh, I) = Tags (I));
         Check (Resolve (Fresh, Identity, Tags (I)) = 0);
         Check (not Retired_Admission (Fresh, Tags (I))); -- still reserved
         Handle (Fresh, 42, 99, True, Label, 4, 0, 0,
           [Version, Identity, Tags (I), Activate], Reply);
         Check (Reply (0) = OK);
         Check (Resolve (Fresh, Identity, Tags (I)) = Tags (I));
         Check (not Retired_Admission (Fresh, Tags (I))); -- active
         Check (Resolve (Fresh, 43, Tags (I)) = 0);
         Reject_Delivery (Fresh, Identity, Tags (I));
         Check (Retired_Admission (Fresh, Tags (I)));
         Check (Storage_Index (Fresh, Tags (I)) = I);
         Check (Resolve (Fresh, Identity, Tags (I)) = 0);
      end loop;
      Quarantine (Fresh);
      for I in Tags'Range loop
         Check (not Retired_Admission (Fresh, Tags (I)));
         Check (Storage_Index (Fresh, Tags (I)) = I);
         Check (Issued_Tag (Fresh, I) = Tags (I));
         Check (Resolve (Fresh, Identity, Tags (I)) = 0);
         Check (Resolve_Retired (Fresh, Identity, Tags (I)) = 0);
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
      Request : B.Decoded := (True, Identity, 1);
   begin
      L.Reserve (Book, (Valid => False), ID);
      pragma Assert (ID = 0);
      L.Reserve (Book, Request, ID);
      pragma Assert (ID = 1 and L.State (Book, ID) = L.Pending);
      pragma Assert (L.Identity (Book, ID) = Identity);
      L.Reserve (Book, Request, Denied_ID);
      pragma Assert (Denied_ID = 0); -- exact replay
      Request.Nonce := 2;
      L.Reserve (Book, Request, Denied_ID);
      pragma Assert (Denied_ID = 0); -- second admission for the same child
      Request := (True, Identity + 2 ** 32, 1);
      L.Reserve (Book, Request, Denied_ID);
      pragma Assert (Denied_ID = 0); -- replayed nonce for another child
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
         L.Reserve (Book, (True, Identity + Unsigned_64 (I) * 2 ** 32, Unsigned_64 (I)), ID);
         pragma Assert (ID = I);
      end loop;
      L.Reserve (Book, (True, Identity + 99 * 2 ** 32, 99), Denied_ID);
      pragma Assert (Denied_ID = 0); -- retained entries never recycled
      for Value in L.Outcome loop
         for Sent in Boolean loop
            declare
               Fresh : L.Ledger;
            begin
               L.Reserve (Fresh, (True, Identity, 1), ID);
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
      for Child in Unsigned_64 range 0 .. 2 loop
         for Reserved in Unsigned_64 range 0 .. 1 loop
            D := B.Decode (17, 17, B.Authority_Tag, B.Label, 4, B.Request_Flags, 0,
              [B.Version, Child, Reserved, 99]);
            pragma Assert (D.Valid = (Child /= 0 and Reserved = 0));
            if D.Valid then
               pragma Assert (D.Identity = Child and D.Nonce = 99);
            end if;
         end loop;
      end loop;
      -- The launcher is an opaque full-width process identity, not a u32 PID.
      D := B.Decode (16#1234_0000_002A#, 16#1234_0000_002A#,
        B.Authority_Tag, B.Label, 4, B.Request_Flags, 0, [B.Version, Identity, 0, 99]);
      pragma Assert (D.Valid);
      D := B.Decode (16#1234_0000_002A#, 42,
        B.Authority_Tag, B.Label, 4, B.Request_Flags, 0, [B.Version, Identity, 0, 99]);
      pragma Assert (not D.Valid);
      D := B.Decode (16#1234_0000_002A#, 16#1235_0000_002A#,
        B.Authority_Tag, B.Label, 4, B.Request_Flags, 0, [B.Version, Identity, 0, 99]);
      pragma Assert (not D.Valid);
      for Fault in 0 .. 12 loop
         D := B.Decode
           ((if Fault = 0 then 0 elsif Fault = 1 then 2 ** 32 else 17),
            (if Fault = 2 then 18 else 17),
            (if Fault = 3 then B.Authority_Tag + 1 else B.Authority_Tag),
            (if Fault = 4 then B.Label + 1 else B.Label),
            (if Fault = 5 then 3 else 4),
            (if Fault = 6 then 0 else B.Request_Flags),
            (if Fault = 7 then 1 else 0),
            [(if Fault = 8 then B.Version + 1 else B.Version),
             (if Fault = 9 then 0 else Identity),
             (if Fault = 10 then Unsigned_64'Last else 0),
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
   Grow (Object, 4);
   Call (Reserve, 0, Who => 43); pragma Assert (Reply (0) = Denied);
   Call (Reserve, 0, Stamp => 100); pragma Assert (Reply (0) = Denied);
   Call (Reserve, 0, Ready => False); pragma Assert (Reply (0) = Unavailable);
   Call (Reserve, 0, Target => 0); pragma Assert (Reply (0) = Bad_Request);
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
   pragma Assert (Tag = First_Tag (1));
   pragma Assert (Resolve (Object, Identity, Tag) = 0);
   pragma Assert (Recipient_Identity (Object, Identity, Tag) = 0);
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
       RS.Tag_Base + RS.Slot_Span * 99 + 1, Unsigned_64'Last) loop
      pragma Assert (Activation_Identity (Object, 42, 99, Label, 4, 0, 0,
        [Version, Identity, Invalid_Tag, Activate]) = 0);
   end loop;
   Call (Activate, Tag); pragma Assert (Reply (0) = OK);
   -- Full identity required: neither old PID nor a different generation works.
   pragma Assert (Resolve (Object, 42, Tag) = 0);
   pragma Assert (Resolve (Object, Identity + 2 ** 32, Tag) = 0);
   pragma Assert (Resolve (Object, Identity, Tag) = Tag);
   pragma Assert (Session_Status (Object, Identity, Tag, True, Status_Label,
     4, 0, 0, [Version, 0, 0, 0]) = [OK, Version, 0, 0]);
   pragma Assert (Session_Status (Object, Identity, Tag, False, Status_Label,
     4, 0, 0, [Version, 0, 0, 0]) = [Unavailable, Version, 0, 0]);
   pragma Assert (Session_Status (Object, 43, Tag, True, Status_Label,
     4, 0, 0, [Version, 0, 0, 0]) = [Denied, Version, 0, 0]);
   for Bad_Field in 0 .. 7 loop
      pragma Assert (Session_Status (Object, Identity, Tag, True,
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
   pragma Assert (Recipient_Identity (Object, Identity, Tag) = Identity);
   pragma Assert (Recipient_Identity (Object, 43, Tag) = 0);
   pragma Assert (Recipient_Identity (Object, Identity, Tag + 1) = 0);
   pragma Assert (Recipient_Identity (Object, 0, Tag) = 0);
   pragma Assert (Recipient_Identity (Object, Identity, Unsigned_64'Last) = 0);
   Call (Activate, Tag); pragma Assert (Reply (0) = Bad_State);
   Call (Abort_Session, Tag, Ready => False); pragma Assert (Reply (0) = OK);
   pragma Assert (Resolve (Object, Identity, Tag) = 0);
   pragma Assert (Recipient_Identity (Object, Identity, Tag) = 0);
   Call (Activate, Tag); pragma Assert (Reply (0) = Bad_State);
   Call (Reserve, 0); pragma Assert (Reply (0) = OK and Reply (2) /= Tag);
   Tag := Reply (2);
   Call (Abort_Session, Tag); pragma Assert (Reply (0) = OK);
   Call (Activate, Tag); pragma Assert (Reply (0) = Bad_State);
   -- Retired sessions keep their slots until recycled: the grown slots run
   -- out, and only Recycle (everything released) frees one, under a new tag.
   for Index in 3 .. 4 loop
      Call (Reserve, 0);
      pragma Assert (Reply (0) = OK);
      pragma Assert (Storage_Index (Object, Reply (2)) = Index);
      Call (Abort_Session, Reply (2));
      pragma Assert (Reply (0) = OK);
   end loop;
   pragma Assert (not Free_Slot (Object));
   Call (Reserve, 0); pragma Assert (Reply = [Unavailable, Version, 0, 0]);
   declare
      Old : constant Unsigned_64 := Issued_Tag (Object, 2);
      Accepted : Boolean;
   begin
      pragma Assert (Retired (Object, 2) and not Recycled (Object, Old));
      Hold (Object, 2, RS.GuC_Context);
      Recycle (Object, 2, All_Released, Accepted);
      pragma Assert (not Accepted); -- the GuC context is not deregistered yet
      Acknowledge_Deregistration (Object, 2);
      Recycle (Object, 2, (All_Released with delta Grants_Returned => False), Accepted);
      pragma Assert (not Accepted);
      Recycle (Object, 2, All_Released, Accepted);
      pragma Assert (Accepted and Issued_Tag (Object, 2) = 0 and Recycled (Object, Old));
      pragma Assert (Storage_Index (Object, Old) = 0);
      Call (Reserve, 0);
      pragma Assert (Reply (0) = OK and Storage_Index (Object, Reply (2)) = 2);
      pragma Assert (Reply (2) > Old and Reply (2) /= Old);
      Call (Abort_Session, Old); pragma Assert (Reply (0) = Bad_State);
   end;
   Quarantine (Object);
   Call (Reserve, 0); pragma Assert (Reply (0) = Unavailable);
   for Reserved_Tag of Words'
     (Intel_GPU_Render_Sessions.Tag_Base + 1,
      Intel_GPU_Render_Sessions.Tag_Base + 17,
      Intel_GPU_Render_Sessions.Tag_Last - 1,
      Intel_GPU_Render_Sessions.Tag_Last)
   loop
      declare
         Collision_Storage : constant Controller_Access := new Controller;
         Collision : Controller renames Collision_Storage.all;
      begin
         Bind (Collision, 42, Reserved_Tag);
         pragma Assert (not Is_Broker (Collision, 42, Reserved_Tag));
         Bind (Collision, 42, 99);
         pragma Assert (not Is_Broker (Collision, 42, 99));
      end;
   end loop;
   declare
      Fresh_Storage : constant Controller_Access := new Controller;
      Invalid_Storage : constant Controller_Access := new Controller;
      Fresh : Controller renames Fresh_Storage.all;
      Invalid_Binding : Controller renames Invalid_Storage.all;
      Fresh_Tag : Unsigned_64;
   begin
      Grow (Fresh, 1); Grow (Invalid_Binding, 1);
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
              [Version, Identity, Fresh_Tag, Activate], Reply);
      pragma Assert (Reply (0) = OK);
      Quarantine (Fresh);
      pragma Assert (Resolve (Fresh, Identity, Fresh_Tag) = 0);
      pragma Assert (Recipient_Identity (Fresh, Identity, Fresh_Tag) = 0);
      Handle (Fresh, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, 0, Reserve], Reply);
      pragma Assert (Reply = [Unavailable, Version, 0, 0]);
      Handle (Fresh, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, Fresh_Tag, Activate], Reply);
      pragma Assert (Reply = [Bad_State, Version, 0, 0]);
   end;
   declare
      Reused_Storage : constant Controller_Access := new Controller;
      Reused_PID : Controller renames Reused_Storage.all;
      Old_Tag, New_Tag : Unsigned_64;
      New_Identity : constant Unsigned_64 := Identity + 2 ** 32;
   begin
      Bind (Reused_PID, 42, 99);
      Grow (Reused_PID, 2);
      Handle (Reused_PID, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, 0, Reserve], Reply);
      Old_Tag := Reply (2);
      Handle (Reused_PID, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, Old_Tag, Activate], Reply);
      pragma Assert (Recipient_Identity (Reused_PID, Identity, Old_Tag) = Identity);
      Handle (Reused_PID, 42, 99, True, Label, 4, 0, 0,
              [Version, Identity, Old_Tag, Abort_Session], Reply);
      Handle (Reused_PID, 42, 99, True, Label, 4, 0, 0,
              [Version, New_Identity, 0, Reserve], Reply);
      New_Tag := Reply (2);
      Handle (Reused_PID, 42, 99, True, Label, 4, 0, 0,
              [Version, New_Identity, New_Tag, Activate], Reply);
      pragma Assert (Reply (0) = OK and New_Tag /= Old_Tag);
      pragma Assert (Recipient_Identity (Reused_PID, New_Identity, New_Tag) = New_Identity);
      pragma Assert (Recipient_Identity (Reused_PID, Identity, Old_Tag) = 0);
   end;
   for Activated in Boolean loop
      declare
         Lost_Storage : constant Controller_Access := new Controller;
         Lost : Controller renames Lost_Storage.all;
         Lost_Tag : Unsigned_64;
      begin
         Bind (Lost, 42, 99);
         Grow (Lost, 1);
         Handle (Lost, 42, 99, True, Label, 4, 0, 0,
                 [Version, Identity, 0, Reserve], Reply);
         Lost_Tag := Reply (2);
         if Activated then
            Handle (Lost, 42, 99, True, Label, 4, 0, 0,
                    [Version, Identity, Lost_Tag, Activate], Reply);
         end if;
         Reject_Delivery (Lost, Identity + 2 ** 32, Lost_Tag);
         pragma Assert (Resolve (Lost, Identity, Lost_Tag) = (if Activated then Lost_Tag else 0));
         Reject_Delivery (Lost, Identity, Lost_Tag);
         Reject_Delivery (Lost, Identity, Lost_Tag);
         Reject_Delivery (Lost, Identity, Unsigned_64'Last);
         pragma Assert (Resolve (Lost, Identity, Lost_Tag) = 0);
         Handle (Lost, 42, 99, True, Label, 4, 0, 0,
                 [Version, Identity, Lost_Tag, Activate], Reply);
         pragma Assert (Reply (0) = Bad_State);
      end;
   end loop;
   declare
      Own_Storage : constant Controller_Access := new Controller;
      Own : Controller renames Own_Storage.all;
      First, Other : Unsigned_64 := 0;
      Accepted : Boolean;
   begin
      Bind (Own, 42, 99);
      Grow (Own, 2);
      for I in 1 .. 2 loop
         Handle (Own, 42, 99, True, Label, 4, 0, 0,
           [Version, Identity, 0, Reserve], Reply);
         if I = 1 then First := Reply (2); else Other := Reply (2); end if;
         Handle (Own, 42, 99, True, Label, 4, 0, 0,
           [Version, Identity, Reply (2), Activate], Reply);
         pragma Assert (Reply (0) = OK);
      end loop;
      for Fault in 0 .. 9 loop
         Close_Own (Own, (if Fault = 0 then 43 else Identity),
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
         pragma Assert (Resolve (Own, Identity, First) = First);
         pragma Assert (Resolve (Own, Identity, Other) = Other);
      end loop;
      Close_Own (Own, Identity, First, Close_Own_Label, 4, 0, 0,
        [Version, 0, 0, 0], Reply);
      pragma Assert (Reply = [OK, Version, First, 0]);
      pragma Assert (Resolve (Own, Identity, First) = 0);
      pragma Assert (Resolve (Own, Identity, Other) = Other);
      -- Idempotent for its own client (Mesa closes its queue first, which
      -- already retired the session), denied to anyone else.
      Close_Own (Own, Identity, First, Close_Own_Label, 4, 0, 0,
        [Version, 0, 0, 0], Reply);
      pragma Assert (Reply = [OK, Version, First, 0]);
      Close_Own (Own, 43, First, Close_Own_Label, 4, 0, 0,
        [Version, 0, 0, 0], Reply);
      pragma Assert (Reply = [Denied, Version, 0, 0]);
      -- A client's end through its transport closes the same way.
      End_Session (Own, Other);
      pragma Assert (Resolve (Own, Identity, Other) = 0 and Retired (Own, 2));
      -- Recycled: the stale tag still closes as finished.
      Recycle (Own, 1, All_Released, Accepted); pragma Assert (Accepted);
      Close_Own (Own, Identity, First, Close_Own_Label, 4, 0, 0,
        [Version, 0, 0, 0], Reply);
      pragma Assert (Reply = [OK, Version, First, 0]);
      pragma Assert (Recycled (Own, First) and not Recycled (Own, Other));
   end;
   Ada.Text_IO.Put_Line ("GPU render control PASS: authority, incarnation, activation, late reply, own-session retirement, transport end, recycling");
end Render_Control_Tests;
