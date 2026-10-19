with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Intel_GPU_Render_Control; use Intel_GPU_Render_Control;
with Intel_GPU_Render_Sessions;
with Intel_GPU_Session_Limits;
-- Random launch/exit interleavings of render sessions against a model of
-- the driver's lifecycle (Linux-hosted, the production session table and
-- admission controller): admission (growing the table only when no slot is
-- free), activation, GuC context registration and image publication, exit
-- by Close_Own, by the transport (queue end or death) or by a lost reply,
-- then the hardware acknowledgements and resource releases arriving late
-- and in any order, and recycling attempted at every step.
-- Checked after every step: live sessions never share a slot or GuC context
-- ID; a slot is never reused before its previous session's GuC context was
-- acknowledged deregistered; recycling succeeds exactly when everything is
-- released; tags never repeat; stale tags never resolve and close as
-- finished; the table grows only when full; nothing leaks at the end.
procedure Session_Lifecycle_Tests is
   package RS renames Intel_GPU_Render_Sessions;
   type Controller_Access is access Controller;
   Object : Controller renames Controller_Access'(new Controller).all;
   Broker : constant := 42;
   Broker_Tag : constant := 99;
   Max_Live : constant := 40;
   Launches_Wanted : constant := 20_000;
   -- Model of one admitted session, by its slot.
   type Stage is (Free, Reserved, Active, Retiring);
   type Model_Session is record
      Current : Stage := Free;
      Tag, Identity : Unsigned_64 := 0;
      Registered, Imaged : Boolean := False;         -- holds taken
      Deregister_Due, Image_Due, Facts_Due : Natural := 0; -- steps left
      Deregistered, Image_Released, Facts_Ready : Boolean := False;
   end record;
   type Model_Array is array (RS.Slot range 1 .. Max_Live + Intel_GPU_Session_Limits.Growth_Step)
     of Model_Session;
   Model : Model_Array;
   -- The slot's GuC context ID may still be known to the GuC.
   GuC_Busy : array (Model_Array'Range) of Boolean := [others => False];
   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Random (Below : Positive) return Natural is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Natural (Seed mod Unsigned_64 (Below));
   end Random;
   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures <= 20 then Put_Line ("FAIL: " & What); end if;
      end if;
   end Check;
   Reply : Words;
   Launched, Recycled_Count, Grows, Max_Grown : Natural := 0;
   Last_Tag : Unsigned_64 := 0;
   Old_Tags : array (1 .. 64) of Unsigned_64 := [others => 0];
   Old_Next : Positive := 1;
   Next_Identity : Unsigned_64 := 16#1_0000_0000#;
   function Live_Count return Natural is
      N : Natural := 0;
   begin
      for M of Model loop
         if M.Current /= Free then N := N + 1; end if;
      end loop;
      return N;
   end Live_Count;
   procedure Grow_If_Full is
   begin
      if not Free_Slot (Object) then
         Check (Grown (Object) < Model'Last, "grows only up to what is needed");
         Grow (Object, Natural'Min (Grown (Object) + Intel_GPU_Session_Limits.Growth_Step, Model'Last));
         Grows := Grows + 1;
         Max_Grown := Natural'Max (Max_Grown, Grown (Object));
      end if;
   end Grow_If_Full;
   procedure Launch is
      Identity : constant Unsigned_64 := Next_Identity;
      Slot : RS.Slot_Index;
   begin
      Next_Identity := Next_Identity + 1;
      Grow_If_Full;
      Handle (Object, Broker, Broker_Tag, True, Label, 4, 0, 0,
        [Version, Identity, 0, Reserve], Reply);
      Check (Reply (0) = OK, "admission with a free slot");
      if Reply (0) /= OK then return; end if;
      Slot := Storage_Index (Object, Reply (2));
      Check (Slot in Model'Range and then Model (Slot).Current = Free, "a free slot is reserved");
      Check (not GuC_Busy (Slot), "a slot is reused only after its GuC context was deregistered");
      Check (Reply (2) > Last_Tag, "tags increase");
      Last_Tag := Reply (2);
      Model (Slot) := (Current => Reserved, Tag => Reply (2), Identity => Identity, others => <>);
      Launched := Launched + 1;
   end Launch;
   procedure Begin_Retiring (S : RS.Slot) is
   begin
      Model (S).Current := Retiring;
      -- Late hardware acknowledgements and releases, any order.
      Model (S).Deregister_Due := (if Model (S).Registered then Random (6) else 0);
      Model (S).Image_Due := (if Model (S).Imaged then Random (6) else 0);
      Model (S).Facts_Due := Random (6);
   end Begin_Retiring;
   procedure Step_Session (S : RS.Slot) is
      M : Model_Session renames Model (S);
   begin
      case M.Current is
         when Free => null;
         when Reserved =>
            case Random (4) is
               when 0 | 1 | 2 =>
                  Handle (Object, Broker, Broker_Tag, True, Label, 4, 0, 0,
                    [Version, M.Identity, M.Tag, Activate], Reply);
                  Check (Reply (0) = OK, "activation");
                  M.Current := Active;
               when others =>
                  Handle (Object, Broker, Broker_Tag, True, Label, 4, 0, 0,
                    [Version, M.Identity, M.Tag, Abort_Session], Reply);
                  Check (Reply (0) = OK, "abort");
                  Begin_Retiring (S);
            end case;
         when Active =>
            Check (Resolve (Object, M.Identity, M.Tag) = M.Tag, "active session resolves");
            case Random (10) is
               when 0 =>
                  if not M.Registered then
                     Hold (Object, S, RS.GuC_Context);
                     M.Registered := True; GuC_Busy (S) := True;
                  end if;
               when 1 =>
                  if not M.Imaged then
                     Hold (Object, S, RS.Context_Image); M.Imaged := True;
                  end if;
               when 2 =>
                  Close_Own (Object, M.Identity, M.Tag, Close_Own_Label, 4, 0, 0,
                    [Version, 0, 0, 0], Reply);
                  Check (Reply = [OK, Version, M.Tag, 0], "close own");
                  Begin_Retiring (S);
               when 3 =>
                  End_Session (Object, M.Tag);
                  Begin_Retiring (S);
               when 4 =>
                  Reject_Delivery (Object, M.Identity, M.Tag);
                  Begin_Retiring (S);
               when others => null;
            end case;
         when Retiring =>
            Check (Resolve (Object, M.Identity, M.Tag) = 0, "retired session does not resolve");
            Check (Retired (Object, S), "retired slot stays retired until recycled");
            if Random (5) = 0 then
               -- Its own client closing again (Mesa after its queue ended).
               Close_Own (Object, M.Identity, M.Tag, Close_Own_Label, 4, 0, 0,
                 [Version, 0, 0, 0], Reply);
               Check (Reply = [OK, Version, M.Tag, 0], "close own is idempotent");
            end if;
            if M.Registered and then not M.Deregistered then
               if M.Deregister_Due = 0 then
                  Acknowledge_Deregistration (Object, S);
                  M.Deregistered := True; GuC_Busy (S) := False;
               else M.Deregister_Due := M.Deregister_Due - 1; end if;
            end if;
            if M.Imaged and then not M.Image_Released then
               if M.Image_Due = 0 then
                  Release_Image (Object, S); M.Image_Released := True;
               else M.Image_Due := M.Image_Due - 1; end if;
            end if;
            if not M.Facts_Ready then
               if M.Facts_Due = 0 then M.Facts_Ready := True;
               else M.Facts_Due := M.Facts_Due - 1; end if;
            end if;
            declare
               Facts : constant RS.Release_Facts :=
                 (Backing_Released => M.Facts_Ready or else Random (2) = 0,
                  Grants_Returned => M.Facts_Ready or else Random (2) = 0,
                  Queue_Forgotten => M.Facts_Ready or else Random (2) = 0,
                  Unreferenced => M.Facts_Ready or else Random (2) = 0);
               Expected : constant Boolean :=
                 RS.All_Released (Facts) and then
                 (not M.Registered or else M.Deregistered) and then
                 (not M.Imaged or else M.Image_Released);
               Accepted : Boolean;
            begin
               Recycle (Object, S, Facts, Accepted);
               Check (Accepted = Expected, "recycled exactly when everything is released");
               if Accepted then
                  Check (Issued_Tag (Object, S) = 0 and Recycled (Object, M.Tag),
                         "recycled slot is free and its tag is finished");
                  Old_Tags (Old_Next) := M.Tag;
                  Old_Next := (if Old_Next = Old_Tags'Last then 1 else Old_Next + 1);
                  M := (others => <>);
                  Recycled_Count := Recycled_Count + 1;
               end if;
            end;
      end case;
   end Step_Session;
   procedure Check_Table is
   begin
      for S in Model'Range loop
         if S <= Grown (Object) then
            Check (Issued_Tag (Object, S) = Model (S).Tag, "table and model agree on every slot");
            if Model (S).Current /= Free then
               Check (Storage_Index (Object, Model (S).Tag) = S, "a live tag names its slot");
               for T in Model'Range loop
                  if T /= S and then Model (T).Current /= Free then
                     Check (RS.Context_ID (S) /= RS.Context_ID (T), "live sessions never share a GuC context ID");
                  end if;
               end loop;
            end if;
         end if;
      end loop;
      for Old of Old_Tags loop
         if Old /= 0 then
            Check (Storage_Index (Object, Old) = 0 and Recycled (Object, Old),
                   "a recycled session's tag never resolves again");
         end if;
      end loop;
   end Check_Table;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      Seed := Unsigned_64'Value (Ada.Command_Line.Argument (1));
   end if;
   Bind (Object, Broker, Broker_Tag);
   while Launched < Launches_Wanted loop
      if Live_Count < Max_Live and then Random (3) = 0 then Launch; end if;
      Step_Session (Random (Model'Last) + 1);
      if Random (64) = 0 then Check_Table; end if;
   end loop;
   -- Every session ends; every acknowledgement arrives; nothing is left.
   for S in Model'Range loop
      if Model (S).Current in Reserved | Active then
         Handle (Object, Broker, Broker_Tag, True, Label, 4, 0, 0,
           [Version, Model (S).Identity, Model (S).Tag, Abort_Session], Reply);
         Begin_Retiring (S);
      end if;
   end loop;
   for Round in 1 .. 1_000 loop
      exit when Live_Count = 0;
      for S in Model'Range loop Step_Session (S); end loop;
   end loop;
   Check_Table;
   Check (Live_Count = 0 and not GuC_Busy'Reduce ("or", False), "every session recycled, no GuC ID held");
   for S in 1 .. Grown (Object) loop
      Check (Issued_Tag (Object, S) = 0, "no leaked slot");
   end loop;
   Check (Max_Grown <= Max_Live + Intel_GPU_Session_Limits.Growth_Step,
          "the table grew no further than the concurrency needed");
   Put_Line ("launches" & Natural'Image (Launched) & ", recycled" & Natural'Image (Recycled_Count) &
             ", grows" & Natural'Image (Grows) & ", slots" & Natural'Image (Grown (Object)) &
             ", checks" & Natural'Image (Checks));
   if Failures = 0 then
      Put_Line ("Session lifecycle PASS: random launch/exit interleavings, late acknowledgements, recycling, growth on demand");
   else
      Put_Line ("Session lifecycle FAIL:" & Natural'Image (Failures));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Session_Lifecycle_Tests;
