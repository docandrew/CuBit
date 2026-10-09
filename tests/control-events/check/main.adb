------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Grant lifecycle events and control messages on CuBit (docs/data-plane.md;
--  tests/control-events/README.md). This launcher lends control-child.app
--  an outlet ring whose grant posts events (procmgr derives the child's
--  grant from it), then:
--  - a control message to a process it did not launch is refused;
--  - revoking the ring reaches the child through the derived grant, and
--    the child's runtime returns it;
--  - a Stop reaches the child, which exits 0 having seen both;
--  - the launcher is told when its ring came back (EVENT_GRANT_RETURNED).
--  The child also reads control-producer.app's outlet through a channel
--  (the ipc-test endpoint, which this launcher holds and passes on).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CCL_Manifest_Bindings;
with CuBit.Child_Exits;
with CuBit.Control_Events;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Launching;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Outlet_Rings;
with CuBit.Process_Events;
with CuBit.Program_Descriptions;
with CuBit.Stream_Regions;
with CuBit.Streams;

procedure Main is
   package CV renames CuBit.Control_Events;
   package LA renames CuBit.Launch_Arguments;
   package PD renames CuBit.Program_Descriptions;
   use type CV.Event_Kind;
   use type CuBit.Launching.Launch_Result;
   use type CuBit.Child_Exits.Termination_Kind;

   Child_Program : constant String := "control-child.app";
   Ring_Pages : constant := 1;
   Poll_Milliseconds : constant := 10;
   Wait_Milliseconds : constant := 10_000;
   Microseconds_Per_Millisecond : constant := 1_000;

   Failed : Boolean := False;

   procedure Say (Text : String; Pass : Boolean) is
   begin
      debugPrint ("control-check: " & Text & (if Pass then " PASS" else " FAIL") & ASCII.LF);
      Failed := Failed or else not Pass;
   end Say;

   --  A flooder for the whole test: this process keeps its ring at the
   --  producer full (topped up at every pause). Its own messages are
   --  refused once its ring is full; the child's opens and reads must
   --  still get through (docs/ipc-delivery.md, step 3: a flooder fills
   --  only its own ring).
   Flood_Label : constant := 16#7E58#;
   Flooded : Natural := 0;
   procedure Top_Up_Flood is
      Junk : Message := NULL_MESSAGE;
      Flood_Limit : constant := 64;
   begin
      Junk.tag := (label => Flood_Label, length => 0, flags => 0, reserved => 0);
      for N in 1 .. Flood_Limit loop
         exit when not capSubmit (CuBit.Messages.CapabilitySlot (CCL_Manifest_Bindings.Slot_ipc_test),
                                  Junk, NO_COMPLETION_TOKEN);
         Flooded := Flooded + 1;
      end loop;
   end Top_Up_Flood;

   procedure Pause is
      Ignore : Unsigned_64;
   begin
      Top_Up_Flood;
      Ignore := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Sleep_Until_Monotonic_Microsecond,
         CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Read_Monotonic_Microseconds)
           + Poll_Milliseconds * Microseconds_Per_Millisecond);
   end Pause;

   Base : Unsigned_64 := 0;
   Grant : Unsigned_64;
   Reference : CuBit.Memory_Grants.Grant_Reference;
   Rings : CuBit.Outlet_Rings.Table;
   Started : CuBit.Launching.Child;

   --  Lend the child a ring for its unix.stdout, and start it.
   function Start return Boolean is
      Descriptor : PD.Bytes (1 .. PD.Maximum_Descriptor_Bytes);
      Length : PD.Descriptor_Length;
      Result : CuBit.Launching.Launch_Result;
      Failure : LA.Launch_Failure;
      S : PD.Signature;
      Index : PD.Connector_Index;
      Ok, Found : Boolean;
      Empty_Arguments : constant LA.Block (1 .. 0) := [others => 0];
      Empty_Grants : constant CuBit.Launch_Grants.Bytes (1 .. 0) := [others => 0];
   begin
      CuBit.Launching.Describe (Child_Program, Descriptor, Length, Result, Failure);
      if Result /= CuBit.Launching.Launched or else Length = 0 then
         return False;
      end if;
      PD.Decode (Descriptor (1 .. Length), S, Ok);
      if not Ok then
         return False;
      end if;
      PD.Find_Connector (S, "unix.stdout", Index, Found);
      if not Found then
         return False;
      end if;
      CuBit.Launching.Lend_Ring
        (Ring_Pages, CuBit.Streams.TYPE_TEXT_LINE, Base, Grant, Reference, Ok);
      if not Ok then
         return False;
      end if;
      Rings.Count := 1;
      Rings.Entries (1) := (Outlet => Index, Grant => Grant);
      CuBit.Launching.Launch
        (Child_Program, Empty_Arguments, Empty_Grants, Started, Result, Failure, Rings);
      return Result = CuBit.Launching.Launched;
   end Start;

   --  Whether the child has written "ready" into the lent ring.
   function Ready return Boolean is
      Buffer : String (1 .. 64);
      Read : Natural;
   begin
      for Waited in 1 .. Wait_Milliseconds / Poll_Milliseconds loop
         Read := CuBit.Stream_Regions.Read_Owned (Base, Ring_Pages, Buffer'Address, Buffer'Length);
         if Read > 0 then
            return Buffer (1 .. Read) = "ready";
         end if;
         Pause;
      end loop;
      return False;
   end Ready;

   Revoked, Has_Ended, Found : Boolean;
   Control : CuBit.Launching.Control_Result;
   use type CuBit.Launching.Control_Result;
   Ended : CuBit.Child_Exits.Report;
   Item : CV.Event;
   Returned : Boolean := False;
begin
   Say ("the child starts with a lent ring", Start);
   Say ("the child writes into it", Ready);

   CuBit.Launching.Send_Control
     ((Process => Registered_Driver (DRIVER_PROCMGR)), CV.Stop, Control);   --  not its child
   Say ("a control message to a process it did not launch is refused",
        Control = CuBit.Launching.Refused);

   CuBit.Memory_Grants.Revoke (Reference, Revoked);
   Say ("the launcher revokes the ring", Revoked);

   --  Three kinds before the child reads any: each is kept
   --  (docs/ipc-delivery.md), Stop last.
   CuBit.Launching.Send_Control (Started, CV.Interrupt, Control);
   Say ("an Interrupt to its own child is accepted", Control = CuBit.Launching.Sent);
   CuBit.Launching.Send_Control (Started, CV.Reload, Control);
   Say ("a Reload to its own child is accepted", Control = CuBit.Launching.Sent);
   CuBit.Launching.Send_Control (Started, CV.Stop, Control);
   Say ("a Stop to its own child is accepted", Control = CuBit.Launching.Sent);

   for Waited in 1 .. Wait_Milliseconds / Poll_Milliseconds loop
      CuBit.Launching.Poll_Exit (Started, Has_Ended, Ended);
      exit when Has_Ended;
      Pause;
   end loop;
   Say ("the child saw the revoke (its runtime returned the ring), then the Stop",
        Has_Ended and then Ended.Kind = CuBit.Child_Exits.Exited and then Ended.Code = 0);

   --  procmgr returns its hold on the ring once the child is gone.
   for Waited in 1 .. Wait_Milliseconds / Poll_Milliseconds loop
      CuBit.Process_Events.Next (Item, Found);
      if Found and then Item.Kind = CV.Grant_Returned
        and then Item.Slot = Unsigned_64 (Reference.slot)
        and then Item.Generation = Unsigned_64 (Reference.generation)
      then
         Returned := True;
         exit;
      end if;
      if not Found then
         Pause;
      end if;
   end loop;
   Say ("the launcher is told its ring came back", Returned);
   Say ("the launcher flooded the producer throughout, and the child still got through",
        Flooded > 0);

   debugPrint ((if Failed then "CONTROL-CHECK: FAIL" else "CONTROL-CHECK: PASS") & ASCII.LF);
end Main;
