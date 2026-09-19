with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Authority_Policy;
with CuBit.Protocols; use CuBit.Protocols;
with CuBit.Protocols.Stream_Policies;
with CuBit.Protocols.Stream_Connections;
procedure Main is
   use CuBit.Protocols.Stream_Policies;
   use CuBit.Protocols.Stream_Connections;
   package AP renames CuBit.Authority_Policy;
   use type AP.Decision;
   --  Fixture identities, not published standard wire schemas.
   Text_Profile : constant CuBit.Protocols.Stream_Policies.Policy :=
     (Element => (Identity => 1, Version => 1,
                  Sizing => Bounded_Size, Wire_Size => 256),
      Delivery => (Kind => Ordered_With_Gaps,
                   When_Lossy_Full => Drop_Oldest_Pending),
      Capacity => (Slots => 4, Maximum_In_Flight => 1,
                   Payload_Bytes => 1024),
      Normal_Close => Drain_Accepted);
   Log_Profile : CuBit.Protocols.Stream_Policies.Policy := Text_Profile;
   Source : Port_Descriptor :=
     (Reference => (Process_Instance => 10, Local_Port => 1, Generation => 1),
      Direction => Output, Profile => Text_Profile);
   Sink : Port_Descriptor :=
     (Reference => (Process_Instance => 20, Local_Port => 1, Generation => 1),
      Direction => Input, Profile => Text_Profile);
   Item, Changed, First_Leg, Second_Leg : Request;
   Evidence, Modified : Approvals;
   Count : Natural := 0;
   procedure Expect (Actual, Expected : Decision) is
   begin
      pragma Assert (Actual = Expected);
      Count := Count + 1;
   end Expect;
   function Approve (R : Request) return Approvals is
     ([others => (For_Connection => Key (R), Outcome => AP.Approved)]);
begin
   Log_Profile.Element.Identity := 2;
   Item := (Controller_Instance => 30,
            Binding => (Identity => 1, Generation => 1),
            Source => Source, Destination => Sink);
   Evidence := Approve (Item);
   Expect (Check (Item, Evidence), Connection_Allowed);

   --  All three independent authorization decisions are required.
   for C in Boolean loop
      for P in Boolean loop
         for S in Boolean loop
            Modified := Evidence;
            Modified (Reconfigure_Binding).Outcome :=
              AP.Evaluate (True, C, True, True);
            Modified (Release_To_Recipient).Outcome :=
              AP.Evaluate (True, P, True, True);
            Modified (Accept_From_Source).Outcome :=
              AP.Evaluate (True, S, True, True);
            Expect (Check (Item, Modified),
              (if not C then Reconfiguration_Denied
               elsif not P then Release_Denied
               elsif not S then Acceptance_Denied else Connection_Allowed));
         end loop;
      end loop;
   end loop;
   for A in Action loop
      for Outcome in AP.Decision loop
         Modified := Evidence;
         Modified (A).Outcome := Outcome;
         pragma Assert
           ((Check (Item, Modified) = Connection_Allowed) =
            (Outcome = AP.Approved));
         Count := Count + 1;
      end loop;
      Modified := Evidence;
      Modified (A).For_Connection.Destination.Reference.Process_Instance := 99;
      pragma Assert (Check (Item, Modified) /= Connection_Allowed);
      Count := Count + 1;
   end loop;

   --  An approval cannot be replayed for another actor, binding or port.
   for Field in 1 .. 10 loop
      Changed := Item;
      case Field is
         when 1 => Changed.Controller_Instance := 31;
         when 2 => Changed.Binding.Identity := 2;
         when 3 => Changed.Binding.Generation := 2;
         when 4 => Changed.Source.Reference.Process_Instance := 11;
         when 5 => Changed.Source.Reference.Local_Port := 2;
         when 6 => Changed.Source.Reference.Generation := 2;
         when 7 => Changed.Destination.Reference.Process_Instance := 21;
         when 8 => Changed.Destination.Reference.Local_Port := 2;
         when 9 => Changed.Destination.Reference.Generation := 2;
         when 10 => Changed.Binding.Generation := Unsigned_64'Last;
      end case;
      Expect (Check (Changed, Evidence), Reconfiguration_Denied);
   end loop;
   for Field in 1 .. 9 loop
      Changed := Item;
      case Field is
         when 1 => Changed.Controller_Instance := 0;
         when 2 => Changed.Binding.Identity := 0;
         when 3 => Changed.Binding.Generation := 0;
         when 4 => Changed.Source.Reference.Process_Instance := 0;
         when 5 => Changed.Source.Reference.Local_Port := 0;
         when 6 => Changed.Source.Reference.Generation := 0;
         when 7 => Changed.Destination.Reference.Process_Instance := 0;
         when 8 => Changed.Destination.Reference.Local_Port := 0;
         when 9 => Changed.Destination.Reference.Generation := 0;
      end case;
      Expect (Check (Changed, Approve (Changed)), Invalid_Reference);
   end loop;
   Changed := Item;
   Changed.Source.Direction := Input;
   Expect (Check (Changed, Approve (Changed)), Wrong_Direction);
   Changed := Item;
   Changed.Destination.Direction := Output;
   Expect (Check (Changed, Approve (Changed)), Wrong_Direction);
   Changed := Item;
   Changed.Destination.Profile.Delivery := (Kind => Latest_Value);
   Expect (Check (Changed, Approve (Changed)), Incompatible_Profiles);
   Changed := Item;
   Changed.Source.Profile.Capacity.Payload_Bytes := 1;
   Expect (Check (Changed, Approve (Changed)), Incompatible_Profiles);
   --  Even a matching profile change cannot reuse an old approval.
   Changed := Item;
   Changed.Source.Profile.Normal_Close := Discard_Pending_With_Report;
   Changed.Destination.Profile := Changed.Source.Profile;
   Expect (Check (Changed, Evidence), Reconfiguration_Denied);

   --  Text cannot connect directly to log records, even with full approval.
   Sink.Profile := Log_Profile;
   Item.Destination := Sink;
   Evidence := Approve (Item);
   Expect (Check (Item, Evidence), Incompatible_Profiles);
   --  An explicit adapter has distinct input/output ports and two approvals.
   First_Leg := Item;
   First_Leg.Destination :=
     (Reference => (Process_Instance => 40, Local_Port => 1, Generation => 1),
      Direction => Input, Profile => Text_Profile);
   Second_Leg := Item;
   Second_Leg.Binding.Identity := 2;
   Source.Reference := (Process_Instance => 40, Local_Port => 2, Generation => 1);
   Source.Profile := Log_Profile;
   Second_Leg.Source := Source;
   Expect (Check (First_Leg, Approve (First_Leg)), Connection_Allowed);
   Expect (Check (Second_Leg, Approve (Second_Leg)), Connection_Allowed);
   Expect (Check (Second_Leg, Approve (First_Leg)), Reconfiguration_Denied);
   Put_Line ("PASS:" & Count'Image & " stream connection admission cases");
end Main;
