with Ada.Text_IO;
with Keyboard_Pending; use Keyboard_Pending;
with Input_Pending;
procedure Keyboard_Tests is
   use type Input_Pending.Item, Word;
   S : State;
   Added : Frame_Length;
   Lost : Boolean;
   Saved : Input_Pending.Item;
   procedure Add (B : Byte) is
   begin Append_Byte (S, B, Added, Lost); end Add;
begin
   -- Every occupancy including the dangerous 31+prefix+suffix boundary.
   for Occupancy in 0 .. Input_Pending.Capacity loop
      declare Empty : State; begin S := Empty; end;
      for I in 1 .. Occupancy loop Add (16#1E#); end loop;
      Add (16#E0#); pragma Assert (Added = 0 and Count (S) = Occupancy);
      Add (16#5B#);
      pragma Assert (Added = 2 and Lost = (Occupancy > 30));
      if Lost then
         pragma Assert (Count (S) = 2 and Element (S, 0).Payload = 16#E0# and
           Element (S, 0).Recover and Element (S, 1).Payload = 16#5B# and not Element (S, 1).Recover);
      end if;
   end loop;
   Reset_Consumer (S); Add (16#E0#); Add (16#E0#);
   pragma Assert (Added = 0 and Count (S) = 0);
   Add (16#5B#);
   Saved := Element (S, 0);
   pragma Assert (Element (S, 0) = Saved); -- no acknowledgement on refusal
   Acknowledge (S); Saved := Element (S, 0);
   pragma Assert (Saved.Payload = 16#5B#);
   pragma Assert (Element (S, 0) = Saved);
   Acknowledge (S); pragma Assert (Count (S) = 0);
   -- If a prefix was already delivered when true local overflow occurs,
   -- the replacement group starts with recovery before its own prefix.
   Add (16#E0#); Add (16#5B#); Acknowledge (S);
   for I in 1 .. 31 loop Add (16#1E#); end loop;
   Add (16#E0#); Add (16#DB#);
   pragma Assert (Lost and Count (S) = 2 and Element (S, 0).Recover and
      Element (S, 0).Payload = 16#E0# and Element (S, 1).Payload = 16#DB#);
   Reset_Consumer (S);
   -- Consumer switch between prefix/suffix cannot deliver a naked suffix.
   Add (16#E0#); Reset_Consumer (S); Add (16#5B#);
   pragma Assert (Added = 0 and Count (S) = 0);
   Add (16#1E#); pragma Assert (Count (S) = 1 and Element (S, 0).Recover);
   Reset_Consumer (S);
   for I in 1 .. 30 loop Add (16#1E#); end loop;
   Add (16#E1#); Add (16#1D#); Add (16#45#); Add (16#E1#); Add (16#9D#); Add (16#C5#);
   pragma Assert (Lost and Added = 6 and Count (S) = 6);
   pragma Assert (Element (S, 0).Payload = 16#E1# and Element (S, 0).Recover);
   pragma Assert (Element (S, 5).Payload = 16#C5# and not Element (S, 5).Recover);
   Ada.Text_IO.Put_Line ("KEYBOARD-PENDING: PASS capacity boundaries, refused prefix/suffix, consumer switch, E1 group");
end Keyboard_Tests;
