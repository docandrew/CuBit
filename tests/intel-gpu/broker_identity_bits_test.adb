with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Broker_Request; use Intel_GPU_Broker_Request;
procedure Broker_Identity_Bits_Test is
   Checks : Natural := 0;
   Child : constant Unsigned_64 := 16#0000_0003_0000_0042#;
   Request : constant Words := [Version, Child, 0, Unsigned_64'Last];
   procedure Check (Condition : Boolean; Context : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with Context;
      end if;
   end Check;
   function Call (Expected, Sender, Stamp : Unsigned_64) return Decoded is
     (Decode (Expected, Sender, Stamp, Label, 4, Request_Flags, 0, Request));
begin
   -- The decoder treats a nonzero trusted launcher identity as opaque.
   -- This is not a claim that every test word names a live kernel process.
   for Bit in 0 .. 63 loop
      declare
         Owner : constant Unsigned_64 := Shift_Left (Unsigned_64 (1), Bit);
         Answer : constant Decoded := Call (Owner, Owner, Authority_Tag);
      begin
         Check (Answer.Valid, "exact opaque identity rejected at bit" & Bit'Image);
         Check (Answer.Identity = Child and Answer.Nonce = Unsigned_64'Last,
                "request fields changed");
         for Changed_Bit in 0 .. 63 loop
            declare
               Mask : constant Unsigned_64 := Shift_Left (Unsigned_64 (1), Changed_Bit);
            begin
               Check (not Call (Owner, Owner xor Mask, Authority_Tag).Valid,
                      "different sender accepted");
               Check (not Call (Owner, Owner, Authority_Tag xor Mask).Valid,
                      "different authority stamp accepted");
            end;
         end loop;
      end;
   end loop;
   Check (not Call (0, 0, Authority_Tag).Valid, "zero owner accepted");
   Check (Call (Unsigned_64'Last, Unsigned_64'Last, Authority_Tag).Valid,
          "full-width opaque identity rejected");
   -- Same low word, different high word must never alias a trusted identity.
   Check (not Call (16#0100_0000_0700_002A#, 16#0700_002A#, Authority_Tag).Valid,
          "truncated sender accepted");
   Check (not Call (16#0100_0000_0700_002A#, 16#0200_0000_0700_002A#,
                    Authority_Tag).Valid, "different high word accepted");
   -- v2 shape: the capability opt-in is required, the identity names a
   -- child, the reserved word is zero and the nonce nonzero.
   declare
      Owner : constant Unsigned_64 := 16#0700_002A#;
   begin
      Check (not Decode (Owner, Owner, Authority_Tag, Label, 4, 0, 0, Request).Valid,
             "request without Accepts_Capability accepted");
      Check (not Decode (Owner, Owner, Authority_Tag, Label, 4, Request_Flags, 0,
                         [Version, 0, 0, 1]).Valid, "zero identity accepted");
      Check (not Decode (Owner, Owner, Authority_Tag, Label, 4, Request_Flags, 0,
                         [Version, Child, 1, 1]).Valid, "nonzero reserved word accepted");
      Check (not Decode (Owner, Owner, Authority_Tag, Label, 4, Request_Flags, 0,
                         [Version, Child, 0, 0]).Valid, "zero nonce accepted");
      Check (not Decode (Owner, Owner, Authority_Tag, Label, 4, Request_Flags, 0,
                         [1, Child, 0, 1]).Valid, "version 1 accepted");
   end;
   Ada.Text_IO.Put_Line ("Broker identity bits PASS checks=" & Checks'Image);
end Broker_Identity_Bits_Test;
