with CuBit.Handles.Fixture;
package body CuBit.Messages is
   function GPU return CuBit.Handles.Endpoint is
     (CuBit.Handles.Fixture.Endpoint_Of (63));
   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64 is
     (Milliseconds);
   function capCall (Target : CuBit.Handles.Endpoint; Msg : in out Message;
                     Deadline : Unsigned_64) return MessageTag is
      -- This synchronous transport fixture does not model kernel deadlines.
      pragma Unreferenced (Deadline);
      Expected : constant MessageTag :=
        (label => (if Budget_Mode then 16#0A2E# else 16#0A20#), length => 4,
         protocol => 0, ipc => <>, reserved => 0);
      Returned : MessageTag := Expected;
      procedure Flip (Tag : in out MessageTag) is
      begin
         case Envelope_Bit is
            when 0 .. 31 =>
               Tag.label := Tag.label xor Shift_Left (Unsigned_32'(1), Envelope_Bit);
            when 32 .. 39 =>
               Tag.length := Tag.length xor Shift_Left (Unsigned_8'(1), Envelope_Bit - 32);
            when 40 .. 47 =>
               Tag.protocol := Tag.protocol xor Shift_Left (Unsigned_8'(1), Envelope_Bit - 40);
            when 48 .. 63 =>
               Tag.reserved := Tag.reserved xor Shift_Left (Unsigned_16'(1), Envelope_Bit - 48);
         end case;
      end Flip;
   begin
      Calls := Calls + 1;
      pragma Assert (CuBit.Handles.Fixture.Word_Of (Target) =
                     CuBit.Handles.Fixture.Word_Of (GPU));
      pragma Assert (Msg.tag = Expected and Msg.authorityTag = 0);
      pragma Assert (Msg.words (0) = (if Budget_Mode then 2 else 1) and
                     Msg.words (1) <= (if Budget_Mode then 0 else 4) and
                     Msg.words (2) = 0 and Msg.words (3) = 0);
      Msg.words := (if Budget_Mode then [0, 33554432, 4096, 15] else [2, 1, 0, 0]);
      if Fault = 1 then return (label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0); end if;
      if Fault = 2 then Msg.tag.protocol := 1; end if;
      if Fault = 3 then
         if Corrupt_Return then Flip (Returned); else Flip (Msg.tag); end if;
      end if;
      return Returned;
   end capCall;
end CuBit.Messages;
