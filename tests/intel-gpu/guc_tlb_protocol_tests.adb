with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GuC_TLB_Protocol;
procedure GuC_TLB_Protocol_Tests is
   package P renames Intel_GPU_GuC_TLB_Protocol;
   use type P.Target;
   use type P.Bit;
   use type P.Bits_4;
   use type P.Bits_19;
   procedure Check (Sequence : Unsigned_32) is
   begin
      for Domain in P.Target loop
         declare
            Request : constant P.Request_Words := P.Build (Sequence, Domain);
            Fields : constant P.Options := P.Decode (Request (2));
            Event : constant P.Completion := P.Decode_Completion ([16#90007001#, Sequence]);
         begin
            pragma Assert (Request (0) = 16#20007000# and Request (1) = Sequence);
            pragma Assert (Request (2) = (if Domain = P.Engines then 16#80000000# else 16#80000003#));
            pragma Assert (Fields.Mode = 0 and Fields.Reserved = 0 and Fields.Flush_Cache = 1);
            pragma Assert (Event.Valid and Event.Sequence = Sequence);
         end;
      end loop;
   end Check;
begin
   for Sequence in Unsigned_32 range 0 .. 65535 loop Check (Sequence); end loop;
   Check (16#80000000#); Check (Unsigned_32'Last);
   for Index in 0 .. 31 loop
      declare
         Event : constant P.Completion := P.Decode_Completion
           ([16#90007001# xor Shift_Left (Unsigned_32'(1), Index), 7]);
      begin pragma Assert (not Event.Valid and Event.Sequence = 0); end;
   end loop;
   for Length in 0 .. 255 loop
      declare
         Payload : P.Words (100 .. 99 + Length) := [others => 0];
         Event : P.Completion;
      begin
         if Length > 0 then Payload (100) := 16#90007001#; end if;
         if Length > 1 then Payload (101) := Unsigned_32'Last; end if;
         Event := P.Decode_Completion (Payload);
         pragma Assert (Event.Valid = (Length = 2));
         pragma Assert (Event.Sequence = (if Length = 2 then Unsigned_32'Last else 0));
      end;
   end loop;
   declare
      Payload : constant P.Words (Natural'Last - 1 .. Natural'Last) :=
        [16#90007001#, 123];
      Event : constant P.Completion := P.Decode_Completion (Payload);
   begin pragma Assert (Event.Valid and Event.Sequence = 123); end;
   Ada.Text_IO.Put_Line ("GuC TLB protocol PASS: heavy/flush field layout, full-width sequences, exact event framing");
end GuC_TLB_Protocol_Tests;
