with Ada.Text_IO;
with Interfaces;
with Compositor_Input_Protocol;
procedure Input_Protocol_Tests is
   package P renames Compositor_Input_Protocol;
   package DP renames P.DP;
   use type P.Request_Decoding;
   use type P.Receipt;
   use type DP.Status_Code;
   use type Interfaces.Unsigned_8;
   use type P.W.Word;
   Request : constant P.Request := (42, 100, (17, 23), 123);
   Canonical : constant DP.Wire_Message := P.Encode (Request);
   Wire : DP.Wire_Message;
begin
   pragma Assert (P.Decode (Canonical) = (True, Request));
   for Byte in Interfaces.Unsigned_8 loop
      Wire := Canonical; Wire.Length := Byte;
      pragma Assert (P.Decode (Wire).Accepted = (Byte = 4));
      Wire := Canonical; Wire.Flags := Byte;
      pragma Assert (P.Decode (Wire).Accepted = (Byte = 0));
   end loop;
   for Field in DP.Payload'Range loop
      if Field /= 1 then
         Wire := Canonical; Wire.Words (Field) := 0;
         pragma Assert (not P.Decode (Wire).Accepted);
      end if;
   end loop;
   Wire := Canonical; Wire.Reserved := 1;
   pragma Assert (not P.Decode (Wire).Accepted);
   Wire := Canonical; Wire.Label := DP.Code (DP.Poll_Input);
   pragma Assert (not P.Decode (Wire).Accepted);
   Wire := Canonical; Wire.Words (2) := 2 ** 32 + P.GR.Maximum_Slot + 1;
   pragma Assert (not P.Decode (Wire).Accepted);
   for Status in DP.Status_Code range DP.Denied .. DP.Resources_Exhausted loop
      Wire := P.Encode (P.Receipt'(Status => Status));
      pragma Assert (P.Decode (Wire, 123) = (Status => Status));
      Wire.Words (3) := 1;
      pragma Assert (P.Decode (Wire, 123).Status = DP.Invalid_Request);
   end loop;
   for Length in P.W.B.Count loop
      for More in Boolean loop
         if Length > 0 or else not More then
            declare R : constant P.Receipt := (DP.Success, 123, Length, 100, More); begin
               Wire := P.Encode (R);
               pragma Assert (P.Decode (Wire, 123) = R);
               pragma Assert (P.Decode (Wire, 124).Status = DP.Invalid_Request);
               Wire.Words (2) := 9;
               pragma Assert (P.Decode (Wire, 123).Status = DP.Invalid_Request);
            end;
         end if;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("INPUT PROTOCOL: PASS request/receipt bounds, generation, identity and malformed envelopes");
end Input_Protocol_Tests;
