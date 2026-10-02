with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Probe_Export;
with Native_GPU_Probe_Protocol;
procedure Probe_Export_Tests is
   package Q renames Native_GPU_Probe_Protocol;
   package G renames CuBit.Memory_Grants;
   use type Q.Words;
   Base : constant Unsigned_64 := Intel_GPU_Buffer_Backing.CPU_Base;
   Identity : constant Unsigned_64 := 7 * 2 ** 32 + 42;
   Auth_Calls : Natural := 0;
   Revoke_Authority : Boolean := False;
   Try_Nested : Boolean := False;
   Nested_Rejected : Boolean := False;
   procedure Attempt_Nested;
   Bytes : Unsigned_64 := Q.Pixel_Bytes;
   function Source return Intel_GPU_Buffer_Reply.Backing is
     (Intel_GPU_Buffer_Reply.From_Linear (16#10001000#, Base + 4096,
        Bytes, 16#10000000#));
   procedure Recipient (Sender, Stamp : Unsigned_64;
     Slot : out CuBit.Messages.CapabilitySlot; ID : out Unsigned_64) is
   begin
      Auth_Calls := Auth_Calls + 1;
      if Try_Nested then
         Try_Nested := False;
         Attempt_Nested;
      end if;
      Slot := 7;
      ID := (if Sender = 42 and then Stamp = 123 and then
                not (Revoke_Authority and Auth_Calls > 1) then Identity else 0);
   end Recipient;
   package E is new Intel_GPU_Probe_Export (Source, Recipient);
   Object : E.Export_State;
   Response : Q.Words;
   Reference : Unsigned_64;
   Before : Natural;
   procedure Attempt_Nested is
      Nested_Response : Q.Words;
   begin
      E.Handle (Object, 42, 123, Q.Label, 4, 0, 0,
        Q.Request (Q.Read_Target), Nested_Response);
      Nested_Rejected := Nested_Response = Q.Reply (Q.Unavailable);
   end Attempt_Nested;
   procedure Call (Request : Q.Words; Sender : Unsigned_64 := 42;
                   Stamp : Unsigned_64 := 123) is
   begin
      E.Handle (Object, Sender, Stamp, Q.Label, 4, 0, 0, Request, Response);
   end Call;
begin
   G.Expected_Slot := 7; G.Expected_Offset := Base + 4096;
   G.Expected_Bytes := Q.Pixel_Bytes; G.Expected_Access := G.Read_Access;
   G.Expected_Reference := (slot => 8, generation => 9);
   for Bad in 0 .. 3 loop
      E.Handle (Object, 42, 123,
        (if Bad = 0 then Q.Label + 1 else Q.Label),
        (if Bad = 1 then 3 else 4), (if Bad = 2 then 1 else 0),
        (if Bad = 3 then 1 else 0), Q.Request (Q.Read_Target), Response);
      pragma Assert (Response = Q.Reply (Q.Invalid_Request) and G.Creates = 0);
   end loop;
   Call (Q.Request (Q.Read_Target), Sender => 43);
   pragma Assert (Response = Q.Reply (Q.Denied) and G.Creates = 0);
   Call (Q.Request (Q.Read_Target), Stamp => 124);
   pragma Assert (Response = Q.Reply (Q.Denied) and G.Creates = 0);
   Call ([1, 0, 0, 1]);
   pragma Assert (Response = Q.Reply (Q.Invalid_Request) and G.Creates = 0);
   Try_Nested := True;
   Call (Q.Request (Q.Read_Target));
   pragma Assert (Nested_Rejected);
   pragma Assert (Q.Valid_Reply (Q.Read_Target, Response) and Response (0) = 0);
   Reference := Response (2);
   Before := G.Creates;
   Call (Q.Request (Q.Read_Target));
   pragma Assert (Response = Q.Reply (Q.Success, Reference) and G.Creates = Before);
   Call (Q.Request (Q.Retire_Target, Reference + 1));
   pragma Assert (Response = Q.Reply (Q.Denied) and G.Revokes = 0);
   Call (Q.Request (Q.Retire_Target, Reference));
   pragma Assert (Response = Q.Reply (Q.Pending));
   Call (Q.Request (Q.Read_Target));
   pragma Assert (Response = Q.Reply (Q.Unavailable));
   G.Gone := True;
   Call (Q.Request (Q.Retire_Target, Reference));
   pragma Assert (Response = Q.Reply (Q.Success));
   Call (Q.Request (Q.Retire_Target, Reference));
   pragma Assert (Response = Q.Reply (Q.Success));
   for Mode in 0 .. 2 loop
      declare
         Item : E.Export_State;
      begin
         Auth_Calls := 0; G.Gone := False;
         Revoke_Authority := Mode = 1;
         Bytes := (if Mode = 2 then 8192 else Q.Pixel_Bytes);
         E.Handle (Item, 42, 123, Q.Label, 4, 0, 0,
           Q.Request (Q.Read_Target), Response);
         if Mode = 0 then
            pragma Assert (Response (0) = 0);
            E.Reject_Delivery (Item);
         else
            pragma Assert (Response = Q.Reply (Q.Unavailable));
         end if;
         Revoke_Authority := False;
         Before := G.Creates;
         E.Handle (Item, 42, 123, Q.Label, 4, 0, 0,
           Q.Request (Q.Read_Target), Response);
         pragma Assert (Response = Q.Reply (Q.Unavailable) and G.Creates = Before);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("probe export PASS");
end Probe_Export_Tests;
