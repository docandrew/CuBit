with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Metric_Protocol;
-- Test-only collector. Deliberately returns a malformed record count without
-- acquiring a producer grant. The real metrics service remains unchanged.
procedure Main is
   package P renames CuBit.Metric_Protocol;
   From : ProcessID;
   Request, Response : Message;
   Ignore, Requests : Unsigned_64 := 0;
begin
   Ignore := registerDriver (P.Publisher_Service_Role);
   if Ignore = Unsigned_64'Last then
      debugPrint ("TEST: FAIL metrics-fault registration" & ASCII.LF);
      return;
   end if;
   loop
      receive (From, Request);
      Response := NULL_MESSAGE;
      Response.tag := (P.Status'Enum_Rep (P.Denied), P.Message_Words, 0, 0);
      if Request.tag.label = P.Operation'Enum_Rep (P.Publish_Batch) and then
        P.Is_Publisher (Request.authorityTag)
      then
         Requests := Requests + 1;
         if Requests > 2 then
            debugPrint ("TEST: FAIL metrics-fault publisher kept submitting" & ASCII.LF);
         end if;
         Response.tag.label := P.Status'Enum_Rep (P.OK);
         Response.words := [Unsigned_64'Last, 0, 0, 0];
         debugPrint ("TEST: metrics-fault invalid count injected" & ASCII.LF);
      end if;
      Ignore := reply (From, Response);
   end loop;
end Main;
