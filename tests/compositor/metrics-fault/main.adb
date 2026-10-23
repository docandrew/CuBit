with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Startup;
with CuBit.Metric_Protocol;
-- Test-only collector. Deliberately returns a malformed record count without
-- acquiring a producer grant. The real metrics service remains unchanged.
procedure Main is
   package P renames CuBit.Metric_Protocol;
   From : Process_ID;
   Request, Response : Message;
   Ignore, Requests : Unsigned_64 := 0;
begin
   Ignore := registerDriver (P.Publisher_Service_Role, Startup.Registration);
   if Ignore = Unsigned_64'Last then
      debugPrint ("TEST: FAIL metrics-fault registration" & ASCII.LF);
      return;
   end if;
   loop
      receive (From, Request);
      Response := NULL_MESSAGE;
      Response.tag := (label => P.Status'Enum_Rep (P.Denied), length => P.Message_Words,
                       protocol => 0, ipc => <>, reserved => 0);
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
