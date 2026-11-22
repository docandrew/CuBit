with Interfaces; use Interfaces;
with CuBit.Handles;

--  Reply authority for these one-process fixtures. The driver code grants
--  memory "via" the saved reply of the request it is answering; here that
--  request is one this process submits to itself (Startup.Device_Manager),
--  so the grants go to this process, which acquires them through the same
--  self endpoint.
package Self_Requests is
   --  Submit one self request, receive it and set its reply aside: that
   --  reply, or the null reply if a step is refused or nothing arrives
   --  within Deadline_Ms. The caller has no other service request pending.
   function Open (Label : Unsigned_32; Deadline_Ms : Unsigned_64)
     return CuBit.Handles.Reply;
   --  Answer it and take the completion: True when both happened in time.
   function Close (Item : CuBit.Handles.Reply; Label : Unsigned_32;
                   Deadline_Ms : Unsigned_64) return Boolean;
end Self_Requests;
