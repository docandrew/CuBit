------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Service endpoints between the two launchers (CAP-002).
--
--  @description
--  procmgr and devmgr each start some services; a program either one starts
--  may need a service the other started. They exchange the services'
--  untagged endpoints (Grant kept, so the receiver can badge copies for its
--  children) with two calls, both from procmgr to devmgr: synchronous calls
--  only go down (procmgr -> devmgr), and devmgr answers each at once.
--
--  Endpoint_Request: procmgr asks for the endpoint of a service devmgr
--  started (call accepting a capability). words (0) = the service role.
--  REPLY_OK carries the endpoint (REPLY_WITH_CAPABILITY); Reply_Error
--  without a capability when no running service of that role is devmgr's.
--
--  Available: procmgr offers devmgr the endpoint of a service procmgr
--  started (call offering a capability). words (0) = the service role.
--  devmgr takes it only for roles it uses (logstore, desktop) and answers
--  REPLY_OK whether or not it took it.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.Service_Exchange with Pure, SPARK_Mode is

   Endpoint_Request : constant Unsigned_32 := 16#0248#;
   Available        : constant Unsigned_32 := 16#0249#;

   Request_Words : constant := 1;
   Role_Word     : constant := 0;

   Reply_OK    : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;

end CuBit.Service_Exchange;
