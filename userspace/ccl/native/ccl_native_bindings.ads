------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The services the native CCL host uses, from the program's startup table
--  by binding name (CAP-002): each is the null handle when the program's
--  manifest did not ask for it (or its launcher did not grant it), and the
--  host then reports the service unavailable.
------------------------------------------------------------------------------
with CuBit.Handles;
with CuBit.Handles.Services;

package CCL_Native_Bindings is
   function Clock            return CuBit.Handles.Endpoint;   --  "clock"
   function Logstore         return CuBit.Handles.Services.Logstore.Optional_Endpoint;      --  "logstore"
   function Filesystem       return CuBit.Handles.Endpoint;   --  "filesystem"
   function Config           return CuBit.Handles.Endpoint;   --  "config"
   function Process_Observer return CuBit.Handles.Endpoint;   --  "process-observer"
   function Log_Observer     return CuBit.Handles.Services.Log_Observer.Optional_Endpoint;  --  "log-observer"
   function Log_Control      return CuBit.Handles.Endpoint;   --  "log-control"
   function Network          return CuBit.Handles.Endpoint;   --  "network"
   function Lexicon          return CuBit.Handles.Endpoint;   --  "lexicon"
end CCL_Native_Bindings;
