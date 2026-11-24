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
   --  netstack's endpoint for inspection only (interfaces, wifi.scan): a
   --  plain network-stack service, no network authority.
   function Network_Stack    return CuBit.Handles.Endpoint;   --  "network-stack"
   --  wifi.svc: (wifi.join), (wifi.leave), (wifi.status), (wifi.passphrase).
   function Wifi             return CuBit.Handles.Endpoint;   --  "wifi"
   function Lexicon          return CuBit.Handles.Endpoint;   --  "lexicon"
   function Installer        return CuBit.Handles.Endpoint;   --  "installer"
   --  llm.svc: (llm.ask ...) and its exchanges (CuBit.Llm_Protocol).
   function Llm              return CuBit.Handles.Endpoint;   --  "llm"
   --  tls.svc: (net.fetch ...) (CuBit.TLS_Protocol); the hosts it may
   --  reach are the program's TLS scope.
   function Tls              return CuBit.Handles.Endpoint;   --  "tls"
   --  secrets.svc: (secrets.set ...) and friends (CuBit.Secrets_Protocol).
   function Secrets          return CuBit.Handles.Endpoint;   --  "secrets"
   --  power.svc, as a requester's badge (CuBit.Power_Requests): machine.*.
   function Power            return CuBit.Handles.Endpoint;   --  "power"
   --  power.svc's power sources, read only (CuBit.Power_Sources): power.*.
   function Power_Sources    return CuBit.Handles.Endpoint;   --  "power-sources"
   --  procmgr's badge "debug-observe for the user session" (DBG-001,
   --  CuBit.Launch_Handles): debug.attach.
   function Debug_Session    return CuBit.Handles.Endpoint;   --  "debug-session"
end CCL_Native_Bindings;
