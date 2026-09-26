pragma Ada_2022;
with Interfaces; use Interfaces;

--  IPC protocol of tls.svc, the TLS client service. See
--  docs/secure-networking-roadmap.md. Clients reach the service through
--  (request-service tls read-write NAME); which names and ports they may
--  connect to comes from their manifest's (tls-scope "host:port") entries,
--  installed by procmgr before the client runs.
package CuBit.TLS_Protocol with Pure is
   --  Registered service identity (SYSINFO_REGISTERED_DRIVER) and catalog
   --  service number.
   Service_Role : constant Unsigned_64 := 23;

   --  procmgr's own endpoint to tls.svc carries this kernel-stamped tag.
   --  Only it may install or revoke client scopes.
   Policy_Tag : constant Unsigned_64 := 16#8000_0000_0000_0003#;

   --  Client operations. A channel is an opaque 64-bit ID, never reused
   --  within a service lifetime, bound to the client process that opened it.
   --
   --  OPEN: tag.length = length of "HOST:PORT" at transfer offset 0;
   --    words = [transfer slot, transfer bytes, 0, transfer generation].
   --    Deferred reply after the handshake: OK [channel] or ERR [Failure].
   --  WRITE: words = [channel, offset, length]. OK [bytes accepted].
   --  READ: words = [channel, offset, max length, optional absolute
   --    monotonic deadline in ms (tag.length = 4)]. Deferred: OK [length],
   --    EOF after the peer's close_notify, or ERR [Failure].
   --  SHUT: words = [channel]. Sends close_notify and releases. OK.
   --  INFO: words = [channel]. OK [version, cipher suite].
   Open_Operation : constant Unsigned_32 := 16#0C01#;
   Write_Operation : constant Unsigned_32 := 16#0C02#;
   Read_Operation : constant Unsigned_32 := 16#0C03#;
   Shut_Operation : constant Unsigned_32 := 16#0C04#;
   Info_Operation : constant Unsigned_32 := 16#0C05#;

   --  Policy operations (Policy_Tag only).
   --  SET_SCOPES: words = [client PID, count, grant slot, grant generation];
   --    the grant holds count 72-byte entries: byte 0 rights (bit 0 =
   --    connect), byte 1 pattern length, bytes 8 .. 71 the pattern.
   --  REVOKE: words = [client PID]. Drops its scopes and closes its channels.
   Set_Scopes_Operation : constant Unsigned_32 := 16#0C10#;
   Revoke_Operation : constant Unsigned_32 := 16#0C11#;

   Reply_OK : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;
   Reply_EOF : constant Unsigned_32 := 16#F006#;

   --  Carried in word 0 of Reply_Error.
   type Failure is
     (Malformed_Request, Scope_Denied, Unknown_Channel, Quota_Exceeded,
      Busy, Connect_Failed, Timeout, Clock_Untrusted, Certificate_Expired,
      Certificate_Untrusted, Certificate_Rejected, Protocol_Alert,
      Peer_Closed, Service_Failure);
   for Failure use
     (Malformed_Request => 1, Scope_Denied => 2, Unknown_Channel => 3,
      Quota_Exceeded => 4, Busy => 5, Connect_Failed => 6, Timeout => 7,
      Clock_Untrusted => 8, Certificate_Expired => 9,
      Certificate_Untrusted => 10, Certificate_Rejected => 11,
      Protocol_Alert => 12, Peer_Closed => 13, Service_Failure => 14);

   --  The runtime discards enumeration names, so 'Image gives positions.
   function Name (Value : Failure) return String is
     (case Value is
         when Malformed_Request => "malformed request",
         when Scope_Denied => "scope denied",
         when Unknown_Channel => "unknown channel",
         when Quota_Exceeded => "quota exceeded",
         when Busy => "busy",
         when Connect_Failed => "connect failed",
         when Timeout => "timeout",
         when Clock_Untrusted => "clock untrusted",
         when Certificate_Expired => "certificate expired",
         when Certificate_Untrusted => "certificate untrusted",
         when Certificate_Rejected => "certificate rejected",
         when Protocol_Alert => "protocol alert",
         when Peer_Closed => "peer closed",
         when Service_Failure => "service failure");

   Scope_Entry_Bytes : constant := 72;
   Maximum_Scopes_Per_Client : constant := 8;
end CuBit.TLS_Protocol;
