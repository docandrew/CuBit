with Interfaces; use Interfaces;
package Intel_GPU_Broker_Request with SPARK_Mode is
   -- procmgr -> devmgr admission request, NOT the devmgr -> GPU protocol.
   -- Kernel-returned Sender and Stamped_Tag authenticate the policy owner.
   -- procmgr uses its existing CSPACE authority to derive an immutable,
   -- generation-bound application endpoint into a reserved devmgr source
   -- slot before submitting. Neither an application PID nor a capability
   -- reconstructed from request words is accepted here.
   Label : constant Unsigned_32 := 16#4948#;
   Authority_Tag : constant Unsigned_64 := 16#4750_4C41_554E_0001#;
   Version : constant Unsigned_64 := 1;
   Launcher_Endpoint_Slot : constant := 61;
   subtype Source_Slot is Natural range 40 .. 55;
   subtype Destination_Slot is Natural range 0 .. 63;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   -- [version, broker-local application endpoint slot, app destination, nonce]
   -- The launcher owns destination reservation/policy. Range validity does
   -- not authorize overwriting existing app authority. Both source and
   -- destination remain reserved through confirmed session retirement.
   -- Nonce is correlation only. The service adapter must reject duplicate or
   -- conflicting requests within the launcher's retained lifetime; decoding
   -- does not reserve a nonce, a source slot, a destination or a GPU session.
   type Decoded (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Source : Source_Slot;
            Destination : Destination_Slot;
            Nonce : Unsigned_64;
         when False => null;
      end case;
   end record;
   function Decode
     (Expected_Launcher, Sender, Stamped_Tag : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words) return Decoded
   with Global => null,
     Post => (if Decode'Result.Valid then
       Expected_Launcher /= 0 and Sender = Expected_Launcher and
       Stamped_Tag = Authority_Tag and Request_Label = Label and
       Length = 4 and Flags = 0 and Reserved = 0 and Request (0) = Version and
       Unsigned_64 (Decode'Result.Source) = Request (1) and
       Unsigned_64 (Decode'Result.Destination) = Request (2) and
       Decode'Result.Nonce = Request (3) and Decode'Result.Nonce /= 0);
   -- This decoder proves only envelope/authentication-field validation and
   -- numeric bounds. The adapter must Capture(Source), require a readable
   -- grantable endpoint, and preserve that exact captured incarnation through
   -- admission. Sender/tag provenance and CSPACE installation are kernel facts.
end Intel_GPU_Broker_Request;
