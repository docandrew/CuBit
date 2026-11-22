with Interfaces; use Interfaces;
with Capability_ABI;
package Intel_GPU_Broker_Request with SPARK_Mode is
   -- procmgr -> devmgr admission request, NOT the devmgr -> GPU protocol.
   -- Kernel-returned Sender and Stamped_Tag authenticate the policy owner
   -- (procmgr). procmgr names the suspended child it launched by its
   -- process identity and submits with Accepts_Capability: an admitted
   -- request is answered with the child's render endpoint (devmgr's
   -- intel-gpu endpoint badged with the session tag), which procmgr installs
   -- into the child through its CSPACE (CAP-002). No capability crosses
   -- from procmgr to devmgr and none is minted from the identity: it only
   -- names whom intel-gpu admits, and the endpoint works for no one else.
   Label : constant Unsigned_32 := 16#4948#;
   Authority_Tag : constant Unsigned_64 := 16#4750_4C41_554E_0001#;
   Version : constant Unsigned_64 := 2;
   -- The request's flags: exactly the capability opt-in.
   Request_Flags : constant Unsigned_8 :=
     Capability_ABI.Flag_Byte (0, Capability_ABI.Accepting);
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   -- [version, child process identity, 0, nonce]
   -- Nonce is correlation only. The service adapter must reject duplicate or
   -- conflicting requests within the launcher's retained lifetime; decoding
   -- reserves nothing.
   type Decoded (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Identity : Unsigned_64;
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
       Length = 4 and Flags = Request_Flags and Reserved = 0 and
       Request (0) = Version and Request (2) = 0 and
       Decode'Result.Identity = Request (1) and Decode'Result.Identity /= 0 and
       Decode'Result.Nonce = Request (3) and Decode'Result.Nonce /= 0);
   -- Reply words: [version, status, nonce, identity]; an Admitted reply
   -- carries the render endpoint, the others carry nothing.
   Admitted : constant Unsigned_64 := 0;
   Rejected : constant Unsigned_64 := 1;
   Uncertain : constant Unsigned_64 := 2;
end Intel_GPU_Broker_Request;
