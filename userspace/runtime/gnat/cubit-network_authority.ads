pragma Ada_2022;
with Interfaces; use Interfaces;

--  Typed network scopes. Wire encoding is protocol data, never authority.
package CuBit.Network_Authority with SPARK_Mode is
   type Operation is (Connect_TCP, Listen_TCP);
   for Operation use (Connect_TCP => 1, Listen_TCP => 2);
   subtype Prefix_Length is Natural range 0 .. 32;
   type Scope is record
      Action : Operation := Connect_TCP;
      Network : Unsigned_32 := 0; -- network-order integer: 10.0.2.0 = 0A000200
      Prefix : Prefix_Length := 0;
      First_Port : Unsigned_16 := 0;
      Last_Port : Unsigned_16 := 0;
      Resolve_Names : Boolean := False;
   end record;
   Denied_Scope : constant Scope := (others => <>);
   Broad_Outbound_TCP : constant Scope :=
     (Connect_TCP, 0, 0, 1, Unsigned_16'Last, True);

   function Mask (Prefix : Prefix_Length) return Unsigned_32;
   function Valid (Item : Scope) return Boolean;
   function Allows
     (Item : Scope; Action : Operation; Address : Unsigned_32;
      Port : Unsigned_16) return Boolean;
   function Includes (Ceiling, Requested : Scope) return Boolean;
   function Descriptor (Item : Scope) return Unsigned_64;
   procedure Decode
     (Address, Descriptor : Unsigned_64; Item : out Scope;
      Success : out Boolean)
     with Post => (if Success then Valid (Item));

   --  Fixed bootstrap endpoint contexts, assigned only by trusted devmgr.
   --  Dynamic tags below are opaque keys into netstack's scope table.
   Policy_Authority_Tag : constant Unsigned_64 := Unsigned_64'Last;
   Manager_Authority_Tag : constant Unsigned_64 := Unsigned_64'Last - 1;
   Driver_Authority_Tag : constant Unsigned_64 := Unsigned_64'Last - 2;
   First_Grant_Tag : constant Unsigned_64 := 2 ** 32;
   Last_Grant_Tag : constant Unsigned_64 := Driver_Authority_Tag - 1;
   Manifest_Request : constant Unsigned_8 := 10;
   Policy_Capability_Slot : constant Unsigned_64 := 19;
   OP_INSTALL_SCOPE : constant Unsigned_32 := 16#0440#;
   OP_RELEASE_SCOPE : constant Unsigned_32 := 16#0441#;
   OP_BIND : constant Unsigned_32 := 16#0424#;
   --  words: listener ID, transfer slot, buffer bytes, transfer generation.
   --  With flag 1, word 2 packs buffer bytes in low 32 bits and a relative
   --  wait (0 .. 30_000 ms) in high 32 bits. Still four inline IPC words.
   --  Without that flag, the service uses its default 30-second timeout.
   --  Expiry releases the pending acquisition.
   --  Reply returns an ordinary owner-bound channel ID, never a TCP index.
   OP_ACCEPT : constant Unsigned_32 := 16#0425#;
   OP_CLOSE_LISTENER : constant Unsigned_32 := 16#0427#;
end CuBit.Network_Authority;
