pragma Ada_2022;
with Interfaces; use Interfaces;

--  Typed network scopes. Wire encoding is protocol data, never authority.
package CuBit.Network_Authority with SPARK_Mode is
   --  Connect_UDP names one remote prefix and port range for a connected
   --  datagram channel. It never permits binding a chosen local port.
   type Operation is (Connect_TCP, Listen_TCP, Connect_UDP);
   for Operation use (Connect_TCP => 1, Listen_TCP => 2, Connect_UDP => 3);
   subtype Prefix_Length is Natural range 0 .. 32;
   --  Channels the holder may keep open at once under this scope, declared
   --  up front and reserved by netstack when the scope is installed
   --  (descriptor bits 49 .. 63). A valid scope asks for at least one.
   Connection_Bits : constant := 15;
   subtype Connection_Count is Natural range 0 .. 2 ** Connection_Bits - 1;
   type Scope is record
      Action : Operation := Connect_TCP;
      Network : Unsigned_32 := 0; -- network-order integer: 10.0.2.0 = 0A000200
      Prefix : Prefix_Length := 0;
      First_Port : Unsigned_16 := 0;
      Last_Port : Unsigned_16 := 0;
      Resolve_Names : Boolean := False;
      Connections : Connection_Count := 0;
   end record;
   Denied_Scope : constant Scope := (others => <>);
   Broad_Outbound_TCP : constant Scope :=
     (Connect_TCP, 0, 0, 1, Unsigned_16'Last, True, Connection_Count'Last);

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
   --  words: process ID. From the policy endpoint when that process has
   --  exited (and before its PID is reused): closes its channels and
   --  listeners and releases its scopes and their reservations.
   OP_RELEASE_OWNER : constant Unsigned_32 := 16#0442#;
end CuBit.Network_Authority;
