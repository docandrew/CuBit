pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Process_IDs; use CuBit.Process_IDs;
with CuBit.Network_Authority; use CuBit.Network_Authority;

--  Installed network scopes and the channels charged to each.
--
--  Every scope declares how many channels its holder may keep open
--  (Scope.Connections). Install reserves that many against the capacity
--  netstack was started with and refuses a scope that would overcommit it,
--  so one holder can never exhaust channels another was promised. Charge
--  takes one of a grant's channels and Refund returns it.
--
--  Proved (tests/network-authority): the reservations never exceed the
--  capacity, and a grant's open channels never exceed its declaration.
package Network_Grants with SPARK_Mode is
   Maximum_Grants : constant := 32;
   subtype Reservation is Natural
     range 0 .. Maximum_Grants * Connection_Count'Last;
   type Table is private;

   function Reserved (State : Table) return Reservation;
   --  No grant has more channels open than it declared.
   function Within_Limits (State : Table) return Boolean;

   --  Only the authenticated policy endpoint may call Install/Release.
   --  Capacity is the number of channels netstack can hold at once.
   procedure Install
     (State : in out Table; Owner : Process_ID; Item : Scope;
      Capacity : Reservation; Tag : out Unsigned_64; Success : out Boolean)
     with Pre  => Reserved (State) <= Capacity and Within_Limits (State),
          Post => Reserved (State) <= Capacity and Within_Limits (State);
   procedure Release (State : in out Table; Owner : Process_ID; Tag : Unsigned_64)
     with Pre  => Within_Limits (State),
          Post => Within_Limits (State) and
                  Reserved (State) <= Reserved (State)'Old;
   --  Release every scope Owner holds (its process has exited), returning
   --  their reservations; Tags lists what was released, zero elsewhere.
   type Tag_List is array (1 .. Maximum_Grants) of Unsigned_64;
   procedure Release_Owner
     (State : in out Table; Owner : Process_ID; Tags : out Tag_List)
     with Pre  => Within_Limits (State),
          Post => Within_Limits (State) and
                  Reserved (State) <= Reserved (State)'Old;
   function Owned (State : Table; Owner : Process_ID; Tag : Unsigned_64) return Boolean;
   --  Owner's scope under Tag (Denied_Scope if it holds none).
   function Scope_Of (State : Table; Owner : Process_ID; Tag : Unsigned_64) return Scope;
   function May_Resolve
     (State : Table; Owner : Process_ID; Tag : Unsigned_64) return Boolean;
   function Allows
     (State : Table; Owner : Process_ID; Tag : Unsigned_64; Action : Operation;
      Address : Unsigned_32; Port : Unsigned_16) return Boolean;

   --  Channels open under Tag, and the number it declared.
   function In_Use (State : Table; Tag : Unsigned_64) return Connection_Count;
   function Limit (State : Table; Tag : Unsigned_64) return Connection_Count;

   --  Take one of Owner's channels under Tag; fails once it has as many
   --  open as it declared.
   procedure Charge
     (State : in out Table; Owner : Process_ID; Tag : Unsigned_64; Success : out Boolean)
     with Pre  => Within_Limits (State),
          Post => Within_Limits (State) and
                  Reserved (State) = Reserved (State)'Old;
   procedure Refund (State : in out Table; Tag : Unsigned_64)
     with Pre  => Within_Limits (State),
          Post => Within_Limits (State) and
                  Reserved (State) = Reserved (State)'Old;
private
   type Grant_Record is record
      Owner : Process_ID := No_Process;
      Tag : Unsigned_64 := 0;
      Item : Scope := Denied_Scope;
      Open : Connection_Count := 0;
   end record;
   type Grant_Array is array (1 .. Maximum_Grants) of Grant_Record;
   type Table is record
      Entries : Grant_Array := [others => <>];
      Next_Tag : Unsigned_64 := First_Grant_Tag;
      Reserved : Reservation := 0;
   end record;
   function Reserved (State : Table) return Reservation is (State.Reserved);
   function Within_Limits (State : Table) return Boolean is
     (for all E of State.Entries => E.Open <= E.Item.Connections);
end Network_Grants;
