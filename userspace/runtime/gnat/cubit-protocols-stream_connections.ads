with CuBit.Authority_Policy;
with CuBit.Protocols.Stream_Policies;

--  Pure connection admission model, NOT a live capability or routing table.
--  A trusted adapter authenticates approvals and resolves current references.
package CuBit.Protocols.Stream_Connections with Pure, SPARK_Mode is
   package Delivery renames CuBit.Protocols.Stream_Policies;
   package Policy renames CuBit.Authority_Policy;
   use type Policy.Decision;

   type Port_Direction is (Input, Output);
   type Port_Reference is record
      Process_Instance : Unsigned_64 := 0;
      Local_Port : Unsigned_32 := 0;
      Generation : Unsigned_64 := 0;
   end record;
   function Valid (Port : Port_Reference) return Boolean is
     (Port.Process_Instance /= 0 and then Port.Local_Port /= 0 and then
      Port.Generation /= 0);

   type Port_Descriptor is record
      Reference : Port_Reference;
      Direction : Port_Direction := Input;
      Profile : Delivery.Policy;
   end record;

   --  Identity is registry-scoped. Generation is the expected CURRENT binding
   --  generation, not one selected by the requester. Zero is never current.
   type Binding_Reference is record
      Identity : Unsigned_64 := 0;
      Generation : Unsigned_64 := 0;
   end record;
   type Connection_Key is record
      Controller_Instance : Unsigned_64 := 0;
      Binding : Binding_Reference;
      Source, Destination : Port_Descriptor;
   end record;
   type Request is record
      Controller_Instance : Unsigned_64 := 0;
      Binding : Binding_Reference;
      Source, Destination : Port_Descriptor;
   end record;
   function Key (Item : Request) return Connection_Key is
     (Controller_Instance => Item.Controller_Instance,
      Binding => Item.Binding,
      Source => Item.Source, Destination => Item.Destination);

   type Action is
     (Reconfigure_Binding, Release_To_Recipient, Accept_From_Source);
   type Approval is record
      For_Connection : Connection_Key;
      Outcome : Policy.Decision := Policy.Not_Requested;
   end record;
   type Approvals is array (Action) of Approval;
   --  Evidence comes from scoped policy/authority resolution, never IPC words
   --  supplied by the app. Each array position has its own authorized issuer.
   function Authorizes
     (Evidence : Approval; Item : Request) return Boolean is
     (Evidence.Outcome = Policy.Approved and then
      Evidence.For_Connection = Key (Item));

   type Decision is
     (Connection_Allowed, Reconfiguration_Denied, Release_Denied,
      Acceptance_Denied, Invalid_Reference, Wrong_Direction,
      Incompatible_Profiles);
   --  Check authority before revealing descriptor validation diagnostics.
   --  Exact profiles: adapters are two separate connections, not an exception
   --  to compatibility. No implicit conversions or wildcard recipients.
   function Check (Item : Request; Evidence : Approvals) return Decision is
     (if not Authorizes (Evidence (Reconfigure_Binding), Item) then
         Reconfiguration_Denied
      elsif not Authorizes (Evidence (Release_To_Recipient), Item) then
         Release_Denied
      elsif not Authorizes (Evidence (Accept_From_Source), Item) then
         Acceptance_Denied
      elsif Item.Controller_Instance = 0 or else
        Item.Binding.Identity = 0 or else Item.Binding.Generation = 0 or else
        not Valid (Item.Source.Reference) or else
        not Valid (Item.Destination.Reference)
      then Invalid_Reference
      elsif Item.Source.Direction /= Output or else
        Item.Destination.Direction /= Input
      then Wrong_Direction
      elsif not Delivery.Compatible
        (Item.Source.Profile, Item.Destination.Profile)
      then Incompatible_Profiles
      else Connection_Allowed);
end CuBit.Protocols.Stream_Connections;
