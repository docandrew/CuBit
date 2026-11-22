pragma Ada_2022;
with Interfaces;
with ACPI_Region_Protocol;
with ACPI_Region_Client;
with ACPI_Field_Remote_Provider;
package Client_Proof_Model with SPARK_Mode is
   package P renames ACPI_Region_Protocol;
   use type Interfaces.Unsigned_64;
   use type P.Packet;
   use type P.Time_Instant;
   use type P.Delivery;
   type Endpoint is new Interfaces.Unsigned_64;
   No_Endpoint : constant Endpoint := 0;
   type Transport is record
      Invocations : Interfaces.Unsigned_64 := 0;
      Next_Reply : P.Packet;
      Next_Delivery : P.Delivery := P.Not_Dispatched;
      Last_Endpoint : Endpoint := No_Endpoint;
      Last_Deadline : P.Time_Instant := 0;
      Last_Request : P.Packet;
   end record;
   procedure Exchange
     (State : in out Transport; Destination : Endpoint; Deadline : P.Time_Instant;
      Item : P.Packet; Reply : out P.Packet; Delivered : out P.Delivery)
     with Global => null,
       Post => State.Invocations = State.Invocations'Old + 1
         and then State.Last_Endpoint = Destination
         and then State.Last_Deadline = Deadline and then State.Last_Request = Item
         and then Reply = State.Next_Reply'Old and then Delivered = State.Next_Delivery'Old;
   package Client is new ACPI_Region_Client (Transport, Endpoint, No_Endpoint, Exchange);
   -- Abstract clock inputs are arbitrary for each call, not assumed finite.
   Clock_Now, Clock_Deadline : P.Time_Instant := 0;
   procedure Read_Window (Now, Deadline : out P.Time_Instant)
     with Global => (Input => (Clock_Now, Clock_Deadline)),
       Post => Now = Clock_Now and then Deadline = Clock_Deadline;
   package Provider is new ACPI_Field_Remote_Provider
     (Transport, Endpoint, No_Endpoint, Exchange, Read_Window);
end Client_Proof_Model;
