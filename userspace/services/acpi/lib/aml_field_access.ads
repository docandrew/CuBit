pragma Ada_2022;
with AML_Decode;
with AML_Identity;
-- Copied named-register transport values, never address authority. Provider
-- implementations are trusted adapters; no arena or namespace access is passed.
package AML_Field_Access with SPARK_Mode, Pure is
   Default_Maximum_Bindings : constant Positive := 32;
   Default_Maximum_Registers : constant Positive := 32;
   Default_Maximum_Confirmations : constant Positive := 32;
   subtype Counter is AML_Decode.Integer_Value;
   type Endpoint_ID is new Counter;
   No_Endpoint : constant Endpoint_ID := 0;
   type Register_ID is new Counter;
   No_Register : constant Register_ID := 0;
   type Operation_ID is new Counter;
   No_Operation : constant Operation_ID := 0;
   type Target is record
      -- Issuer identity selects one context; it grants no backend rights.
      Provider : AML_Identity.Identity := AML_Identity.No_Identity;
      Endpoint : Endpoint_ID := No_Endpoint;
      Epoch : Counter := 0;
      Register : Register_ID := No_Register;
   end record;
   No_Target : constant Target := (others => <>);
   type Request is record
      Destination : Target;
      For_Write : Boolean := False;
      Value : AML_Decode.Integer_Value := 0;
   end record;
   -- Failed_Completed denotes a completed effect with unusable result.
   -- It is not an unknown completion and never owns a pending receipt.
   type Completion is (Denied, Completed, Failed_Completed, Indeterminate);
   type Response is record
      Status : Completion := Denied;
      Provider : AML_Identity.Identity := AML_Identity.No_Identity;
      Value : AML_Decode.Integer_Value := 0;
      Operation : Operation_ID := No_Operation;
   end record;
   -- Correlation is transport-owned, not a backend Finish ticket, and not
   -- namespace authority. Reconciliation must use authoritative evidence.
   type Observation is record
      Present : Boolean := False;
      Provider : AML_Identity.Identity := AML_Identity.No_Identity;
      Item : Request;
      Operation : Operation_ID := No_Operation;
   end record;
   No_Observation : constant Observation := (others => <>);
end AML_Field_Access;
