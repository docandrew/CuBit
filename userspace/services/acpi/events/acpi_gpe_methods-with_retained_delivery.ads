pragma Ada_2022;
-- One instance per serialized native source owner. A fresh local Invocation is
-- used for each delivery; one persistent fault slot owns failed result cleanup.
-- Failure is terminal for this instance, even after its result is released.
-- This unit owns no source hardware, runtime window, notification or rearm.
generic
package ACPI_GPE_Methods.With_Retained_Delivery with SPARK_Mode is
   type Delivery_Result is (Delivered, Refused, Delivery_Failed);
   type Failure is (None, Handler_Refused, Result_Retained, Callback_Exception);
   function Failed return Boolean;
   function Pending_Result return Boolean;
   function Why return Failure;
   function Observation return Invocation_Report;
   -- The same trusted evaluator/releaser contracts as the parent apply. On a
   -- callback exception this instance refuses all later dispatch; caller keeps
   -- the original Core and physical owner until independently reconciled.
   procedure Deliver (Service : aliased in out Core.State; Item : Binding;
      Budget : Natural; Result : out Delivery_Result)
     with Pre => Core.Valid (Service);
   -- Cleanup only, on the saved Core. Neither successful cleanup nor a foreign
   -- refusal resets failure/consumption or authorizes hardware rearm.
   procedure Close_Result (Service : in out Core.State; Action : out Action_Result)
     with Pre => Core.Valid (Service);
end ACPI_GPE_Methods.With_Retained_Delivery;
