pragma Ada_2022;
with Interfaces;
package CuBit.HID_Offer_Exchange with Pure, SPARK_Mode is
   use type Interfaces.Unsigned_64;
   Offer_Request : constant Interfaces.Unsigned_32 := 16#0252#;
   Request_Words : constant := 3;
   Request_Word : constant := 0;
   Reference_Word : constant := 1;
   Length_Word : constant := 2;
   Reply_Words : constant := 4;
   Reply_Request_Word : constant := 0;
   Disposition_Word : constant := 1;
   Detail_Word : constant := 2;
   Queue_Position_Word : constant := 3;
   Offer_Budget_Milliseconds : constant := 1_000;
   type Disposition is (Queued, Not_Approved, Malformed, Capacity, Unknown_Controller,
      Controller_Mismatch, GPIO_Not_Admitted, Duplicate_Device, Copy_Failed);
   for Disposition use (Queued => 1, Not_Approved => 2, Malformed => 3, Capacity => 4,
      Unknown_Controller => 5, Controller_Mismatch => 6, GPIO_Not_Admitted => 7,
      Duplicate_Device => 8, Copy_Failed => 9);
   function Valid_Disposition (Value : Interfaces.Unsigned_64) return Boolean is
     (for some D in Disposition => Disposition'Enum_Rep (D) = Value);
end CuBit.HID_Offer_Exchange;
