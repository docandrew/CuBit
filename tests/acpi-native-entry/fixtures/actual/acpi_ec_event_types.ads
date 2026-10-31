pragma Ada_2022;
with Interfaces;
--  Internal descriptive event, not a wire ABI or hardware capability.
package ACPI_EC_Event_Types with SPARK_Mode, Pure is
   type Controller_ID is new Interfaces.Unsigned_64;
   type Epoch_ID is new Interfaces.Unsigned_64;
   type Token_ID is new Interfaces.Unsigned_64;
   type Event_Number is range 1 .. Integer (Interfaces.Unsigned_8'Last);
   type Notice is record
      Controller : Controller_ID := 0;
      Epoch : Epoch_ID := 0;
      Token : Token_ID := 0;
      Number : Event_Number := Event_Number'First;
   end record;
   type Notice_Option (Present : Boolean := False) is record
      case Present is
         when True => Item : Notice;
         when False => null;
      end case;
   end record;
   type Publication is (Enqueued, Backpressure, Indeterminate);
   subtype Delivery_Budget is Positive range 1 .. 1_000_000;
end ACPI_EC_Event_Types;
