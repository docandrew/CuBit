pragma Ada_2022;
with Interfaces;
package ACPI_PM1_Global_Release with SPARK_Mode, Pure is
   subtype Control_Word is Interfaces.Unsigned_16;
   use type Control_Word;
   Global_Release : constant Control_Word := 16#0004#;
   Sleep_Enable : constant Control_Word := 16#2000#;
   Command_Bits : constant Control_Word := Global_Release or Sleep_Enable;
   -- Preserve each bank independently. Never carry a read-back write-only
   -- sleep command into this release notification. No PM1 status/W1C access.
   function Compose (Observed : Control_Word) return Control_Word is
     ((Observed and not Command_Bits) or Global_Release)
     with Post => (Compose'Result and Global_Release) /= 0
       and then (Compose'Result and Sleep_Enable) = 0
       and then (Compose'Result and not Command_Bits) = (Observed and not Command_Bits);
   type Completion is (Completed, Not_Attempted, Indeterminate);
   use type Interfaces.Unsigned_64;
   function Classify_Transfer
     (For_Write, Finished : Boolean; Read_Value : Interfaces.Unsigned_64) return Completion is
     (if not Finished then (if For_Write then Indeterminate else Not_Attempted)
      elsif For_Write or else Read_Value <= Interfaces.Unsigned_64 (Control_Word'Last) then Completed
      else Not_Attempted)
     with Post =>
       (if For_Write and not Finished then Classify_Transfer'Result = Indeterminate)
       and then (Classify_Transfer'Result = Completed) =
         (Finished and then (For_Write or else Read_Value <= Interfaces.Unsigned_64 (Control_Word'Last)));
   -- Not_Attempted means no write occurred. A false/unknown hardware result
   -- cannot be interpreted as Not_Attempted without independent evidence.
end ACPI_PM1_Global_Release;
