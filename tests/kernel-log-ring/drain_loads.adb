pragma Ada_2022;
pragma Warnings (Off, "formal parameter*not referenced");
package body Drain_Loads with SPARK_Mode => Off is
   function Load_Header return Header is ((others => <>));
   function Load_Sequence (Where : Slot) return Unsigned_64 is (Where);
   function Load_Record (Where : Slot) return Log_Record is ((others => <>));
   procedure Deliver (Number : Record_Number; Item : Log_Record) is null;
   procedure Dropped (First, Last : Record_Number) is null;
end Drain_Loads;
