------------------------------------------------------------------------------
--  Opaque loads and callbacks for Drain_Instance.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Kernel_Log_Publication; use Kernel_Log_Publication;

package Drain_Loads with SPARK_Mode is
   function Load_Header return Header;
   function Load_Sequence (Where : Slot) return Unsigned_64;
   function Load_Record (Where : Slot) return Log_Record;
   procedure Deliver (Number : Record_Number; Item : Log_Record);
   procedure Dropped (First, Last : Record_Number);
end Drain_Loads;
