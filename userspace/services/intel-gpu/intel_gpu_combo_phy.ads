with Interfaces;
package Intel_GPU_Combo_PHY with SPARK_Mode is
   use Interfaces;
   type PHY is (A, B);
   type Field is (Misc, TX_8, PCS_1, Comp_1, Comp_9, Comp_10,
                  Comp_8, Comp_0, CL_5, Comp_3);
   type Snapshot is array (Field) of Unsigned_32;
   -- Compare only the fields this restoration owns or uses for process/voltage
   -- selection. Status/calibration/reserved observations are not configuration.
   function Same_Configuration (Item : Field; Left, Right : Unsigned_32) return Boolean;
   function Same_Configuration (Left, Right : Snapshot) return Boolean is
     (for all F in Field => Same_Configuration (F, Left (F), Right (F)));
   function Read_Offset (Port : PHY; Item : Field) return Unsigned_32;
   function Write_Offset (Port : PHY; Item : Field) return Unsigned_32
     with Pre => Item /= Comp_3;
   type Write_Item is record
      Register : Field := Misc;
      Value : Unsigned_32 := 0;
   end record;
   type Write_List is array (Positive range 1 .. 9) of Write_Item;
   type Outcome is (Invalid_Read, Unknown_Process, Already_Ready, Restore);
   type Plan is record
      Status : Outcome := Invalid_Read;
      Count : Natural range 0 .. 9 := 0;
      Writes : Write_List := (others => (others => <>));
   end record;
   -- ADL-N combo PHYs only (not Type-C PHYs). Caller must retain power,
   -- serialize access, capture a valid stable snapshot, and restore A before B.
   -- This pure plan is NOT authority to write, nor proof of restored hardware.
   -- Executor must revalidate its snapshot and read back after ordered writes.
   function Prepare (Port : PHY; State : Snapshot) return Plan
     with Global => null,
       Post => (if Prepare'Result.Status = Restore then
                  Prepare'Result.Count = (if Port = A then 9 else 8)
                else Prepare'Result.Count = 0);
end Intel_GPU_Combo_PHY;
