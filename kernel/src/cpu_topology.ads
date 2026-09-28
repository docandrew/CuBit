pragma Ada_2022;
with Interfaces;

-- Dense software CPU indices are NOT APIC destinations. Built by the BSP
-- before starting any AP; thereafter immutable (no CPU hotplug yet).
package CPU_Topology with SPARK_Mode => On is
   use type Interfaces.Unsigned_8;
   subtype APIC_ID is Interfaces.Unsigned_8 range 0 .. 254;
   subtype CPU_Index is Natural range 0 .. 254;
   subtype CPU_Count is Positive range 1 .. 255;
   type ID_Array is array (CPU_Index) of APIC_ID;
   type Topology is private;
   function Count (T : Topology) return CPU_Count;
   function Destination (T : Topology; CPU : CPU_Index) return APIC_ID
     with Pre => CPU < Count (T);
   function Unique (T : Topology) return Boolean with Ghost;
   procedure Initialize (T : out Topology; BSP : APIC_ID)
     with Post => Count (T) = 1 and Destination (T, 0) = BSP and Unique (T);
   -- Duplicate records (including the BSP's MADT record) never add a CPU.
   procedure Include_CPU (T : in out Topology; ID : APIC_ID)
     with Pre => Unique (T),
          Post => Unique (T) and Count (T) >= Count (T'Old)
            and Destination (T, 0) = Destination (T'Old, 0);
private
   type Topology is record
      Length : CPU_Count := 1;
      IDs : ID_Array := [others => 0];
   end record;
   function Count (T : Topology) return CPU_Count is (T.Length);
   function Destination (T : Topology; CPU : CPU_Index) return APIC_ID
     is (T.IDs (CPU));
   function Unique (T : Topology) return Boolean is
     (for all I in CPU_Index range 0 .. T.Length - 1 =>
        (for all J in CPU_Index range 0 .. T.Length - 1 =>
           (if I /= J then T.IDs (I) /= T.IDs (J))));
end CPU_Topology;
