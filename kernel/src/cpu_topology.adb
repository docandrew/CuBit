pragma Ada_2022;
package body CPU_Topology with SPARK_Mode => On is
   procedure Initialize (T : out Topology; BSP : APIC_ID) is
   begin
      T := (Length => 1, IDs => [others => BSP]);
   end Initialize;
   procedure Include_CPU (T : in out Topology; ID : APIC_ID) is
   begin
      for I in CPU_Index range 0 .. T.Length - 1 loop
         if T.IDs (I) = ID then
            return;
         end if;
         pragma Loop_Invariant
           (for all J in CPU_Index range 0 .. I => T.IDs (J) /= ID);
      end loop;
      if T.Length < CPU_Count'Last then
         T.IDs (T.Length) := ID;
         T.Length := T.Length + 1;
      end if;
   end Include_CPU;
end CPU_Topology;
