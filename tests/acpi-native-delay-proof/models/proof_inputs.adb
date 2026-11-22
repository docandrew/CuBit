pragma Ada_2022;
package body Proof_Inputs with SPARK_Mode,
  Refined_State => (Inputs => (Raw_Time, Raw_Ready, Raw_Accepted)) is
   Raw_Time : Interfaces.Unsigned_64 := 0 with Volatile, Async_Writers;
   Raw_Ready : Boolean := False with Volatile, Async_Writers;
   Raw_Accepted : Boolean := False with Volatile, Async_Writers;
   procedure Read_Raw (Value : out Interfaces.Unsigned_64; Available : out Boolean) is
   begin Value := Raw_Time; Available := Raw_Ready; end Read_Raw;
   procedure Sleep_Raw (Until_Time : Interfaces.Unsigned_64; Accepted : out Boolean) is
      pragma Unreferenced (Until_Time);
   begin Accepted := Raw_Accepted; end Sleep_Raw;
end Proof_Inputs;
