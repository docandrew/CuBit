pragma Ada_2022;
with Interfaces;
package Proof_Inputs with SPARK_Mode,
  Abstract_State => (Inputs with External => Async_Writers) is
   procedure Read_Raw (Value : out Interfaces.Unsigned_64; Available : out Boolean)
     with Global => (Input => Inputs);
   procedure Sleep_Raw (Until_Time : Interfaces.Unsigned_64; Accepted : out Boolean)
     with Global => (Input => Inputs);
end Proof_Inputs;
