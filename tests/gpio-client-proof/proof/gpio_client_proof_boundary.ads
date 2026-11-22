pragma Ada_2022;
with GPIO_Control_Wire;
package GPIO_Client_Proof_Boundary with SPARK_Mode,
  Abstract_State => (Transport with External => Async_Writers) is
   procedure Submit (Item : GPIO_Control_Wire.Request;
      Token : GPIO_Control_Wire.Word; Accepted : out Boolean)
     with Import, Global => (In_Out => Transport), Always_Terminates;
   -- Proof-only arbitrary acceptance result. Interface effects/termination,
   -- no reentry or owner-state aliasing, and no accepted submission on False
   -- are trusted transport obligations; no native IPC correctness is proved.
end GPIO_Client_Proof_Boundary;
