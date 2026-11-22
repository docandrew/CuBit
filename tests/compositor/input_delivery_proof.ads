with Compositor_Input_Delivery;
with Compositor_Input_Batch_Wire;
with CuBit.Grant_References;
package Input_Delivery_Proof with SPARK_Mode is
   package W renames Compositor_Input_Batch_Wire;
   procedure Acquire
     (Owner : W.Identity; Grant : CuBit.Grant_References.Reference;
      Mapping : out W.Word; Acquired : out Boolean)
     with Import, Global => null;
   procedure Write
     (Mapping : W.Word; Payload : W.Snapshot_Words; Written : out Boolean)
     with Import, Global => null;
   procedure Return_Loan
     (Grant : CuBit.Grant_References.Reference; Confirmed : out Boolean)
     with Import, Global => null;
   package Delivery is new Compositor_Input_Delivery (Acquire, Write, Return_Loan);
end Input_Delivery_Proof;
