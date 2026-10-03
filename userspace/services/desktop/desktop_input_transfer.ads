with Compositor_Input_Batch_Wire;
with Compositor_Input_Delivery;
with CuBit.Grant_References;

-- Desktop-internal foreign boundary. Call only after authenticating the
-- surface against the kernel-supplied sender. Engine states must live outside
-- resettable input channels so pending acquisitions survive window teardown.
package Desktop_Input_Transfer with SPARK_Mode => Off is
   package W renames Compositor_Input_Batch_Wire;
   package GR renames CuBit.Grant_References;
   procedure Acquire
     (Owner : W.Identity; Grant : GR.Reference;
      Mapping : out W.Word; Acquired : out Boolean);
   procedure Write
     (Mapping : W.Word; Payload : W.Snapshot_Words; Written : out Boolean);
   procedure Return_Loan (Grant : GR.Reference; Confirmed : out Boolean);
   package Engine is new Compositor_Input_Delivery (Acquire, Write, Return_Loan);
end Desktop_Input_Transfer;
