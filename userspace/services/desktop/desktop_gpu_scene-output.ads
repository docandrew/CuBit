with Compositor_Damage;
with Desktop_Readback_Output;
with Compositor_Formats;
-- Serialized adapter for the CPU presentation path. The caller retains the
-- exact output writer/mapping from capture until Complete or a full repaint.
-- GPU scene completion is only the start of transfer, never publication.
package Desktop_GPU_Scene.Output with SPARK_Mode is
   package R renames Desktop_Readback_Output;
   procedure Pump
     (Scene : in out State; Copy : in out R.State;
      Target : Compositor_Formats.Image; Bytes : Compositor_Formats.Byte_Count;
      Writer : R.P.Ticket; Poll_Only, Capture_Accepted : Boolean;
      Byte_Budget : Natural; Result : out Output_Completion; Repair : Compositor_Damage.State)
     with Global => (In_Out => D.Engine),
       Pre => Compositor_Damage.Valid (Repair) and Valid (Scene) and R.Valid (Copy) and D.Valid,
       Post => Valid (Scene) and R.Valid (Copy) and D.Valid;
end Desktop_GPU_Scene.Output;
