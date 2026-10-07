with Desktop_Readback_Output;
with Compositor_Formats;
-- Serialized adapter for the CPU presentation path. The caller retains the
-- exact output writer/mapping from capture until Complete or a full repaint.
-- GPU scene completion is only the start of transfer, never publication.
package Desktop_GPU_Scene.Output with SPARK_Mode => Off is
   package R renames Desktop_Readback_Output;
   procedure Pump
     (Scene : in out State; Copy : in out R.State;
      Target : Compositor_Formats.Image; Bytes : Compositor_Formats.Byte_Count;
      Writer : R.P.Ticket; Poll_Only, Capture_Accepted : Boolean;
      Byte_Budget : Natural; Result : out Output_Completion);
end Desktop_GPU_Scene.Output;
