pragma Ada_2022;

package body CuBit.SameBoy_Batches with SPARK_Mode is

   procedure Clear (B : in out Batch) is
   begin
      B.First := 0;
      B.Count := 0;
      B.Overflow := False;
   end Clear;

   procedure Append (B : in out Batch; Frame : Stereo_Frame) is
   begin
      if B.Count < Capacity then
         B.Frames (B.Count) := Frame;
         B.Count := B.Count + 1;
      else
         B.Overflow := True;
      end if;
   end Append;

   procedure Accept_Written (B : in out Batch; Written : Frame_Count) is
   begin
      B.First := B.First + Written;
      if B.First = B.Count then
         B.First := 0;
         B.Count := 0;
      end if;
   end Accept_Written;

end CuBit.SameBoy_Batches;
