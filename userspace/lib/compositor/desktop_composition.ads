--  Checked application blit geometry extracted from Desktop.drawClientBuffer.
--  Scene order, acquisitions and display completion remain Desktop's concern.
package Desktop_Composition with SPARK_Mode, Pure is
   type Rectangle is record
      X, Y, W, H : Natural := 0;
   end record;
   type Blit_Plan is record
      Target_X, Target_Y, Source_X, Source_Y : Natural := 0;
      Width, Height : Natural := 0;
   end record;
   function Plan
     (Target_Width, Target_Height, Source_Width, Source_Height : Natural;
      Destination : Rectangle; Clipped : Boolean; Clip : Rectangle)
      return Blit_Plan
   with Post =>
     Plan'Result.Target_X <= Target_Width and then
     Plan'Result.Target_Y <= Target_Height and then
     Plan'Result.Source_X <= Source_Width and then
     Plan'Result.Source_Y <= Source_Height and then
     Plan'Result.Width <= Target_Width - Plan'Result.Target_X and then
     Plan'Result.Height <= Target_Height - Plan'Result.Target_Y and then
     Plan'Result.Width <= Source_Width - Plan'Result.Source_X and then
     Plan'Result.Height <= Source_Height - Plan'Result.Source_Y and then
     (if Plan'Result.Width > 0 and Plan'Result.Height > 0 then
        Plan'Result.Target_X >= Destination.X and then
        Plan'Result.Target_Y >= Destination.Y and then
        Plan'Result.Source_X = Plan'Result.Target_X - Destination.X and then
        Plan'Result.Source_Y = Plan'Result.Target_Y - Destination.Y and then
        Plan'Result.Source_X <= Destination.W and then
        Plan'Result.Source_Y <= Destination.H and then
        Plan'Result.Width <= Destination.W - Plan'Result.Source_X and then
        Plan'Result.Height <= Destination.H - Plan'Result.Source_Y and then
        (if Clipped then
           Plan'Result.Target_X >= Clip.X and then
           Plan'Result.Target_Y >= Clip.Y and then
           Plan'Result.Target_X - Clip.X <= Clip.W and then
           Plan'Result.Target_Y - Clip.Y <= Clip.H and then
           Plan'Result.Width <= Clip.W - (Plan'Result.Target_X - Clip.X) and then
           Plan'Result.Height <= Clip.H - (Plan'Result.Target_Y - Clip.Y)));
end Desktop_Composition;
