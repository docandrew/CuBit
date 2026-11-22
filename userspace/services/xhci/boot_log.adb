with CuBit.Log;
with CuBit.Messages;

package body Boot_Log is

   procedure Line (Text : String; Warning : Boolean := False) is
   begin
      if Warning then
         CuBit.Log.Warning (Text);
      else
         CuBit.Log.Info (Text);
      end if;
   end Line;

   procedure Debug (Text : String) is
   begin
      CuBit.Messages.debugPrint (Text & ASCII.LF);
   end Debug;

   procedure Flush is
   begin
      CuBit.Log.Flush;
   end Flush;
end Boot_Log;
