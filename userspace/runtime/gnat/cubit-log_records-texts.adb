pragma Ada_2022;
package body CuBit.Log_Records.Texts with SPARK_Mode is
   function Text (Item : Log_Record) return String is
     (Item.Content (1 .. Item.Length));

   function Name (Item : Field) return String is
     (Item.Label (1 .. Item.Label_Length));

   function Severity_Of (Text : String) return Severity is
   begin
      for Level in Severity loop
         if Text = Severity_Literal (Level) then
            return Level;
         end if;
         pragma Loop_Invariant (for all Earlier in Severity'First .. Level => Text /= Severity_Literal (Earlier));
      end loop;
      raise Program_Error;
   end Severity_Of;
end CuBit.Log_Records.Texts;
