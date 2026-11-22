with CCL.Handler_References;
with CCL.Resources;
package body Result_Fixture with SPARK_Mode is
   procedure Reply
     (Kind : CCL.Host_Values.Value_Kind; Item : out CCL.Host_Values.Call_Result) is
      use CCL.Host_Values;
      Text_Value_Data : Text;
      Ref : CCL.Handler_References.Reference;
   begin
      -- Intentionally change the kind more than once. The caller cannot
      -- constrain the envelope's value component to its previous alternative.
      Item := (Value => Integer_Constant (0), Success => False, Why => <>);
      case Kind is
         when Integer_Value => Item.Value := Integer_Constant (42);
         when Boolean_Value => Item.Value := Boolean_Constant (True);
         when Text_Value => Item.Value := Text_Constant (Text_Value_Data);
         when Handler_Value => Item.Value := Handler_Constant (Ref);
         when Object_Value => Item.Value := (Kind => Object_Value, Object => <>);
         when Resource_Value => Item.Value := Resource_Constant (CCL.Resources.No_Reference);
      end case;
      Item.Success := True;
   end Reply;
end Result_Fixture;
