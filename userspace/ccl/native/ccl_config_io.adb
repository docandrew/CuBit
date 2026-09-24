with CuBit.Config_Reader;
with CuBit.Config_Inspection;
package body CCL_Config_IO is
   use CuBit.Config_Inspection;
   function Available return Boolean is
      Value : Text;
      Result : Status;
   begin
      CuBit.Config_Reader.Query (Probe, "", Value, Result);
      return Result = OK;
   end Available;
   procedure Query
     (Op : CCL.Interfaces.Config.Operation; Key : String;
      Value : out CCL.Host_Values.Text; Success : out Boolean)
   is
      Data : Text;
      Result : Status;
   begin
      CuBit.Config_Reader.Query
        ((case Op is when CCL.Interfaces.Config.Get => Read_Value,
                     when CCL.Interfaces.Config.Keys => List_Keys), Key, Data, Result);
      Value := (others => <>);
      Success := Result = OK;
      if Success then
         CCL.Host_Values.Copy_Text (Data.Data (1 .. Data.Length), Value, Success);
      end if;
   end Query;
end CCL_Config_IO;
