with CCL.UI_Labels;
with CCL.UI_Buttons;
with CCL.VM;
with CCL.Host_Values;
package body CCL.Interfaces.Workbench_UI with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.UI_Labels.Operation;
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Operation : CCL.Catalog.Operation_Descriptor;
   begin
      CCL.Catalog.Define_Interface ("ui", 1, 2, Descriptor_Digest, Descriptor, Error);
      if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      for Op in CCL.UI_Labels.Operation loop
         if Op = CCL.UI_Labels.Set_Text then
            CCL.Catalog.Define_Host_Operation
              (CCL.UI_Labels.Name (Op), 1,
               (Argument => CCL.Host_Values.Text_Value,
                Argument_Text_Limit => CCL.Host_Values.Maximum_Text_Length,
                Result => CCL.Host_Values.Boolean_Value,
                Authority => CCL.VM.Control_Authority, others => <>), Operation, Error);
         else
         CCL.Catalog.Define_Operation
           (CCL.UI_Labels.Name (Op), 1,
            (Argument => (if Op = CCL.UI_Labels.Set_Value then CCL.VM.Integer_Value else CCL.VM.Boolean_Value),
             Result => CCL.VM.Boolean_Value, Authority => CCL.VM.Control_Authority,
             others => <>), Operation, Error);
         end if;
         if Error /= CCL.Catalog.Catalog_Valid then return; end if;
         CCL.Catalog.Add_Operation (Descriptor, Operation, Error);
         if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      end loop;
      for Op in CCL.UI_Buttons.Operation loop
         declare
            use type CCL.UI_Buttons.Operation;
         begin
            CCL.Catalog.Define_Host_Operation
              (CCL.UI_Buttons.Name (Op), (if Op = CCL.UI_Buttons.Close_Button then 0 else 1),
               (Argument => (case Op is when CCL.UI_Buttons.On_Click => CCL.Host_Values.Handler_Value,
                   when CCL.UI_Buttons.Set_Text => CCL.Host_Values.Text_Value,
                   when CCL.UI_Buttons.Close_Button => CCL.Host_Values.Integer_Value),
                Argument_Text_Limit => (if Op = CCL.UI_Buttons.Set_Text then CCL.Host_Values.Maximum_Text_Length else 0),
                Result => CCL.Host_Values.Boolean_Value, Authority => CCL.VM.Control_Authority, others => <>),
               Operation, Error);
            if Error /= CCL.Catalog.Catalog_Valid then return; end if;
            CCL.Catalog.Add_Operation (Descriptor, Operation, Error);
            if Error /= CCL.Catalog.Catalog_Valid then return; end if;
         end;
      end loop;
      CCL.Catalog.Publish (Item, Descriptor, Error);
   end Publish;
end CCL.Interfaces.Workbench_UI;
