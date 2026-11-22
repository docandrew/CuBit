with CCL.Host_Values;
with CCL.VM;
package body CCL.Interfaces.Config with SPARK_Mode is
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      use type CCL.Catalog.Catalog_Error;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Op : CCL.Catalog.Operation_Descriptor;
      -- SHA-256 of interfaces/config.ccl-interface, not a provider identity.
      Digest : constant CCL.Catalog.Descriptor_Digest :=
        [16#4E99C2ED7BF05E15#, 16#D3C14A2BCD431D42#,
         16#5BA6A243242D2C09#, 16#19EFF138FFBC78E5#];
   begin
      CCL.Catalog.Define_Interface ("config", 1, 0, Digest, Descriptor, Error);
      if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      for Kind in Operation loop
         CCL.Catalog.Define_Host_Operation
           (Name (Kind), 1,
            (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => 128,
             Result => CCL.Host_Values.Text_Value, Result_Text_Limit => 1024,
             Authority => CCL.VM.Observe_Authority, others => <>), Op, Error);
         if Error /= CCL.Catalog.Catalog_Valid then return; end if;
         CCL.Catalog.Add_Operation (Descriptor, Op, Error);
         if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      end loop;
      CCL.Catalog.Publish (Item, Descriptor, Error);
   end Publish;
end CCL.Interfaces.Config;
