with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;
with CCL.Interface_Sources;
with CCL.Interfaces.Images;

package body CCL.Interfaces.Display with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Objects.Catalog.Publication_Result;

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Area : out CCL.Objects.Binding; Accepted : out Boolean)
   is
      Unbound : CCL.Objects.Binding;
   begin
      Area := Unbound;
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then
         CCL.Objects.Bind
           (Types, CCL.Interface_Sources.Named_Type (Types, "Capture_Area"), AREA_KEY, Area, Accepted);
      end if;
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Types : CCL.Types.Registry := CCL.Catalog.Visible_Types (Item);
      Area : CCL.Objects.Binding;
      Accepted : Boolean;
      Result : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Area, Accepted);
      if not Accepted then return; end if;
      CCL.Catalog.Publish_Schema (Item, Area, Result);
      if Result not in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published then
         return;
      end if;
      CCL.Catalog.Define_Interface ("display", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         --  Seeing the screen observes the system; the capture itself is
         --  the host's display-capture authority and the person's switch.
         CCL.Catalog.Define_Host_Operation
           (Name (Op), (if Op = Capture then 0 else 1),
            (Argument => (if Op = Capture then CCL.Host_Values.Integer_Value else CCL.Host_Values.Object_Value),
             Argument_Schema => (if Op = Capture then CCL.Objects.No_Schema else AREA_KEY),
             Result => CCL.Host_Values.Object_Value, Result_Schema => CCL.Interfaces.Images.IMAGE_KEY,
             Authority => CCL.VM.Observe_Authority, others => <>),
            Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;
end CCL.Interfaces.Display;
