with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;

with CCL.Interface_Sources;

package body CCL.Interfaces.Images with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Objects.Catalog.Publication_Result;
   use type Argument_Shape;

   IMAGE_FIELDS : constant := 3;
   SIZE_FIELDS : constant := 2;
   GRID_FIELDS : constant := 3;
   SCALED_FIELDS : constant := 2;

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean)
   is
      Specialized : CCL.Types.List_Result;
      Series_Type : CCL.Types.Type_Reference;
      Images_Type : CCL.Types.Type_Reference;
      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Interface_Sources.Named_Type (Types, Name));
   begin
      Bound := (others => <>);
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then
         CCL.Types.Specialize_List (Types, CCL.Types.Integer_Type, Series_Type, Specialized);
         Accepted := Specialized in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      end if;
      if Accepted then
         CCL.Types.Specialize_List (Types, Named ("Image"), Images_Type, Specialized);
         Accepted := Specialized in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Image"), IMAGE_KEY, Bound.Image, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Size"), SIZE_KEY, Bound.Size, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Series_Type, SERIES_KEY, Bound.Series, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Grid"), GRID_KEY, Bound.Grid, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Images_Type, IMAGES_KEY, Bound.Images, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Scaled"), SCALED_KEY, Bound.Scaled, Accepted); end if;
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Types : CCL.Types.Registry := CCL.Catalog.Visible_Types (Item);
      Bound : Contracts;
      Accepted : Boolean;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      procedure Publish_Schema (Contract : CCL.Objects.Binding) is
         Result : CCL.Objects.Catalog.Publication_Result;
      begin
         if Accepted then
            CCL.Catalog.Publish_Schema (Item, Contract, Result);
            Accepted := Result in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published;
         end if;
      end Publish_Schema;
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Bound, Accepted);
      Publish_Schema (Bound.Image);
      Publish_Schema (Bound.Size);
      Publish_Schema (Bound.Series);
      Publish_Schema (Bound.Grid);
      Publish_Schema (Bound.Images);
      Publish_Schema (Bound.Scaled);
      if not Accepted then
         return;
      end if;
      CCL.Catalog.Define_Interface ("image", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         if Shape_Of (Op) = Name_Argument then
            --  Reading a file uses the host's workspace authority.
            CCL.Catalog.Define_Host_Operation
              (Name (Op), 1,
               (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => MAX_FILE_NAME,
                Result => CCL.Host_Values.Object_Value, Result_Schema => IMAGE_KEY,
                Authority => CCL.VM.Observe_Authority, others => <>),
               Entry_Operation, Error);
         else
            CCL.Catalog.Define_Host_Operation
              (Name (Op), 1,
               (Argument => CCL.Host_Values.Object_Value,
                Argument_Schema => (case Shape_Of (Op) is
                                       when Series_Argument => SERIES_KEY,
                                       when Grid_Argument => GRID_KEY,
                                       when Images_Argument => IMAGES_KEY,
                                       when Scaled_Argument => SCALED_KEY,
                                       when Size_Argument | Name_Argument => SIZE_KEY),
                Result => CCL.Host_Values.Object_Value, Result_Schema => IMAGE_KEY,
                Authority => CCL.VM.No_Authority, others => <>),
               Entry_Operation, Error);
         end if;
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;

   procedure Image_Value
     (Contract : CCL.Objects.Binding; Width, Height : Natural; Id : Integer_64;
      Result : out CCL.Objects.Image; Built : out Boolean)
   is
      Step : CCL.Objects.Build_Result;
   begin
      Result := CCL.Objects.Empty (Contract);
      CCL.Objects.Append (Result, CCL.Objects.Product_Cell (IMAGE_FIELDS), Step);
      if Step = CCL.Objects.Added then
         CCL.Objects.Append (Result, CCL.Objects.Integer_Cell (Integer_64 (Width)), Step);
      end if;
      if Step = CCL.Objects.Added then
         CCL.Objects.Append (Result, CCL.Objects.Integer_Cell (Integer_64 (Height)), Step);
      end if;
      if Step = CCL.Objects.Added then
         CCL.Objects.Append (Result, CCL.Objects.Integer_Cell (Id), Step);
      end if;
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Result, Contract);
   end Image_Value;
end CCL.Interfaces.Images;
