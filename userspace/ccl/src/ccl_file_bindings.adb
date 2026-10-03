with CCL.Interfaces.Files;
with CCL.Objects;
with CCL.Objects.Views;
with CCL_Places;
with CuBit.Failures;

package body CCL_File_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Objects.Build_Result;
   package Files renames CCL.Interfaces.Files;
   package Views renames CCL.Objects.Views;
   package Failures renames CuBit.Failures;

   Contracts : Files.Contracts;
   PATH_SEPARATOR : constant Character := '/';

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in Files.FIRST_BINDING .. Files.Binding_Of (Files.Operation'Last));

   function Valid_Name (Name : String) return Boolean is
     (Name'Length in 1 .. Files.MAXIMUM_NAME and then Name /= "." and then Name /= ".." and then
      (for all C of Name => C in ' ' .. '~' and then C /= PATH_SEPARATOR and then C /= '\'));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      Files.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, Files.PLACE_KEY, Contracts.Place);
      CCL.Catalog.Resolve_Schema (Catalog, Files.CHILD_KEY, Contracts.Child);
      CCL.Catalog.Resolve_Schema (Catalog, Files.LISTING_KEY, Contracts.Listing);
      Success := CCL.Objects.Is_Bound (Contracts.Place) and then
        CCL.Objects.Is_Bound (Contracts.Child) and then CCL.Objects.Is_Bound (Contracts.Listing);
      for Op in Files.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "fs." & Files.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, Files.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   --  A Place as an image: its root and path.
   procedure Place_Value (Root, Path : String; Result : out CCL.Objects.Image; Built : out Boolean) is
      Step : CCL.Objects.Build_Result;
   begin
      Result := CCL.Objects.Empty (Contracts.Place);
      CCL.Objects.Append (Result, CCL.Objects.Product_Cell (Files.PLACE_FIELDS), Step);
      if Step = CCL.Objects.Added then CCL.Objects.Append_Text (Result, Root, Step); end if;
      if Step = CCL.Objects.Added then CCL.Objects.Append_Text (Result, Path, Step); end if;
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Result, Contracts.Place);
   end Place_Value;

   function Joined (Root, Path : String) return String is
     (if Path'Length = 0 then Root else Root & PATH_SEPARATOR & Path);

   --  The manifest line that grants reading Where.
   function Read_Scope (Where : String) return String is
     ("the program's manifest must declare (filesystem-scope (rights read) """ & Where & """)");

   --  Why listing Where did not succeed, in the words of its result.
   function Listing_Failure (Result : CCL_Places.Result_Kind; Where : String) return Failures.Failure is
     (case Result is
         when CCL_Places.Listed_All => Failures.Failed
           (Failures.Exhausted, "the listing of " & Where & " does not fit one result"),
         when CCL_Places.Not_Found => Failures.Failed
           (Failures.Not_Found, "nothing is at " & Where),
         when CCL_Places.Access_Denied => Failures.Failed
           (Failures.Outside_Scope, Where & " is outside this program's filesystem scope", Read_Scope (Where)),
         when CCL_Places.Unavailable => Failures.Failed
           (Failures.Unavailable, "the filesystem service is not running or refused the request queue",
            "the program's manifest must request the filesystem service " &
            "(request-service filesystem read-write filesystem)"),
         when CCL_Places.Failed => Failures.Failed
           (Failures.Device_Error, "the filesystem service could not read " & Where));

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Image : CCL.Objects.Image;
      Built, Captured : Boolean := False;
      Object : Views.Snapshot;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      case Files.Operation'Val (Binding - Files.FIRST_BINDING) is
         when Files.Home =>
            declare
               Root : constant String := CCL_Places.Home;
            begin
               if Root'Length in 1 .. Files.MAXIMUM_PATH then
                  Place_Value (Root, "", Image, Built);
               else
                  Reply.Why := Failures.Failed
                    (Failures.Not_Granted, "this program has no workspace",
                     "the program's manifest must declare a place it may use, such as " &
                     "(filesystem-scope (rights read write create) ""@nvme:0/work"")");
               end if;
            end;
         when Files.Enter =>
            if Argument.Kind = CCL.Host_Values.Object_Value then
               Views.Capture (Object, Contracts.Child, Argument.Object, Captured);
            end if;
            if Captured then
               declare
                  Place : constant Views.Cursor := Views.Field (Object, Views.Root (Object), 1);
                  Root : constant String := Views.Text (Object, Views.Field (Object, Place, 1));
                  Path : constant String := Views.Text (Object, Views.Field (Object, Place, 2));
                  Name : constant String := Views.Text (Object, Views.Field (Object, Views.Root (Object), 2));
                  Below : constant String := (if Path'Length = 0 then Name else Path & PATH_SEPARATOR & Name);
               begin
                  if not Valid_Name (Name) then
                     Reply.Why := Failures.Failed
                       (Failures.Invalid_Argument,
                        """" & Name & """ is not one entry's name (no '/', '\', '.' or '..')",
                        "enter one level at a time, and use fs.up for the parent");
                  elsif Below'Length > Files.MAXIMUM_PATH then
                     Reply.Why := Failures.Failed
                       (Failures.Invalid_Argument, "the path would be longer than" &
                        Natural'Image (Files.MAXIMUM_PATH) & " bytes");
                  else
                     Place_Value (Root, Below, Image, Built);
                  end if;
               end;
            end if;
         when Files.Up =>
            --  The parent, never above the root: up from the root stays there.
            if Argument.Kind = CCL.Host_Values.Object_Value then
               Views.Capture (Object, Contracts.Place, Argument.Object, Captured);
            end if;
            if Captured then
               declare
                  Root : constant String := Views.Text (Object, Views.Field (Object, Views.Root (Object), 1));
                  Path : constant String := Views.Text (Object, Views.Field (Object, Views.Root (Object), 2));
                  Cut : Natural := 0;
               begin
                  for I in Path'Range loop
                     if Path (I) = PATH_SEPARATOR then Cut := I; end if;
                  end loop;
                  Place_Value (Root, (if Cut = 0 then "" else Path (Path'First .. Cut - 1)), Image, Built);
               end;
            end if;
         when Files.List =>
            if Argument.Kind = CCL.Host_Values.Object_Value then
               Views.Capture (Object, Contracts.Place, Argument.Object, Captured);
            end if;
            if Captured then
               declare
                  Root : constant String := Views.Text (Object, Views.Field (Object, Views.Root (Object), 1));
                  Path : constant String := Views.Text (Object, Views.Field (Object, Views.Root (Object), 2));
                  Entries : CCL_Places.Listing;
                  Count : CCL_Places.Listed_Count;
                  Total : Natural;
                  Result : CCL_Places.Result_Kind;
                  Step : CCL.Objects.Build_Result := CCL.Objects.Added;
                  use type CCL_Places.Result_Kind;
                  procedure Put (Cell : CCL.Objects.Cell) is
                  begin
                     if Step = CCL.Objects.Added then CCL.Objects.Append (Image, Cell, Step); end if;
                  end Put;
                  function Number (Value : Unsigned_64) return CCL.Objects.Cell is
                    (CCL.Objects.Integer_Cell
                       (if Value > Unsigned_64 (Integer_64'Last) then Integer_64'Last else Integer_64 (Value)));
               begin
                  CCL_Places.List (Joined (Root, Path), Entries, Count, Total, Result);
                  if Result = CCL_Places.Listed_All then
                     Image := CCL.Objects.Empty (Contracts.Listing);
                     Put (CCL.Objects.Sequence_Cell (Count));
                     for I in 1 .. Count loop
                        Put (CCL.Objects.Product_Cell (Files.METADATA_FIELDS));
                        if Step = CCL.Objects.Added then
                           CCL.Objects.Append_Text
                             (Image, Entries (I).Name (1 .. Entries (I).Name_Length), Step);
                        end if;
                        Put (CCL.Objects.Variant_Cell (Files.File_Kind'Pos (Entries (I).Kind) + 1));
                        Put (CCL.Objects.Unit_Cell);
                        Put (Number (Entries (I).Size));
                        Put (Number (Entries (I).Modified_Ms));
                        Put (Number (Unsigned_64 (Entries (I).Mode)));
                        Put (Number (Unsigned_64 (Entries (I).Links)));
                     end loop;
                     Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Image, Contracts.Listing);
                  end if;
                  if not Built then
                     Reply.Why := Listing_Failure (Result, Joined (Root, Path));
                  end if;
               end;
            end if;
      end case;
      if Built then
         Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => True, Why => <>);
      end if;
   end Invoke;
end CCL_File_Bindings;
