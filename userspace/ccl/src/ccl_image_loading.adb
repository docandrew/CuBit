with CCL.Image_Formats;
with CCL.Image_Store;
with CCL.Interfaces.Images;
with CCL.Objects;
with CCL_Workspace;
with CuBit.Failures;

package body CCL_Image_Loading is
   use Interfaces;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL_Workspace.Storage_Result;
   package Images renames CCL.Interfaces.Images;
   package Store renames CCL.Image_Store;

   LOAD_BINDING : constant Unsigned_32 := Images.Binding_Of (Images.Load);
   Image_Contract : CCL.Objects.Binding;

   function Handles (Binding : Unsigned_32) return Boolean is (Binding = LOAD_BINDING);

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Resolved : CCL.Catalog.Resolved_Operation;
      Grant : CCL.Catalog.Grant_Result;
   begin
      --  No workspace, no loading: the operation stays discoverable only.
      Success := True;
      if not CCL_Workspace.Supported then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, Images.IMAGE_KEY, Image_Contract);
      CCL.Catalog.Resolve (Catalog, "image.load", Resolved, Success);
      Success := Success and then CCL.Objects.Is_Bound (Image_Contract);
      if not Success then return; end if;
      CCL.Catalog.Install (Grants, Resolved, LOAD_BINDING, Grant);
      Success := Grant = CCL.Catalog.Grant_Added;
   end Install;

   procedure Start (Width, Height : Positive) is
   begin
      Store.Start (Width, Height, 0);
   end Start;
   procedure Emit (X, Y : Natural; Pixel : Unsigned_32) is
   begin
      Store.Set (X, Y, Pixel);
   end Emit;
   package Formats is new CCL.Image_Formats
     (Maximum_Side => Store.Maximum_Side, Maximum_Pixels => Store.Maximum_Pixels,
      Start => Start, Emit => Emit);
   Decoder : Formats.Decoder;

   procedure Consume (Chunk : String; Keep_Going : out Boolean) is
   begin
      for C of Chunk loop
         Formats.Feed (Decoder, Character'Pos (C));
         exit when Formats.Failed (Decoder);
      end loop;
      Keep_Going := not Formats.Failed (Decoder);
   end Consume;
   procedure Read is new CCL_Workspace.Read_Binary (Consume);

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Result : CCL_Workspace.Storage_Result;
      Id : Store.Image_Id;
      Value : CCL.Objects.Image;
      Built : Boolean;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if Binding /= LOAD_BINDING or else Argument.Kind /= CCL.Host_Values.Text_Value then return; end if;
      Formats.Reset (Decoder);
      Read (Argument.Content.Data (1 .. Argument.Content.Length), Result);
      if Result /= CCL_Workspace.Succeeded or else not Formats.Complete (Decoder) then
         --  A partial draft is abandoned, never stored.
         Store.Discard;
         Reply.Why :=
           (if Result /= CCL_Workspace.Succeeded
            then CCL_Workspace.Failure_Of (Result, Argument.Content.Data (1 .. Argument.Content.Length))
            else CuBit.Failures.Failed
              (CuBit.Failures.Invalid_Argument,
               Argument.Content.Data (1 .. Argument.Content.Length) &
               " is not a whole QOI or binary PPM image within" &
               Natural'Image (Store.Maximum_Pixels) & " pixels"));
         return;
      end if;
      Store.Finish (Id);
      Images.Image_Value (Image_Contract, Store.Width (Id), Store.Height (Id), Id, Value, Built);
      if Built then
         Reply := (Value => CCL.Host_Values.Object_Constant (Value), Success => True, Why => <>);
      end if;
   end Invoke;
end CCL_Image_Loading;
