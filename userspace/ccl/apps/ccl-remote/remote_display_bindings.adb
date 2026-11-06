------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with CuBit.Failures;
with CCL.Image_Store;
with CCL.Interfaces.Display;
with CCL.Interfaces.Images;
with CCL.Objects;
with CCL.Objects.Views;
with Remote_Capture;
with Remote_Wire;

package body Remote_Display_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Image_Store.Image_Id;
   use type Remote_Wire.Capture_Result;
   use type CCL.Interfaces.Display.Operation;
   package D renames CCL.Interfaces.Display;
   package Views renames CCL.Objects.Views;
   package W renames Remote_Wire;

   Image_Contract, Area_Contract : CCL.Objects.Binding;

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in D.FIRST_BINDING .. D.Binding_Of (D.Operation'Last));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      D.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, CCL.Interfaces.Images.IMAGE_KEY, Image_Contract);
      CCL.Catalog.Resolve_Schema (Catalog, D.AREA_KEY, Area_Contract);
      Success := CCL.Objects.Is_Bound (Image_Contract) and then CCL.Objects.Is_Bound (Area_Contract);
      for Op in D.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "display." & D.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, D.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Op : D.Operation;
      Output : W.Output_Number := 0;
      Where : Remote_Capture.Area;
      Outcome : W.Capture_Result;
      Width, Height : Natural;
      Pixels : System.Address;
      Id : CCL.Image_Store.Image_Id := CCL.Image_Store.No_Image;
      Result : CCL.Objects.Image;
      Built : Boolean;

      procedure Refuse (Why : CuBit.Failures.Reason; Detail : String; Remedy : String := "") is
      begin
         Reply.Why := CuBit.Failures.Failed (Why, Detail, Remedy);
      end Refuse;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then return; end if;
      Op := D.Operation'Val (Binding - D.FIRST_BINDING);
      if Op = D.Capture_Area then
         declare
            Object : Views.Snapshot;
            Captured : Boolean;
            function Field (Index : Positive) return Integer_64 is
              (CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Views.Root (Object), Index))));
         begin
            if Argument.Kind /= CCL.Host_Values.Object_Value then return; end if;
            Views.Capture (Object, Area_Contract, Argument.Object, Captured);
            if not Captured then return; end if;
            if Field (1) not in 0 .. W.Maximum_Output
              or else (for some I in 2 .. D.AREA_FIELDS => Field (I) not in 0 .. Integer_64 (W.Area_Field'Last))
            then
               Refuse (CuBit.Failures.Invalid_Argument,
                       "an output is 0 .." & W.Maximum_Output'Image & ", an area's fields 0 .. 65535");
               return;
            end if;
            Output := W.Output_Number (Field (1));
            Where := (W.Area_Field (Field (2)), W.Area_Field (Field (3)),
                      W.Area_Field (Field (4)), W.Area_Field (Field (5)));
         end;
      end if;
      Remote_Capture.Take (Output, Where, Outcome, Width, Height, Pixels);
      if Outcome = W.Captured and then
        (Width not in CCL.Image_Store.Large_Side or else Height not in CCL.Image_Store.Large_Side
         or else not CCL.Image_Store.Fits_Large (Width, Height))
      then
         Outcome := W.Capture_Failed;
      end if;
      if Outcome = W.Captured then
         declare
            type Frame_Bytes is array (Natural range 0 .. Width * Height * 4 - 1) of Unsigned_8;
            Frame : Frame_Bytes with Import, Address => Pixels;
            function Source (X, Y : Natural) return CCL.Image_Store.Pixel is
              (Shift_Left (Unsigned_32 (Frame ((Y * Width + X) * 4 + 2)), 16) or
               Shift_Left (Unsigned_32 (Frame ((Y * Width + X) * 4 + 1)), 8) or
               Unsigned_32 (Frame ((Y * Width + X) * 4)));
            procedure Keep is new CCL.Image_Store.Keep_Large (Source);
         begin
            Keep (Width, Height, Id);
         end;
         if Id = CCL.Image_Store.No_Image then
            Outcome := W.Capture_Failed;
         end if;
      end if;
      Remote_Capture.Note
        ("(display." & D.Name (Op) & ") output" & Output'Image &
         (if Outcome = W.Captured then Width'Image & "x" & Height'Image (2 .. Height'Image'Last) else ""),
         Outcome);
      case Outcome is
         when W.Captured =>
            CCL.Interfaces.Images.Image_Value (Image_Contract, Width, Height, Id, Result, Built);
            if Built then
               Reply := (Value => CCL.Host_Values.Object_Constant (Result), Success => True, Why => <>);
            end if;
         when W.Capture_Off =>
            Refuse (CuBit.Failures.Refused, Remote_Capture.Reason (Outcome),
                    "the person at the device turns on Allow screen capture (Deskbar, Remote access)");
         when W.Rate_Limited =>
            Refuse (CuBit.Failures.Refused, Remote_Capture.Reason (Outcome), "ask again in a second");
         when W.No_Capability =>
            Refuse (CuBit.Failures.Not_Granted, Remote_Capture.Reason (Outcome));
         when W.Bad_Output | W.Bad_Area =>
            Refuse (CuBit.Failures.Invalid_Argument, Remote_Capture.Reason (Outcome));
         when W.No_Session | W.No_Frame | W.Capture_Failed =>
            Refuse (CuBit.Failures.Unavailable, Remote_Capture.Reason (Outcome));
      end case;
   end Invoke;
end Remote_Display_Bindings;
