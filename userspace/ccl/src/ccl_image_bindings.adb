with CCL.Image_Store;
with CCL.Interfaces.Images;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Types;

package body CCL_Image_Bindings is
   use Interfaces;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Interfaces.Images.Operation;
   use type CCL.Interfaces.Images.Argument_Shape;
   use type CCL.Image_Store.Image_Id;
   package Images renames CCL.Interfaces.Images;
   package Store renames CCL.Image_Store;
   package Views renames CCL.Objects.Views;

   FIRST_BINDING : constant Unsigned_32 := Images.FIRST_BINDING;
   function Binding_Of (Item : Images.Operation) return Unsigned_32 renames Images.Binding_Of;
   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in FIRST_BINDING .. Binding_Of (Images.Drawing'Last));

   --  The contracts as this catalog resolved them.
   Image_Contract, Size_Contract, Series_Contract, Grid_Contract,
     Images_Contract, Scaled_Contract : CCL.Objects.Binding;

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      Images.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, Images.IMAGE_KEY, Image_Contract);
      CCL.Catalog.Resolve_Schema (Catalog, Images.SIZE_KEY, Size_Contract);
      CCL.Catalog.Resolve_Schema (Catalog, Images.SERIES_KEY, Series_Contract);
      CCL.Catalog.Resolve_Schema (Catalog, Images.GRID_KEY, Grid_Contract);
      CCL.Catalog.Resolve_Schema (Catalog, Images.IMAGES_KEY, Images_Contract);
      CCL.Catalog.Resolve_Schema (Catalog, Images.SCALED_KEY, Scaled_Contract);
      Success := CCL.Objects.Is_Bound (Image_Contract) and then CCL.Objects.Is_Bound (Size_Contract) and then
        CCL.Objects.Is_Bound (Series_Contract) and then CCL.Objects.Is_Bound (Grid_Contract) and then
        CCL.Objects.Is_Bound (Images_Contract) and then CCL.Objects.Is_Bound (Scaled_Contract);
      for Op in Images.Drawing loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "image." & Images.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   ---------------------------------------------------------------------------
   --  Drawing. Charts share the console's dark palette, so a chart reads the
   --  same in the console, the Workbench and the Observatory.
   ---------------------------------------------------------------------------
   CHART_WIDTH  : constant := 320;
   CHART_HEIGHT : constant := 120;
   CHART_MARGIN : constant := 6;
   CHART_BACKGROUND : constant Store.Pixel := 16#0B1220#;
   CHART_GRID       : constant Store.Pixel := 16#1A2438#;
   CHART_LINE       : constant Store.Pixel := 16#5CC8FF#;
   CHART_FILL       : constant Store.Pixel := 16#123048#;
   CHART_BAR        : constant Store.Pixel := 16#7EE0B5#;
   CHART_BAR_NEGATIVE : constant Store.Pixel := 16#FF6B7A#;
   GRID_LINES : constant := 4;
   MAXIMUM_VALUES : constant := CCL.Objects.Maximum_Cells;
   type Value_Array is array (1 .. MAXIMUM_VALUES) of Integer_64;

   --  A colour between two, at Numerator/Denominator of the way.
   function Mix (From, To : Store.Pixel; Numerator, Denominator : Integer) return Store.Pixel is
      Whole : constant Positive := Integer'Max (1, Denominator);
      Part : constant Natural := Integer'Max (0, Integer'Min (Numerator, Whole));
      function Channel (Shift : Natural) return Unsigned_32 is
         A : constant Integer := Integer (Shift_Right (From, Shift) and 16#FF#);
         B : constant Integer := Integer (Shift_Right (To, Shift) and 16#FF#);
      begin
         return Unsigned_32 (A + (B - A) * Part / Whole);
      end Channel;
   begin
      return Shift_Left (Channel (16), 16) or Shift_Left (Channel (8), 8) or Channel (0);
   end Mix;

   --  Heat colours from cold to hot, interpolated.
   HEAT_STOPS : constant array (0 .. 4) of Store.Pixel :=
     [16#1B1F4B#, 16#3B4CC0#, 16#5CC8FF#, 16#FFD866#, 16#FF6B4A#];
   function Heat (Value, Low, High : Integer_64) return Store.Pixel is
      SCALE : constant := 1_000;
      Span : constant Integer_64 := Integer_64'Max (1, High - Low);
      Level : constant Natural :=
        Natural (Integer_64'Min (SCALE, (Integer_64'Max (0, Value - Low) * SCALE) / Span));
      Band : constant Natural := Natural'Min (HEAT_STOPS'Last - 1, Level * (HEAT_STOPS'Last) / SCALE);
      Band_Start : constant Natural := Band * SCALE / HEAT_STOPS'Last;
   begin
      return Mix (HEAT_STOPS (Band), HEAT_STOPS (Band + 1), (Level - Band_Start) * HEAT_STOPS'Last, SCALE);
   end Heat;

   procedure Line (X0, Y0, X1, Y1 : Integer; Colour : Store.Pixel) is
      DX : constant Integer := abs (X1 - X0);
      DY : constant Integer := -abs (Y1 - Y0);
      SX : constant Integer := (if X0 < X1 then 1 else -1);
      SY : constant Integer := (if Y0 < Y1 then 1 else -1);
      Error : Integer := DX + DY;
      X : Integer := X0;
      Y : Integer := Y0;
   begin
      loop
         if X >= 0 and then Y >= 0 then
            Store.Set (X, Y, Colour);
            if Y + 1 < CHART_HEIGHT then Store.Set (X, Y + 1, Colour); end if;
         end if;
         exit when X = X1 and then Y = Y1;
         declare
            Twice : constant Integer := 2 * Error;
         begin
            if Twice >= DY then Error := Error + DY; X := X + SX; end if;
            if Twice <= DX then Error := Error + DX; Y := Y + SY; end if;
         end;
      end loop;
   end Line;

   procedure Chart_Frame is
   begin
      Store.Start (CHART_WIDTH, CHART_HEIGHT, CHART_BACKGROUND);
      for G in 1 .. GRID_LINES - 1 loop
         for X in 0 .. CHART_WIDTH - 1 loop
            Store.Set (X, CHART_MARGIN + G * (CHART_HEIGHT - 2 * CHART_MARGIN) / GRID_LINES, CHART_GRID);
         end loop;
      end loop;
   end Chart_Frame;

   procedure Chart (Values : Value_Array; Count : Positive; As_Bars : Boolean) is
      Low : Integer_64 := Values (1);
      High : Integer_64 := Values (1);
      Plot_Height : constant Positive := CHART_HEIGHT - 2 * CHART_MARGIN;
      Plot_Width : constant Positive := CHART_WIDTH - 2 * CHART_MARGIN;
      function Y_Of (Value : Integer_64) return Integer is
        (CHART_MARGIN + Plot_Height - 1 -
         Integer ((Value - Low) * Integer_64 (Plot_Height - 1) / Integer_64'Max (1, High - Low)));
      function X_Of (Index : Positive) return Integer is
        (if Count = 1 then CHART_WIDTH / 2
         else CHART_MARGIN + (Index - 1) * (Plot_Width - 1) / (Count - 1));
   begin
      for I in 1 .. Count loop
         Low := Integer_64'Min (Low, Values (I));
         High := Integer_64'Max (High, Values (I));
      end loop;
      if As_Bars then
         Low := Integer_64'Min (Low, 0);
         High := Integer_64'Max (High, 0);
      end if;
      Chart_Frame;
      if As_Bars then
         declare
            Slot : constant Positive := Positive'Max (1, Plot_Width / Count);
            Gap : constant Natural := (if Slot > 3 then Slot / 4 else 0);
            Zero : constant Integer := Y_Of (0);
         begin
            for I in 1 .. Count loop
               declare
                  Top : constant Integer := Integer'Min (Y_Of (Values (I)), Zero);
                  Bottom : constant Integer := Integer'Max (Y_Of (Values (I)), Zero);
                  Left : constant Integer := CHART_MARGIN + (I - 1) * Slot;
               begin
                  for X in Left .. Left + Slot - Gap - 1 loop
                     for Y in Top .. Bottom loop
                        Store.Set (X, Y, Mix ((if Values (I) < 0 then CHART_BAR_NEGATIVE else CHART_BAR),
                                              CHART_FILL, Y - Top, Positive'Max (1, (Bottom - Top) * 2)));
                     end loop;
                  end loop;
               end;
            end loop;
         end;
      else
         --  Area under the line, then the line over it.
         for I in 1 .. Count - 1 loop
            declare
               X0 : constant Integer := X_Of (I);
               X1 : constant Integer := X_Of (I + 1);
            begin
               for X in X0 .. X1 loop
                  declare
                     Y : constant Integer :=
                       Y_Of (Values (I)) + (Y_Of (Values (I + 1)) - Y_Of (Values (I))) *
                         (X - X0) / Integer'Max (1, X1 - X0);
                  begin
                     for Fill_Y in Y .. CHART_MARGIN + Plot_Height - 1 loop
                        Store.Set (X, Fill_Y, Mix (CHART_FILL, CHART_BACKGROUND, Fill_Y - Y,
                                                   Positive'Max (1, CHART_MARGIN + Plot_Height - Y)));
                     end loop;
                  end;
               end loop;
            end;
         end loop;
         for I in 1 .. Count - 1 loop
            Line (X_Of (I), Y_Of (Values (I)), X_Of (I + 1), Y_Of (Values (I + 1)), CHART_LINE);
         end loop;
         if Count = 1 then
            Line (CHART_MARGIN, Y_Of (Values (1)), CHART_WIDTH - CHART_MARGIN, Y_Of (Values (1)), CHART_LINE);
         end if;
      end if;
   end Chart;

   --  An integer list's elements, from a view at Position.
   procedure Read_List
     (Object : Views.Snapshot; Position : Views.Cursor; Values : out Value_Array;
      Count : out Natural)
   is
   begin
      Values := [others => 0];
      Count := Natural'Min (Views.Length (Object, Position), MAXIMUM_VALUES);
      for I in 1 .. Count loop
         Values (I) := CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Element (Object, Position, I)));
      end loop;
   end Read_List;

   function Field_Integer (Object : Views.Snapshot; Index : CCL.Types.Component_Index) return Integer_64 is
     (CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Views.Root (Object), Index))));

   --  An Image value's stored id, from a view of (Image width height id);
   --  No_Image unless its pixels are in the store at that size.
   IMAGE_ID_FIELD : constant := 3;
   function Stored (Object : Views.Snapshot; Position : Views.Cursor) return Store.Image_Id is
      Id : constant Store.Image_Id :=
        CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Position, IMAGE_ID_FIELD)));
      Width : constant Integer_64 := CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Position, 1)));
      Height : constant Integer_64 := CCL.Objects.Integer_Of (Views.Scalar (Object, Views.Field (Object, Position, 2)));
   begin
      if Store.Known (Id) and then Integer_64 (Store.Width (Id)) = Width and then
        Integer_64 (Store.Height (Id)) = Height
      then
         return Id;
      end if;
      return Store.No_Image;
   end Stored;

   --  Copy a stored image into the draft at (Left, Top).
   procedure Place (Id : Store.Image_Id; Left, Top : Natural) is
   begin
      for Y in 0 .. Store.Height (Id) - 1 loop
         for X in 0 .. Store.Width (Id) - 1 loop
            Store.Set (Left + X, Top + Y, Store.Pixel_At (Id, X, Y));
         end loop;
      end loop;
   end Place;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Op : Images.Operation;
      Object : Views.Snapshot;
      Captured : Boolean;
      Values : Value_Array;
      Count : Natural;
      Id : Store.Image_Id := Store.No_Image;
      Result : CCL.Objects.Image;
      Built : Boolean;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) or else Argument.Kind /= CCL.Host_Values.Object_Value then return; end if;
      Op := Images.Operation'Val (Binding - FIRST_BINDING);
      if Images.Shape_Of (Op) = Images.Name_Argument then return; end if;
      Views.Capture
        (Object,
         (case Images.Shape_Of (Op) is
             when Images.Series_Argument => Series_Contract,
             when Images.Grid_Argument => Grid_Contract,
             when Images.Images_Argument => Images_Contract,
             when Images.Scaled_Argument => Scaled_Contract,
             when Images.Size_Argument | Images.Name_Argument => Size_Contract),
         Argument.Object, Captured);
      if not Captured then return; end if;
      case Images.Shape_Of (Op) is
         when Images.Name_Argument => return;
         when Images.Images_Argument =>
            declare
               Count : constant Natural := Views.Length (Object, Views.Root (Object));
               Parts : array (1 .. CCL.Objects.Maximum_Cells) of Store.Image_Id := [others => Store.No_Image];
               Width, Height : Natural := 0;
               Offset : Natural := 0;
            begin
               if Count = 0 then return; end if;
               for I in 1 .. Count loop
                  Parts (I) := Stored (Object, Views.Element (Object, Views.Root (Object), I));
                  if Parts (I) = Store.No_Image then return; end if;   --  expired or forged size
                  if Op = Images.Stack then
                     Width := Natural'Max (Width, Store.Width (Parts (I)));
                     Height := Height + Store.Height (Parts (I));
                  else
                     Width := Width + Store.Width (Parts (I));
                     Height := Natural'Max (Height, Store.Height (Parts (I)));
                  end if;
                  if Width > Store.Maximum_Side or else Height > Store.Maximum_Side then return; end if;
               end loop;
               if not Store.Fits (Width, Height) then return; end if;
               Store.Start (Width, Height, CHART_BACKGROUND);
               for I in 1 .. Count loop
                  if Op = Images.Stack then
                     Place (Parts (I), 0, Offset);
                     Offset := Offset + Store.Height (Parts (I));
                  else
                     Place (Parts (I), Offset, 0);
                     Offset := Offset + Store.Width (Parts (I));
                  end if;
               end loop;
            end;
         when Images.Scaled_Argument =>
            declare
               Source : constant Store.Image_Id := Stored (Object, Views.Field (Object, Views.Root (Object), 1));
               Factor : constant Integer_64 := Field_Integer (Object, 2);
            begin
               if Source = Store.No_Image or else Factor not in 1 .. Images.MAXIMUM_SCALE then return; end if;
               declare
                  F : constant Positive := Positive (Factor);
                  Width : constant Natural := Store.Width (Source) * F;
                  Height : constant Natural := Store.Height (Source) * F;
               begin
                  if Width > Store.Maximum_Side or else Height > Store.Maximum_Side or else
                    not Store.Fits (Width, Height)
                  then
                     return;
                  end if;
                  Store.Start (Width, Height, 0);
                  for Y in 0 .. Height - 1 loop
                     for X in 0 .. Width - 1 loop
                        Store.Set (X, Y, Store.Pixel_At (Source, X / F, Y / F));
                     end loop;
                  end loop;
               end;
            end;
         when Images.Series_Argument =>
            Read_List (Object, Views.Root (Object), Values, Count);
            if Count = 0 then return; end if;
            Chart (Values, Count, As_Bars => Op = Images.Bars);
         when Images.Grid_Argument =>
            declare
               Width : constant Integer_64 := Field_Integer (Object, 1);
               Height : constant Integer_64 := Field_Integer (Object, 2);
               Low, High : Integer_64;
            begin
               Read_List (Object, Views.Field (Object, Views.Root (Object), 3), Values, Count);
               if Width not in 1 .. Store.Maximum_Side or else Height not in 1 .. Store.Maximum_Side or else
                 Count = 0 or else Integer_64 (Count) /= Width * Height
               then
                  return;
               end if;
               Low := Values (1);
               High := Values (1);
               for I in 1 .. Count loop
                  Low := Integer_64'Min (Low, Values (I));
                  High := Integer_64'Max (High, Values (I));
               end loop;
               Store.Start (Positive (Width), Positive (Height), 0);
               for I in 1 .. Count loop
                  Store.Set ((I - 1) mod Natural (Width), (I - 1) / Natural (Width),
                    (if Op = Images.Pixels then Store.Pixel (Values (I) mod 16#100_0000#)
                     else Heat (Values (I), Low, High)));
               end loop;
            end;
         when Images.Size_Argument =>
            declare
               Width : constant Integer_64 := Field_Integer (Object, 1);
               Height : constant Integer_64 := Field_Integer (Object, 2);
            begin
               if Width not in 1 .. Store.Maximum_Side or else Height not in 1 .. Store.Maximum_Side or else
                 not Store.Fits (Positive (Width), Positive (Height))
               then
                  return;
               end if;
               Store.Start (Positive (Width), Positive (Height), 0);
               for Y in 0 .. Natural (Height) - 1 loop
                  for X in 0 .. Natural (Width) - 1 loop
                     Store.Set (X, Y, Mix (Heat (Integer_64 (X), 0, Width - 1), CHART_BACKGROUND,
                                           Y, Natural (Height) * 2));
                  end loop;
               end loop;
            end;
      end case;
      Store.Finish (Id);
      Images.Image_Value (Image_Contract, Store.Width (Id), Store.Height (Id), Id, Result, Built);
      if Built then
         Reply := (Value => CCL.Host_Values.Object_Constant (Result), Success => True, Why => <>);
      end if;
   end Invoke;
end CCL_Image_Bindings;
