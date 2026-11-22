------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where and how large a new application window opens.
------------------------------------------------------------------------------
package body Desktop_Window_Placement with SPARK_Mode is

   --  The extent along one axis: the default share or the request, within
   --  the work area and at least the minimum the work area has room for.
   function Size (Work, Requested, Minimum : Extent; Portion : Share; Policy : Sizing_Policy) return Extent is
     (Natural'Min (Work, Natural'Max (Natural'Max (Requested,
                                                   (if Policy = As_Requested and then Requested > 0 then 0
                                                    else Part (Work, Portion))),
                                      Natural'Min (Minimum, Work))))
     with Post => Size'Result <= Work
       and then Size'Result >= Natural'Min (Minimum, Work)
       and then Size'Result >= Natural'Min (Requested, Work)
       and then (if Policy = As_Requested and then Requested > 0 then
                   Size'Result <= Natural'Max (Requested, Minimum));

   --  How many cascade positions fit along an axis.
   function Steps (Work, Size, Corner : Extent) return Positive is
     (if Work > Corner + Size then (Work - Corner - Size) / Cascade_Step + 1 else 1);

   --  Along one axis: the window starts at Corner + Offset and is cut to
   --  end inside the work area, unless that would make it smaller than the
   --  minimum the work area has room for; then it moves back to fit.
   procedure Fit (Work, Wanted, Minimum, Start : Extent; Position, Length : out Extent)
     with Pre => Wanted <= Work and then Wanted >= Natural'Min (Minimum, Work),
          Post => Position + Length <= Work
            and then Length >= Natural'Min (Minimum, Work)
            and then Length >= Natural'Min (Wanted, Work - Position)
            and then Length <= Natural'Max (Wanted, Minimum)
   is
      Floor : constant Extent := Natural'Min (Minimum, Work);
   begin
      if Start <= Work and then Wanted <= Work - Start then
         Position := Start;
         Length := Wanted;
      elsif Start <= Work and then Work - Start >= Floor then
         Position := Start;
         Length := Work - Start;
      else
         Position := Work - Floor;
         Length := Floor;
      end if;
   end Fit;

   function Place
     (Work_Width, Work_Height : Extent;
      Requested_Width, Requested_Height : Extent;
      Minimum_Width, Minimum_Height : Extent;
      Slot : Slot_Number;
      Policy : Sizing_Policy) return Placement
   is
      Width : constant Extent := Size (Work_Width, Requested_Width, Minimum_Width, Width_Share, Policy);
      Height : constant Extent := Size (Work_Height, Requested_Height, Minimum_Height, Height_Share, Policy);
      Left : constant Extent := Natural'Min (Cascade_Left, Work_Width / 2);
      Top : constant Extent := Natural'Min (Cascade_Top, Work_Height / 2);
      Count : constant Positive := Natural'Min (Steps (Work_Width, Width, Left), Steps (Work_Height, Height, Top));
      Offset : constant Extent := Natural'Min (Slot mod Count * Cascade_Step, Maximum_Extent);
      Result : Placement;
   begin
      Fit (Work_Width, Width, Minimum_Width, Natural'Min (Left + Offset, Maximum_Extent), Result.X, Result.Width);
      Fit (Work_Height, Height, Minimum_Height, Natural'Min (Top + Offset, Maximum_Extent), Result.Y, Result.Height);
      return Result;
   end Place;
end Desktop_Window_Placement;
