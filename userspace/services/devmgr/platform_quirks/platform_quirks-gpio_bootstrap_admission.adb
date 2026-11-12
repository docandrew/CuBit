pragma Ada_2022;
with Platform_Quirks.Intel_GPIO_Communities;

package body Platform_Quirks.GPIO_Bootstrap_Admission with SPARK_Mode is

   function Controller_Matches
     (Controller : Platform_Quirks.GPIO_Controller; HID : GP.HID_Text) return Boolean is
     (case Controller is
        when Platform_Quirks.No_Controller  => False,
        when Platform_Quirks.ADL_N_INTC1057 =>
          HID = Platform_Quirks.Intel_GPIO_Communities.ADL_N_GPIO_Controller_HID);

   --  The first span (1-based) whose shape is wrong, or 0.
   function First_Bad_Shape (R : GP.Request) return GP.Span_Count
   with Post => (if First_Bad_Shape'Result = 0 then Shapes_Valid (R)
                 else First_Bad_Shape'Result <= R.Spans)
   is
   begin
      for I in 1 .. R.Spans loop
         if not Shape_Valid (R.Items (I)) then
            return I;
         end if;
         pragma Loop_Invariant (for all J in 1 .. I => Shape_Valid (R.Items (J)));
      end loop;
      return 0;
   end First_Bad_Shape;

   --  The first span (1-based) touching an earlier one, or 0.
   function First_Overlapping (R : GP.Request) return GP.Span_Count
   with Pre  => Shapes_Valid (R),
        Post => (if First_Overlapping'Result = 0 then Spans_Disjoint (R)
                 else First_Overlapping'Result <= R.Spans)
   is
   begin
      for I in 1 .. R.Spans loop
         for J in 1 .. I - 1 loop
            if DW.Touches (Pages_Of (R.Items (I)), Pages_Of (R.Items (J))) then
               return I;
            end if;
            pragma Loop_Invariant
              (for all K in 1 .. J =>
                 not DW.Touches (Pages_Of (R.Items (I)), Pages_Of (R.Items (K))));
         end loop;
         pragma Loop_Invariant
           (for all A in 1 .. I =>
              (for all B in 1 .. A - 1 =>
                 not DW.Touches (Pages_Of (R.Items (A)), Pages_Of (R.Items (B)))));
      end loop;
      --  Touches is symmetric: disjoint below the diagonal is disjoint.
      pragma Assert
        (for all A in 1 .. R.Spans =>
           (for all B in 1 .. R.Spans =>
              (if B < A then not DW.Touches (Pages_Of (R.Items (A)), Pages_Of (R.Items (B))))));
      pragma Assert
        (for all A in 1 .. R.Spans =>
           (for all B in 1 .. R.Spans =>
              (if A < B then not DW.Touches (Pages_Of (R.Items (B)), Pages_Of (R.Items (A))))));
      return 0;
   end First_Overlapping;

   --  Why one span is refused, or Admitted.
   function Span_Answer (S : GP.Span; V : Memory_View) return GX.Disposition
   with Pre  => Shape_Valid (S) and then View_Valid (V),
        Post => (Span_Answer'Result = GX.Admitted) = Span_Admissible (S, V)
                and then Span_Answer'Result in
                  GX.Admitted | GX.Not_Chipset_MMIO | GX.Overlaps_RAM | GX.Overlaps_BAR
                  | GX.Overlaps_PCI_Config | GX.Overlaps_Pool_Member | GX.Straddles
   is
      P : constant DW.Page_Range := Pages_Of (S);
   begin
      if DW.Touches_Any (P, V.RAM_Ranges) then
         return GX.Overlaps_RAM;
      elsif DW.Touches_Any (P, V.BAR_Extents) then
         return GX.Overlaps_BAR;
      elsif DW.Touches_Any (P, V.ECAM_Ranges) then
         return GX.Overlaps_PCI_Config;
      elsif DW.Touches_Any (P, V.Pool_Members) then
         return GX.Overlaps_Pool_Member;
      elsif Inside_Some (P, V.Device_Roots) then
         return GX.Admitted;
      elsif DW.Touches_Any (P, V.Device_Roots) then
         return GX.Straddles;
      end if;
      return GX.Not_Chipset_MMIO;
   end Span_Answer;

   function Decide
     (G : Gate; Controller : Platform_Quirks.GPIO_Controller;
      R : GP.Request; V : Memory_View) return Decision
   is
      Bad : GP.Span_Count;
      Answer : GX.Disposition;
   begin
      if not G.Discovery_Ran then
         return (Answer => GX.No_Discovery_Failure, Span => 0);
      elsif G.Discovery_Found then
         return (Answer => GX.P2SB_Available, Span => 0);
      elsif G.Admitted_Before then
         return (Answer => GX.Already_Admitted, Span => 0);
      elsif not Controller_Matches (Controller, R.HID) or else not Present_And_Enabled (R.STA) then
         return (Answer => GX.Unknown_Controller, Span => 0);
      end if;
      Bad := First_Bad_Shape (R);
      if Bad /= 0 then
         return (Answer => GX.Malformed, Span => Bad);
      end if;
      Bad := First_Overlapping (R);
      if Bad /= 0 then
         return (Answer => GX.Malformed, Span => Bad);
      end if;
      for I in 1 .. R.Spans loop
         Answer := Span_Answer (R.Items (I), V);
         if Answer /= GX.Admitted then
            return (Answer => Answer, Span => I);
         end if;
         pragma Loop_Invariant (for all J in 1 .. I => Span_Admissible (R.Items (J), V));
      end loop;
      return (Answer => GX.Admitted, Span => 0);
   end Decide;

end Platform_Quirks.GPIO_Bootstrap_Admission;
