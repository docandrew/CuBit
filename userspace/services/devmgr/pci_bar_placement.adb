pragma Ada_2022;
package body PCI_BAR_Placement with SPARK_Mode is

   --  The first multiple of Pages at or above From (Found), or none below
   --  the page limit. Pages is a power of two, so the multiple is From
   --  rounded up with the size mask.
   procedure Align_Up (From : Page; Pages : Page; Aligned : out Page; Found : out Boolean)
   with
     Pre  => Power_Of_Two (Pages) and then Pages <= Page_Limit / 2,
     Post => (if Found then Aligned >= From and then (Aligned and (Pages - 1)) = 0
                            and then Aligned <= Page_Limit - Pages)
   is
      Mask : constant Unsigned_64 := Pages - 1;
      Up   : constant Unsigned_64 := (Unsigned_64 (From) + Mask) and not Mask;
   begin
      pragma Assert (Up >= Unsigned_64 (From));
      pragma Assert ((Up and Mask) = 0);
      if Up <= Page_Limit - Pages then
         Aligned := Up;
         Found := True;
      else
         Aligned := 0;
         Found := False;
      end if;
   end Align_Up;

   procedure Place
     (Windows : Span_Array;
      Taken   : Span_Array;
      Pages   : Page;
      Result  : out Outcome;
      Where   : out Span)
   is
   begin
      Where := (0, 0);
      if not Power_Of_Two (Pages) or else Pages > Page_Limit / 2 then
         Result := (if Power_Of_Two (Pages) then No_Room else Invalid_Size);
         return;
      end if;
      Result := No_Room;

      for WI in Windows'Range loop
         declare
            W : constant Span := Windows (WI);
            Candidate : Page;
            Aligned : Boolean;
            Blocker : Natural;
         begin
            if Valid (W) then
               Align_Up (W.First, Pages, Candidate, Aligned);
               --  Every turn either returns, leaves the window or moves
               --  Candidate strictly up past a taken span.
               while Aligned and then Candidate + Pages <= W.Past loop
                  pragma Loop_Invariant ((Candidate and (Pages - 1)) = 0);
                  pragma Loop_Invariant (Candidate >= W.First);
                  pragma Loop_Invariant (Candidate <= Page_Limit - Pages);
                  pragma Loop_Variant (Increases => Candidate);
                  Blocker := 0;
                  for TI in Taken'Range loop
                     pragma Loop_Invariant
                       (Blocker = 0 and then
                        (for all K in Taken'First .. TI - 1 =>
                           not Overlaps ((Candidate, Candidate + Pages), Taken (K))));
                     if Overlaps ((Candidate, Candidate + Pages), Taken (TI)) then
                        Blocker := TI;
                        exit;
                     end if;
                  end loop;
                  if Blocker = 0 then
                     Where := (Candidate, Candidate + Pages);
                     Result := Placed;
                     pragma Assert (Inside (Where, W));
                     return;
                  end if;
                  --  Past the blocking span: it ends above Candidate.
                  declare
                     Next : Page;
                  begin
                     Align_Up (Taken (Blocker).Past, Pages, Next, Aligned);
                     exit when not Aligned or else Next <= Candidate;
                     Candidate := Next;
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Place;
end PCI_BAR_Placement;
