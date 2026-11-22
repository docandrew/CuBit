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

   --  The lowest aligned span of Pages pages inside Region that overlaps
   --  nothing taken (Found), trying each aligned candidate and stepping past
   --  the span that blocks it.
   procedure Search
     (Region : Span;
      Taken  : Span_Array;
      Pages  : Page;
      Found  : out Boolean;
      Where  : out Span)
   with
     Pre  => Power_Of_Two (Pages) and then Pages <= Page_Limit / 2 and then Valid (Region),
     Post =>
       (if Found then
          Valid (Where)
          and then Where.Past - Where.First = Pages
          and then (Where.First and (Pages - 1)) = 0
          and then Inside (Where, Region)
          and then (for all T of Taken => not Overlaps (Where, T)))
   is
      Candidate : Page;
      Aligned : Boolean;
      Blocker : Natural;
   begin
      Found := False;
      Where := (0, 0);
      Align_Up (Region.First, Pages, Candidate, Aligned);
      --  Every turn either returns, leaves the region or moves Candidate
      --  strictly up past a taken span.
      while Aligned and then Candidate + Pages <= Region.Past loop
         pragma Loop_Invariant ((Candidate and (Pages - 1)) = 0);
         pragma Loop_Invariant (Candidate >= Region.First);
         pragma Loop_Invariant (Candidate <= Page_Limit - Pages);
         pragma Loop_Invariant (not Found);
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
            Found := True;
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
   end Search;

   procedure Place
     (Windows : Span_Array;
      Allowed : Span_Array;
      Taken   : Span_Array;
      Pages   : Page;
      Result  : out Outcome;
      Where   : out Span)
   is
      Region : Span;
      Found : Boolean;
      Candidate : Span;
   begin
      Where := (0, 0);
      if not Power_Of_Two (Pages) or else Pages > Page_Limit / 2 then
         Result := (if Power_Of_Two (Pages) then No_Room else Invalid_Size);
         return;
      end if;
      Result := No_Room;

      --  Window by window in the caller's order; in each, only where it
      --  meets an allowed run.
      for WI in Windows'Range loop
         if Valid (Windows (WI)) then
            for RI in Allowed'Range loop
               Region := Meet (Windows (WI), Allowed (RI));
               if Valid (Region) then
                  Search (Region, Taken, Pages, Found, Candidate);
                  if Found then
                     Where := Candidate;
                     Result := Placed;
                     pragma Assert (Inside (Where, Windows (WI)));
                     pragma Assert (Valid (Allowed (RI)) and then Inside (Where, Allowed (RI)));
                     return;
                  end if;
               end if;
               pragma Loop_Invariant (Result = No_Room);
            end loop;
         end if;
         pragma Loop_Invariant (Result = No_Room);
      end loop;
   end Place;
end PCI_BAR_Placement;
