pragma Ada_2022;
with Devmgr_Growable;
with Devmgr_Heap;
package body Devmgr_Host_Windows is
   package Store is new Devmgr_Growable (Root_Window, Devmgr_Heap.Allocate, Devmgr_Heap.Release);
   use type Store.Growth_Status;
   Windows : Store.Store;
   Published : Boolean := False;

   --  Pages below 4 GiB.
   Low_4G_Pages : constant := 2 ** 20;

   function Same_Root (W : Root_Window; Segment : Unsigned_16; First_Bus : Unsigned_8) return Boolean is
     (W.Segment = Segment and then W.First_Bus = First_Bus);

   function Covers (W : Root_Window; Segment : Unsigned_16; Bus : Unsigned_8) return Boolean is
     (W.Segment = Segment and then Bus in W.First_Bus .. W.Last_Bus);

   procedure Replace_Root
     (Segment : Unsigned_16; First_Bus, Last_Bus : Unsigned_8;
      Windows : Window_Array; Result : out Update_Result)
   is
      Kept : Natural := 0;
      Report : Store.Capacity_Report;
   begin
      --  Reserve the complete replacement before changing any stored window.
      declare
         Demand : Natural := 0;
      begin
         for I in 1 .. Store.Length (Devmgr_Host_Windows.Windows) loop
            if not Same_Root (Store.Element_At (Devmgr_Host_Windows.Windows, I), Segment, First_Bus) then
               Demand := Demand + 1;
            end if;
         end loop;
         for W of Windows loop
            if PL.Valid (W.Span) then
               if Demand = Natural'Last then
                  Result := No_Memory;
                  return;
               end if;
               Demand := Demand + 1;
            end if;
         end loop;
         Store.Reserve (Devmgr_Host_Windows.Windows, Demand, Report);
         if Report.Status /= Store.Grown then
            Result := No_Memory;
            return;
         end if;
      end;

      --  Drop the root's old windows (compacting in place), then append.
      for I in 1 .. Store.Length (Devmgr_Host_Windows.Windows) loop
         declare
            W : constant Root_Window := Store.Element_At (Devmgr_Host_Windows.Windows, I);
         begin
            if not Same_Root (W, Segment, First_Bus) then
               Kept := Kept + 1;
               Store.Set_At (Devmgr_Host_Windows.Windows, Kept, W);
            end if;
         end;
      end loop;
      Store.Set_Length (Devmgr_Host_Windows.Windows, Kept);
      for W of Windows loop
         if PL.Valid (W.Span) then
            Store.Append (Devmgr_Host_Windows.Windows,
                          (Segment => Segment, First_Bus => First_Bus, Last_Bus => Last_Bus,
                           Window => W.Span, Prefetchable => W.Prefetchable), Report);
            if Report.Status /= Store.Grown then
               Result := No_Memory;
               return;
            end if;
         end if;
      end loop;
      Published := True;
      Result := Updated;
   end Replace_Root;

   function Known return Boolean is (Published);

   function Count_For (Segment : Unsigned_16; Bus : Unsigned_8) return Natural is
      N : Natural := 0;
   begin
      for I in 1 .. Store.Length (Windows) loop
         if Covers (Store.Element_At (Windows, I), Segment, Bus) then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Count_For;

   procedure Windows_For
     (Segment : Unsigned_16; Bus : Unsigned_8; High_First : Boolean;
      Result : out PL.Span_Array; Last : out Natural)
   is
      procedure Take (Want_Low : Boolean) is
      begin
         for I in 1 .. Store.Length (Windows) loop
            declare
               W : constant Root_Window := Store.Element_At (Windows, I);
            begin
               if Covers (W, Segment, Bus) and then (W.Window.First < Low_4G_Pages) = Want_Low
                 and then Last < Result'Length
               then
                  Last := Last + 1;
                  Result (Result'First + Last - 1) := W.Window;
               end if;
            end;
         end loop;
      end Take;
   begin
      Result := [others => (0, 0)];
      Last := 0;
      Take (Want_Low => not High_First);
      Take (Want_Low => High_First);
   end Windows_For;
end Devmgr_Host_Windows;
