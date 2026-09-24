pragma Ada_2022;
package body CuBit.Output_Registry with SPARK_Mode is
   use type L.Named_Display_ID;
   use type W.Work_Area;

   procedure Advance (Registry : in out State; Allowed : out Boolean)
     with Post =>
       Registry.Entries = Registry'Old.Entries and then
       (if Allowed then not Registry.Closing and then
          Registry.Serial > Registry'Old.Serial
        else Registry.Closing and then Registry.Serial = Registry'Old.Serial);

   procedure Advance (Registry : in out State; Allowed : out Boolean) is
   begin
      Allowed := False;
      if Registry.Closing then
         return;
      elsif Registry.Serial >= Revision (Maximum_Revision) then
         --  Fail closed instead of recycling generations. Metadata closure
         --  deliberately does not claim that device readers have quiesced.
         Registry.Closing := True;
      else
         Registry.Serial := Registry.Serial + 1;
         Allowed := True;
      end if;
   end Advance;

   procedure Register
     (Registry : in out State; Item : Description;
      Reference : out Output_Reference; Result : out Mutation_Result)
   is
      Free : L.Viewport_Count := 0;
      Allowed : Boolean;
   begin
      Reference := No_Output;
      if Registry.Closing then
         Result := Closed;
         return;
      end if;
      for I in L.Viewport_Index loop
         if Registry.Entries (I).Occupied then
            if Registry.Entries (I).Item.Area.Display = Item.Area.Display or
              Registry.Entries (I).Item.Backend = Item.Backend
            then
               Result := Identity_Conflict;
               return;
            end if;
         elsif Free = 0 then
            Free := I;
         end if;
      end loop;
      if Free = 0 then
         Result := Full;
         return;
      end if;
      Advance (Registry, Allowed);
      if not Allowed then
         Result := Exhausted;
         return;
      end if;
      Registry.Entries (Free) := (True, Registry.Serial, Item);
      Reference := (Registry.Instance, Free, Registry.Serial);
      Result := Applied;
   end Register;

   procedure Update
     (Registry : in out State; Reference : Output_Reference;
      Item : Description; Replacement : out Output_Reference;
      Result : out Mutation_Result)
   is
      Allowed : Boolean;
   begin
      Replacement := No_Output;
      if Registry.Closing then
         Result := Closed;
         return;
      end if;
      if not Live (Registry, Reference) then
         Result := Stale_Reference;
         return;
      end if;
      if Registry.Entries (Reference.Slot).Item.Area.Display /=
        Item.Area.Display or else
        Registry.Entries (Reference.Slot).Item.Backend /= Item.Backend
      then
         Result := Identity_Conflict;
         return;
      end if;
      Advance (Registry, Allowed);
      if not Allowed then
         Result := Exhausted;
         return;
      end if;
      Registry.Entries (Reference.Slot) := (True, Registry.Serial, Item);
      Replacement := (Registry.Instance, Reference.Slot, Registry.Serial);
      Result := Applied;
   end Update;

   procedure Retire
     (Registry : in out State; Reference : Output_Reference;
      Result : out Mutation_Result)
   is
      Allowed : Boolean;
   begin
      if Registry.Closing then
         Result := Closed;
         return;
      end if;
      if not Live (Registry, Reference) then
         Result := Stale_Reference;
         return;
      end if;
      Advance (Registry, Allowed);
      if not Allowed then
         Result := Exhausted;
         return;
      end if;
      Registry.Entries (Reference.Slot).Occupied := False;
      Result := Applied;
   end Retire;

   function Sound (Registry : State; View : Snapshot) return Boolean is
     (for all I in 1 .. View.Visible.Count =>
        Live (Registry, View.References (I)) and then
        Presentable (Describe (Registry, View.References (I))) and then
        Describe (Registry, View.References (I)).Area =
          View.Visible.Items (I));

   function Capture (Registry : State) return Snapshot is
      View : Snapshot :=
        (Instance => Registry.Instance, Serial => Registry.Serial,
         others => <>);
   begin
      if Registry.Closing then
         return View;
      end if;
      for I in L.Viewport_Index loop
         if Registry.Entries (I).Occupied and then
           Presentable (Registry.Entries (I).Item)
         then
            View.Visible.Count := View.Visible.Count + 1;
            View.Visible.Items (View.Visible.Count) :=
              Registry.Entries (I).Item.Area;
            View.References (View.Visible.Count) :=
              (Registry.Instance, I, Registry.Entries (I).Stamp);
         end if;
         pragma Loop_Invariant (View.Visible.Count <= I);
         pragma Loop_Invariant (Sound (Registry, View));
      end loop;
      return View;
   end Capture;
end CuBit.Output_Registry;
