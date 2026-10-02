package body Compositor_Cache with SPARK_Mode is
   use Compositor_Policy;
   use type Compositor_Formats.Image;
   use type System.Address;
   procedure Initialize (S : in out State; Opt_In : Boolean) is
   begin
      S.Tried := True;
      if Opt_In then Start (S.Library, S.Started); end if;
      S.Status := Initial (Opt_In, S.Started);
   end Initialize;
   procedure Forget (S : in out State; Index : Slot) is
      Safe : Boolean;
   begin
      if S.Views (Index).View /= No_Handle then
         Release_View (S.Library, S.Views (Index).View, Safe);
         if not Safe then S.Status := Restart_Required; return; end if;
         S.Views (Index) := (others => <>);
      end if;
   end Forget;
   procedure Forget_Targets (S : in out State) is
      Before : constant State := S with Ghost;
   begin
      for I in Target_Slot loop
         Forget (S, I);
         pragma Loop_Invariant
           (for all J in Retained_Source_Slot => Empty (S, J) = Empty (Before, J));
         pragma Loop_Invariant
           (if Can_Retire (S) then
              (for all J in Target_Slot => (if J <= I then Empty (S, J))));
         exit when not Can_Retire (S);
      end loop;
   end Forget_Targets;
   procedure Ensure (S : in out State; Index : Color_Slot;
                     Description : Compositor_Formats.Image;
                     Capacity : Compositor_Formats.Byte_Count; Success : out Boolean) is
   begin
      Success := False;
      if S.Status /= Ready then return; end if;
      if not Compositor_Formats.Valid (Description, Capacity) then
         S.Status := Disabled; return;
      end if;
      if S.Views (Index).View /= No_Handle and then
        S.Views (Index).Description = Description
      then
         Success := True; return;
      end if;
      Forget (S, Index);
      if not Can_Retire (S) then return; end if;
      Import_View (S.Library, Description, S.Views (Index).View);
      if S.Views (Index).View = No_Handle then
         S.Status := Disabled; return;
      end if;
      S.Views (Index).Description := Description;
      Success := True;
   end Ensure;
   procedure Ensure_Mask
     (S : in out State; Index : Mask_Slot; Description : Compositor_Formats.Image;
      Capacity : Compositor_Formats.Byte_Count; Success : out Boolean) is
   begin
      Success := False;
      if S.Status /= Ready then return; end if;
      if not Valid_Mask (Description, Capacity) then
         S.Status := Disabled; return;
      end if;
      if S.Views (Index).View /= No_Handle and then
        S.Views (Index).Description = Description
      then Success := True; return; end if;
      Forget (S, Index);
      if not Can_Retire (S) then return; end if;
      Import_Mask (S.Library, Description, S.Views (Index).View);
      if S.Views (Index).View = No_Handle then
         S.Status := Disabled; return;
      end if;
      S.Views (Index).Description := Description;
      Success := True;
   end Ensure_Mask;
   procedure Ensure_Source (S : in out State; Description : Compositor_Formats.Image;
                            Capacity : Compositor_Formats.Byte_Count;
                            Index : out Source_Slot; Success : out Boolean) is
   begin
      Index := Source_Slot'First;
      Success := False;
      for I in Source_Slot loop
         if S.Views (I).View /= No_Handle and then
           S.Views (I).Description.Pixels = Description.Pixels
         then
            Index := I; Ensure (S, I, Description, Capacity, Success); return;
         end if;
      end loop;
      for I in Source_Slot loop
         if S.Views (I).View = No_Handle then
            Index := I; Ensure (S, I, Description, Capacity, Success); return;
         end if;
      end loop;
      S.Status := Disabled;
   end Ensure_Source;
   procedure Forget_Source (S : in out State; Pixels : System.Address) is
   begin
      for I in Source_Slot loop
         if S.Views (I).Description.Pixels = Pixels then Forget (S, I); end if;
         exit when not Can_Retire (S);
      end loop;
   end Forget_Source;
   procedure Render_Checked (S : in out State; Target, Source : Slot; Success : out Boolean) is
      Result : Completion;
   begin
      Success := False;
      if S.Status /= Ready then return; end if;
      if S.Views (Target).View = No_Handle or else
        S.Views (Source).View = No_Handle or else
        not Fits_Views (S.Views (Source).Description, S.Views (Target).Description)
      then
         S.Status := Disabled; return;
      end if;
      Begin_Draw (S.Status);
      Draw_Views (S.Library, S.Views (Target).View, S.Views (Source).View, Result);
      Finish_Draw (S.Status, Result);
      Success := Result = Rendered;
   end Render_Checked;
   procedure Render (S : in out State; Target, Source : Slot;
                     Description : Compositor_Formats.Draw; Success : out Boolean) is
      function Fits_Views (Source, Target : Compositor_Formats.Image) return Boolean is
        (Compositor_Formats.Fits (Description, Source, Target));
      procedure Draw_Views (Library : in out Library_State; Target, Source : Handle;
                            Result : out Completion) is
      begin
         Render_View (Library, Target, Source, Description, Result);
      end Draw_Views;
      procedure Execute is new Render_Checked (Fits_Views, Draw_Views);
   begin
      Execute (S, Target, Source, Success);
   end Render;
   procedure Render_Masks
     (S : in out State; Target : Target_Slot; Sources : Mask_Indices;
      Length : Batch_Count; Success : out Boolean) is
      Handles : Handle_Batch := (others => No_Handle);
      Result : Completion;
   begin
      Success := False;
      if S.Status /= Ready then return; end if;
      if Length = 0 then Success := True; return; end if;
      if S.Views (Target).View = No_Handle then S.Status := Disabled; return; end if;
      -- Validate the complete bounded list before issuing any foreign draw.
      for I in 1 .. Length loop
         if S.Views (Sources (I)).View = No_Handle or else
           not Fits_Source (I, S.Views (Sources (I)).Description, S.Views (Target).Description)
         then S.Status := Disabled; return; end if;
         Handles (I) := S.Views (Sources (I)).View;
      end loop;
      Begin_Draw (S.Status);
      Draw_Batch (S.Library, S.Views (Target).View, Handles, Length, Result);
      Finish_Draw (S.Status, Result);
      Success := Result = Rendered;
   end Render_Masks;
   procedure Shutdown (S : in out State) is
   begin
      for I in Slot loop
         Forget (S, I);
         if not Can_Retire (S) then return; end if;
         pragma Loop_Invariant (Can_Retire (S));
         pragma Loop_Invariant (for all J in Slot'First .. I => Empty (S, J));
      end loop;
      if S.Started then Stop (S.Library); end if;
      S.Started := False;
      S.Status := Disabled;
   end Shutdown;
end Compositor_Cache;
