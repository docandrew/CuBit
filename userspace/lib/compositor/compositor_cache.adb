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
   procedure Ensure (S : in out State; Index : Slot;
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
   procedure Render (S : in out State; Target, Source : Slot;
                     Description : Compositor_Formats.Draw; Success : out Boolean) is
      Result : Completion;
   begin
      Success := False;
      if S.Status /= Ready then return; end if;
      if S.Views (Target).View = No_Handle or else
        S.Views (Source).View = No_Handle or else
        not Compositor_Formats.Fits (Description, S.Views (Source).Description,
                                    S.Views (Target).Description)
      then
         S.Status := Disabled; return;
      end if;
      Begin_Draw (S.Status);
      Render_View (S.Library, S.Views (Target).View, S.Views (Source).View,
                   Description, Result);
      Finish_Draw (S.Status, Result);
      Success := Result = Rendered;
   end Render;
   procedure Shutdown (S : in out State) is
   begin
      for I in Slot loop
         Forget (S, I);
         if not Can_Retire (S) then return; end if;
      end loop;
      if S.Started then Stop (S.Library); end if;
      S.Started := False;
      S.Status := Disabled;
   end Shutdown;
end Compositor_Cache;
