with Mesa_Cache;
package body Desktop_Compositor with SPARK_Mode,
  Refined_State => (Engine => Cache) is
   Cache : Mesa_Cache.State;
   function Selected return Boolean is (True);
   procedure Draw_Client
     (Target, Source : Compositor_Formats.Image;
      Target_Bytes, Source_Bytes : Compositor_Formats.Byte_Count;
      Plan : Desktop_Composition.Blit_Plan; Drag_Target : Boolean;
      Drawn, Must_Restart : out Boolean) is
      use Compositor_Formats;
      Target_Index : constant Mesa_Cache.Slot := (if Drag_Target then 1 else 0);
      Source_Index : Mesa_Cache.Source_Slot;
      OK : Boolean;
      Description : constant Draw :=
        (Word (Plan.Source_X), Word (Plan.Source_Y), Word (Plan.Width), Word (Plan.Height),
         Word (Plan.Target_X), Word (Plan.Target_Y), Word (Plan.Width), Word (Plan.Height),
         Word (Plan.Target_X), Word (Plan.Target_Y), Word (Plan.Width), Word (Plan.Height), 0);
   begin
      Drawn := False;
      Must_Restart := not Mesa_Cache.Can_Retire (Cache);
      if Must_Restart then return; end if;
      if not Mesa_Cache.Attempted (Cache) then Mesa_Cache.Initialize (Cache, True); end if;
      Mesa_Cache.Ensure (Cache, Target_Index, Target, Target_Bytes, OK);
      if OK then
         Mesa_Cache.Ensure_Source (Cache, Source, Source_Bytes, Source_Index, OK);
         if OK then Mesa_Cache.Render (Cache, Target_Index, Source_Index, Description, Drawn); end if;
      end if;
      Must_Restart := not Mesa_Cache.Can_Retire (Cache);
      if not Drawn and not Must_Restart then
         Mesa_Cache.Shutdown (Cache);
         Must_Restart := not Mesa_Cache.Can_Retire (Cache);
      end if;
   end Draw_Client;
   procedure Forget_Source (Pixels : System.Address; Safe : out Boolean) is
   begin
      Safe := Mesa_Cache.Can_Retire (Cache);
      if Safe then
         Mesa_Cache.Forget_Source (Cache, Pixels);
         Safe := Mesa_Cache.Can_Retire (Cache);
      end if;
   end Forget_Source;
end Desktop_Compositor;
