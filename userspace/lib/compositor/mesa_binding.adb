with Mesa_FFI;
package body Mesa_Binding with SPARK_Mode => Off is
   use type System.Address;
   procedure Start (Library : out Context; Success : out Boolean) is
   begin
      Library.Pointer := Mesa_FFI.Create;
      Success := Library.Pointer /= System.Null_Address;
   end Start;
   procedure Import_View (Library : in out Context; Description : Compositor_Formats.Image;
                          View : out System.Address) is
      Value : aliased Compositor_Formats.Image := Description;
   begin
      View := Mesa_FFI.Import_Image (Library.Pointer, Value'Access);
   end Import_View;
   procedure Render_View (Library : in out Context; Target, Source : System.Address;
                         Description : Compositor_Formats.Draw;
                         Result : out Compositor_Policy.Completion) is
      Value : aliased Compositor_Formats.Draw := Description;
      Code : Mesa_FFI.Word;
   begin
      Code := Mesa_FFI.Render (Library.Pointer, Target, Source, Value'Access);
      Result := (case Code is
        when 0 => Compositor_Policy.Rendered,
        when 1 => Compositor_Policy.Rejected,
        when 2 => Compositor_Policy.Failed_Quiescent,
        when others => Compositor_Policy.Access_Unknown);
   end Render_View;
   procedure Fill_View (Library : in out Context; Target : System.Address;
                        Left, Top, Width, Height, Color : Compositor_Formats.Word;
                        Result : out Compositor_Policy.Completion) is
      Code : constant Mesa_FFI.Word := Mesa_FFI.Fill (Library.Pointer, Target, Left, Top, Width, Height, Color);
   begin
      Result := (case Code is
        when 0 => Compositor_Policy.Rendered,
        when 1 => Compositor_Policy.Rejected,
        when 2 => Compositor_Policy.Failed_Quiescent,
        when others => Compositor_Policy.Access_Unknown);
   end Fill_View;
   procedure Release_View (Library : in out Context; View : System.Address; Safe : out Boolean) is
      use type Mesa_FFI.Word;
   begin
      Safe := Mesa_FFI.Release (Library.Pointer, View) = 0;
   end Release_View;
   procedure Stop (Library : in out Context) is
   begin
      Mesa_FFI.Destroy (Library.Pointer);
      Library.Pointer := System.Null_Address;
   end Stop;
end Mesa_Binding;
