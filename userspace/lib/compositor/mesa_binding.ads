with System;
with Compositor_Formats;
with Compositor_Policy;
--  Trusted implementations only translate C ABI arguments and status.
--  The context parameter represents the exclusively owned Mesa library state.
package Mesa_Binding with SPARK_Mode is
   type Context is private;
   procedure Start (Library : out Context; Success : out Boolean) with Global => null;
   procedure Import_View (Library : in out Context; Description : Compositor_Formats.Image;
                          View : out System.Address) with Global => null;
   procedure Render_View (Library : in out Context; Target, Source : System.Address;
                         Description : Compositor_Formats.Draw;
                         Result : out Compositor_Policy.Completion) with Global => null;
   procedure Fill_View (Library : in out Context; Target : System.Address;
                        Left, Top, Width, Height, Color : Compositor_Formats.Word;
                        Result : out Compositor_Policy.Completion) with Global => null;
   procedure Release_View (Library : in out Context; View : System.Address; Safe : out Boolean) with Global => null;
   procedure Stop (Library : in out Context) with Global => null;
private
   type Context is record
      Pointer : System.Address := System.Null_Address;
   end record;
end Mesa_Binding;
