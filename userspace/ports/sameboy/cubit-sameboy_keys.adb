pragma Ada_2022;

package body CuBit.SameBoy_Keys with SPARK_Mode is

   function Bound (Code : Scancode) return Binding is
     (case Code is
         when 16#48# => (Is_Pad => True, Key => Up),
         when 16#50# => (Is_Pad => True, Key => Down),
         when 16#4B# => (Is_Pad => True, Key => Left),
         when 16#4D# => (Is_Pad => True, Key => Right),
         when 16#2C# => (Is_Pad => True, Key => B),
         when 16#2D# => (Is_Pad => True, Key => A),
         when 16#0F# => (Is_Pad => True, Key => Select_Button),
         when 16#1C# => (Is_Pad => True, Key => Start),
         when 16#01# => (Is_Pad => False, Action => Quit),
         when 16#19# => (Is_Pad => False, Action => Toggle_Pause),
         when 16#3C# => (Is_Pad => False, Action => Next_Rom),
         when 16#3F# => (Is_Pad => False, Action => Reset),
         when 16#42# => (Is_Pad => False, Action => Toggle_Mute),
         when 16#43# => (Is_Pad => False, Action => Quieter),
         when 16#44# => (Is_Pad => False, Action => Louder),
         when others => (Is_Pad => False, Action => No_Command));

   procedure Note (Held : in out Held_Keys; Code : Scancode; Down : Boolean;
                   First_Press : out Boolean) is
   begin
      First_Press := Down and then not Held (Code);
      Held (Code) := Down;
   end Note;

end CuBit.SameBoy_Keys;
