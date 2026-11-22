------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  SameBoy's keyboard bindings (docs/c-removal.md): desktop scan codes to
--  Game Boy buttons and frontend commands, and the volume steps.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.SameBoy_Keys with SPARK_Mode, Pure is

   --  A PS/2 set 1 make code, as desktop key events carry it.
   type Scancode is range 0 .. 127;

   --  SameBoy's GB_key_t, in its order (tests/sameboy-port checks).
   type Pad_Key is (Right, Left, Up, Down, A, B, Select_Button, Start);
   for Pad_Key use (Right => 0, Left => 1, Up => 2, Down => 3, A => 4, B => 5,
                    Select_Button => 6, Start => 7);

   type Command is
     (No_Command, Quit, Toggle_Pause, Next_Rom, Reset, Toggle_Mute, Quieter,
      Louder);

   type Binding (Is_Pad : Boolean := False) is record
      case Is_Pad is
         when True  => Key : Pad_Key;
         when False => Action : Command;
      end case;
   end record;

   --  Arrows: the pad; X / Z: A / B; Enter / Tab: Start / Select; Escape
   --  quits; P pauses; F2 next cartridge; F5 reset; F8 mute; F9 / F10
   --  volume down / up.
   function Bound (Code : Scancode) return Binding;

   --  Keys currently held, to tell a press from auto-repeat.
   type Held_Keys is array (Scancode) of Boolean;
   No_Keys_Held : constant Held_Keys := [others => False];

   --  Record a key event; First_Press is True for a press of a key that was
   --  not already held.
   procedure Note (Held : in out Held_Keys; Code : Scancode; Down : Boolean;
                   First_Press : out Boolean)
     with Post => Held (Code) = Down and then
                  First_Press = (Down and then not Held'Old (Code)) and then
                  (for all C in Scancode =>
                     (if C /= Code then Held (C) = Held'Old (C)));

   --  Commands that act once per press; Quit acts on any key-down.
   function Acts_On_Press (Action : Command) return Boolean is
     (Action not in No_Command | Quit);

   Maximum_Volume : constant := 100;
   Volume_Step    : constant := 5;
   Initial_Volume : constant := 70;
   subtype Volume_Percent is Natural range 0 .. Maximum_Volume;

   function Quieter (Volume : Volume_Percent) return Volume_Percent is
     (if Volume >= Volume_Step then Volume - Volume_Step else Volume);
   function Louder (Volume : Volume_Percent) return Volume_Percent is
     (if Volume <= Maximum_Volume - Volume_Step then Volume + Volume_Step
      else Volume);

end CuBit.SameBoy_Keys;
