with Interfaces; use Interfaces;
with CuBit.UI.Input;

--  Which modifiers an input event carries (the desktop's encoding,
--  CuBit.UI.Input): key events in payload1's low word, pointer events and
--  INPUT_RESYNC in its high word. Nothing is remembered between events: a
--  key release that went to another window (a dialog, the Apps menu, a
--  focus change) can never leave Ctrl stuck, so a plain click never adds
--  a cursor. Pure, so the hosted tests check it.
package Text_Input is
   function Modifiers_Of (Event : CuBit.UI.Input.Input_Event) return Unsigned_64 is
     (if Event.kind in CuBit.UI.Input.INPUT_KEY_DOWN | CuBit.UI.Input.INPUT_KEY_UP
      then CuBit.UI.Input.Key_Modifiers (Event)
      elsif Event.kind in CuBit.UI.Input.INPUT_POINTER_DOWN | CuBit.UI.Input.INPUT_POINTER_UP
                          | CuBit.UI.Input.INPUT_POINTER_MOVE | CuBit.UI.Input.INPUT_RESYNC
      then CuBit.UI.Input.Pointer_Modifiers (Event)
      else 0);
end Text_Input;
