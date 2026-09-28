------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Rings; use CuBit.Channel_Rings;
with CuBit.Datagram_Rings;

package body CuBit.Datagram_Rings_C is

   use type Interfaces.C.int;
   use type CuBit.Datagram_Rings.Put_Result;
   use type CuBit.Datagram_Rings.Take_Result;

   function Well_Formed (R : Channel_Rings_C.Ring) return Boolean is
     (Valid_Size (Unsigned_64 (R.Size)) and then R.Count <= R.Size);

   function Put
     (P      : access Channel_Rings_C.Ring;
      Ring   : System.Address;
      Data   : System.Address;
      Length : Unsigned_32) return Interfaces.C.int
   is
   begin
      if not Well_Formed (P.all) or else
        Length > CuBit.Datagram_Rings.Maximum_Payload
      then
         return -1;
      end if;
      declare
         V : Producer :=
           (Size => Natural (P.Size), Produced => Index (P.Own),
            Fill => Natural (P.Count));
         Bytes_Of_Ring : Bytes (0 .. V.Size - 1) with Import, Address => Ring;
         Record_Data : Bytes (1 .. Natural (Length))
           with Import, Address => Data;
         Result : CuBit.Datagram_Rings.Put_Result;
      begin
         CuBit.Datagram_Rings.Put (V, Bytes_Of_Ring, Record_Data, Result);
         P.Own := Unsigned_32 (V.Produced);
         P.Count := Unsigned_32 (V.Fill);
         return (case Result is
                    when CuBit.Datagram_Rings.Put => 1,
                    when CuBit.Datagram_Rings.No_Room => 0,
                    when CuBit.Datagram_Rings.Too_Large => -1);
      end;
   end Put;

   function Take
     (C         : access Channel_Rings_C.Ring;
      Ring      : System.Address;
      Into      : System.Address;
      Room      : Unsigned_32;
      Length    : access Unsigned_32;
      Truncated : access Interfaces.C.int) return Interfaces.C.int
   is
   begin
      Length.all := 0;
      Truncated.all := 0;
      if not Well_Formed (C.all) or else Room > Unsigned_32 (Natural'Last - 1)
      then
         return -1;
      end if;
      declare
         V : Consumer :=
           (Size => Natural (C.Size), Consumed => Index (C.Own),
            Available => Natural (C.Count));
         Bytes_Of_Ring : Bytes (0 .. V.Size - 1) with Import, Address => Ring;
         Target : Bytes (0 .. Natural (Room) - 1) with Import, Address => Into;
         Got : Natural;
         Cut : Boolean;
         Result : CuBit.Datagram_Rings.Take_Result;
      begin
         CuBit.Datagram_Rings.Take
           (V, Bytes_Of_Ring, Target, Got, Cut, Result);
         C.Own := Unsigned_32 (V.Consumed);
         C.Count := Unsigned_32 (V.Available);
         Length.all := Unsigned_32 (Got);
         Truncated.all := (if Cut then 1 else 0);
         return (case Result is
                    when CuBit.Datagram_Rings.Taken => 1,
                    when CuBit.Datagram_Rings.Empty => 0,
                    when CuBit.Datagram_Rings.Malformed => -1);
      end;
   end Take;

end CuBit.Datagram_Rings_C;
