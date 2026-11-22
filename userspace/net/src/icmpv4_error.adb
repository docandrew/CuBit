------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body ICMPv4_Error with SPARK_Mode is

   procedure Parse (Message : Bytes; E : out Error; OK : out Boolean) is
      Size : Natural;
      T    : Natural;   --  the quoted transport header
   begin
      E := (others => <>);
      OK := False;
      if Message'Length < Quoted_At + IPv4_Header.Minimum_Size + Quoted_Transport or else
        Message (0) not in Destination_Unreachable | Time_Exceeded or else
        Shift_Right (Message (Quoted_At), 4) /= 4
      then
         return;
      end if;
      Size := Quoted_Size (Message);
      if Size < IPv4_Header.Minimum_Size or else
        Quoted_At + Size + Quoted_Transport > Message'Length
      then
         return;
      end if;
      T := Quoted_At + Size;
      E := (Of_Kind          => Classify (Message (0), Message (1)),
            Code             => Message (1),
            Next_Hop_MTU     => U16 (Message, 6),
            Protocol         => Message (Quoted_At + 9),
            Source           => [Message (Quoted_At + 12), Message (Quoted_At + 13),
                                 Message (Quoted_At + 14), Message (Quoted_At + 15)],
            Destination      => [Message (Quoted_At + 16), Message (Quoted_At + 17),
                                 Message (Quoted_At + 18), Message (Quoted_At + 19)],
            Source_Port      => U16 (Message, T),
            Destination_Port => U16 (Message, T + 2),
            Sequence         => U32 (Message, T + 4));
      OK := True;
   end Parse;

end ICMPv4_Error;
