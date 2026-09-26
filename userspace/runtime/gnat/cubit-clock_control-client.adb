with CuBit.Messages; use CuBit.Messages;
package body CuBit.Clock_Control.Client is
   procedure Submit
     (Item : Sample; Result : out Outcome;
      Quality : out CuBit.Clocks.Time_Quality; Success : out Boolean)
   is
      Msg : Message := NULL_MESSAGE;
      Encoded : constant Words := Encode (Item);
   begin
      Result := Rejected_Out_Of_Range;
      Quality := CuBit.Clocks.Unknown_Time;
      Msg.tag.label := Submit_Sample;
      Msg.tag.length := 4;
      for I in Encoded'Range loop
         Msg.words (I) := Encoded (I);
      end loop;
      Msg.tag := capCall (Endpoint_Slot, Msg);
      Success := Msg.tag.label = 16#F000# and then Msg.tag.length = 2
        and then Msg.tag.flags = 0 and then Msg.tag.reserved = 0
        and then Msg.words (0) <= Outcome'Enum_Rep (Outcome'Last)
        and then Msg.words (1) <=
          CuBit.Clocks.Time_Quality'Enum_Rep (CuBit.Clocks.Time_Quality'Last);
      if Success then
         Result := Outcome'Enum_Val (Msg.words (0));
         Quality := CuBit.Clocks.Time_Quality'Enum_Val (Msg.words (1));
      end if;
   end Submit;
end CuBit.Clock_Control.Client;
