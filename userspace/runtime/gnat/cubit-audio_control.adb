with CuBit.Messages; use CuBit.Messages;
package body CuBit.Audio_Control is
   procedure Exchange
     (Operation : Unsigned_32; Level : Percent; Muted : Boolean;
      Value : out State; Success : out Boolean);
   procedure Exchange
     (Operation : Unsigned_32; Level : Percent; Muted : Boolean;
      Value : out State; Success : out Boolean) is
      Msg : Message := NULL_MESSAGE;
   begin
      Value := (others => <>);
      Msg.tag.label := Operation;
      if Operation = Set_State then
         Msg.tag.length := 2;
         Msg.words (0) := Unsigned_64 (Level);
         Msg.words (1) := Boolean'Pos (Muted);
      end if;
      Msg.tag := capCall (Endpoint_Slot, Msg);
      Success := Msg.tag.label = 16#F000# and then Msg.tag.length = 4
        and then Msg.tag.flags = 0 and then Msg.tag.reserved = 0
        and then Msg.words (0) <= 100 and then Msg.words (1) <= 1
        and then Msg.words (2) <= 1 and then Msg.words (3) = 0;
      if Success then
         Value := (Percent (Msg.words (0)), Msg.words (1) = 1,
                   Msg.words (2) = 1);
      end if;
   end Exchange;
   procedure Read (Value : out State; Success : out Boolean) is
   begin
      Exchange (Get_State, 0, False, Value, Success);
   end Read;
   procedure Set (Level : Percent; Muted : Boolean;
                  Value : out State; Success : out Boolean) is
   begin
      Exchange (Set_State, Level, Muted, Value, Success);
   end Set;
end CuBit.Audio_Control;
