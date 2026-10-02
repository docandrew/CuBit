with System.Address_To_Access_Conversions;
package body CuBit.Messages is
   package Entries is new System.Address_To_Access_Conversions (CompletionEntry);
   Saved_Token : Unsigned_64;
   function syscall (Call : Unsigned_64; Arg0, Arg1, Arg2, Arg3 : Unsigned_64 := 0) return Unsigned_64 is
      Offsets : constant array (0 .. 2) of Unsigned_64 := [16#64000#, 16#162000#, 16#6C000#];
   begin
      if Call = SYSCALL_GETTIME then
         Clocks := Clocks + 1;
         if Mode = 7 then return Unsigned_64'Last; end if;
         if Mode = 8 and Clocks > 1 then return 0; end if;
         if Mode = 9 then return 100; end if;
         return Unsigned_64 (Clocks);
      end if;
      pragma Assert (Call = SYSCALL_MAP_DEVICE and Maps <= 2);
      pragma Assert (Arg0 = Base + Offsets (Maps));
      pragma Assert (Arg1 = 16#61500000# + Unsigned_64 (Maps) * 4096);
      pragma Assert (Arg2 = 1 and Arg3 = 0);
      Maps := Maps + 1;
      if Mode = 6 and Maps = 2 then return Unsigned_64'Last; end if;
      return 0;
   end syscall;
   function capSubmit (Slot : Unsigned_64; Msg : Message; Token : Unsigned_64) return Boolean is
   begin
      pragma Assert (Slot = 15 and Msg.tag = (16#0234#, 1, 0, 0));
      pragma Assert (Msg.words = [Unsigned_64 (Maps), 0, 0, 0]);
      pragma Assert (Token = 16#49470050# + Unsigned_64 (Maps));
      Submissions := Submissions + 1; Saved_Token := Token;
      return Mode /= 5;
   end capSubmit;
   function Poll_Completion (Result : System.Address) return Unsigned_64 is
      Item : constant Entries.Object_Pointer := Entries.To_Pointer (Result);
   begin
      Polls := Polls + 1;
      if Mode in 9 | 10 then return 0; end if;
      Item.all := (Saved_Token, ((16#F000#, 0, 0, 0), [others => 0]), 0);
      case Mode is
         when 1 => Item.msg.words (0) := 1;
         when 2 => Item.msg.tag.length := 1;
         when 3 => Item.msg.tag.label := 16#F001#;
         when 4 => if Polls = 1 then Item.msg.tag.label := 16#F002#; end if;
         when 11 => Item.status := 1;
         when 12 => Item.msg.tag.flags := 1;
         when 13 => Item.token := Saved_Token + 1;
         when others => null;
      end case;
      return 1;
   end Poll_Completion;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result is
   begin pragma Assert (Deadline > 0); return Deadline_Reached; end Wait_For_Activity_Until;
end CuBit.Messages;
