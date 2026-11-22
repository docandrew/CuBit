with System.Address_To_Access_Conversions;
package body CuBit.Messages is
   package Entries is new System.Address_To_Access_Conversions (CompletionEntry);
   function syscall
     (call : Unsigned_64; arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64 := 0)
      return Unsigned_64 is
   begin
      if call = SYSCALL_GETTIME then
         Clocks := Clocks + 1;
         if Mode = 7 then return Unsigned_64'Last; end if;
         if Mode = 8 and Clocks > 1 then return 0; end if;
         if Mode = 9 then return 100; end if;
         return Unsigned_64 (Clocks);
      end if;
      raise Program_Error with "unexpected system call";
   end syscall;
   function Poll_Completion (result : System.Address) return Unsigned_64 is
      Item : constant Entries.Object_Pointer := Entries.To_Pointer (result);
   begin
      Polls := Polls + 1;
      if Mode in 9 | 10 then return 0; end if;
      Item.all := (0, Saved_Token,
        ((label => 16#F000#, length => 2, protocol => 0, ipc => <>, reserved => 0), 0, [Base + 16#80_0000#, Expected_Bytes, 0, 0]),
        1, 0, True);
      case Mode is
         when 1 => Item.msg.words (0) := Item.msg.words (0) + 4096;
         when 2 => Item.msg.words (1) := Expected_Bytes + 4096;
         when 3 => Item.msg.tag := (label => 16#F001#, length => 0,
                                    protocol => 0, ipc => <>, reserved => 0);
         when 4 =>
            if Polls = 1 then
               Item.msg.tag := (label => 16#F002#, length => 0,
                                protocol => 0, ipc => <>, reserved => 0);
               Item.msg.words := [others => 0];
            end if;
         when 11 => Item.status := 1;
         when 12 => Item.msg.tag.protocol := 1;
         when 13 => Item.msg.words (3) := 1;
         when 14 => Item.token := Saved_Token + 1;
         when others => null;
      end case;
      return 1;
   end Poll_Completion;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result is
   begin
      pragma Assert (Deadline > 0);
      return Deadline_Reached;
   end Wait_For_Activity_Until;
end CuBit.Messages;
