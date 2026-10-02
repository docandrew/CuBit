with System.Storage_Elements;
package body CuBit.Messages is
   function saveReplyCap (destSlot : Unsigned_64) return Unsigned_64 is
   begin
      Save_Count := Save_Count + 1;
      Saved_Slot := destSlot;
      return Save_Result;
   end saveReplyCap;
   function replyCap (slot : CapabilitySlot; msg : Message) return Unsigned_64 is
   begin
      Reply_Count := Reply_Count + 1;
      Replied_Slot := slot;
      Last_Reply := msg;
      return Reply_Result;
   end replyCap;
   function capSubmit (slot : CapabilitySlot; msg : Message;
                       token : Unsigned_64) return Boolean is
   begin
      Last_Slot := slot;
      Last_Submit := msg;
      Last_Token := token;
      return Submit_Result;
   end capSubmit;
   function Syscall (Number : Unsigned_64;
     A, B, C, D, E, F : Unsigned_64 := 0) return Unsigned_64 is
   begin
      case Number is
         when SYSCALL_GETPID => return 10;
         when SYSCALL_INSPECT_CAPABILITY =>
            declare
               Output : Words with Import, Address =>
                 System.Storage_Elements.To_Address
                   (System.Storage_Elements.Integer_Address (C));
            begin
               pragma Assert (A = 10 and B in 7 | 30 | 31 | 40 .. 55);
               Output := (if B in 40 .. 55 then Application_Inspection
                          else Inspection);
            end;
            return 1;
         when SYSCALL_POLICY_MINT_CAPABILITY_FOR_INCARNATION |
              SYSCALL_POLICY_DELEGATE_ENDPOINT =>
            Last_Operation := Number;
            Last_Arguments := [A, B, C, D, E, F];
            return Grant_Result;
         when others => raise Program_Error;
      end case;
   end Syscall;
end CuBit.Messages;
