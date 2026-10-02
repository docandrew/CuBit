with System.Storage_Elements;
package body CuBit.Messages is
   function Syscall (Number : Unsigned_64;
     A, B, C, D, E, F : Unsigned_64 := 0) return Unsigned_64 is
   begin
      case Number is
         when SYSCALL_GETPID => return Current_PID;
         when SYSCALL_INSPECT_CAPABILITY =>
            declare
               Output : Words with Import, Address =>
                 System.Storage_Elements.To_Address
                   (System.Storage_Elements.Integer_Address (C));
            begin
               pragma Assert (A = Current_PID and B = 4);
               Output := Inspection;
            end;
            return Inspect_Result;
         when SYSCALL_POLICY_MINT_CAPABILITY_FOR_INCARNATION |
              SYSCALL_POLICY_DELEGATE_ENDPOINT =>
            Last_Number := Number;
            Arguments := [A, B, C, D, E, F];
            Mint_Calls := Mint_Calls + 1;
            return Mint_Result;
         when others => raise Program_Error;
      end case;
   end Syscall;
end CuBit.Messages;
