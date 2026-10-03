with Interfaces; use Interfaces;
with System; with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code; use System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Desktop_Messages;
with Client_Input_Channel_Policy;
package body Client_Input_Channel with SPARK_Mode => Off is
   package Policy renames Client_Input_Channel_Policy;
   package MG renames CuBit.Memory_Grants;
   package W renames Cache.W;
   use type Policy.Phase;
   use type Cache.DP.Status_Code;
   Control : Policy.State;
   Base : Unsigned_64 := 0;
   Grant : MG.Grant_Reference;
   Guard : Unsigned_32 := 0 with Atomic;
   Disabled : Boolean := False with Atomic;
   function Is_Disabled return Boolean is (Disabled);
   function Exchange (Value : Unsigned_32) return Unsigned_32 is
      Previous : Unsigned_32 := Value;
   begin
      Asm ("xchgl %0, %1",
        Outputs => (Unsigned_32'Asm_Output ("+r", Previous), Unsigned_32'Asm_Output ("+m", Guard)),
        Clobber => "memory", Volatile => True);
      return Previous;
   end Exchange;
   procedure Fetch
     (S : in out Cache.State; Surface : W.Identity;
      After : W.Word; Loaded : out Boolean)
   is
      Ignore : Unsigned_32;
      procedure Locked_Fetch is
         Needed, OK : Boolean;
         Identity : W.Word;
         Reply : Message;
      begin
         if Cache.Remaining (S) /= 0 then return; end if;
         Policy.Begin_Setup (Control, Needed);
         if Needed then
            Base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 4096);
            OK := Base /= 0 and then Base mod 4096 = 0 and then Base <= Unsigned_64'Last - 4096;
            if OK then
               MG.Create_Via_Capability (CAP_SLOT_DESKTOP,
                 To_Address (Integer_Address (Base)), 1, True, Grant, OK);
            end if;
            Policy.Finish_Setup (Control, OK);
         end if;
         if Policy.Mode (Control) /= Policy.Ready then return; end if;
         Policy.Reserve (Control, Identity);
         if Identity = 0 then return; end if;
         Reply := CuBit.Desktop_Messages.From_Wire
           (Cache.P.Encode (Cache.P.Request'(Surface, After, Grant, Identity)));
         Reply.tag := capCall (CAP_SLOT_DESKTOP, Reply);
         if Cache.P.Decode (CuBit.Desktop_Messages.To_Wire (Reply), Identity).Status /= Cache.DP.Success then
            -- A failed reply may conceal a retained writer. Never reuse or
            -- release this page; its bounded storage lives until process exit.
            Policy.Disable (Control);
            return;
         end if;
         declare
            Shared : W.Snapshot_Words
              with Import, Address => To_Address (Integer_Address (Base)), Volatile;
            Local : constant W.Snapshot_Words := Shared;
         begin
            Cache.Load (S, Local, CuBit.Desktop_Messages.To_Wire (Reply), Surface, Identity, After, Loaded);
         end;
         if not Loaded then Policy.Disable (Control); end if;
      end Locked_Fetch;
   begin
      Loaded := False;
      if Exchange (1) /= 0 then return; end if;
      Locked_Fetch;
      Disabled := Policy.Mode (Control) = Policy.Disabled;
      Ignore := Exchange (0);
   end Fetch;
end Client_Input_Channel;
