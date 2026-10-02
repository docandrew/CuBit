with Interfaces; use Interfaces;
with Native_GPU_Query;
package body GPU_Budget_Probe is
   use CuBit.Messages;
   Sequence : Natural := 0;
   procedure Server (Sender : ProcessID; Request : Message) is
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      Response.tag := (16#0A2E#, 4, 0, 0);
      Response.words := [1,0,0,0];
      if Request.tag = Response.tag and then Request.words = [1,0,0,0] then
         Sequence := Sequence + 1;
         case Sequence is
            when 1 => Response.words := [0,33554432,4096,15];
            when 2 => Response.words := [4,0,0,0];
            when 3 => Response.tag.label := 16#0A20#;
            when 4 => Response.tag.length := 3;
            when others => null;
         end case;
      end if;
      -- Exercise the same saved-reply primitive as the production async
      -- budget adapter, without emulating hardware or supervisor authority.
      if saveReplyCap (60) = 1 then
         Delivered := replyCap (60, Response);
      else
         debugPrint ("TEST: FAIL GPU-BUDGET-IPC save reply" & ASCII.LF);
         Delivered := reply (Sender, Response);
      end if;
      if Delivered /= 1 then
         debugPrint ("TEST: FAIL GPU-BUDGET-IPC delivery" & ASCII.LF);
      end if;
   end Server;
   procedure Client (Slot : CapabilitySlot) is
      use type Native_GPU_Query.Reply_Words;
      Words : aliased Native_GPU_Query.Reply_Words := [others => Unsigned_64'Last];
      Status : Unsigned_32;
      Passed : Boolean := True;
   begin
      for Step in 1 .. 4 loop
         Words := [others => Unsigned_64'Last];
         Status := Native_GPU_Query.Budget (Unsigned_64 (Slot), Words'Access);
         case Step is
            when 1 => Passed := Passed and Status = 0 and Words = [0,33554432,4096,15];
            when 2 => Passed := Passed and Status = 0 and Words = [4,0,0,0];
            when others => Passed := Passed and Status = 1 and Words = [0,0,0,0];
         end case;
      end loop;
      debugPrint ((if Passed then "GPU-BUDGET-IPC: PASS" else
                   "TEST: FAIL GPU-BUDGET-IPC client") & ASCII.LF);
   end Client;
end GPU_Budget_Probe;
