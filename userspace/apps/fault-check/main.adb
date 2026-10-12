with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;

--  Process-kill diagnostics check: write to an address outside every
--  reservation, so the kernel kills this process (kind not-present). The
--  kill must come back as a "kernel: process killed #N" record in logstore,
--  on the Desktop bootstrap screen and as procmgr's exit line; the headless
--  test process-kill looks for all three.
procedure Main is
   use ASCII;
   --  An address in the first page, which no process maps.
   Unmapped : constant Integer_Address := 16#10#;
   Target : Unsigned_64 with Import, Volatile,
     Address => To_Address (Unmapped);
begin
   debugPrint ("fault-check: writing an unmapped address" & LF);
   Target := 16#DEAD#;
   debugPrint ("fault-check: FAIL the write did not fault" & LF);
end Main;
