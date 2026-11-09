with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;

--  Native regression for LOG-004: recursion with a large frame until the
--  stack reservation runs out. The kernel stops the program and files a
--  typed fault report under its identity: a page fault in the band below
--  the stack, "(stack overflow)" (tests/headless/run.sh,
--  CCL_CONSOLE_DEMO=kernel-log).
procedure Main is
   --  Bytes each frame touches: a page per frame keeps every page of the
   --  reservation demand-mapped on the way down, as real overflows do.
   Frame_Bytes : constant := 4_096;
   Word_Bytes  : constant := 8;
   type Frame is array (1 .. Frame_Bytes / Word_Bytes) of Unsigned_64;
   --  Deeper than any stack reservation (a volatile, so the recursion is
   --  not known to be endless at compile time).
   Limit : Unsigned_64 := Unsigned_64'Last with Volatile;

   function Deeper (Depth : Unsigned_64) return Unsigned_64;
   function Deeper (Depth : Unsigned_64) return Unsigned_64 is
      Local : Frame with Volatile;
   begin
      if Depth = Limit then
         return 0;
      end if;
      Local (Local'First) := Depth;
      Local (Local'Last) := Depth;
      --  Not a tail call: the frame is read after the call returns.
      return Deeper (Depth + 1) + Local (Local'First) + Local (Local'Last);
   end Deeper;

   Result : Unsigned_64;
begin
   debugPrint ("stack-probe: recursing until the stack runs out" & ASCII.LF);
   Result := Deeper (0);
   debugPrint ((if Result = 0 then "stack-probe: FAIL the recursion returned"
                else "stack-probe: FAIL the recursion returned a value") & ASCII.LF);
end Main;
