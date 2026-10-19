with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with Startup;

--  Out-of-memory reclaim check (headless oom-reclaim). Lend pages to
--  procmgr until this process owns many grants, take every free page with
--  SBRK, then touch new stack pages. The kernel kills this process because
--  no page is left, and must reclaim it (revoking every grant) while memory
--  is still exhausted: process teardown may not allocate. The test passes
--  when the kernel records the out-of-memory kill, procmgr reports the exit
--  and the kernel never panics.
procedure Main is
   use ASCII;

   Page_Bytes : constant := 4096;
   --  Grants held at the kill: enough that the old teardown walk needed a
   --  page of scratch memory for its snapshot.
   Loans : constant := 1_100;
   --  SBRK requests start here and halve on refusal, down to one page.
   Largest_Request : constant Unsigned_64 := 64 * 1024 * 1024;
   Refused : constant Unsigned_64 := Unsigned_64'Last;
   Bytes_Per_MiB : constant := 1024 * 1024;

   type Page is array (1 .. Page_Bytes) of Unsigned_8
     with Alignment => Page_Bytes;
   subtype Loan_Index is Positive range 1 .. Loans;
   type Page_Array is array (Loan_Index) of Page;
   Lent_Pages : Page_Array;

   function Decimal (Value : Unsigned_64) return String is
      Result : String (1 .. 20);
      First : Natural := Result'Last + 1;
      V : Unsigned_64 := Value;
   begin
      loop
         First := First - 1;
         Result (First) := Character'Val (Character'Pos ('0') + Natural (V mod 10));
         V := V / 10;
         exit when V = 0;
      end loop;
      return Result (First .. Result'Last);
   end Decimal;

   Held : Unsigned_64 := 0;
   Request : Unsigned_64 := Largest_Request;
   Taken : Unsigned_64 := 0;

   --  Write one byte to each stack page below this frame in turn, until
   --  the kernel kills this process for want of a page. Stores through
   --  volatile overlays, not recursion: the compiler sees that an endless
   --  recursion never returns and may drop its frames.
   procedure Touch_Stack is
      Anchor : Unsigned_8 with Volatile;
      Here : Integer_Address :=
        To_Integer (Anchor'Address) - To_Integer (Anchor'Address) mod Page_Bytes;
   begin
      Anchor := 0;
      loop
         Here := Here - Page_Bytes;
         declare
            Probe : Unsigned_8 with Import, Volatile, Address => To_Address (Here);
         begin
            Probe := Anchor;
         end;
      end loop;
   end Touch_Stack;
begin
   for I in Loan_Index loop
      declare
         Loan : CuBit.Memory_Grants.Grant_Reference;
         Lent : Boolean;
      begin
         Lent_Pages (I) (1) := 1;
         CuBit.Memory_Grants.Create_Via_Capability
           (Startup.Process_Observer, Lent_Pages (I)'Address, 1, False, Loan, Lent);
         exit when not Lent;
         Held := Held + 1;
      end;
   end loop;
   debugPrint ("oom-check: holding " & Decimal (Held) & " grants" & LF);

   while Request >= Page_Bytes loop
      if syscall (SYSCALL_SBRK, Request) = Refused then
         Request := Request / 2;
      else
         Taken := Taken + Request;
      end if;
   end loop;
   debugPrint ("oom-check: memory exhausted after " &
               Decimal (Taken / Bytes_Per_MiB) & " MiB" & LF);

   Touch_Stack;
   debugPrint ("oom-check: FAIL not killed" & LF);
end Main;
