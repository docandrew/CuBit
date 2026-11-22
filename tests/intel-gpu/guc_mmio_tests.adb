with Interfaces; use Interfaces;
with Intel_GPU_GuC_CT_Setup; with Intel_GPU_GuC_MMIO;
with Ada.Text_IO;
procedure GuC_MMIO_Tests is
   procedure Run (Mode : Natural) is
      Words : Intel_GPU_GuC_CT_Setup.Words := [others => 0];
      Writes, Notifications, Reads : Natural := 0;
      Time : Unsigned_64 := 0;
      Posted : Boolean := False;
      function Owner return Boolean is (Mode /= 1 and then (Mode /= 15 or Notifications = 0));
      function Read_Word (Index : Natural) return Unsigned_32 is
      begin
         if Index /= 0 then
            Posted := True;
            return (if Mode = 16 then Unsigned_32'Last else Words (Index));
         end if;
         Reads := Reads + 1;
         return (case Mode is
           when 2 => Unsigned_32'Last,
           when 3 => 16#E0000001#,
           when 4 => 16#90000001#,
           when 5 => 16#D0000000#,
           when 6 => (if Reads = 1 then 16#B0000001# else 16#F0000001#),
           when 7 => (if Reads = 1 then 16#B0000001# else 16#508#),
           when 8 => (if Notifications = 1 then 16#D0000001# else 16#F0000001#),
           when 9 => 16#508#,
           when 17 => 16#B0000001#,
           when others => 16#F0000001#);
      end Read_Word;
      procedure Write_Word (Index : Natural; Value : Unsigned_32; Success : out Boolean) is
      begin
         Writes := Writes + 1; Words (Index) := Value; Posted := False;
         Success := Mode /= 10;
      end Write_Word;
      procedure Notify (Success : out Boolean) is
      begin
         pragma Assert (Posted); Notifications := Notifications + 1;
         Success := Mode /= 11;
      end Notify;
      function Now return Unsigned_64 is
      begin
         Time := Time + 1;
         return (if Mode = 12 then Unsigned_64'Last
                 elsif Mode = 13 and Time >= 3 then 0
                 elsif Mode = 14 and Time >= 3 then 10_001 else Time);
      end Now;
      procedure Pause is begin null; end Pause;
      package M is new Intel_GPU_GuC_MMIO (Owner,Read_Word,Write_Word,Notify,Now,Pause);
      use type M.Result;
      Object : M.Channel;
      Reply : Unsigned_32; Status : M.Result;
      Request : Intel_GPU_GuC_CT_Setup.Request := (4,[16#508#,16#9060002#,16#201000#,0]);
      Before : Natural;
   begin
      if Mode = 18 then Request.Length := 0; end if;
      if Mode = 19 then Request.Data (0) := 16#80000508#; end if;
      M.Exchange (Object,Request,10,Reply,Status);
      case Mode is
         when 0 | 6 | 8 => pragma Assert (Status = M.Complete and not M.Broken (Object));
         when 1 | 18 | 19 => pragma Assert (Status = M.Rejected and Writes = 0);
         when 2 | 10 | 11 | 15 | 16 => pragma Assert (Status = M.Access_Failed);
         when 3 => pragma Assert (Status = M.Firmware_Failed);
         when 4 | 7 => pragma Assert (Status = M.Invalid_Reply);
         when 5 => pragma Assert (Status = M.Retry_Exhausted and Notifications = 4);
         when 9 | 14 | 17 => pragma Assert (Status = M.Timed_Out);
         when 12 => pragma Assert (Status = M.Invalid_Clock and Writes = 0);
         when 13 => pragma Assert (Status = M.Invalid_Clock);
         when others => raise Program_Error;
      end case;
      if Mode = 8 then pragma Assert (Notifications = 2 and Writes = 8); end if;
      if Mode not in 0 | 1 | 6 | 8 | 18 | 19 then
         pragma Assert (M.Broken (Object)); Before := Writes;
         M.Exchange (Object,Request,10,Reply,Status);
         pragma Assert (Status = M.Rejected and Writes = Before);
      end if;
   end Run;
begin
   for Mode in 0 .. 19 loop Run (Mode); end loop;
   Ada.Text_IO.Put_Line ("MMIO HXG PASS: posting order,busy,retry,failure,poll budget,broken-channel reuse");
end GuC_MMIO_Tests;
