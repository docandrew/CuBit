pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Memory_Grants;
with CuBit.Benchmark_Clock;
with CuBit.Timing_Histograms;

procedure Main is
   package Clock renames CuBit.Benchmark_Clock;
   package Timing renames CuBit.Timing_Histograms;
   package Grants renames CuBit.Memory_Grants;
   Path : constant String := "@nvme:0/cubit-latency.dat";
   Block_Bytes : constant := 4096;
   Block_Count : constant := 16;
   Samples : constant := 512;
   Raw, Aligned, Rate : Unsigned_64;
   Loan : Grants.Grant_Reference;
   Granted, Ignored, OK : Boolean := False;

   function Run return Boolean is
      Buffer : String (1 .. Block_Bytes)
        with Import, Address => To_Address (Integer_Address (Aligned));
      Handle : File_Handle;
      Opened : Boolean := False;
      Msg : Message;
      Opening, Sequential, Random_Read, Overwrite : Timing.Histogram;
      Before, After : Unsigned_64;
      Seed : Unsigned_32 := 17;

      function Call (Request : Message) return Boolean is
      begin
         Msg := Request;
         Msg.tag := capCall (CAP_SLOT_FS, Msg);
         return Msg.tag.label = REPLY_OK;
      end Call;

      function Close return Boolean is
         Success : constant Boolean := Call (Close_Request (Handle));
      begin
         Opened := False;
         return Success;
      end Close;

      procedure Put_Path is
      begin
         Buffer := [others => Character'Val (0)];
         Buffer (1 .. Path'Length) := Path;
      end Put_Path;

      function Fail (Why : String) return Boolean is
      begin
         debugPrint ("BENCH: FAIL storage " & Why & ASCII.LF);
         if Opened and then not Close then
            debugPrint ("BENCH: FAIL storage cleanup close" & ASCII.LF);
         end if;
         return False;
      end Fail;

      function Timed_Call (Request : Message; H : in out Timing.Histogram)
        return Boolean is
         Success : Boolean;
      begin
         -- Message setup and checks are outside the interval.
         Msg := Request;
         Before := Clock.Read_Counter;
         Msg.tag := capCall (CAP_SLOT_FS, Msg);
         After := Clock.Read_Counter;
         Success := Msg.tag.label = REPLY_OK and After >= Before;
         if Success then Timing.Add (H, After - Before); end if;
         return Success;
      end Timed_Call;
   begin
      Put_Path;
      -- Never overwrite an existing user's file, even outside the runner.
      if not Call (Open_Request
        (Loan, Path'Length, OPEN_CREATE or OPEN_EXCLUSIVE or OPEN_READ_WRITE))
      then return Fail ("exclusive create"); end if;
      Handle := File_Handle (Msg.words (0));
      Opened := True;
      for Block in 0 .. Block_Count - 1 loop
         Buffer := [others => Character'Val (65 + Block)];
         if not Call (Write_Request (Handle, Loan, Block_Bytes)) or else
           Msg.words (0) /= Block_Bytes
         then return Fail ("initialize"); end if;
      end loop;
      if not Close then return Fail ("initial close"); end if;

      -- Repeated warm-path opens. Path publication and close are not timed.
      for I in 1 .. Samples + 32 loop
         Put_Path;
         if I <= 32 then
            OK := Call (Open_Request (Loan, Path'Length, OPEN_READ_WRITE));
         else
            OK := Timed_Call
              (Open_Request (Loan, Path'Length, OPEN_READ_WRITE), Opening);
         end if;
         if not OK then return Fail ("open"); end if;
         Handle := File_Handle (Msg.words (0));
         Opened := True;
         if not Close then return Fail ("close"); end if;
      end loop;
      Put_Path;
      if not Call (Open_Request (Loan, Path'Length, OPEN_READ_WRITE)) then
         return Fail ("read/write open");
      end if;
      Handle := File_Handle (Msg.words (0));
      Opened := True;

      for Random_Order in Boolean loop
         for I in 1 .. Samples + 32 loop
            declare
               Block : Natural;
            begin
               Seed := Seed * 1664525 + 1013904223;
               Block := (if Random_Order then Natural (Shift_Right (Seed, 16) mod Block_Count)
                         else (I - 1) mod Block_Count);
               if not Call (Seek_Request
                 (Handle, Unsigned_64 (Block * Block_Bytes), From_Start))
               then return Fail ("seek"); end if;
               Buffer := [others => Character'Val (0)];
               if I <= 32 then
                  OK := Call (Read_Request (Handle, Loan, Block_Bytes));
               elsif Random_Order then
                  OK := Timed_Call (Read_Request (Handle, Loan, Block_Bytes), Random_Read);
               else
                  OK := Timed_Call (Read_Request (Handle, Loan, Block_Bytes), Sequential);
               end if;
               if not OK or else Msg.words (0) /= Block_Bytes then
                  return Fail ("read");
               end if;
               if (for some C of Buffer => C /= Character'Val (65 + Block)) then
                  return Fail ("read content");
               end if;
            end;
         end loop;
      end loop;

      for I in 1 .. Samples + 32 loop
         if not Call (Seek_Request (Handle, 0, From_Start)) then
            return Fail ("overwrite seek");
         end if;
         Buffer := [others => 'Z'];
         if I <= 32 then
            OK := Call (Write_Request (Handle, Loan, Block_Bytes));
         else
            OK := Timed_Call (Write_Request (Handle, Loan, Block_Bytes), Overwrite);
         end if;
         if not OK or else Msg.words (0) /= Block_Bytes then
            return Fail ("overwrite");
         end if;
      end loop;
      if not Call (Seek_Request (Handle, 0, From_Start)) then
         return Fail ("verify seek");
      end if;
      Buffer := [others => Character'Val (0)];
      if not Call (Read_Request (Handle, Loan, Block_Bytes)) or else
        Msg.words (0) /= Block_Bytes or else (for some C of Buffer => C /= 'Z')
      then return Fail ("overwrite content"); end if;
      if not Close then return Fail ("final close"); end if;
      Clock.Report ("fs-open-existing", Opening);
      Clock.Report ("fs-read-4k-sequential-warm", Sequential);
      Clock.Report ("fs-read-4k-random-warm", Random_Read);
      Clock.Report ("fs-write-4k-overwrite", Overwrite);
      return True;
   end Run;
begin
   Clock.Calibrate (Rate);
   if Rate = 0 then
      debugPrint ("BENCH: FAIL storage calibration" & ASCII.LF); return;
   end if;
   Raw := syscall (SYSCALL_SBRK, 2 * Block_Bytes);
   if Raw = Unsigned_64'Last then
      debugPrint ("BENCH: FAIL storage allocation" & ASCII.LF); return;
   end if;
   Aligned := (Raw + Block_Bytes - 1) and not (Unsigned_64 (Block_Bytes) - 1);
   Grants.Create_Via_Capability
     (CAP_SLOT_FS, To_Address (Integer_Address (Aligned)), 1, True, Loan, Granted);
   if not Granted then
      debugPrint ("BENCH: FAIL storage grant" & ASCII.LF); return;
   end if;
   debugPrint ("STORAGE-BENCH: START samples=512 block_bytes=4096 working_set=65536" & ASCII.LF);
   OK := Run;
   Grants.Revoke (Loan, Ignored);
   if OK and Ignored then
      debugPrint ("STORAGE-BENCH: COMPLETE" & ASCII.LF);
   else
      debugPrint ("BENCH: FAIL storage run/revoke" & ASCII.LF);
   end if;
end Main;
