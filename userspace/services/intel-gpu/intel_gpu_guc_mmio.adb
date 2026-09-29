package body Intel_GPU_GuC_MMIO is
   use Interfaces;
   function Broken (Object : Channel) return Boolean is (Object.Failed);
   procedure Exchange (Object : in out Channel;
     Request : Intel_GPU_GuC_CT_Setup.Request; Poll_Limit : Positive;
     Reply : out Unsigned_32; Status : out Result) is
      First, Previous, Stamp : Unsigned_64;
      Deadline : Unsigned_64 := 10_000;
      Send : Boolean := True;
      Busy : Boolean := False;
      Retries : Natural range 0 .. 3 := 0;
      OK : Boolean;
      Value : Unsigned_32;
   begin
      Reply := 0; Status := Rejected;
      if Object.Failed or else not Owner_Ready or else Request.Length not in 2 .. 4 or else
        (Request.Data (0) and 16#F0000000#) /= 0 then return; end if;
      Object.Failed := True;
      First := Now;
      Status := Invalid_Clock;
      if First = Unsigned_64'Last then return; end if;
      Previous := First;
      for Poll in 1 .. Poll_Limit loop
         Stamp := Now;
         Status := Invalid_Clock;
         if Stamp = Unsigned_64'Last or else Stamp < Previous then return; end if;
         Previous := Stamp;
         Status := Timed_Out;
         if Stamp - First >= Deadline then return; end if;
         Status := Access_Failed;
         if not Owner_Ready then return; end if;
         if Send then
            for Index in 0 .. Request.Length - 1 loop
               if not Owner_Ready then return; end if;
               Write_Word (Index, Request.Data (Index), OK);
               if not OK then return; end if;
            end loop;
            -- Posting read orders all request stores before notification.
            Value := Read_Word (Request.Length - 1);
            if Value = Unsigned_32'Last or else not Owner_Ready then return; end if;
            Notify (OK);
            if not OK then return; end if;
            Send := False; Busy := False;
         end if;
         Value := Read_Word (0);
         if Value = Unsigned_32'Last or else not Owner_Ready then return; end if;
         Reply := Value;
         Stamp := Now;
         Status := Invalid_Clock;
         if Stamp = Unsigned_64'Last or else Stamp < Previous then return; end if;
         Previous := Stamp;
         Status := Timed_Out;
         if Stamp - First >= Deadline then return; end if;
         if (Value and 16#80000000#) = 0 then
            if Busy then Status := Invalid_Reply; return; end if;
         else
            case Shift_Right (Value, 28) and 7 is
               when 3 => Busy := True; Deadline := 1_000_000;
               when 5 =>
                  Status := Retry_Exhausted;
                  if Retries = 3 then return; end if;
                  Retries := Retries + 1; Send := True;
               when 6 => Status := Firmware_Failed; return;
               when 7 =>
                  Status := Complete; Object.Failed := False; return;
               when others => Status := Invalid_Reply; return;
            end case;
         end if;
         if Poll < Poll_Limit then Pause; end if;
      end loop;
      Status := Timed_Out;
   end Exchange;
end Intel_GPU_GuC_MMIO;
