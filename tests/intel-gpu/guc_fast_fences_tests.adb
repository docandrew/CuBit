with Interfaces; use Interfaces;
with Intel_GPU_GuC_Fast_Fences;
procedure GuC_Fast_Fences_Tests is
   use Intel_GPU_GuC_Fast_Fences;
   Object : Stream;
   Fence, Again : Unsigned_16;
   OK : Boolean;
begin
   -- Four full wire wraps, not merely the previous 256-ID reservation.
   for I in 0 .. 131071 loop
      Prepare (Object, Fence, OK);
      pragma Assert (OK and Fence = 16#8000# + Unsigned_16 (I mod 32768));
      Prepare (Object, Again, OK);
      pragma Assert (not OK and Again = 0 and Pending (Object));
      Sent (Object, Not_Published);
      Prepare (Object, Again, OK);
      pragma Assert (OK and Again = Fence);
      Reject_Response (Object, 42);
      pragma Assert (not Failed (Object));
      Sent (Object, Published);
      pragma Assert (not Failed (Object) and not Pending (Object));
   end loop;
   -- Old failure remains fatal even after the same diagnostic ID wrapped.
   Reject_Response (Object, 16#8000#);
   pragma Assert (Failed (Object));
   Prepare (Object, Fence, OK);
   pragma Assert (not OK and Fence = 0);
   Sent (Object, Not_Published);
   pragma Assert (Failed (Object));
   for Mode in 0 .. 2 loop
      declare Fresh : Stream; begin
         if Mode = 0 then
            Sent (Fresh, Published); -- no prepared send
         elsif Mode = 1 then
            Prepare (Fresh, Fence, OK); pragma Assert (OK);
            Sent (Fresh, Uncertain);
         else
            -- Even an unissued fast fence is not silently discarded.
            Reject_Response (Fresh, 16#FFFF#);
         end if;
         pragma Assert (Failed (Fresh));
         Prepare (Fresh, Fence, OK);
         pragma Assert (not OK and Fence = 0);
      end;
   end loop;
end GuC_Fast_Fences_Tests;
