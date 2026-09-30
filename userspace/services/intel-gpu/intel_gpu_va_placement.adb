package body Intel_GPU_VA_Placement with SPARK_Mode is
   procedure Find
     (Window : Extent; Used : Extents; Bytes, Alignment : Unsigned_64;
      First : out Unsigned_64; Found : out Boolean)
   is
      Candidate, Padding, Next_Candidate : Unsigned_64;
      Moved : Boolean;
   begin
      First := 0; Found := False;
      if Window.First >= Window.Limit or else Window.Limit > 2 ** 48
        or else Window.First mod 4096 /= 0 or else Window.Limit mod 4096 /= 0
        or else Used'Length > 4096 or else Bytes = 0 or else Bytes mod 4096 /= 0
        or else Alignment < 4096 or else Alignment > 2 ** 48
        or else (Alignment and (Alignment - 1)) /= 0
      then return; end if;
      for Claim of Used loop
         if Claim.First >= Claim.Limit or else Claim.First < Window.First
           or else Claim.Limit > Window.Limit or else Claim.First mod 4096 /= 0
           or else Claim.Limit mod 4096 /= 0 then return; end if;
      end loop;
      Candidate := Window.First;
      for Pass in 1 .. Used'Length + 1 loop
         pragma Loop_Invariant (Candidate >= Window.First and not Found and First = 0);
         Padding := (Alignment - Candidate mod Alignment) mod Alignment;
         if Candidate > Unsigned_64'Last - Padding then return; end if;
         Candidate := Candidate + Padding;
         if Candidate >= Window.Limit or else Bytes > Window.Limit - Candidate
         then return; end if;
         Moved := False; Next_Candidate := Candidate;
         for I in Used'Range loop
            pragma Loop_Invariant (not Found and First = 0);
            if Candidate < Used (I).Limit and then
              Used (I).First < Candidate + Bytes
            then
               Next_Candidate := Used (I).Limit; Moved := True; exit;
            end if;
         end loop;
         if not Moved then
            -- Explicit predicate also guards against future search changes.
            if Available (Window, Used, Candidate, Bytes) and then
              Candidate mod Alignment = 0 then
               First := Candidate; Found := True;
               pragma Assert (Available (Window, Used, First, Bytes));
               pragma Assert (Alignment >= 4096 and First mod Alignment = 0);
            end if;
            return;
         end if;
         Candidate := Next_Candidate;
      end loop;
   end Find;
end Intel_GPU_VA_Placement;
