with Intel_GPU_GGTT;
package body Intel_GPU_GGTT_Reservations with SPARK_Mode is
   function Valid (Object : Ledger) return Boolean is
     ((Object.Ready or else Object.Used = 0) and then
      (for all I in 1 .. Object.Used =>
         Object.Claims (I).First < Object.Claims (I).Limit and then
         Object.Claims (I).First >= Object.Aperture.First and then
         Object.Claims (I).Limit <= Object.Aperture.Limit and then
         (for all J in 1 .. I - 1 =>
            Object.Claims (J).Limit <= Object.Claims (I).First or else
            Object.Claims (I).Limit <= Object.Claims (J).First)));
   function Count (Object : Ledger) return Natural is (Object.Used);
   function Claims (Object : Ledger) return Claim_Model is
     ((Values => Object.Claims));
   function Preserves
     (Before, After : Claim_Model; Prior_Count : Natural) return Boolean is
     (for all I in 1 .. Prior_Count => Before.Values (I) = After.Values (I));
   function Last_Claim_Is
     (Object : Ledger; First, Bytes : Unsigned_64) return Boolean is
     (Object.Used > 0 and then
      Object.Claims (Object.Used).First = First and then
      Object.Claims (Object.Used).Limit >= First and then
      Object.Claims (Object.Used).Limit - First = Bytes);
   function Table_Size (Object : Ledger) return Unsigned_64 is (Object.Table_Bytes);
   function Aperture_First (Object : Ledger) return Unsigned_64 is
     (if Object.Ready then Object.Aperture.First else 0);
   function Aperture_Bytes (Object : Ledger) return Unsigned_64 is
     (if Object.Ready then Object.Aperture.Limit - Object.Aperture.First else 0);
   function Space_Free
     (Object : Ledger; First, Bytes : Unsigned_64) return Boolean is
     (Object.Ready and then Bytes > 0 and then
      First >= Object.Aperture.First and then First < Object.Aperture.Limit and then
      Bytes <= Object.Aperture.Limit - First and then
      (for all I in 1 .. Object.Used =>
         Object.Claims (I).Limit <= First or else
         First + Bytes <= Object.Claims (I).First));
   procedure Find_Free
     (Object : Ledger; Bytes, Alignment : Unsigned_64;
      First : out Unsigned_64; Found : out Boolean)
   is
      Candidate, Next_Candidate, Padding : Unsigned_64;
      Moved : Boolean;
   begin
      First := 0;
      Found := False;
      if not Object.Ready or else Bytes = 0 or else Bytes mod 4096 /= 0
        or else Alignment < 4096 or else Alignment > 2 ** 32
        or else (Alignment and (Alignment - 1)) /= 0
      then return; end if;
      Candidate := Object.Aperture.First;
      -- Each collision advances beyond a different retained claim. Claims
      -- need not be sorted; at most Used collisions can occur. Keep an
      -- explicit bound even if a future implementation changes that premise.
      for Pass in 1 .. Object.Used + 1 loop
         pragma Loop_Invariant
           (Candidate >= Object.Aperture.First and not Found and First = 0);
         Padding := (Alignment - Candidate mod Alignment) mod Alignment;
         if Candidate > Unsigned_64'Last - Padding then return; end if;
         Candidate := Candidate + Padding;
         if Candidate >= Object.Aperture.Limit or else
           Bytes > Object.Aperture.Limit - Candidate
         then return; end if;
         Moved := False;
         Next_Candidate := Candidate;
         for I in 1 .. Object.Used loop
            if Candidate < Object.Claims (I).Limit and then
              Object.Claims (I).First < Candidate + Bytes
            then
               Next_Candidate := Object.Claims (I).Limit;
               Moved := True;
               exit;
            end if;
            pragma Loop_Invariant
              (not Moved and then Candidate >= Object.Aperture.First and then
               Candidate < Object.Aperture.Limit and then
               Bytes <= Object.Aperture.Limit - Candidate and then
               (for all J in 1 .. I => Object.Claims (J).Limit <= Candidate or else
                  Candidate + Bytes <= Object.Claims (J).First));
         end loop;
         if not Moved then
            pragma Assert (Candidate >= Object.Aperture.First);
            pragma Assert (Candidate < Object.Aperture.Limit);
            pragma Assert (Bytes <= Object.Aperture.Limit - Candidate);
            pragma Assert
              (for all I in 1 .. Object.Used =>
                 Object.Claims (I).Limit <= Candidate or else
                 Candidate + Bytes <= Object.Claims (I).First);
            pragma Assert (Space_Free (Object, Candidate, Bytes));
            First := Candidate;
            Found := True;
            pragma Assert (Space_Free (Object, First, Bytes));
            return;
         end if;
         Candidate := Next_Candidate;
      end loop;
      First := 0;
      Found := False;
   end Find_Free;
   procedure Allocate
     (Object : in out Ledger; Bytes, Alignment : Unsigned_64;
      First : out Unsigned_64; Status : out Result)
   is
      Candidate : Unsigned_64;
      Found : Boolean;
   begin
      First := 0;
      Status := Rejected;
      Find_Free (Object, Bytes, Alignment, Candidate, Found);
      if not Found then return; end if;
      Reserve (Object, Candidate, Bytes, Status);
      if Status = Reserved then First := Candidate; end if;
   end Allocate;
   procedure Admit
     (Object : in out Ledger; Table_Bytes, First, Bytes : Unsigned_64;
      Success : out Boolean)
   is
      Plan : constant Intel_GPU_GGTT.Window :=
        Intel_GPU_GGTT.Plan_Window (Table_Bytes, First, Bytes);
   begin
      Success := False;
      if Object.Ready or else not Plan.Valid or else Bytes mod 4096 /= 0
        or else First > Unsigned_64'Last - Bytes
      then return; end if;
      Object.Aperture := (First, First + Bytes);
      Object.Table_Bytes := Table_Bytes;
      Object.Ready := True;
      Success := True;
   end Admit;
   procedure Reserve
     (Object : in out Ledger; First, Bytes : Unsigned_64;
      Status : out Result)
   is
      Limit : Unsigned_64;
   begin
      Status := Rejected;
      if not Object.Ready or else Bytes = 0 or else Bytes mod 4096 /= 0
        or else First mod 4096 /= 0
        or else First < Object.Aperture.First
        or else First >= Object.Aperture.Limit
        or else Bytes > Object.Aperture.Limit - First
      then return; end if;
      Limit := First + Bytes;
      for I in 1 .. Object.Used loop
         if First < Object.Claims (I).Limit and then
           Object.Claims (I).First < Limit
         then Status := Overlap; return; end if;
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              Object.Claims (J).Limit <= First or else
              Limit <= Object.Claims (J).First);
      end loop;
      if Object.Used = Object.Claims'Length then
         Status := Exhausted; return;
      end if;
      Object.Used := Object.Used + 1;
      Object.Claims (Object.Used) := (First, Limit);
      Status := Reserved;
   end Reserve;
end Intel_GPU_GGTT_Reservations;
