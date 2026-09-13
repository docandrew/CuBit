with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Boot_Frame_Allocator;
with Proof_Instance;
procedure Main is
   Cases : Natural := 0;

   generic
      Last : Positive;
      Exhaustive : Boolean;
   procedure Exercise;

   procedure Exercise is
      package Core is new Boot_Frame_Allocator (Last);
      type Oracle_Bitmap is array (Core.Frame) of Boolean;
      Expected : Oracle_Bitmap := [others => False];
      Actual : Core.State;
      Expected_High : Core.Frame := 0;
      Seed : Unsigned_32 := 16#C0B17#;

      procedure Check is
         Count : Natural := 0;
      begin
         for F in Core.Frame loop
            pragma Assert (Core.Is_Free (Actual, F) = Expected (F));
            if Expected (F) then
               Count := Count + 1;
            end if;
         end loop;
         pragma Assert (not Core.Is_Free (Actual, 0));
         pragma Assert (Core.Highest (Actual) = Expected_High);
         pragma Assert (Core.Free_Count (Actual) = Count);
      end Check;

      procedure Reset is
      begin
         Core.Initialize (Actual);
         Expected := [others => False];
         Expected_High := 0;
         Check;
      end Reset;

      procedure Admit (F : Core.Payload_Frame) is
      begin
         Core.Admit (Actual, F);
         Expected (F) := True;
         -- Duplicate/overlapping firmware admission must not inflate a count.
         Core.Admit (Actual, F);
      end Admit;

      procedure Claim (Size : Core.Request_Size) is
         Expected_First : Core.Frame := 0;
         First : Core.Frame;
         Fits : Boolean;
         Before : constant Core.State := Actual;
         use type Core.State;
      begin
         -- Independent oracle: check every candidate interval, not a run counter.
         for Candidate in 1 .. Last - Size + 1 loop
            Fits := True;
            for F in Candidate .. Candidate + Size - 1 loop
               if not Expected (F) then
                  Fits := False;
                  exit;
               end if;
            end loop;
            if Fits then
               Expected_First := Candidate;
               exit;
            end if;
         end loop;
         Core.Reserve (Actual, Size, First);
         pragma Assert (First = Expected_First);
         if First = 0 then
            pragma Assert (Actual = Before);
         else
            for F in First .. First + Size - 1 loop
               pragma Assert (Expected (F));
               Expected (F) := False;
            end loop;
            Expected_High := Core.Frame'Max (Expected_High, First + Size - 1);
         end if;
         Cases := Cases + 1;
         Check;
      end Claim;

      procedure Pattern (Mask : Natural) is
      begin
         Reset;
         for F in Core.Payload_Frame loop
            if (Mask / 2 ** (F - 1)) mod 2 = 1 then
               Admit (F);
            end if;
         end loop;
         Check;
      end Pattern;
   begin
      Reset;
      Claim (Last); -- no usable firmware memory
      for F in Core.Payload_Frame loop
         Admit (F);
      end loop;
      Check;
      Claim (Last); -- exact arena exhaustion, excluding PFN zero
      Claim (1);
      Claim (Last);

      if Exhaustive then
         for Mask in 0 .. 2 ** Last - 1 loop
            for First_Size in Core.Request_Size loop
               for Second_Size in Core.Request_Size loop
                  Pattern (Mask);
                  Claim (First_Size);
                  Claim (Second_Size);
               end loop;
            end loop;
         end loop;
      else
         -- Endpoints, 64-bit boundaries and fragmented production-sized arenas.
         Reset;
         Admit (Last);
         Claim (2);
         Claim (1);
         Claim (1);
         Reset;
         for F in 62 .. 66 loop
            Admit (F);
         end loop;
         Claim (6);
         Claim (5);
         Claim (1);
         for Round in 1 .. 12 loop
            Reset;
            for F in Core.Payload_Frame loop
               Seed := Seed * 1_664_525 + 1_013_904_223;
               if Seed mod 16 /= 0 then
                  Admit (F);
               end if;
            end loop;
            Check;
            for Request in 1 .. 100 loop
               Seed := Seed * 1_664_525 + 1_013_904_223;
               Claim (1 + Natural (Seed mod 96));
            end loop;
         end loop;
      end if;
   end Exercise;

   procedure Small is new Exercise (10, True);
   procedure Singleton is new Exercise (1, True);
   procedure Production is new Exercise (16_383, False);
begin
   Singleton;
   Small;
   Production;
   Put_Line ("PASS boot reservations:" & Cases'Image &
     " requests; first-fit oracle, exact bitmap/count/high-water and failure atomicity");
end Main;
