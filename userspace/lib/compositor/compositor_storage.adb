package body Compositor_Storage with SPARK_Mode is
   function Prefix (S : State; N : Natural) return Long_Long_Integer is
     (if N = 0 then 0 else Prefix (S, N - 1) + Long_Long_Integer (S.Items (Slot (N)).Size));
   procedure Establish_Zero (S : State) with Ghost,
     Pre => (for all I in Slot => S.Items (I).Size = 0),
     Post => Total (S) = 0
   is
   begin
      for I in Slot loop
         pragma Assert (Prefix (S, Natural (I)) = Prefix (S, Natural (I) - 1) + Long_Long_Integer (S.Items (I).Size));
         pragma Loop_Invariant (Prefix (S, Natural (I)) = 0);
      end loop;
   end Establish_Zero;
   procedure Establish_Change (Before, After : State; Changed : Slot) with Ghost,
     Pre => (for all I in Slot => (if I /= Changed then After.Items (I).Size = Before.Items (I).Size)),
     Post => Total (After) = Total (Before) - Long_Long_Integer (Before.Items (Changed).Size) +
        Long_Long_Integer (After.Items (Changed).Size)
   is
   begin
      for I in Slot loop
         pragma Assert (Prefix (After, Natural (I)) = Prefix (After, Natural (I) - 1) + Long_Long_Integer (After.Items (I).Size));
         pragma Assert (Prefix (Before, Natural (I)) = Prefix (Before, Natural (I) - 1) + Long_Long_Integer (Before.Items (I).Size));
         pragma Loop_Invariant
           (Prefix (After, Natural (I)) = Prefix (Before, Natural (I)) +
              (if Changed <= I then Long_Long_Integer (After.Items (Changed).Size) -
                 Long_Long_Integer (Before.Items (Changed).Size) else 0));
      end loop;
   end Establish_Change;
   function Open (Byte_Limit : Natural) return State is
      Result : constant State := (Capacity => Byte_Limit, others => <>);
   begin
      Establish_Zero (Result);
      return Result;
   end Open;
   procedure Reserve (S : in out State; Size : Positive; T : out Ticket) is
      Before : constant State := S with Ghost;
   begin
      T := No_Ticket;
      if S.Serial = Last_Identity or Size > S.Capacity - S.Used then return; end if;
      for I in Slot loop
         if S.Items (I).Stage = Free then
            S.Serial := S.Serial + 1;
            S.Items (I) := (S.Serial, Size, Allocating);
            S.Used := S.Used + Size;
            Establish_Change (Before, S, I);
            T := (I, S.Serial);
            return;
         end if;
      end loop;
   end Reserve;
   procedure Allocated (S : in out State; T : Ticket; Success : Boolean) is
      Before : constant State := S with Ghost;
   begin
      S.Items (T.Position).Stage := (if Success then Live else Quarantined);
      Establish_Change (Before, S, T.Position);
   end Allocated;
   procedure Begin_Release (S : in out State; T : Ticket; Readers_Retired : Boolean) is
      Before : constant State := S with Ghost;
   begin
      if Readers_Retired then S.Items (T.Position).Stage := Releasing; end if;
      Establish_Change (Before, S, T.Position);
   end Begin_Release;
   procedure Released (S : in out State; T : Ticket; Confirmed : Boolean) is
      Refund : constant Natural := S.Items (T.Position).Size;
      Before : constant State := S with Ghost;
   begin
      if Confirmed then
         S.Items (T.Position).Size := 0;
         Establish_Change (Before, S, T.Position);
         -- The remaining exact sum is nonnegative, so refund cannot underflow.
         S.Used := S.Used - Refund;
         S.Items (T.Position).Stage := Free;
      else
         S.Items (T.Position).Stage := Quarantined;
      end if;
      Establish_Change (Before, S, T.Position);
   end Released;
end Compositor_Storage;
