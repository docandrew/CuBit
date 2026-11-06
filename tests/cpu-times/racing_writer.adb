pragma Ada_2022;
with CPU_Times_Publication; use CPU_Times_Publication;
with CPU_Times_Publication.Sample;

package body Racing_Writer is

   Versions : constant := 200_000;
   Reads    : constant := 2_000_000;

   Shared_Sequence   : Unsigned_64 := 0 with Atomic;
   Shared_Since      : Unsigned_64 := 0 with Atomic;
   Shared_Idle       : Unsigned_64 := 0 with Atomic;
   Shared_Thread     : Unsigned_64 := 0 with Atomic;
   Shared_Scheduler  : Unsigned_64 := 0 with Atomic;
   Shared_Dispatches : Unsigned_64 := 0 with Atomic;
   Writer_Done : Boolean := False with Atomic;
   Start : Boolean := False with Atomic;

   function Load_Sequence (CPU : Processor) return Unsigned_64 is
     (if CPU = 0 then Shared_Sequence else 0);
   function Load_Values (CPU : Processor) return Fields is
     (if CPU /= 0 then (others => <>) else
     ((State      => CPU_State'Enum_Rep (Running),
       Since      => Shared_Since,
       Totals     => (Idle_Ticks      => Shared_Idle,
                      Thread_Ticks    => Shared_Thread,
                      Scheduler_Ticks => Shared_Scheduler),
       Dispatches => Shared_Dispatches)));
   --  Behind every Since: the reader adds no interval in progress.
   function Load_Counter return Unsigned_64 is (0);

   procedure Read is new CPU_Times_Publication.Sample
     (Load_Sequence, Load_Values, Load_Counter);

   function Consistent (T : Times; Dispatches : Unsigned_64) return Boolean is
     (T.Thread_Ticks = 2 * T.Idle_Ticks and then T.Scheduler_Ticks = 3 * T.Idle_Ticks
      and then Dispatches = T.Idle_Ticks);

   task Writer;
   task body Writer is
      Spin : Unsigned_64 with Volatile;
   begin
      for Round in 1 .. 2 loop
         while not Start loop
            null;
         end loop;
         for V in 1 .. Unsigned_64 (Versions) loop
            Shared_Sequence := Opened (Shared_Sequence);
            Shared_Since := V;
            Shared_Idle := V;
            for I in 1 .. 20 loop
               Spin := Unsigned_64 (I);
            end loop;
            Shared_Thread := 2 * V;
            Shared_Scheduler := 3 * V;
            Shared_Dispatches := V;
            Shared_Sequence := Closed (Shared_Sequence);
         end loop;
         Start := False;
         Writer_Done := True;
      end loop;
   end Writer;

   procedure Run (Torn : out Unsigned_64; Stable : out Natural) is
      R : CPU_Reading;
   begin
      Torn := 0;
      Stable := 0;
      Writer_Done := False;
      Start := True;
      for I in 1 .. Reads loop
         Read (0, R);
         if R.Kind = Accounted_Times then
            Stable := Stable + 1;
            if not Consistent (R.Totals, R.Dispatches) then
               Torn := Torn + 1;
            end if;
         end if;
         exit when Writer_Done;
      end loop;
      while not Writer_Done loop
         null;
      end loop;
   end Run;

   procedure Run_Unprotected (Torn : out Unsigned_64) is
      F : Fields;
   begin
      Torn := 0;
      Writer_Done := False;
      Shared_Sequence := 0;
      Start := True;
      for I in 1 .. Reads loop
         F := Load_Values (0);
         if not Consistent (F.Totals, F.Dispatches) then
            Torn := Torn + 1;
         end if;
         exit when Writer_Done;
      end loop;
      while not Writer_Done loop
         null;
      end loop;
   end Run_Unprotected;

end Racing_Writer;
