pragma Ada_2022;

procedure Clock_Publication.Sample
  (Nanoseconds : out Unsigned_64; Success : out Boolean)
with SPARK_Mode
is
   Before   : Unsigned_64;
   Snapshot : Parameters;
   Now      : Unsigned_64;
begin
   for Attempt in Read_Attempt loop
      Before := Sequence;
      if not Writing (Before) then
         Snapshot := Fields;
         Now := Counter;
         if Stable (Before, Sequence) then
            Convert (Snapshot, Now, Nanoseconds, Success);
            return;
         end if;
      end if;
   end loop;
   Nanoseconds := 0;
   Success := False;
end Clock_Publication.Sample;
