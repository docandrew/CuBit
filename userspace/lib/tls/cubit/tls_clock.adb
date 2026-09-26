with Interfaces; use Interfaces;
with CuBit.Clocks;
with Civil_Time;

package body TLS_Clock is
   function Now return X509.Date_Time is
      Stamp : CuBit.Clocks.Snapshot;
      OK : Boolean;
      Date : Civil_Time.Date_Time;
   begin
      CuBit.Clocks.Read (Stamp, OK);
      if not OK or else not CuBit.Clocks.Is_Valid_Wall_Time (Stamp.Quality) then
         return (others => 0);
      end if;
      --  CuBit.Clocks' civil fields are local time; X.509 needs UTC.
      Date := Civil_Time.Split (Integer_64 (Stamp.UTC_Seconds));
      return (Year => Date.Year, Month => Date.Month, Day => Date.Day,
              Hour => Date.Hour, Minute => Date.Minute, Second => Date.Second);
   end Now;
end TLS_Clock;
