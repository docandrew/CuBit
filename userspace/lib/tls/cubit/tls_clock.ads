with X509;

--  SPARKTLS Get_Time callback for CuBit: the clock service's UTC, split into
--  civil fields. Returns an all-zero date (which fails validation closed)
--  when wall time is unavailable. A library-level function, so its access
--  value needs no trampoline.
--  The clock service is the program's "clock" startup entry.
package TLS_Clock is
   Clock_Binding : constant String := "clock";
   function Now return X509.Date_Time;
end TLS_Clock;
