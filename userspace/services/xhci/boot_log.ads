with Interfaces;
package Boot_Log is
   -- Temporarily quiet while collecting Intel GPU bring-up diagnostics.
   Enabled : constant Boolean := False;
   procedure Write (Text : String);
   procedure Poll;
   function Deadline return Interfaces.Unsigned_64;
end Boot_Log;
