--  supervision-child (tests/supervision, headless test supervision): logs
--  exactly the typed parameters it decoded, then exits with the code its
--  mode names, or lingers (sleeping) until procmgr reaps it.
with CuBit.Console_Route;
with CuBit.Waits;
with Ada.Command_Line;
with Interfaces;
with CuBit.Messages;
with Parameters;
with Startup;
pragma Unreferenced (Startup);

procedure Main is
   use Parameters;
   P : constant Child_Parameters_Value := Value;
   Linger_Step_Ms : constant := 1_000;
   Ignore : Interfaces.Unsigned_64;
   function Image (N : Long_Long_Integer) return String is
      Text : constant String := Long_Long_Integer'Image (N);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   CuBit.Messages.debugPrint
     ("supervision-child: decoded label=" &
      (case P.label is when First => "First", when Lingering => "Lingering") &
      " count=" & Image (Long_Long_Integer (P.count)) & " mode=" &
      (case P.mode.Choice is
          when Exit_With => "Exit_With " & Image (Long_Long_Integer (P.mode.Exit_With_Payload)),
          when Linger => "Linger") & ASCII.LF);
   case P.mode.Choice is
      when Exit_With =>
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (P.mode.Exit_With_Payload));
      when Linger =>
         CuBit.Messages.debugPrint ("supervision-child: lingering until its supervisor ends" & ASCII.LF);
         loop
            Ignore := CuBit.Waits.Sleep_For (Linger_Step_Ms);
         end loop;
   end case;
end Main;
