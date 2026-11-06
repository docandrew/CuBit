pragma Ada_2022;
package body Llm_Reports with SPARK_Mode is
   use type P.Error_Kind;
   use type P.Problem;

   function Stop_Name (Value : P.Stop_Reason) return String is (P.Image (Value));

   function Error_Name (Value : P.Error_Kind) return String is (P.Image (Value));

   function Problem_Name (Value : P.Problem) return String is (P.Image (Value));

   function Redact (Text, Key : String) return String is
      Result : String (1 .. Maximum_Shown) := [others => ' '];
      Last : Natural range 0 .. Maximum_Shown := 0;
      --  The offset of the next character of Text to show.
      K : Natural := 0;
      procedure Put (C : Character) is
      begin
         if Last < Maximum_Shown then
            Last := Last + 1;
            Result (Last) := (if C in ' ' .. '~' then C else '?');
         end if;
      end Put;
      function Key_At (Offset : Natural) return Boolean is
        (Key'Length > 0 and then Offset < Text'Length and then Text'Length - Offset >= Key'Length
         and then (for all J in 0 .. Key'Length - 1 =>
                     Text (Text'First + Offset + J) = Key (Key'First + J)))
      with Pre => Offset <= Text'Length;
   begin
      while K < Text'Length loop
         pragma Loop_Variant (Increases => K);
         pragma Loop_Invariant (K < Text'Length);
         if Key_At (K) then
            for C of Redacted loop
               Put (C);
            end loop;
            K := K + Key'Length;
         else
            Put (Text (Text'First + K));
            K := K + 1;
         end if;
      end loop;
      return Result (1 .. Last);
   end Redact;

   Decimal_Digits : constant String (1 .. 10) := "0123456789";
   --  At most twenty digits.
   function Image (Value : Unsigned_64) return String
   with Post => Image'Result'Length in 1 .. 20
   is
      Result : String (1 .. 20) := [others => '0'];
      First : Positive range 1 .. 20 := 20;
      V : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Decimal_Digits (Natural (Unsigned_64'Min (V mod 10, 9)) + 1);
         V := V / 10;
         if V = 0 then
            First := I;
            exit;
         end if;
      end loop;
      return Result (First .. Result'Last);
   end Image;

   function Line (Facts : Exchange_Facts; Mission, Model, Key : String) return String is
      Result : String (1 .. Maximum_Line) := [others => ' '];
      Last : Natural range 0 .. Maximum_Line := 0;
      procedure Put (Text : String) is
      begin
         for C of Text loop
            exit when Last = Maximum_Line;
            Last := Last + 1;
            Result (Last) := C;
         end loop;
      end Put;
   begin
      Put ("llm: exchange ");
      Put (Image (Facts.Exchange));
      Put (" source ");
      Put (Image (Facts.Source));
      Put (" mission """);
      Put (Redact (Mission, Key));
      Put (""" model ");
      Put (Redact (Model, Key));
      Put (" stop ");
      Put (Stop_Name (Facts.Stop));
      if Facts.Error /= P.No_Error then
         Put (" error ");
         Put (Error_Name (Facts.Error));
      end if;
      if Facts.Fault /= P.No_Problem then
         Put (" problem ");
         Put (Problem_Name (Facts.Fault));
      end if;
      if Facts.Status /= 0 then
         Put (" status ");
         Put (Image (Unsigned_64 (Facts.Status)));
      end if;
      Put (" input ");
      Put (Image (Facts.Usage.Input));
      Put (" output ");
      Put (Image (Facts.Usage.Output));
      Put (" cache_read ");
      Put (Image (Facts.Usage.Cache_Read));
      Put (" cache_write ");
      Put (Image (Facts.Usage.Cache_Write));
      if Facts.Priced then
         Put (" cost_nano_usd ");
         Put (Image (Facts.Cost));
      else
         Put (" cost unpriced");
      end if;
      Put (" duration_ms ");
      Put (Image (Facts.Duration_Ms));
      Put (" text_bytes ");
      Put (Image (Facts.Text_Bytes));
      if Facts.Tool_Calls > 0 then
         Put (" tool_calls ");
         Put (Image (Unsigned_64 (Facts.Tool_Calls)));
      end if;
      return Result (1 .. Last);
   end Line;
end Llm_Reports;
