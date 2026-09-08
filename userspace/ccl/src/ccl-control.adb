with CCL.Language;
with CCL.Interfaces.Clock;

package body CCL.Control with SPARK_Mode is
   use Standard.Interfaces;
   use type CCL.Language.Interpretation_Status;

   procedure Execute
     (Session : in out CCL.Sessions.Session; Op : Operation;
      Source : String; Host : Observation; Result : out Response)
   is
      Overflow : Boolean := False;
      Outcome : CCL.Language.Interpretation_Result;
      procedure Put (Text : String) is
      begin
         if Text'Length > Max_Response - Result.Length then
            Overflow := True;
         else
            Result.Data (Result.Length + 1 .. Result.Length + Text'Length) := Text;
            Result.Length := Result.Length + Text'Length;
         end if;
      end Put;
      procedure Quote (Text : String) is
         Hex : constant String := "0123456789abcdef";
      begin
         Put ("""");
         for C of Text loop
            case C is
               when '"' | '\' => Put (String'('\', C));
               when ' ' .. '!' | '#' .. '[' | ']' .. '~' => Put (String'(1 => C));
               when others =>
                  Put ("\u00" & Hex (Character'Pos (C) / 16 + 1) &
                       Hex (Character'Pos (C) mod 16 + 1));
            end case;
         end loop;
         Put ("""");
      end Quote;
      procedure Number (N : Unsigned_64) is
         Text : constant String := Unsigned_64'Image (N);
      begin
         --  Decimal string: preserve full-width integer precision in JS.
         Quote (Text (Text'First + 1 .. Text'Last));
      end Number;
      procedure Clock_Value is
      begin
         Put ("{""available"":");
         Put ((if Host.Clock_Available then "true" else "false"));
         Put (",""monotonicMs"":");
         if Host.Clock_Available then Number (Host.Monotonic_Ms); else Put ("null"); end if;
         Put ("}");
      end Clock_Value;
   begin
      Result := (others => <>);
      case Op is
         when Inspect_Bindings =>
            Put ("{""protocol"":1,""scope"":""adapter-bindings-only"",""processId"":");
            Number (Host.Process_Id);
            Put (",""networkProcessId"":"); Number (Host.Network_Process);
            Put (",""clockProcessId"":"); Number (Host.Clock_Process);
            Put (",""transport"":""plaintext-development-relay"",""peerAuthenticated"":false,");
            Put ("""clock"":"); Clock_Value;
            Put (",""interface"":{""name"":""clock"",""version"":""1.0"", ");
            Put ("""source"":""bundled descriptor; not live advertisement or signature verification"",");
            Put ("""digestWords"": [");
            for I in CCL.Interfaces.Clock.DESCRIPTOR_DIGEST'Range loop
               if I /= CCL.Interfaces.Clock.DESCRIPTOR_DIGEST'First then Put (","); end if;
               Number (CCL.Interfaces.Clock.DESCRIPTOR_DIGEST (I));
            end loop;
            Put ("],""operation"":""clock.monotonic-ms"",""parameters"":[],");
            Put ("""result"":{""kind"":""Integer"",""bits"":64,""unit"":""ms""},");
            Put ("""authority"":""observe"",""bindingEvidence"":""native endpoint call result""}}");
         when Read_Clock =>
            Put ("{""protocol"":1,""clock"":"); Clock_Value; Put ("}");
         when Evaluate_Expression =>
            CCL.Sessions.Submit (Session, Source, CCL.Sessions.Default_Fuel, Outcome);
            Put ("{""protocol"":1,""ok"":");
            Put ((if Outcome.Status = CCL.Language.Succeeded then "true" else "false"));
            Put (",""message"":"); Quote (CCL.Sessions.Result_Image (Outcome));
            Put (",""type"":");
            Quote ((case CCL.Sessions.Result_Type (Outcome) is
               when CCL.Language.Integer_Type => "Integer",
               when CCL.Language.Boolean_Type => "Boolean",
               when CCL.Language.String_Type => "String",
               when CCL.Language.Character_Type => "Character",
               when CCL.Language.Invalid_Type => "Diagnostic"));
            Put (",""position"":"); Number (Unsigned_64 (Outcome.Diagnostic_Position));
            Put (",""fuelRemaining"":"); Number (Unsigned_64 (Outcome.Fuel_Remaining));
            Put ("}");
      end case;
      if Overflow then
         Result := (others => <>);
         Put ("{""protocol"":1,""ok"":false,""message"":""Response budget exceeded""}");
      end if;
   end Execute;
end CCL.Control;
