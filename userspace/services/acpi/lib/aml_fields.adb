pragma Ada_2022;
package body AML_Fields with SPARK_Mode is
   use AML_Decode;
   use type AML_Names.Parse_Status;
   function Read_Entry (Data : Bytes) return Entry_Result is
      subtype Failure_Status is AML_Decode.Status range Truncated .. Limit_Exceeded;
      function Failure (Status : Failure_Status) return Entry_Result is
        ((Status => Status));
      Prefix : Byte;
      Offset : Positive;
      Part : AML_Names.Segment := "____";
      Length : Field_Length_Result;
   begin
      if Data'Length = 0 then return (Status => Truncated); end if;
      Prefix := Data (Data'First);
      case Prefix is
         when 0 => Offset := 1;
         when 1 | 3 =>
            if Data'Length < (if Prefix = 1 then 3 else 4) then
               return (Status => Truncated);
            end if;
            return (Status => Accepted,
                    Kind => (if Prefix = 1 then Access_Field else Extended_Access_Field),
                    Consumed => (if Prefix = 1 then 3 else 4),
                    Access_Type => Data (Data'First + 1),
                    Attribute => Data (Data'First + 2),
                    Access_Length => (if Prefix = 3 then Data (Data'First + 3) else 0),
                    others => <>);
         when 2 =>
            if Data'Length = 1 then return (Status => Truncated); end if;
            if Data (Data'First + 1) = 16#11# then
               if Data'Length = 2 then return (Status => Truncated); end if;
               declare
                  P : constant Package_Result :=
                    Read_Package (Data (Data'First + 2 .. Data'Last));
               begin
                  if P.Kind /= Accepted then return Failure (P.Kind); end if;
                  -- BufferSize TermArg must exist; its value is evaluated later.
                  if P.Extent = P.Encoding_Bytes then return (Status => Malformed); end if;
                  return (Status => Accepted, Kind => Buffer_Connection,
                          Consumed => P.Extent + 2, others => <>);
               end;
            else
               declare
                  N : constant AML_Names.Name_Result :=
                    AML_Names.Read_Name (Data (Data'First + 1 .. Data'Last));
               begin
                  case N.Kind is
                     when AML_Names.Accepted =>
                        return (Status => Accepted, Kind => Name_Connection,
                                Consumed => N.Consumed + 1, others => <>);
                     when AML_Names.Truncated => return (Status => Truncated);
                     when AML_Names.Malformed => return (Status => Malformed);
                     when AML_Names.Limit_Exceeded => return (Status => Limit_Exceeded);
                  end case;
               end;
            end if;
         when others =>
            if not AML_Names.Lead (Prefix) then return (Status => Malformed); end if;
            if Data'Length < 4 then return (Status => Truncated); end if;
            for I in Part'Range loop
               Part (I) := Character'Val (Data (Data'First + (I - 1)));
               pragma Loop_Invariant
                 (for all J in 1 .. I => Character'Pos (Part (J)) =
                    Natural (Data (Data'First + (J - 1))));
            end loop;
            if not AML_Names.Valid (Part) then return (Status => Malformed); end if;
            Offset := 4;
      end case;
      if Data'Length <= Offset then return (Status => Truncated); end if;
      Length := Read_Field_Length (Data (Data'First + Offset .. Data'Last));
      if Length.Kind /= Accepted then return Failure (Length.Kind); end if;
      return (Status => Accepted,
              Kind => (if Prefix = 0 then Reserved_Field else Named_Field),
              Consumed => Offset + Length.Encoding_Bytes,
              Name => Part, Bits => Length.Bits, others => <>);
   end Read_Entry;
end AML_Fields;
