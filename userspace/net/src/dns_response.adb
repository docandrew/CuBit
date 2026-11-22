------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body DNS_Response with SPARK_Mode is

   --  Past the name at Pos: labels up to a zero, or a two-byte pointer.
   procedure Skip_Name (Message : Bytes; Pos : in out Natural; OK : out Boolean) with
     Pre  => Message'First = 0 and then Message'Length <= Maximum_Message and then
             Pos <= Message'Length,
     Post => Pos <= Message'Length and then Pos >= Pos'Old
   is
      Length : Unsigned_8;
   begin
      OK := False;
      loop
         pragma Loop_Invariant (Pos <= Message'Length and then Pos >= Pos'Loop_Entry);
         pragma Loop_Variant (Increases => Pos);
         if Pos >= Message'Length then
            return;
         end if;
         Length := Message (Pos);
         Pos := Pos + 1;
         if Length = 0 then
            OK := True;
            return;
         elsif (Length and Pointer_Bits) = Pointer_Bits then
            if Pos >= Message'Length then
               return;
            end if;
            Pos := Pos + 1;
            OK := True;
            return;
         elsif Length > Maximum_Label or else Natural (Length) > Message'Length - Pos then
            return;
         end if;
         Pos := Pos + Natural (Length);
      end loop;
   end Skip_Name;

   procedure Parse (Message : Bytes; R : out Response; OK : out Boolean) is
      Off   : Natural;
      Valid : Boolean;
      Data  : Natural;
   begin
      R := (others => <>);
      OK := False;
      if Message'Length < Header_Size or else
        (U16 (Message, 2) and Response_Flag) = 0 or else U16 (Message, 4) /= 1
      then
         return;
      end if;
      R.Id := U16 (Message, 0);
      R.Rcode := Response_Code (U16 (Message, 2) and Rcode_Bits);
      Off := Header_Size;
      DNS_Name.Read_Name (Message, Off, R.Name, R.Name_Length, Valid);
      if not Valid or else Off > Message'Length - Question_Tail or else
        U16 (Message, Off) /= Type_A or else U16 (Message, Off + 2) /= Class_IN
      then
         return;
      end if;
      Off := Off + Question_Tail;
      OK := True;
      for Answer in 1 .. Natural (U16 (Message, 6)) loop
         pragma Loop_Invariant
           (Off >= Header_Size and then Off <= Message'Length and then
            not R.Has_Address and then R.Id = U16 (Message, 0) and then
            R.Rcode = Response_Code (U16 (Message, 2) and Rcode_Bits) and then
            R.Name_Length >= 1);
         Skip_Name (Message, Off, Valid);
         if not Valid or else Message'Length - Off < Answer_Fixed then
            return;
         end if;
         Data := Natural (U16 (Message, Off + Answer_Fixed - 2));
         if Data > Message'Length - Off - Answer_Fixed then
            return;
         end if;
         if U16 (Message, Off) = Type_A and then U16 (Message, Off + 2) = Class_IN and then
           Data = IPv4_Length
         then
            R.Address_At := Off + Answer_Fixed;
            R.Address := [for K in IPv4'Range => Message (R.Address_At + K)];
            R.Has_Address := True;
            return;
         end if;
         Off := Off + Answer_Fixed + Data;
      end loop;
   end Parse;

end DNS_Response;
