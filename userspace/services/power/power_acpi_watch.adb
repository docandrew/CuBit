pragma Ada_2022;
package body Power_ACPI_Watch with SPARK_Mode is

   procedure Decide
     (Label : Unsigned_32; Length : Natural; Reply : Words; ID : Word; Seen : Generation;
      Next : out Step; New_Seen : out Generation; Why : out Status; Answered : out Boolean)
   is
   begin
      Next := Give_Up;
      New_Seen := Seen;
      Why := Malformed;
      Answered := False;
      if Label /= Watch_Label or else Length /= Request_Words or else Reply (ID_Word) /= ID
        or else Reply (Schema_Word) /= Schema or else not Valid_Status (Reply (Status_Word))
        or else Reply (Generation_Word) > Maximum_Generation
      then
         return;
      end if;
      Why := Status'Val (Reply (Status_Word));
      if Why = Changed and then Reply (Generation_Word) <= Seen then
         --  A Changed that does not move on is not one acpi.svc writes.
         Why := Malformed;
         return;
      end if;
      Answered := True;
      case Why is
         when Changed =>
            Next := Read_Then_Watch;
            New_Seen := Reply (Generation_Word);
         when Superseded =>
            Next := Watch_Again;
         when Unavailable | Full =>
            Next := Retry_Later;
         when Future_Generation =>
            Next := Resynchronize;
            New_Seen := 0;
         when Stopping | Exhausted | Denied | Malformed =>
            Next := Give_Up;
      end case;
   end Decide;
end Power_ACPI_Watch;
