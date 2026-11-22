with Interfaces;
package ACPI_Requests is
 type State is record Marker:Natural:=0;end record;
 function Valid(S:State) return Boolean is (S.Marker=0);
 type Outcome is (OK,Denied,Malformed,Stale,Wrong_Order,Resource_Limit,Table_Rejected,Incomplete,Not_Found,Wrong_Table_Kind,Index_Out_Of_Range,Unsupported_Record_Kind);
 type Words is array(Natural range 0..3) of Interfaces.Unsigned_64;
 type Response is record Status:Outcome:=Malformed;Data:Words:=[others=>0];end record;
 type Packet is record Label:Interfaces.Unsigned_32:=0;Length:Interfaces.Unsigned_8:=4;Data:Words:=[others=>0];end record;
end ACPI_Requests;
