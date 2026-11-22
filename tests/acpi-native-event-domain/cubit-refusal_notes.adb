with Endpoint_Test_Boundary;package body CuBit.Refusal_Notes is
 procedure Warn_Once (Service,Key,Text : String) is
 pragma Unreferenced(Service,Key,Text);
 begin Endpoint_Test_Boundary.Warnings:=Endpoint_Test_Boundary.Warnings+1;end Warn_Once;
end CuBit.Refusal_Notes;
