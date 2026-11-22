with AML_Delays;
with AML_Namespace;
procedure Handle_Conversion is
   package N is new AML_Namespace (8, AML_Delays.Unavailable_Provider);
   package C1 is new N.Owned.Collecting;
   package C2 is new N.Owned.Collecting;
   H1 : C1.Value_Handle := C1.No_Value;
   H2 : C2.Value_Handle;
begin
   H2 := C2.Value_Handle (H1);
end Handle_Conversion;
