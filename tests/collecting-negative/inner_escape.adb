with AML_Delays;
with AML_Namespace;
procedure Inner_Escape is
   package N is new AML_Namespace (8, AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   A : C.Arena;
   Success : Boolean;
begin
   N.Owned.Reset (A.Inner, Success);
end Inner_Escape;
