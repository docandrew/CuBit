with AML_Delays;
with AML_Namespace;
procedure Raw_Arena_Escape is
   package N is new AML_Namespace (8, AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   A : C.Arena;
   Success : Boolean;
begin
   N.Owned.Reset (A, Success);
end Raw_Arena_Escape;
