pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.TLS_Scopes; use CuBit.TLS_Scopes;

--  Linux-hosted checks for TLS scope parsing and matching (-gnata).
procedure Scopes_Main is
   S : Scope;
   OK : Boolean;
   Long_63 : constant String (1 .. 63) := [others => 'a'];
   function Parsed (Text : String) return Boolean is
      Item : Scope;
      Result : Boolean;
   begin
      Parse (Text, Item, Result);
      return Result;
   end Parsed;
begin
   --  Exact names.
   Parse ("tls-test.cubit.internal:443", S, OK);
   pragma Assert (OK and S.Kind = Exact and S.First_Port = 443 and S.Last_Port = 443);
   pragma Assert (Allows (S, "tls-test.cubit.internal", 443));
   pragma Assert (Allows (S, "TLS-Test.Cubit.Internal", 443)); -- case-insensitive
   pragma Assert (not Allows (S, "tls-test.cubit.internal", 444));
   pragma Assert (not Allows (S, "x.tls-test.cubit.internal", 443));
   pragma Assert (not Allows (S, "tls-test.cubit.internal.", 443));
   pragma Assert (not Allows (S, "tls-test.cubit.interna", 443));

   --  Port ranges.
   Parse ("tls-test.cubit.internal:18460-18463", S, OK);
   pragma Assert (OK and Allows (S, "tls-test.cubit.internal", 18460));
   pragma Assert (Allows (S, "tls-test.cubit.internal", 18463));
   pragma Assert (not Allows (S, "tls-test.cubit.internal", 18464));
   pragma Assert (not Allows (S, "tls-test.cubit.internal", 18459));

   --  Suffixes match at a label boundary and need at least one more label.
   Parse ("*.example.com:443", S, OK);
   pragma Assert (OK and S.Kind = Suffix);
   pragma Assert (Allows (S, "www.example.com", 443));
   pragma Assert (Allows (S, "a.b.example.com", 443));
   pragma Assert (not Allows (S, "example.com", 443));
   pragma Assert (not Allows (S, "badexample.com", 443));
   pragma Assert (not Allows (S, ".example.com", 443));
   pragma Assert (not Allows (S, "www.example.com.evil", 443));

   --  Any name, but never an IP literal.
   Parse ("*:443", S, OK);
   pragma Assert (OK and S.Kind = Any_Name and Allows (S, "anything.example", 443));
   pragma Assert (not Allows (S, "10.0.2.2", 443));
   pragma Assert (not Allows (S, "anything.example", 80));

   --  Name syntax.
   pragma Assert (Valid_Name ("localhost"));
   pragma Assert (Valid_Name (Long_63));             -- 63-char label
   pragma Assert (not Valid_Name (Long_63 & "a"));   -- 64-char label
   pragma Assert (not Valid_Name (""));
   pragma Assert (not Valid_Name ("a..b"));
   pragma Assert (not Valid_Name ("-a.b"));
   pragma Assert (not Valid_Name ("a-.b"));
   pragma Assert (not Valid_Name ("a.b-"));
   pragma Assert (not Valid_Name ("a_b.com"));
   pragma Assert (not Valid_Name ("10.0.2.2"));
   pragma Assert (not Valid_Name ("a.123"));
   pragma Assert (Valid_Name ("123.example"));
   pragma Assert (not Valid_Name ("a.b."));
   pragma Assert (not Valid_Name (Long_63 & ".bb")); -- 66 characters

   --  Rejected scopes.
   pragma Assert (not Parsed ("tls-test.cubit.internal"), "tls-test.cubit.internal");
   pragma Assert (not Parsed ("tls-test.cubit.internal:"), "tls-test.cubit.internal:");
   pragma Assert (not Parsed (":443"), ":443");
   pragma Assert (not Parsed ("*.com:443"), "*.com:443");
   pragma Assert (not Parsed ("*.:443"), "*.:443");
   pragma Assert (not Parsed ("**:443"), "**:443");
   pragma Assert (not Parsed ("*.*.example.com:443"), "*.*.example.com:443");
   pragma Assert (not Parsed ("Upper.example:443"), "Upper.example:443");
   pragma Assert (not Parsed ("x.example:0"), "x.example:0");
   pragma Assert (not Parsed ("x.example:65536"), "x.example:65536");
   pragma Assert (not Parsed ("x.example:0443"), "x.example:0443");
   pragma Assert (not Parsed ("x.example:500-400"), "x.example:500-400");
   pragma Assert (not Parsed ("x.example:1-"), "x.example:1-");
   pragma Assert (not Parsed ("x.example:-5"), "x.example:-5");
   pragma Assert (not Parsed ("x.example:1-2-3"), "x.example:1-2-3");
   pragma Assert (not Parsed ("10.0.2.2:443"), "10.0.2.2:443");
   pragma Assert (not Parsed ("x.example:44a"), "x.example:44a");
   pragma Assert (not Parsed ("x example:443"), "x example:443");
   pragma Assert (not Parsed ("x.example::443"), "x.example::443");
   pragma Assert (Parsed ("x.example:1-65535"));
   Ada.Text_IO.Put_Line
     ("TLS scopes: exact/suffix/any matching, label boundaries, ports, " &
      "name syntax, IP literals, malformed scopes PASS");
end Scopes_Main;
