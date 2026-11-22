package body Desktop_Startup_Progress is
 Value : Stage := Stage'First;
 procedure Enter(Point : Stage) is begin Value := Point; end;
 function Current return Stage is (Value);
end Desktop_Startup_Progress;
