with Load_Work;
procedure CPU_Control is
begin
   Load_Work (12_000, CPU_Control => True);
end CPU_Control;
