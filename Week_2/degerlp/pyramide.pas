PROGRAM Pyramide;
USES Crt;

VAR     InputFile: Text;
        CurrentLine: String;
        Path: String;
        LineCounter: Integer;

BEGIN
        path := 'Pyramide.txt';
        Assign(InputFile, Path);
        LineCounter := 0;

        REPEAT
                ReadLn(InputFile, CurrentLine);
                LineCounter := LineCounter + 1;
        UNTIL EOF (InputFile);
        Close(InputFile);
        WriteLn(LineCounter);
        ReadKey;


END.