program Listen;
var
        n: Integer;
        k: Integer;
        i: Integer;
        target: Integer;
        list: array of Integer;
        sortedList: array of Integer;


procedure writeArray(data: array of Integer);
var
        j: Integer;
begin
    for j := 0 to Length(data)-1 do
    begin
        Write(data[j]);
        Write(' ');
    end;
    writeln();
end;

begin
        ReadLn(n, k);
        SetLength(list, n);
        for i:= 0 to n-1 do
        begin
                read(list[i]);
        end;
        //writeArray(list);
        setLength(sortedList, n);
        if k > n
                then k := k mod n;
        //WriteLn(k);
        for i := 0 to n-1 do
        begin
              target := i+k;
              if target > n-1
                then target := target - n;
              sortedList[i] := list[target];
        end;

        writeArray(sortedList);

        readln;

end.
