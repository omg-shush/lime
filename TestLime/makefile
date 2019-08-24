
LC=dotnet run -p ../limec/limec.fsproj --
LFLAGS=--target=ast

all: Bool.ast Integer.ast List.ast Map.ast Test.ast

clean:
    del Bool.ast
    del Integer.ast
    del List.ast
    del Map.ast
    del Test.ast

Bool.ast: Bool.lime
    $(LC) $(LFLAGS) "Bool.lime -> $@"

Integer.ast: Integer.lime
    $(LC) $(LFLAGS) "Integer.lime -> $@"

List.ast: List.lime
    $(LC) $(LFLAGS) "List.lime -> $@"

Map.ast: Map.lime List.ast
    $(LC) $(LFLAGS) "Map.lime -> $@"

Test.ast: Test.lime Bool.ast Integer.ast List.ast Map.ast
    $(LC) $(LFLAGS) "Test.lime -> $@"
